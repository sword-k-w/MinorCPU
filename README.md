# Minor CPU

## Contribution

### livandan (Wenzhen Li)

- Data Cache (DCache.scala)
- Memory Arbiter (MA.scala)
- Branch Predictor (Predictor.scala)
- handle lbu/lb, lhu/lh
- most part of unit test
- Interpreter (not finished because not needed finally)
- part of MMIO (frontend)

### sword (Jianhao Wang)

- Instruction Fetcher (IF.scala)
- Instruction Queue (IQ.scala)
- Instruction Cache (ICache.scala)
- Register File (RF.scala)
- Reorder Buffer (RoB.scala)
- Reservation Station (RS.scala)
- Arithmetic Logical Unit (ALU.scala)
- Load Store Queue (LSQ.scala)
- Write Buffer (WB.scala)
- Return Address Stack (RAS.scala)
- part of MMIO (backend)

## How to Get Verilog Version of Our Code

The main part of our cpu is written in Chisel. To get verilog version, run `sbt run` at root directory. `CPU.v` will generated in folder `generated`.

## Architecture

### Frontend

**components** : Instruction Fetcher, Instruction Queue, Instruction Cache, Data Cache, Memory Arbiter, Branch Predictor, Return Address Stack

Frontend interacts with memory, including fetching instructions and handling memory access operations, and decodes instructions.

#### Instruction Fetcher(IF)

It contains a PC(reg) in it and constantly sends quest to ICache to get next instruction. When fetching a valid (raw) instruction, IF decodes it and queries Predictor or RAS to get next PC if it is a branch or jalr. 

If the new instruction is valid and IQ is ready, the new instruction will be sent to IQ and PC will be next PC in the next cycle.

#### Instruction Queue(IQ)

It is a buffer for instructions. It gets instruction from IF when not full and issues instructions to RF, RS, RoB and LSQ(if store or load).

#### Instruction Cache(ICache)

1-way. contains a state machine (represents idle or busy).

#### Data Cache(DCache)

1-way. contains a state machine. difference from ICache : the length of data can be 1/2/4 byte(s); supporting data write. 

#### Memory Arbiter (MA)

There is only one read port and write port in our CPU. Therefore, MA acts the role in deciding to process which memory quest from ICache and DCache and returning the results to them.

#### Branch Predictor (Predictor)

PC with same 2nd ~ 7th bits share a local predictor. Each local predictor stores corresponding branch history and each history pattern has a 2-bit saturating counter. Prediction result depends on the PC and its history pattern.

#### Return Address Stack (RAS)

It tracks the call sequence. jal pushes PC + 4 onto RAS and jalr asks the top of RAS.

### Backend

**components** : Register File, Reservation Station, Reorder Buffer, Arithmetic Logical Unit, Load Store Queue, Write Buffer

#### Register File (RF)

32 reg and dependence. Add dependence when IQ issues and eliminate dependence when RoB commits. 

#### Reservation Station (RS)

The size of RS is same as RoB. Thus, the id of new instruction is same as RoB. Each cycle, RS:

1. adds new instruction and eliminates dependence according to broadcasts from ALU, LSQ and WB.
2. selects a valid instruction and sends it to ALU.

#### Reorder Buffer (RoB)

Each cycle, RoB:

1. adds new instruction and sets ready state according to broadcasts from ALU, LSQ and WB.
2. commits the head instruction if it is ready.

#### Arithmetical Logical Unit (ALU)

calculate and send result to RS, RoB and LSQ.

#### Load Store Queue (LSQ)

Each cycle, LSQ:

1. adds new instruction and sets ready state according to broadcasts from ALU and RoB.
2. sends read quest (WB must be empty) to DCache or pushes the head instruction to WB if head instruction is ready (depending on the type of head instruction).

#### Write Buffer (WB)

sends write quest to DCache. Never reflush.

## How to Handle MMIO

- ALU:

  1. ALU calculates address and find it MMIO.

  2. ALU broadcasts the result to RoB and LSQ, and tell them it's a MMIO.

- RoB:

  1. Initially, RoB regards it as a normal load.

  2. When ALU broadcasts it isn't, RoB updates it state and pretends to commit it.

  3. When WB broadcasts the final result, RoB commit it.
  
- LSQ:

  1. Initially, LSQ regards it as a normal load.

  2. When ALU broadcasts is isn't, LSQ updates it state and regard it as store except some extra info (dest).

- WB:

  1. WB gets instruction from LSQ, and already knows it's mmio.

  2. WB sends quest to MA and broadcasts the result to RS and RoB.