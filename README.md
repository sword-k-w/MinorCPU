# Minor CPU

## Contribution

### livandan

- Data Cache (DCache.scala)
- Memory Arbiter (MA.scala)
- Branch Predictor (Predictor.scala)
- handle lbu/lb, lhu/lh
- most part of unit test
- Interpreter (not finished because not needed finally)
- part of MMIO (frontend)

### sword

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

**Always use big endian in README!** 

eg: 0010 means 2th-low bit is 1.

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