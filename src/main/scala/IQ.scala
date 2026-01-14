import chisel3._
import chisel3.util._

class Instruction extends Bundle {
  val rd = UInt(5.W)
  val rs1 = UInt(5.W)
  val rs2 = UInt(5.W)
  val op = UInt(5.W) // use first five bits
  val funct = UInt(4.W) // funct7[1] | funct3
  val immediate = UInt(32.W)
  val predict_address = UInt(30.W) // for jalr and branch, store the speculation result to check whether need to restart in RoB
  val for_jalr = UInt(12.W)
  val mmio = Bool()
}

// instruction queue
class IQ extends Module {
  val io = IO(new Bundle {
    val predict_failed = Input(Bool())

    val new_instruction = Flipped(Decoupled(new Instruction)) // from IF

    val instruction_to_rob = Decoupled(new Instruction)

    val instruction_to_rs = Valid(new Instruction)

    val instruction_to_lsq = Decoupled(new Instruction)

    val new_dependence_valid = Output(Bool())
    val new_reg_id = Output(UInt(5.W))
  })

  val head = RegInit(0.U(5.W))
  val tail = RegInit(0.U(5.W))
  val entry = Reg(Vec(32, new Instruction))

  val new_head = Wire(UInt(5.W))
  val new_tail = Wire(UInt(5.W))
  val new_entry = Wire(Vec(32, new Instruction))

  val issue_instruction = Reg(new Instruction)
  val issue_instruction_valid = RegInit(false.B)

  val new_dependence_valid = RegInit(false.B)
  val new_reg_id = RegInit(0.U(5.W))

  issue_instruction_valid := false.B
  new_dependence_valid := false.B

  when (io.predict_failed) {
    new_head := 0.U
    new_tail := 0.U
    for (i <- 0 until 32) {
      new_entry(i) := entry(i)
    }
  } .otherwise {
    new_tail := Mux(io.new_instruction.valid, tail + 1.U, tail)
    for (i <- 0 until 32) {
      when (i.U === tail && io.new_instruction.valid) {
        new_entry(i) := io.new_instruction.bits
      } .otherwise {
        new_entry(i) := entry(i)
      }
    }
    when (head =/= new_tail && io.instruction_to_rob.ready && io.instruction_to_lsq.ready) {
      issue_instruction_valid := true.B
      issue_instruction := new_entry(head)
      new_reg_id := new_entry(head).rd
      new_dependence_valid := new_entry(head).op =/= "b01000".U && new_entry(head).op =/= "b11000".U
      new_head := head + 1.U
    } .otherwise {
      new_head := head
    }
  }

  io.instruction_to_rob.valid := issue_instruction_valid
  io.instruction_to_rob.bits := issue_instruction
  io.instruction_to_rs.valid := issue_instruction_valid
  io.instruction_to_rs.bits := issue_instruction
  io.instruction_to_lsq.valid := issue_instruction_valid &&
    (issue_instruction.op === "b00000".U || issue_instruction.op === "b01000".U)
  io.instruction_to_lsq.bits := issue_instruction
  io.new_dependence_valid := new_dependence_valid
  io.new_reg_id := new_reg_id

  head := new_head
  tail := new_tail
  for (i <- 0 until 32) {
    entry(i) := new_entry(i)
  }
  io.new_instruction.ready := new_tail + 1.U =/= new_head
}
