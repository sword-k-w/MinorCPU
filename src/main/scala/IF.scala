import chisel3._
import chisel3.util._


// Instruction Fetcher
class IF extends Module {
  val io = IO(new Bundle {
    // TODO : some input ports for ready signals

    val quest = Output(UInt(32.W)) // PC
    // always sends quest to ICache

    val quest_result = Flipped(Valid(UInt(32.W)))
    val quest_result2 = Flipped(Valid(UInt(32.W))) // data from memory

    val instruction = Decoupled(new Instruction)

    val modified_pc = Flipped(Valid(UInt(32.W)))
  })

  val pc = RegInit(0.U(32.W))

  // printf("(IF) : pc = %d\n", pc)
  // printf("     : io.quest_result.valid = %d\n", io.quest_result.valid)
  // printf("     : io.quest_result.bits = %d\n", io.quest_result.bits)
  // printf("     : io.quest_result2.valid = %d\n", io.quest_result2.valid)
  // printf("     : io.quest_result2.bits = %d\n", io.quest_result2.bits)

  val raw_instruction = Mux(io.quest_result.valid, io.quest_result.bits, io.quest_result2.bits)
  val op = raw_instruction(6, 2)
  io.instruction.bits.op := op
  io.instruction.bits.rd := raw_instruction(11, 7)
  io.instruction.bits.rs1 := raw_instruction(19, 15)
  io.instruction.bits.rs2 := raw_instruction(24, 20)
  io.instruction.bits.funct := 0.U
  io.instruction.bits.immediate := 0.U
  io.instruction.bits.predict_address := 0.U
  io.instruction.bits.for_jalr := 0.U

  when (op === "b01100".U) { // R
    io.instruction.bits.funct := raw_instruction(30) ## raw_instruction(14, 12)
  } .elsewhen (op === "b00100".U) { // IA
    when (raw_instruction(14, 12) === "b101".U) {
      io.instruction.bits.funct := raw_instruction(30) ## raw_instruction(14, 12)
      io.instruction.bits.immediate := raw_instruction(24, 20)
    } .otherwise {
      io.instruction.bits.funct := raw_instruction(14, 12)
      io.instruction.bits.immediate := raw_instruction(31, 20).asSInt.pad(32).asUInt
    }
  } .elsewhen (op === "b00000".U) { // IM
    io.instruction.bits.funct := raw_instruction(14, 12)
    io.instruction.bits.immediate := raw_instruction(31, 20).asSInt.pad(32).asUInt
  } .elsewhen (op === "b11001".U) { // jalr
    io.instruction.bits.immediate := pc + 4.U
    io.instruction.bits.for_jalr := raw_instruction(31, 20).asSInt.pad(32).asUInt
  } .elsewhen (op === "b01000".U) { // S
    io.instruction.bits.funct := raw_instruction(14, 12)
    io.instruction.bits.immediate := (raw_instruction(31, 25) ## raw_instruction(11, 7)).asSInt.pad(32).asUInt
  } .elsewhen (op === "b11000".U) { // B
    io.instruction.bits.funct := raw_instruction(14, 12)
    io.instruction.bits.immediate := pc + ((raw_instruction(31) ## raw_instruction(7) ## raw_instruction(30, 25)
      ## raw_instruction(11, 8) ## 0.U(1.W)).asSInt.pad(32).asUInt) // no prediction now
  } .elsewhen (op === "b11001".U) { // J
    io.instruction.bits.immediate := pc + 4.U
  } .elsewhen (op === "b00101".U) { // auipc
    io.instruction.bits.immediate := (raw_instruction(31, 20) ## 0.U(12.W)) + pc
  } .otherwise { // lui
    io.instruction.bits.immediate := raw_instruction(31, 20) ## 0.U(12.W)
  }

  when (io.modified_pc.valid) {
    pc := io.modified_pc.bits
    io.quest := io.modified_pc.bits
    io.instruction.valid := false.B
  } .elsewhen (io.quest_result.valid || io.quest_result2.valid) {
    io.instruction.valid := true.B

    val new_pc = Wire(UInt(32.W))
    when (io.instruction.fire) {
      new_pc := pc + 4.U
      when (op === "b11001".U) {
        new_pc := (raw_instruction(31) ## raw_instruction(19, 12) ## raw_instruction(20)
          ## raw_instruction(30, 21) ## 0.U(1.W)).asSInt.pad(32).asUInt
      }
    } .otherwise {
      new_pc := pc
    }

    pc := new_pc
    io.quest := new_pc
    io.instruction.bits.predict_address := new_pc(31, 2)
  } .otherwise {
    io.quest := pc
    io.instruction.valid := false.B
  }
}