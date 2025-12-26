import chisel3._
import chisel3.util._

class InstructionBundle extends Bundle {
  val instruction = UInt(32.W)
  val address     = UInt(32.W)
}

// Instruction Fetcher
class IF extends Module {
  val io = IO(new Bundle {
    // TODO : some input ports for ready signals

    val quest = Output(UInt(32.W)) // PC
    // always sends quest to ICache

    val quest_result = Flipped(Valid(UInt(32.W)))
    val quest_result2 = Flipped(Valid(UInt(32.W))) // data from memory

    val instruction = Decoupled(new InstructionBundle)

    val modified_pc = Flipped(Valid(UInt(32.W)))
  })

  val pc = RegInit(0.U(32.W))

  io.quest := pc

  // printf("(IF) : pc = %d\n", pc)
  // printf("     : io.quest_result.valid = %d\n", io.quest_result.valid)
  // printf("     : io.quest_result.bits = %d\n", io.quest_result.bits)
  // printf("     : io.quest_result2.valid = %d\n", io.quest_result2.valid)
  // printf("     : io.quest_result2.bits = %d\n", io.quest_result2.bits)

  val result_valid = io.quest_result.valid || io.quest_result2.valid

  when (io.modified_pc.valid) {
    pc := io.modified_pc.bits
    io.quest := io.modified_pc.bits
  } .elsewhen (result_valid && io.instruction.ready) {
    pc := pc + 4.U
    io.quest := pc + 4.U
  }

  io.instruction.bits.instruction := io.quest_result.bits
  when (io.quest_result2.valid) {
    io.instruction.bits.instruction := io.quest_result2.bits
  }
  io.instruction.bits.address := pc
  io.instruction.valid := result_valid
}