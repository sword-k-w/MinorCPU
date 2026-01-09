import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class LSQSpec extends AnyFlatSpec with ChiselScalatestTester {
  class TestLSQ extends Module {
    val io = IO(new Bundle {
      val predict_failed = Input(Bool())
      val rob_tail = Input(UInt(5.W))
      val new_instruction = Flipped(Valid(new Instruction))

      val rob_broadcast_result = Flipped(Valid(new RoBBroadcastResult))
      val alu_broadcast_result = Flipped(Valid(new ALUBroadcastResult))

      val broadcast_to_rs = Valid(new ToRSResult)
      val broadcast_to_rob = Valid(new LSQToRoBResult)

      val memory_quest_from_lsq = Valid(new MemoryQuest)
      val memory_result_for_lsq = Flipped(Valid(UInt(32.W)))

      val memory_quest_from_wb = Valid(new MemoryQuest)
      val memory_result_for_wb = Flipped(Valid(UInt(32.W)))

      val lsq_is_full = Output(UInt(5.W))
    })
    val lsq = Module(new LSQ)
    val wb = Module(new WB)
    lsq.io.wb_is_full <> wb.io.is_full
    lsq.io.wb_is_empty <> wb.io.is_empty
    lsq.io.store_to_wb <> wb.io.new_instruction
    wb.io.memory_quest <> io.memory_quest_from_wb
    wb.io.memory_result <> io.memory_result_for_wb
    lsq.io.predict_failed <> io.predict_failed
    lsq.io.rob_tail <> io.rob_tail
    lsq.io.new_instruction <> io.new_instruction
    lsq.io.rob_broadcast_result <> io.rob_broadcast_result
    lsq.io.alu_broadcast_result <> io.alu_broadcast_result
    lsq.io.broadcast_to_rs <> io.broadcast_to_rs
    lsq.io.broadcast_to_rob <> io.broadcast_to_rob
    lsq.io.memory_quest <> io.memory_quest_from_lsq
    lsq.io.memory_result <> io.memory_result_for_lsq
    lsq.io.is_full <> io.lsq_is_full
  }

  "LSQ" should "pass" in {
    test(new TestLSQ) { dut =>

    }
  }
}
