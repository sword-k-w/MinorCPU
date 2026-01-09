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

  "LSQ" should "handle single load instruction" in {
    test(new TestLSQ) { dut =>
      dut.io.predict_failed.poke(false.B)

      dut.io.new_instruction.bits.funct.poke(2.U)
      dut.io.new_instruction.bits.immediate.poke(0.U)
      dut.io.new_instruction.bits.rs2.poke(0.U)
      dut.io.new_instruction.bits.rs1.poke(0.U)
      dut.io.new_instruction.bits.rd.poke(0.U)
      dut.io.new_instruction.bits.predict_address.poke(0.U)

      dut.io.rob_broadcast_result.bits.value.poke(0.U)
      dut.io.rob_broadcast_result.bits.addr.poke(0.U)
      dut.io.rob_broadcast_result.bits.dest.poke(0.U)
      dut.io.rob_broadcast_result.valid.poke(false.B)

      dut.io.alu_broadcast_result.bits.dest.poke(0.U)
      dut.io.alu_broadcast_result.bits.addr.poke(0.U)
      dut.io.alu_broadcast_result.valid.poke(false.B)

      dut.io.memory_result_for_lsq.bits.poke(0.U)
      dut.io.memory_result_for_lsq.valid.poke(false.B)

      dut.io.memory_result_for_wb.bits.poke(0.U)
      dut.io.memory_result_for_wb.valid.poke(false.B)

      // Cycle 0 : add a load instruction
      dut.io.rob_tail.poke(0.U)
      dut.io.new_instruction.bits.op.poke("b00000".U)
      dut.io.new_instruction.valid.poke(true.B)

      dut.clock.step(1)

      dut.io.new_instruction.valid.poke(false.B)

      // Cycle 1 : address of load is finished

      dut.io.alu_broadcast_result.bits.dest.poke(0.U)
      dut.io.alu_broadcast_result.bits.addr.poke(114514.U)
      dut.io.alu_broadcast_result.valid.poke(true.B)

      // quest should be sent in next cycle
      dut.io.memory_quest_from_lsq.valid.expect(false.B)

      dut.clock.step(1)

      dut.io.alu_broadcast_result.valid.poke(false.B)

      // Cycle 2 : answer the memory quest
      dut.io.memory_quest_from_lsq.valid.expect(true.B)
      dut.io.memory_quest_from_lsq.bits.wr_en.expect(false.B)
      dut.io.memory_quest_from_lsq.bits.addr.expect(114514.U)

      dut.io.memory_result_for_lsq.valid.poke(true.B)
      dut.io.memory_result_for_lsq.bits.poke(1919810.U)

      // broadcast should be sent in next cycle
      dut.io.broadcast_to_rs.valid.expect(false.B)

      dut.clock.step(1)

      dut.io.memory_result_for_lsq.valid.poke(false.B)

      dut.io.broadcast_to_rs.valid.expect(true.B)
      dut.io.broadcast_to_rs.bits.value.expect(1919810.U)
      dut.io.broadcast_to_rs.bits.dest.expect(0.U)
    }
  }
  "LSQ" should "handle multiple load instruction" in {
    test(new TestLSQ) { dut =>
      dut.io.predict_failed.poke(false.B)

      dut.io.new_instruction.bits.funct.poke(2.U)
      dut.io.new_instruction.bits.immediate.poke(0.U)
      dut.io.new_instruction.bits.rs2.poke(0.U)
      dut.io.new_instruction.bits.rs1.poke(0.U)
      dut.io.new_instruction.bits.rd.poke(0.U)
      dut.io.new_instruction.bits.predict_address.poke(0.U)

      dut.io.rob_broadcast_result.bits.value.poke(0.U)
      dut.io.rob_broadcast_result.bits.addr.poke(0.U)
      dut.io.rob_broadcast_result.bits.dest.poke(0.U)
      dut.io.rob_broadcast_result.valid.poke(false.B)

      dut.io.alu_broadcast_result.bits.dest.poke(0.U)
      dut.io.alu_broadcast_result.bits.addr.poke(0.U)
      dut.io.alu_broadcast_result.valid.poke(false.B)

      dut.io.memory_result_for_lsq.bits.poke(0.U)
      dut.io.memory_result_for_lsq.valid.poke(false.B)

      dut.io.memory_result_for_wb.bits.poke(0.U)
      dut.io.memory_result_for_wb.valid.poke(false.B)

      // Cycle 0 : add a load instruction a
      dut.io.rob_tail.poke(0.U)
      dut.io.new_instruction.bits.op.poke("b00000".U)
      dut.io.new_instruction.valid.poke(true.B)

      dut.clock.step(1)

      // Cycle 1 : add a load instruction b
      dut.io.rob_tail.poke(1.U)
      dut.clock.step(1)

      // Cycle 2 : add a load instruction c and address of load b is finished
      dut.io.rob_tail.poke(2.U)
      dut.io.alu_broadcast_result.bits.dest.poke(1.U)
      dut.io.alu_broadcast_result.bits.addr.poke(10.U)
      dut.io.alu_broadcast_result.valid.poke(true.B)

      dut.clock.step(1)

      dut.io.alu_broadcast_result.valid.poke(false.B)

      // Cycle 3 : add a load instruction d
      dut.io.memory_quest_from_lsq.valid.expect(false.B) // load b must wait until load a is finished

      dut.io.rob_tail.poke(3.U)
      dut.clock.step(1)

      dut.io.new_instruction.valid.poke(false.B)

      // Cycle 4 : address of load d is finished

      dut.io.alu_broadcast_result.bits.dest.poke(3.U)
      dut.io.alu_broadcast_result.bits.addr.poke(20.U)
      dut.io.alu_broadcast_result.valid.poke(true.B)

      dut.clock.step(1)

      // Cycle 5 : address of load a is finished
      dut.io.memory_quest_from_lsq.valid.expect(false.B)

      dut.io.alu_broadcast_result.bits.dest.poke(0.U)
      dut.io.alu_broadcast_result.bits.addr.poke(5.U)

      dut.clock.step(1)

      dut.io.alu_broadcast_result.valid.poke(false.B)

      // Cycle 6 : nothing happened

      dut.io.memory_quest_from_lsq.valid.expect(true.B)
      dut.io.memory_quest_from_lsq.bits.wr_en.expect(false.B)
      dut.io.memory_quest_from_lsq.bits.addr.expect(5.U)

      dut.clock.step(1)

      // Cycle 7 : answer the memory quest
      dut.io.memory_quest_from_lsq.valid.expect(true.B)
      dut.io.memory_quest_from_lsq.bits.wr_en.expect(false.B)
      dut.io.memory_quest_from_lsq.bits.addr.expect(5.U)

      dut.io.memory_result_for_lsq.valid.poke(true.B)
      dut.io.memory_result_for_lsq.bits.poke(100.U)

      dut.io.broadcast_to_rs.valid.expect(false.B)

      dut.clock.step(1)

      dut.io.memory_result_for_lsq.valid.poke(false.B)

      // Cycle 8 : broadcast the result of load a, load b should begin to quest memory now

      dut.io.broadcast_to_rs.valid.expect(true.B)
      dut.io.broadcast_to_rs.bits.value.expect(100.U)
      dut.io.broadcast_to_rs.bits.dest.expect(0.U)

      dut.io.memory_quest_from_lsq.valid.expect(true.B)
      dut.io.memory_quest_from_lsq.bits.wr_en.expect(false.B)
      dut.io.memory_quest_from_lsq.bits.addr.expect(10.U)

      dut.clock.step(1)

      // Cycle 9 : nothing happened

      dut.io.broadcast_to_rs.valid.expect(false.B)
      dut.io.memory_quest_from_lsq.valid.expect(true.B)
      dut.io.memory_quest_from_lsq.bits.wr_en.expect(false.B)
      dut.io.memory_quest_from_lsq.bits.addr.expect(10.U)

      dut.clock.step(1)

      // Cycle 10 : answer the memory quest, address of load c is finished

      dut.io.memory_result_for_lsq.valid.poke(true.B)
      dut.io.memory_result_for_lsq.bits.poke(200.U)

      dut.io.alu_broadcast_result.valid.poke(true.B)
      dut.io.alu_broadcast_result.bits.dest.poke(2.U)
      dut.io.alu_broadcast_result.bits.addr.poke(15.U)

      dut.clock.step(1)

      dut.io.memory_result_for_lsq.valid.poke(false.B)
      dut.io.alu_broadcast_result.valid.poke(false.B)

      dut.io.broadcast_to_rs.valid.expect(true.B)
      dut.io.broadcast_to_rs.bits.value.expect(200.U)
      dut.io.broadcast_to_rs.bits.dest.expect(1.U)

      dut.io.memory_quest_from_lsq.valid.expect(true.B)
      dut.io.memory_quest_from_lsq.bits.wr_en.expect(false.B)
      dut.io.memory_quest_from_lsq.bits.addr.expect(15.U)

      // the rest is omitted
    }
  }
}
