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

      val lsq_is_full = Output(Bool())
    })
    val lsq = Module(new LSQ)
    val wb = Module(new WB)
    lsq.io.wb_is_full <> wb.io.is_full
    lsq.io.wb_is_empty <> wb.io.is_empty
    lsq.io.store_to_wb <> wb.io.new_instruction
    wb.io.memory_quest <> io.memory_quest_from_wb
    // todo: need a new test with separated wb_hit_result and wb_miss_result
    wb.io.hit_result <> io.memory_result_for_wb
    wb.io.miss_result <> io.memory_result_for_wb
    lsq.io.predict_failed <> io.predict_failed
    lsq.io.rob_tail <> io.rob_tail
    lsq.io.new_instruction <> io.new_instruction
    lsq.io.rob_broadcast_result <> io.rob_broadcast_result
    lsq.io.alu_broadcast_result <> io.alu_broadcast_result
    lsq.io.broadcast_to_rs <> io.broadcast_to_rs
    lsq.io.broadcast_to_rob <> io.broadcast_to_rob
    lsq.io.memory_quest <> io.memory_quest_from_lsq
    // todo: need a new test with separated lsq_hit_result and lsq_miss_result
    lsq.io.hit_result <> io.memory_result_for_lsq
    lsq.io.miss_result <> io.memory_result_for_lsq
    lsq.io.new_instruction.ready <> io.lsq_is_full
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
  "LSQ" should "handle single store instruction" in {
    test(new TestLSQ) { dut =>
      // 初始化
      dut.io.predict_failed.poke(false.B)
      dut.io.new_instruction.bits.funct.poke(2.U) // size = 2 (32-bit)
      dut.io.new_instruction.bits.immediate.poke(0.U)
      dut.io.new_instruction.bits.rs2.poke(0.U)
      dut.io.new_instruction.bits.rs1.poke(0.U)
      dut.io.new_instruction.bits.rd.poke(0.U)
      dut.io.new_instruction.bits.predict_address.poke(0.U)
      dut.io.rob_broadcast_result.valid.poke(false.B)
      dut.io.alu_broadcast_result.valid.poke(false.B)
      dut.io.memory_result_for_lsq.valid.poke(false.B)
      dut.io.memory_result_for_wb.valid.poke(false.B)

      // Cycle 0: 送入 Store 指令（op = b01000）
      dut.io.rob_tail.poke(0.U)
      dut.io.new_instruction.bits.op.poke("b01000".U)
      dut.io.new_instruction.valid.poke(true.B)
      dut.clock.step(1)
      dut.io.new_instruction.valid.poke(false.B)

      // Cycle 1: ROB 广播 Store 指令的地址和值
      dut.io.rob_broadcast_result.bits.dest.poke(0.U)
      dut.io.rob_broadcast_result.bits.addr.poke(0x100.U)
      dut.io.rob_broadcast_result.bits.value.poke(0x12345678.U)
      dut.io.rob_broadcast_result.valid.poke(true.B)
      dut.clock.step(1)
      dut.io.rob_broadcast_result.valid.poke(false.B)

      // Cycle 2: 检查 Store 送入 WB，LSQ head 前进
      dut.clock.step(1)

      // Cycle 3: WB 发起内存写请求
      dut.io.memory_quest_from_wb.valid.expect(true.B)
      dut.io.memory_quest_from_wb.bits.wr_en.expect(true.B)
      dut.io.memory_quest_from_wb.bits.addr.expect(0x100.U)
      dut.io.memory_quest_from_wb.bits.value.expect(0x12345678.U)
      dut.io.memory_quest_from_wb.bits.size.expect(2.U)
      dut.clock.step(1)

      // Cycle 4: 响应 WB 的内存写请求，WB head 前进
      dut.io.memory_result_for_wb.valid.poke(true.B)
      dut.clock.step(1)
      dut.io.memory_result_for_wb.valid.poke(false.B)
      dut.io.memory_quest_from_wb.valid.expect(false.B) // WB 队列空，停止请求
    }
  }
  "LSQ" should "reset on prediction failure" in {
    test(new TestLSQ) { dut =>
      // 初始化
      dut.io.predict_failed.poke(false.B)
      dut.io.new_instruction.bits.funct.poke(2.U)
      dut.io.new_instruction.bits.immediate.poke(0.U)
      dut.io.new_instruction.bits.rs2.poke(0.U)
      dut.io.new_instruction.bits.rs1.poke(0.U)
      dut.io.new_instruction.bits.rd.poke(0.U)
      dut.io.new_instruction.bits.predict_address.poke(0.U)
      dut.io.rob_broadcast_result.valid.poke(false.B)
      dut.io.alu_broadcast_result.valid.poke(false.B)
      dut.io.memory_result_for_lsq.valid.poke(false.B)
      dut.io.memory_result_for_wb.valid.poke(false.B)

      // Cycle 0: 送入 Load 指令
      dut.io.rob_tail.poke(0.U)
      dut.io.new_instruction.bits.op.poke("b00000".U)
      dut.io.new_instruction.valid.poke(true.B)
      dut.clock.step(1)
      dut.io.new_instruction.valid.poke(false.B)

      // Cycle 1: 送入 Store 指令
      dut.io.rob_tail.poke(1.U)
      dut.io.new_instruction.bits.op.poke("b01000".U)
      dut.io.new_instruction.valid.poke(true.B)
      dut.clock.step(1)
      dut.io.new_instruction.valid.poke(false.B)

      // Cycle 2: 触发预测失败
      dut.io.predict_failed.poke(true.B)
      dut.clock.step(1)
      dut.io.predict_failed.poke(false.B)

      // Cycle 3: 检查 LSQ 指针重置（head = tail），指令被清空
      dut.io.memory_quest_from_lsq.valid.expect(false.B)
      dut.clock.step(1)

      // 验证后续送入新指令正常
      dut.io.rob_tail.poke(2.U)
      dut.io.new_instruction.bits.op.poke("b00000".U)
      dut.io.new_instruction.valid.poke(true.B)
      dut.clock.step(1)
      dut.io.new_instruction.valid.poke(false.B)

      dut.io.alu_broadcast_result.bits.dest.poke(2.U)
      dut.io.alu_broadcast_result.bits.addr.poke(0x200.U)
      dut.io.alu_broadcast_result.valid.poke(true.B)
      dut.clock.step(1)
      dut.io.alu_broadcast_result.valid.poke(false.B)

      dut.io.memory_quest_from_lsq.valid.expect(true.B)
      dut.io.memory_quest_from_lsq.bits.addr.expect(0x200.U)
    }
  }
  "LSQ" should "reject new instruction when full" in {
    test(new TestLSQ) { dut =>
      // 初始化
      dut.io.predict_failed.poke(false.B)
      dut.io.rob_broadcast_result.valid.poke(false.B)
      dut.io.alu_broadcast_result.valid.poke(false.B)
      dut.io.memory_result_for_lsq.valid.poke(false.B)
      dut.io.memory_result_for_wb.valid.poke(false.B)

      // 循环送入 32 个 Load 指令（触发 LSQ 满）
      for (i <- 0 until 31) {
        dut.io.rob_tail.poke(i.U)
        dut.io.new_instruction.bits.op.poke("b00000".U)
        dut.io.new_instruction.bits.funct.poke(2.U)
        dut.io.new_instruction.valid.poke(true.B)
        dut.clock.step(1)
      }

      // LSQ 满
      dut.io.new_instruction.valid.poke(false.B)
      dut.io.lsq_is_full.expect(true.B) // 满状态标记
      dut.clock.step(1)
    }
  }
}
