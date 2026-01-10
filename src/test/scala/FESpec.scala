import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.control.Breaks._

// front end including instruction fetcher, instruction cache and memory arbiter (but doesn't arbit)
class FESpec extends AnyFlatSpec with ChiselScalatestTester {
  class FE extends Module {
    val io = IO(new Bundle {
      val instruction = Decoupled(new Instruction)
      
      val modified_pc = Flipped(Valid(UInt(32.W)))
      
      val mem_din  = Input(UInt(8.W))
      val mem_dout = Output(UInt(8.W))
      val mem_a    = Output(UInt(32.W))
      val mem_wr   = Output(Bool())
    })

    val if_ = Module(new IF)
    val icache = Module(new ICache)
    val ma = Module(new MA)
    if_.io.instruction <> io.instruction
    if_.io.modified_pc <> io.modified_pc
    if_.io.quest <> icache.io.quest
    if_.io.quest_result <> icache.io.quest_result
    if_.io.quest_result2 <> icache.io.quest_result2
    ma.io.i_quest <> icache.io.mem_quest
    icache.io.mem_result <> ma.io.i_result
    icache.io.predict_failed <> io.modified_pc.valid
    ma.io.mem_din <> io.mem_din
    ma.io.mem_dout <> io.mem_dout
    ma.io.mem_a <> io.mem_a
    ma.io.mem_wr <> io.mem_wr
    ma.io.predict_failed <> io.modified_pc.valid
  }
  "FE" should "pass" in {
    test(new FE) { dut =>
      var cycleCount = 0
      println(s"\n[Cycle $cycleCount]")

      dut.io.instruction.ready.poke(true.B)
      dut.io.modified_pc.valid.poke(false.B)
      val length = 5
      val max_waiting_time = 10
      val memory : List[Int] = (0 until length * 8).map { idx =>
        if ((idx + 1) % 4 == 0) {
          idx - 1
        } else {
          0
        }
      }.toList

      def Step() : Unit = {
        val addr = dut.io.mem_a.peekInt().toInt
        // println(s"(memory) : ask ${addr}")
        dut.clock.step(1)
        cycleCount += 1
        println(s"\n[Cycle $cycleCount]")
        if (addr < 8 * length) {
          dut.io.mem_din.poke(memory(addr))
        }
      }

      def Run() : Unit = {
        for (i <- 0 until length) {
          var flag = false
          breakable {
            for (_ <- 0 until max_waiting_time) {
              if (dut.io.instruction.valid.peekBoolean()) {
                flag = true
                break()
              }
              Step()
              dut.io.modified_pc.valid.poke(false.B)
            }
          }
          assert(flag, s"failed to fetch address $i.")
          dut.io.instruction.bits.op.expect(i.U(32.W))
          println(s"fetched ${dut.io.instruction.bits.op.peekInt().toInt}")
          Step()
          dut.io.modified_pc.valid.poke(false.B)
        }
      }

      Run()
      
      dut.io.modified_pc.valid.poke(true.B)
      dut.io.modified_pc.bits.poke(0.U)

      Run()
    }
  }
}