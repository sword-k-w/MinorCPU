import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.control.Breaks._

// front end including instruction fetcher, instruction cache and memory arbiter (but doesn't arbit)
class MASpec extends AnyFlatSpec with ChiselScalatestTester {
  "MA" should "pass" in {
    test(new MA) { dut =>
      var cycleCount = 0
      println(s"\n[Cycle $cycleCount]")

      val length = 5
      val max_waiting_time = 10
      val memory : List[Int] = (0 until length * 8).map { idx =>
        if (idx % 4 == 0) {
          idx / 4 + 1
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

      // initialize -- nothing to do at cycle 0
      dut.io.predict_failed.poke(false.B)

      dut.io.i_quest.valid.poke(false.B)
      dut.io.i_quest.bits.poke(0.U)

      dut.io.d_quest_from_lsq.valid.poke(false.B)
      dut.io.d_quest_from_lsq.bits.addr.poke(0.U)
      dut.io.d_quest_from_lsq.bits.value.poke(0.U)
      dut.io.d_quest_from_lsq.bits.size.poke(0.U)
      dut.io.d_quest_from_lsq.bits.wr_en.poke(false.B)

      dut.io.d_quest_from_wb.valid.poke(false.B)
      dut.io.d_quest_from_wb.bits.addr.poke(0.U)
      dut.io.d_quest_from_wb.bits.value.poke(0.U)
      dut.io.d_quest_from_wb.bits.size.poke(0.U)
      dut.io.d_quest_from_wb.bits.wr_en.poke(false.B)

      dut.io.mem_din.poke(0.U)

      Step() // cycle 0

      dut.io.i_result.valid.expect(false.B)
      dut.io.d_result_to_lsq.valid.expect(false.B)
      dut.io.d_result_to_wb.valid.expect(false.B)
      dut.io.mem_dout.expect(0.U)
      dut.io.mem_a.expect(0.U)
      dut.io.mem_wr.expect(false.B)

      // cycle 1 -- get lsq_quest( <lw> addr: 0 size: 2 expect: 1 at cycle 5 )
      //            and i_quest ( bits: 4 expect 2 at cycle 9 )
      dut.io.predict_failed.poke(false.B)

      dut.io.i_quest.valid.poke(true.B)
      dut.io.i_quest.bits.poke(4.U)

      dut.io.d_quest_from_lsq.valid.poke(true.B)
      dut.io.d_quest_from_lsq.bits.addr.poke(0.U)
      dut.io.d_quest_from_lsq.bits.value.poke(0.U)
      dut.io.d_quest_from_lsq.bits.size.poke(2.U)
      dut.io.d_quest_from_lsq.bits.wr_en.poke(false.B)

      dut.io.d_quest_from_wb.valid.poke(false.B)
      dut.io.d_quest_from_wb.bits.addr.poke(0.U)
      dut.io.d_quest_from_wb.bits.value.poke(0.U)
      dut.io.d_quest_from_wb.bits.size.poke(0.U)
      dut.io.d_quest_from_wb.bits.wr_en.poke(false.B)

      Step() // cycle 1

      dut.io.i_result.valid.expect(false.B)
      dut.io.d_result_to_lsq.valid.expect(false.B)
      dut.io.d_result_to_wb.valid.expect(false.B)
      dut.io.mem_dout.expect(0.U)
      dut.io.mem_a.expect(1.U)
      dut.io.mem_wr.expect(false.B)

      // cycle 2 -- loaded byte 0 for lsq
      dut.io.predict_failed.poke(false.B)

      dut.io.i_quest.valid.poke(true.B)
      dut.io.i_quest.bits.poke(4.U)

      dut.io.d_quest_from_lsq.valid.poke(true.B)
      dut.io.d_quest_from_lsq.bits.addr.poke(0.U)
      dut.io.d_quest_from_lsq.bits.value.poke(0.U)
      dut.io.d_quest_from_lsq.bits.size.poke(2.U)
      dut.io.d_quest_from_lsq.bits.wr_en.poke(false.B)

      dut.io.d_quest_from_wb.valid.poke(false.B)
      dut.io.d_quest_from_wb.bits.addr.poke(0.U)
      dut.io.d_quest_from_wb.bits.value.poke(0.U)
      dut.io.d_quest_from_wb.bits.size.poke(0.U)
      dut.io.d_quest_from_wb.bits.wr_en.poke(false.B)

      Step()

      // cycle 3 -- loaded byte 1 for lsq
      dut.io.i_result.valid.expect(false.B)
      dut.io.d_result_to_lsq.valid.expect(false.B)
      dut.io.d_result_to_wb.valid.expect(false.B)
      dut.io.mem_dout.expect(0.U)
      dut.io.mem_a.expect(2.U)
      dut.io.mem_wr.expect(false.B)

      dut.io.predict_failed.poke(false.B)

      dut.io.i_quest.valid.poke(true.B)
      dut.io.i_quest.bits.poke(4.U)

      dut.io.d_quest_from_lsq.valid.poke(true.B)
      dut.io.d_quest_from_lsq.bits.addr.poke(0.U)
      dut.io.d_quest_from_lsq.bits.value.poke(0.U)
      dut.io.d_quest_from_lsq.bits.size.poke(2.U)
      dut.io.d_quest_from_lsq.bits.wr_en.poke(false.B)

      dut.io.d_quest_from_wb.valid.poke(false.B)
      dut.io.d_quest_from_wb.bits.addr.poke(0.U)
      dut.io.d_quest_from_wb.bits.value.poke(0.U)
      dut.io.d_quest_from_wb.bits.size.poke(0.U)
      dut.io.d_quest_from_wb.bits.wr_en.poke(false.B)

      Step()

      // cycle 4 -- loaded byte 2 for lsq
      dut.io.i_result.valid.expect(false.B)
      dut.io.d_result_to_lsq.valid.expect(false.B)
      dut.io.d_result_to_wb.valid.expect(false.B)
      dut.io.mem_dout.expect(0.U)
      dut.io.mem_a.expect(3.U)
      dut.io.mem_wr.expect(false.B)

      dut.io.predict_failed.poke(false.B)

      dut.io.i_quest.valid.poke(true.B)
      dut.io.i_quest.bits.poke(4.U)

      dut.io.d_quest_from_lsq.valid.poke(true.B)
      dut.io.d_quest_from_lsq.bits.addr.poke(0.U)
      dut.io.d_quest_from_lsq.bits.value.poke(0.U)
      dut.io.d_quest_from_lsq.bits.size.poke(2.U)
      dut.io.d_quest_from_lsq.bits.wr_en.poke(false.B)

      dut.io.d_quest_from_wb.valid.poke(false.B)
      dut.io.d_quest_from_wb.bits.addr.poke(0.U)
      dut.io.d_quest_from_wb.bits.value.poke(0.U)
      dut.io.d_quest_from_wb.bits.size.poke(0.U)
      dut.io.d_quest_from_wb.bits.wr_en.poke(false.B)

      Step()

      // cycle 5 -- loaded byte 3 for lsq, lsq quest completed!
      dut.io.i_result.valid.expect(false.B)
      dut.io.d_result_to_lsq.valid.expect(true.B)
      dut.io.d_result_to_lsq.bits.expect(1.U)
      dut.io.d_result_to_wb.valid.expect(false.B)
      dut.io.mem_dout.expect(0.U)
      dut.io.mem_a.expect(4.U)
      dut.io.mem_wr.expect(false.B)

      dut.io.predict_failed.poke(false.B)

      dut.io.i_quest.valid.poke(true.B)
      dut.io.i_quest.bits.poke(4.U)

      dut.io.d_quest_from_lsq.valid.poke(false.B)
      dut.io.d_quest_from_lsq.bits.addr.poke(0.U)
      dut.io.d_quest_from_lsq.bits.value.poke(0.U)
      dut.io.d_quest_from_lsq.bits.size.poke(0.U)
      dut.io.d_quest_from_lsq.bits.wr_en.poke(false.B)

      dut.io.d_quest_from_wb.valid.poke(false.B)
      dut.io.d_quest_from_wb.bits.addr.poke(0.U)
      dut.io.d_quest_from_wb.bits.value.poke(0.U)
      dut.io.d_quest_from_wb.bits.size.poke(0.U)
      dut.io.d_quest_from_wb.bits.wr_en.poke(false.B)

      Step()

      // cycle 6 -- loaded byte 4 for ICache
      dut.io.i_result.valid.expect(false.B)
      dut.io.d_result_to_lsq.valid.expect(false.B)
      dut.io.d_result_to_wb.valid.expect(false.B)
      dut.io.mem_dout.expect(0.U)
      dut.io.mem_a.expect(5.U)
      dut.io.mem_wr.expect(false.B)

      dut.io.predict_failed.poke(false.B)

      dut.io.i_quest.valid.poke(true.B)
      dut.io.i_quest.bits.poke(4.U)

      dut.io.d_quest_from_lsq.valid.poke(false.B)
      dut.io.d_quest_from_lsq.bits.addr.poke(0.U)
      dut.io.d_quest_from_lsq.bits.value.poke(0.U)
      dut.io.d_quest_from_lsq.bits.size.poke(0.U)
      dut.io.d_quest_from_lsq.bits.wr_en.poke(false.B)

      dut.io.d_quest_from_wb.valid.poke(false.B)
      dut.io.d_quest_from_wb.bits.addr.poke(0.U)
      dut.io.d_quest_from_wb.bits.value.poke(0.U)
      dut.io.d_quest_from_wb.bits.size.poke(0.U)
      dut.io.d_quest_from_wb.bits.wr_en.poke(false.B)

      Step()

      // cycle 7 -- loaded byte 5 for ICache
      dut.io.i_result.valid.expect(false.B)
      dut.io.d_result_to_lsq.valid.expect(false.B)
      dut.io.d_result_to_wb.valid.expect(false.B)
      dut.io.mem_dout.expect(0.U)
      dut.io.mem_a.expect(6.U)
      dut.io.mem_wr.expect(false.B)

      dut.io.predict_failed.poke(false.B)

      dut.io.i_quest.valid.poke(true.B)
      dut.io.i_quest.bits.poke(4.U)

      dut.io.d_quest_from_lsq.valid.poke(false.B)
      dut.io.d_quest_from_lsq.bits.addr.poke(0.U)
      dut.io.d_quest_from_lsq.bits.value.poke(0.U)
      dut.io.d_quest_from_lsq.bits.size.poke(0.U)
      dut.io.d_quest_from_lsq.bits.wr_en.poke(false.B)

      dut.io.d_quest_from_wb.valid.poke(false.B)
      dut.io.d_quest_from_wb.bits.addr.poke(0.U)
      dut.io.d_quest_from_wb.bits.value.poke(0.U)
      dut.io.d_quest_from_wb.bits.size.poke(0.U)
      dut.io.d_quest_from_wb.bits.wr_en.poke(false.B)

      Step()

      // cycle 8 -- loaded byte 6 for ICache
      dut.io.i_result.valid.expect(false.B)
      dut.io.d_result_to_lsq.valid.expect(false.B)
      dut.io.d_result_to_wb.valid.expect(false.B)
      dut.io.mem_dout.expect(0.U)
      dut.io.mem_a.expect(7.U)
      dut.io.mem_wr.expect(false.B)

      dut.io.predict_failed.poke(false.B)

      dut.io.i_quest.valid.poke(true.B)
      dut.io.i_quest.bits.poke(4.U)

      dut.io.d_quest_from_lsq.valid.poke(false.B)
      dut.io.d_quest_from_lsq.bits.addr.poke(0.U)
      dut.io.d_quest_from_lsq.bits.value.poke(0.U)
      dut.io.d_quest_from_lsq.bits.size.poke(0.U)
      dut.io.d_quest_from_lsq.bits.wr_en.poke(false.B)

      dut.io.d_quest_from_wb.valid.poke(false.B)
      dut.io.d_quest_from_wb.bits.addr.poke(0.U)
      dut.io.d_quest_from_wb.bits.value.poke(0.U)
      dut.io.d_quest_from_wb.bits.size.poke(0.U)
      dut.io.d_quest_from_wb.bits.wr_en.poke(false.B)

      Step()

      // cycle 9 -- loaded byte 7 for ICache, ICache quest completed!
      dut.io.i_result.valid.expect(true.B)
      dut.io.i_result.bits.expect(2)
      dut.io.d_result_to_lsq.valid.expect(false.B)
      dut.io.d_result_to_wb.valid.expect(false.B)
      dut.io.mem_dout.expect(0.U)
      dut.io.mem_a.expect(0.U)
      dut.io.mem_wr.expect(false.B)

      dut.io.predict_failed.poke(false.B)

      dut.io.i_quest.valid.poke(false.B)
      dut.io.i_quest.bits.poke(0.U)

      dut.io.d_quest_from_lsq.valid.poke(false.B)
      dut.io.d_quest_from_lsq.bits.addr.poke(0.U)
      dut.io.d_quest_from_lsq.bits.value.poke(0.U)
      dut.io.d_quest_from_lsq.bits.size.poke(0.U)
      dut.io.d_quest_from_lsq.bits.wr_en.poke(false.B)

      dut.io.d_quest_from_wb.valid.poke(false.B)
      dut.io.d_quest_from_wb.bits.addr.poke(0.U)
      dut.io.d_quest_from_wb.bits.value.poke(0.U)
      dut.io.d_quest_from_wb.bits.size.poke(0.U)
      dut.io.d_quest_from_wb.bits.wr_en.poke(false.B)

      Step()

      // cycle 10 -- all finished
      dut.io.i_result.valid.expect(false.B)
      dut.io.d_result_to_lsq.valid.expect(false.B)
      dut.io.d_result_to_wb.valid.expect(false.B)
      dut.io.mem_dout.expect(0.U)
      dut.io.mem_a.expect(0.U)
      dut.io.mem_wr.expect(false.B)
    }
  }
}