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

      val length = 20
      val memory : Array[Int] = (0 until length * 8).map { idx =>
        if (idx % 4 == 0) {
          idx / 4 + 1
        } else {
          0
        }
      }.toArray

      var lastAddr  = 0
      var lastWr    = false
      var lastDout  = 0

      dut.io.mem_din.poke(0.U)

      def Step(): Unit = {
        println(s"\n[Cycle $cycleCount]")

        if (lastAddr < 8 * length) {
          if (lastWr) {
            memory(lastAddr) = lastDout
            println(s"  WRITE mem[$lastAddr] = $lastDout")
          } else {
            val rdata = memory(lastAddr)
            dut.io.mem_din.poke(rdata.U)
            println(s"  READ  mem[$lastAddr] -> mem_din = $rdata")
          }
        } else {
          println(s"  [WARNING] lastAddr = $lastAddr out of range")
          dut.io.mem_din.poke(0.U)
        }

        val addrNow = dut.io.mem_a.peekInt().toInt
        val wrNow   = dut.io.mem_wr.peekBoolean()
        val doutNow = dut.io.mem_dout.peekInt().toInt

        println(s"  DUT mem_a = $addrNow, mem_wr = $wrNow, mem_dout = $doutNow")

        lastAddr = addrNow
        lastWr   = wrNow
        lastDout = doutNow

        dut.clock.step(1)
        cycleCount += 1
      }

      def ShowCurrentMemory() : Unit = {
        val total = length * 8
        val elementsPerLine = 16

        for (start <- 0 until total by elementsPerLine) {
          val end = Math.min(start + elementsPerLine, total)
          print(f"[$start%3d..${end-1}%3d]: ")
          for (i <- start until end) {
            print(f"${memory(i)}%4d")
          }
          println()
        }
      }

      def WaitAndCheck(): Unit = {
        dut.io.predict_failed.poke(false.B)

        dut.io.i_quest.valid.poke(false.B)
        dut.io.i_quest.bits.poke(0.U)

        dut.io.d_quest.valid.poke(false.B)
        dut.io.d_quest.bits.addr.poke(0.U)
        dut.io.d_quest.bits.value.poke(0.U)
        dut.io.d_quest.bits.size.poke(0.U)
        dut.io.d_quest.bits.wr_en.poke(false.B)

        Step()

        ShowCurrentMemory()
      }

      // initialize -- nothing to do at cycle 0
      dut.io.predict_failed.poke(false.B)

      dut.io.i_quest.valid.poke(false.B)
      dut.io.i_quest.bits.poke(0.U)

      dut.io.d_quest.valid.poke(false.B)
      dut.io.d_quest.bits.addr.poke(0.U)
      dut.io.d_quest.bits.value.poke(0.U)
      dut.io.d_quest.bits.size.poke(0.U)
      dut.io.d_quest.bits.wr_en.poke(false.B)

      dut.io.mem_din.poke(0.U)

      Step()

      // cycle 1 -- get d_quest( <lw> addr: 0 size: 2 expect: 1 at cycle 5 )
      //            and i_quest ( bits: 4 expect 2 at cycle 9 )
      dut.io.predict_failed.poke(false.B)

      dut.io.i_quest.valid.poke(true.B)
      dut.io.i_quest.bits.poke(4.U)

      dut.io.d_quest.valid.poke(true.B)
      dut.io.d_quest.bits.addr.poke(0.U)
      dut.io.d_quest.bits.value.poke(0.U)
      dut.io.d_quest.bits.size.poke(2.U)
      dut.io.d_quest.bits.wr_en.poke(false.B)

      Step()

      // cycle 2 -- loaded byte 0 for DCache
      dut.io.predict_failed.poke(false.B)

      dut.io.i_quest.valid.poke(true.B)
      dut.io.i_quest.bits.poke(4.U)

      dut.io.d_quest.valid.poke(true.B)
      dut.io.d_quest.bits.addr.poke(0.U)
      dut.io.d_quest.bits.value.poke(0.U)
      dut.io.d_quest.bits.size.poke(2.U)
      dut.io.d_quest.bits.wr_en.poke(false.B)

      Step()

      // cycle 3 -- loaded byte 1 for DCache
      dut.io.predict_failed.poke(false.B)

      dut.io.i_quest.valid.poke(true.B)
      dut.io.i_quest.bits.poke(4.U)

      dut.io.d_quest.valid.poke(true.B)
      dut.io.d_quest.bits.addr.poke(0.U)
      dut.io.d_quest.bits.value.poke(0.U)
      dut.io.d_quest.bits.size.poke(2.U)
      dut.io.d_quest.bits.wr_en.poke(false.B)

      Step()

      // cycle 4 -- loaded byte 2 for DCache
      dut.io.predict_failed.poke(false.B)

      dut.io.i_quest.valid.poke(true.B)
      dut.io.i_quest.bits.poke(4.U)

      dut.io.d_quest.valid.poke(true.B)
      dut.io.d_quest.bits.addr.poke(0.U)
      dut.io.d_quest.bits.value.poke(0.U)
      dut.io.d_quest.bits.size.poke(2.U)
      dut.io.d_quest.bits.wr_en.poke(false.B)

      Step()

      // cycle 5 -- loaded byte 3 for DCache, DCache quest completed!
      dut.io.predict_failed.poke(false.B)

      dut.io.i_quest.valid.poke(true.B)
      dut.io.i_quest.bits.poke(4.U)

      dut.io.d_quest.valid.poke(false.B)
      dut.io.d_quest.bits.addr.poke(0.U)
      dut.io.d_quest.bits.value.poke(0.U)
      dut.io.d_quest.bits.size.poke(0.U)
      dut.io.d_quest.bits.wr_en.poke(false.B)

      Step()

      // cycle 6 -- loaded byte 4 for ICache
      dut.io.predict_failed.poke(false.B)

      dut.io.i_quest.valid.poke(true.B)
      dut.io.i_quest.bits.poke(4.U)

      dut.io.d_quest.valid.poke(false.B)
      dut.io.d_quest.bits.addr.poke(0.U)
      dut.io.d_quest.bits.value.poke(0.U)
      dut.io.d_quest.bits.size.poke(0.U)
      dut.io.d_quest.bits.wr_en.poke(false.B)

      Step()

      // cycle 7 -- loaded byte 5 for ICache
      dut.io.predict_failed.poke(false.B)

      dut.io.i_quest.valid.poke(true.B)
      dut.io.i_quest.bits.poke(4.U)

      dut.io.d_quest.valid.poke(false.B)
      dut.io.d_quest.bits.addr.poke(0.U)
      dut.io.d_quest.bits.value.poke(0.U)
      dut.io.d_quest.bits.size.poke(0.U)
      dut.io.d_quest.bits.wr_en.poke(false.B)

      Step()

      // cycle 8 -- loaded byte 6 for ICache
      dut.io.predict_failed.poke(false.B)

      dut.io.i_quest.valid.poke(true.B)
      dut.io.i_quest.bits.poke(4.U)

      dut.io.d_quest.valid.poke(false.B)
      dut.io.d_quest.bits.addr.poke(0.U)
      dut.io.d_quest.bits.value.poke(0.U)
      dut.io.d_quest.bits.size.poke(0.U)
      dut.io.d_quest.bits.wr_en.poke(false.B)

      Step()

      // cycle 9 -- loaded byte 7 for ICache, ICache quest completed!
      //            poke DCache read instruction (lh), start wb
      dut.io.predict_failed.poke(false.B)

      dut.io.i_quest.valid.poke(false.B)
      dut.io.i_quest.bits.poke(0.U)

      dut.io.d_quest.valid.poke(false.B)
      dut.io.d_quest.bits.addr.poke(0.U)
      dut.io.d_quest.bits.value.poke(0.U)
      dut.io.d_quest.bits.size.poke(0.U)
      dut.io.d_quest.bits.wr_en.poke(false.B)

      Step()

      // cycle 10 -- read the second byte for wb instruction (lh)
      //             finish wb read!
      dut.io.predict_failed.poke(false.B)

      dut.io.i_quest.valid.poke(false.B)
      dut.io.i_quest.bits.poke(0.U)

      dut.io.d_quest.valid.poke(false.B)
      dut.io.d_quest.bits.addr.poke(0.U)
      dut.io.d_quest.bits.value.poke(0.U)
      dut.io.d_quest.bits.size.poke(0.U)
      dut.io.d_quest.bits.wr_en.poke(false.B)

      Step()

      // cycle 11 -- poke wb read instruction (lb),
      //             start and immediately complete wb reading mission
      dut.io.predict_failed.poke(false.B)

      dut.io.i_quest.valid.poke(false.B)
      dut.io.i_quest.bits.poke(0.U)

      dut.io.d_quest.valid.poke(false.B)
      dut.io.d_quest.bits.addr.poke(0.U)
      dut.io.d_quest.bits.value.poke(0.U)
      dut.io.d_quest.bits.size.poke(0.U)
      dut.io.d_quest.bits.wr_en.poke(false.B)

      Step()

      ShowCurrentMemory()

      // cycle 12 -- poke wb write instruction (sb),
      //             start and immediately complete wb writing mission
      dut.io.predict_failed.poke(false.B)

      dut.io.i_quest.valid.poke(false.B)
      dut.io.i_quest.bits.poke(0.U)

      dut.io.d_quest.valid.poke(false.B)
      dut.io.d_quest.bits.addr.poke(0.U)
      dut.io.d_quest.bits.value.poke(0.U)
      dut.io.d_quest.bits.size.poke(0.U)
      dut.io.d_quest.bits.wr_en.poke(false.B)

      Step()

      ShowCurrentMemory()

      WaitAndCheck()

    }
  }
}