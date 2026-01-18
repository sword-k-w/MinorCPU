import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class DCacheSpec extends AnyFlatSpec with ChiselScalatestTester {

  class DF extends Module {
    val io = IO(new Bundle {
      val mem_din  = Input(UInt(8.W))
      val mem_dout = Output(UInt(8.W))
      val mem_a    = Output(UInt(32.W))
      val mem_wr   = Output(Bool())

      val predict_failed = Input(Bool())

      val lsq_quest = Flipped(Valid(new MemoryQuest))
      val lsq_result_hit = Valid(UInt(32.W))
      val lsq_result_mem = Valid(UInt(32.W))

      val wb_quest = Flipped(Valid(new MemoryQuest))
      val wb_result_hit = Valid(UInt(32.W))
      val wb_result_mem = Valid(UInt(32.W))

    })

    val dcache = Module(new DCache())
    val ma = Module(new MA)

    dcache.io.mem_quest <> ma.io.d_quest
    dcache.io.mem_result <> ma.io.d_result
    dcache.io.predict_failed <> io.predict_failed
    dcache.io.lsq_quest <> io.lsq_quest
    dcache.io.wb_quest <> io.wb_quest
    io.wb_result_mem <> dcache.io.wb_result_mem
    io.wb_result_hit <> dcache.io.wb_result_hit
    io.lsq_result_mem <> dcache.io.lsq_result_mem
    io.lsq_result_hit <> dcache.io.lsq_result_hit

    ma.io.i_quest.valid := false.B
    ma.io.i_quest.bits := 0.U
    ma.io.predict_failed := false.B
    ma.io.mem_din <> io.mem_din
    io.mem_a <> ma.io.mem_a
    io.mem_dout <> ma.io.mem_dout
    io.mem_wr <> ma.io.mem_wr
  }

  "DCache" should "pass" in {
    test(new DF) { dut =>
      var cycleCount = 0

      val length = 20
      val memory : Array[Int] = (0 until length * 8).map { idx =>
        idx
      }.toArray

      var lastAddress = 0
      var lastWriteEnable = false
      var lastDout = 0

      dut.io.predict_failed.poke(false.B)
      dut.io.mem_din.poke(0.U)

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
      def Step() : Unit = {
        println(s"\n[Cycle $cycleCount]")

        if (dut.io.lsq_result_hit.valid.peekBoolean()) {
          println(s" DUT get lsq result <cache hit>: ${dut.io.lsq_result_hit.bits.peekInt()}")
        } else if (dut.io.lsq_result_mem.valid.peekBoolean()) {
          println(s" DUT get lsq result <from memory>: ${dut.io.lsq_result_mem.bits.peekInt()}")
        }
        if (dut.io.wb_result_hit.valid.peekBoolean()) {
          println(s" DUT get wb result <cache hit>: ${dut.io.wb_result_hit.bits.peekInt()}")
        } else if (dut.io.wb_result_mem.valid.peekBoolean()) {
          println(s" DUT get wb result <from memory>: ${dut.io.wb_result_mem.bits.peekInt()}")
        }

        if (lastAddress < 8 * length) {
          if (lastWriteEnable) {
            memory(lastAddress) = lastDout
            println(s" MA WRITE mem[$lastAddress] = $lastDout")
          } else {
            val rdata = memory(lastAddress)
            dut.io.mem_din.poke(rdata.U)
            println(s" MA READ  mem[$lastAddress] -> mem_din = $rdata")
          }
        } else {
          println(s" [MA WARNING] lastAddr = $lastAddress out of range")
          dut.io.mem_din.poke(0.U)
        }

        val NowAddress = dut.io.mem_a.peekInt().toInt
        val NowWriteEnable = dut.io.mem_wr.peekBoolean()
        val NowDout = dut.io.mem_dout.peekInt().toInt

        println(s" MA mem_a = $NowAddress, mem_wr = $NowWriteEnable, mem_dout = $NowDout")

        lastAddress = NowAddress
        lastWriteEnable = NowWriteEnable
        lastDout = NowDout

        ShowCurrentMemory()

        dut.clock.step(1)
        cycleCount += 1
      }
      def PokeInputs(poke_lsq : Boolean, lsq_read_size : Int, lsq_read_address : Int, poke_wb : Boolean,
          wb_write_enable : Boolean, wb_size : Int, wb_address : Int, wb_write_value : Int) : Unit = {
        dut.io.lsq_quest.valid.poke(poke_lsq)
        dut.io.lsq_quest.bits.wr_en.poke(false)
        dut.io.lsq_quest.bits.size.poke(lsq_read_size)
        dut.io.lsq_quest.bits.value.poke(0)
        dut.io.lsq_quest.bits.addr.poke(lsq_read_address)
        dut.io.wb_quest.valid.poke(poke_wb)
        dut.io.wb_quest.bits.wr_en.poke(wb_write_enable)
        dut.io.wb_quest.bits.size.poke(wb_size)
        dut.io.wb_quest.bits.value.poke(wb_write_value)
        dut.io.wb_quest.bits.addr.poke(wb_address)
      }

      // cycle 0
      PokeInputs(poke_lsq = false, 0, 0, poke_wb = false, wb_write_enable = false, 0, 0, 0)
      Step()
    }
  }

}