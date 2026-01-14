import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class RSSpec extends AnyFlatSpec with ChiselScalatestTester {
  "RS" should "pass" in {
    test(new RS) { dut =>
      dut.io.predict_failed.poke(false.B)

      // init
    }
  }
}