import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class RFSpec extends AnyFlatSpec with ChiselScalatestTester {
  "RF" should "pass" in {
    test(new RF) { dut =>
      dut.io.predict_failed.poke(false.B)
      dut.io.commit_valid.poke(false.B)
      dut.io.commit_rob_id.poke(0.U)
      dut.io.commit_reg_id.poke(0.U)
      dut.io.commit_value.poke(0.U)

      dut.io.new_dependence_valid.poke(false.B)
      dut.io.new_reg_id.poke(0.U)
      dut.io.new_dependence.poke(0.U)

      dut.io.qry1_addr.poke(0.U)
      dut.io.qry2_addr.poke(0.U)

      dut.clock.step(1)

      dut.io.qry1_addr.poke(1.U)
      dut.io.new_dependence_valid.poke(true.B)
      dut.io.new_reg_id.poke(1.U)
      dut.io.qry1_has_dependence.expect(false.B)
      dut.io.qry1_val.expect(0.U)
    }
  }
}