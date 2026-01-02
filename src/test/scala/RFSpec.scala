import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class RFSpec extends AnyFlatSpec with ChiselScalatestTester {
  "RF" should "pass" in {
    test(new RF) { dut =>
      dut.io.predict_failed.poke(false.B)
      dut.io.commit_info.valid.poke(false.B)
      dut.io.commit_info.bits.rob_id.poke(0.U)
      dut.io.commit_info.bits.reg_id.poke(0.U)
      dut.io.commit_info.bits.value.poke(0.U)

      dut.io.new_dependence_valid.poke(false.B)
      dut.io.new_reg_id.poke(0.U)
      dut.io.new_dependence.poke(0.U)

      dut.io.qry1_addr.poke(0.U)
      dut.io.qry2_addr.poke(0.U)

      // init

      dut.clock.step(1)

      dut.io.qry1_addr.poke(1.U)
      dut.io.new_dependence_valid.poke(true.B)
      dut.io.new_reg_id.poke(1.U)
      dut.io.new_dependence.poke(2.U)
      dut.io.qry1_has_dependence.expect(false.B)
      dut.io.qry1_val.expect(0.U)

      dut.clock.step(1)

      dut.io.qry1_addr.poke(1.U)
      dut.io.new_dependence_valid.poke(false.B)
      dut.io.qry1_has_dependence.expect(true.B)

      // dependence query should have one cycle latency

      dut.clock.step(1)

      dut.io.commit_info.valid.poke(true.B)
      dut.io.commit_info.bits.reg_id.poke(1.U)
      dut.io.commit_info.bits.rob_id.poke(2.U)
      dut.io.commit_info.bits.value.poke(3.U)

      dut.clock.step(1)

      dut.io.qry1_addr.poke(1.U)
      dut.io.qry1_has_dependence.expect(false.B)
      dut.io.qry1_val.expect(3.U)

      // after commit, the dependence should be eliminated, and reg value should be updated

      // TODO : add more test to check the correctness of RF
    }
  }
}