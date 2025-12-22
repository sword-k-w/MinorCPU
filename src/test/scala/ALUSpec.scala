import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ALUSpec extends AnyFlatSpec with ChiselScalatestTester {
  "ALU" should "pass" in {
    test(new ALU) {dut =>
      dut.io.in1.poke(5.U)
      dut.io.in2.poke(3.U)
      dut.io.op.poke("b0_000".U)
      dut.io.out.expect(8.U)
      dut.clock.step(1)
      
      dut.io.op.poke("b1_000".U)
      dut.io.out.expect(2.U)
      dut.clock.step(1)

      dut.io.op.poke("b0_111".U)
      dut.io.out.expect(1.U)
      dut.clock.step(1)

      dut.io.op.poke("b0_110".U)
      dut.io.out.expect(7.U)
      dut.clock.step(1)

      dut.io.op.poke("b0_100".U)
      dut.io.out.expect(6.U)
      dut.clock.step(1)

      dut.io.op.poke("b0_001".U)
      dut.io.out.expect(40.U)
      dut.clock.step(1)

      dut.io.in1.poke("hFFFFFFFF".U)
      dut.io.in2.poke(2.U)
      dut.io.op.poke("b0_101".U)
      dut.io.out.expect("h3FFFFFFF".U)
      dut.clock.step(1)

      dut.io.op.poke("b1_101".U)
      dut.io.out.expect("hFFFFFFFF".U)
      dut.clock.step(1)

      dut.io.in1.poke(5.U)
      dut.io.in2.poke(3.U)
      dut.io.op.poke("b0_010".U) 
      dut.io.out.expect(0.U)
      dut.clock.step(1)

      dut.io.in2.poke(5.U)
      dut.io.out.expect(0.U)
      dut.clock.step(1)

      dut.io.in2.poke(7.U)
      dut.io.out.expect(1.U)
    }
  }
}