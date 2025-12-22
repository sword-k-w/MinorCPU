import chisel3._
import chisel3.util._

class ALU extends Module {
  val io = IO(new Bundle {
    val in1  =  Input(UInt(32.W))
    val in2  =  Input(UInt(32.W))
    val op   =  Input(Bits(4.W))
    val out  =  Output(UInt(32.W))
  })

  val res = WireDefault(0.U(32.W))
  switch(io.op) {
    is("b0_000".U) { res := io.in1 + io.in2 }
    is("b1_000".U) { res := io.in1 - io.in2 }
    is("b0_111".U) { res := io.in1 & io.in2 }
    is("b0_110".U) { res := io.in1 | io.in2 }
    is("b0_100".U) { res := io.in1 ^ io.in2 }
    is("b0_001".U) { res := io.in1 << io.in2(4, 0) } // truncation
    is("b0_101".U) { res := io.in1 >> io.in2(4, 0) }
    is("b1_101".U) { res := (io.in1.asSInt >> io.in2(4, 0)).asUInt }
    is("b0_010".U) { res := (io.in1.asSInt < io.in2.asSInt).asUInt }
    is("b0_011".U) { res := (io.in1 < io.in2).asUInt }
  }

  io.out := res
}