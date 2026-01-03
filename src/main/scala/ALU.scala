import chisel3._
import chisel3.util._

class ALUQuest extends Bundle {
  val in1 = UInt(32.W)
  val in2 = UInt(32.W)
  val in3 = UInt(32.W)
  val op = UInt(5.W)
  val funct = UInt(4.W)
  val is_zero = Bool()
  val dest = UInt(5.W)
}

class ALU extends Module {
  val io = IO(new Bundle {
    val quest = Flipped(Valid(new ALUQuest))
  })

}