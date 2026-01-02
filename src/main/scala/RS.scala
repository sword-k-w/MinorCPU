import chisel3._
import chisel3.util._

class RSEntry extends Bundle {
  val op = UInt(2.W) // 11 is jalr, 10 is store, 00 is other
  val busy = Bool()
  val value1 = UInt(32.W)
  val valid1 = Bool()
  val value2 = UInt(32.W)
  val valid2 = Bool()
  val dest = UInt(5.W)
  val immediate_s = UInt(32.W)
  val is_zero = Bool()
}

class RS extends Module {
  val io = IO(new Bundle {
    val predict_failed = Input(Bool())

    val new_instruction = Flipped(Valid(new Instruction))
  })

  val entry = Reg(Vec(32, new RSEntry))

  when (io.predict_failed) {
    for (member <- entry) {
      member.busy := false.B
    }
  } .otherwise {

  }
}
