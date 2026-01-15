import chisel3._
import chisel3.util._

class RAS extends Module {
  val io = IO(new Bundle {
    val new_address = Flipped(ValidIO(UInt(30.W)))
    val pop_valid = Input(Bool())
    val top  = Output(UInt(30.W))
  })
  val address = RegInit(VecInit(Seq.fill(32)(0.U(30.W))))
  val head = RegInit(31.U)
  when (io.new_address.valid) {
    val new_head = head + 1.U
    address(new_head) := io.new_address.bits
    head := new_head
  } .elsewhen (io.pop_valid) {
    head := head - 1.U
  }
  io.top := address(head)
}