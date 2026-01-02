import chisel3._
import chisel3.util._

class RoBBroadcastResult extends Bundle {
  val dest = UInt(5.W)
  val value = UInt(32.W)
  val addr = UInt(32.W)
}

class LSQ extends Module {

}
