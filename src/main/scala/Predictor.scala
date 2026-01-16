import chisel3._
import chisel3.util._

class UpdateInfo extends Bundle {
  val hashed_pc = UInt(8.W)
  val actual_result = Bool()
}

class Predictor extends Module {
  val io = IO(new Bundle {
    val update_info = Flipped(Valid(new UpdateInfo))
    val queried_pc = Input(UInt(6.W))
    val predict_result = Output(Bool())
  })
  val confidence = RegInit(VecInit(Seq.fill(256)(0.U(2.W)))) // index: pc(12, 2) ## the_second_latest ## latest
  val local_history = RegInit(VecInit(Seq.fill(64)(0.U(2.W))))

  when (io.update_info.valid) {
    val history = local_history(io.update_info.bits.hashed_pc)
    val index = io.update_info.bits.hashed_pc ## history
    val cfd = TreeMux.TreeMux(index, confidence.toSeq)
    val lh = TreeMux.TreeMux(io.update_info.bits.hashed_pc, local_history.toSeq)
    when (io.update_info.bits.actual_result) {
      when (cfd =/= 3.U) {
        confidence(index) := cfd + 1.U
      }
      local_history(io.update_info.bits.hashed_pc) := lh(0) ## 1.U
    } .otherwise {
      when (confidence(index) =/= 0.U) {
        confidence(index) := confidence(index) - 1.U
      }
      local_history(io.update_info.bits.hashed_pc) := lh(0) ## 0.U
    }
  }

  io.predict_result := confidence(io.queried_pc ## TreeMux.TreeMux(io.queried_pc, local_history.toSeq))(1)

}