import chisel3._
import chisel3.util._

class UpdateInfo extends Bundle {
  val hashed_pc = UInt(10.W)
  val actual_result = Bool()
}

class Predictor extends Module {
  val io = IO(new Bundle {
    val update_info = Flipped(Valid(new UpdateInfo))
    val queried_pc = Input(UInt(10.W))
    val predict_result = Output(Bool())
  })
  val confidence = RegInit(VecInit(Seq.fill(4096)(0.U(2.W)))) // index: pc(12, 2) ## the_second_latest ## latest
  val local_history = RegInit(VecInit(Seq.fill(1024)(0.U(2.W))))

  when (io.update_info.valid) {
    val history = local_history(io.update_info.bits.hashed_pc)
    val index = io.update_info.bits.hashed_pc ## history
    when (io.update_info.bits.actual_result) {
      when (confidence(index) =/= 3.U) {
        confidence(index) := confidence(index) + 1.U
      }
      local_history(io.update_info.bits.hashed_pc) := local_history(io.update_info.bits.hashed_pc)(0) ## 1.U
    } .otherwise {
      when (confidence(index) =/= 0.U) {
        confidence(index) := confidence(index) - 1.U
      }
      local_history(io.update_info.bits.hashed_pc) := local_history(io.update_info.bits.hashed_pc)(0) ## 0.U
    }
  }

  io.predict_result := confidence(io.queried_pc ## local_history(io.queried_pc))(1)

}