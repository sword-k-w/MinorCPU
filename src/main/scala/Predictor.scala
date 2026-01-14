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
  val confidence = RegInit(VecInit(Seq.fill(1024)(0.U(2.W))))

  when (io.update_info.valid) {
    when (io.update_info.bits.actual_result && confidence(io.update_info.bits.hashed_pc) =/= 3.U) {
      confidence(io.update_info.bits.hashed_pc) := confidence(io.update_info.bits.hashed_pc) + 1.U
    } .elsewhen (io.update_info.bits.actual_result === false.B && confidence(io.update_info.bits.hashed_pc) =/= 0.U) {
      confidence(io.update_info.bits.hashed_pc) := confidence(io.update_info.bits.hashed_pc) - 1.U
    }
  }

  when (confidence(io.queried_pc)(1) === 1.U) {
    io.predict_result := true.B
  } .otherwise {
    io.predict_result := false.B
  }
}