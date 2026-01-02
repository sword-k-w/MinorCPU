import chisel3._
import chisel3.util._


// If new dependence addr and qry addr are same, the qry can't see new dependence.
// Which is right, e.g. add x1 x1 x2
// Register File
class RF extends Module {
  val io = IO(new Bundle {
    val predict_failed = Input(Bool())

    val commit_valid  = Input(Bool())
    val commit_rob_id = Input(UInt(5.W))
    val commit_reg_id = Input(UInt(5.W))
    val commit_value  = Input(UInt(32.W))

    val new_dependence_valid = Input(Bool())
    val new_reg_id           = Input(UInt(5.W))
    val new_dependence       = Input(UInt(5.W))

    val qry1_addr           = Input(UInt(5.W))
    val qry1_val            = Output(UInt(32.W))
    val qry1_has_dependence = Output(Bool())
    val qry1_dependence     = Output(UInt(5.W))
   
    val qry2_addr           = Input(UInt(5.W))
    val qry2_val            = Output(UInt(32.W))
    val qry2_has_dependence = Output(Bool())
    val qry2_dependence     = Output(UInt(5.W))
  })

  val reg_data = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))
  val has_dependence = RegInit(VecInit(Seq.fill(32)(false.B)))
  val dependence = RegInit(VecInit(Seq.fill(32)(0.U(5.W))))

  when (io.predict_failed) {
    for (i <- 0 until 32) {
      has_dependence(i) := false.B
    }
  } .otherwise {
    when (io.commit_valid && io.commit_reg_id =/= 0.U) {
      reg_data(io.commit_reg_id) := io.commit_value
      when (io.commit_rob_id === dependence(io.commit_reg_id)) {
        has_dependence(io.commit_reg_id) := false.B
      }
    }
    when (io.new_dependence_valid && io.new_reg_id =/= 0.U) {
      dependence(io.new_reg_id) := io.new_dependence
      has_dependence(io.new_reg_id) := true.B
    }
  }

  io.qry1_val := reg_data(io.qry1_addr)
  io.qry1_has_dependence := has_dependence(io.qry1_addr)
  io.qry1_dependence := dependence(io.qry1_addr)

  io.qry2_val := reg_data(io.qry2_addr)
  io.qry2_has_dependence := has_dependence(io.qry2_addr)
  io.qry2_dependence := dependence(io.qry2_addr)
}