import chisel3._
import chisel3.util._

// Memory Arbiter
class MA extends Module {
  val io = IO(new Bundle {
    val predict_failed = Input(Bool())

    // info from Instruction Cache
    val i_quest  = Flipped(Valid(UInt(32.W)))
    val i_result = Valid(UInt(32.W))

    // TODO : info from Data Cache / Data Memory Quest


    val mem_din  = Input(UInt(8.W))
    val mem_dout = Output(UInt(8.W))
    val mem_a    = Output(UInt(32.W))
    val mem_wr   = Output(Bool())
  })

  val state = RegInit(false.B) // Arbiter state : false is idle, true is reading from memory
  val just_finished = RegInit(false.B) // whether just finished reading 4 bytes
  val index = RegInit(0.U(32.W))
  
  val tmp_array = RegInit(VecInit(Seq.fill(4)(0.U(8.W))))
  
  io.i_result.bits := 0.U
  io.i_result.valid := false.B
  io.mem_dout := 0.U
  io.mem_a := 0.U
  io.mem_wr := false.B
  just_finished := false.B
  
  // printf("(MA) : state = %d\n", state)
  // printf("     : index = %d\n", index)
  // printf("     : just_finished = %d\n", just_finished)
  // printf("     : tmp_array(0) = %d\n", tmp_array(0))
  // printf("     : tmp_array(1) = %d\n", tmp_array(1))
  // printf("     : tmp_array(2) = %d\n", tmp_array(2))
  // printf("     : tmp_array(3) = %d\n", tmp_array(3))
  // printf("     : io.mem_din = %d\n", io.mem_din)
  // printf("     : io.i_result = %d\n", tmp_array(0) ## tmp_array(1) ## tmp_array(2) ## io.mem_din)

  when (io.predict_failed) {
    state := false.B
  } .otherwise {
    when (state) {
      val next_index = index + 1.U
      tmp_array(index(1, 0) & 3.U) := io.mem_din
      io.mem_a := next_index
      index := next_index
      when ((next_index(1, 0) & 3.U) === 3.U) {
        state := false.B
        just_finished := true.B
      }
    } .otherwise {
      when (io.i_quest.valid) {
        state := true.B
        io.mem_a := io.i_quest.bits
        index := io.i_quest.bits
      }
    }
    when (just_finished) {
      io.i_result.bits := tmp_array(0) ## tmp_array(1) ## tmp_array(2) ## io.mem_din
      io.i_result.valid := true.B
    }
  }
}