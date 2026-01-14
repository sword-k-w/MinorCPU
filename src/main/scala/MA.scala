import chisel3._
import chisel3.util._

// Memory Arbiter
class MA extends Module {
  val io = IO(new Bundle {
    val predict_failed = Input(Bool())

    // info from Instruction Cache
    val i_quest  = Flipped(Valid(UInt(32.W)))
    val i_result = Valid(UInt(32.W))

    // info from LSQ
    val d_quest_from_lsq = Flipped(Valid(new MemoryQuest))
    val d_result_to_lsq = Valid(UInt(32.W))

    // info from WB
    val d_quest_from_wb = Flipped(Valid(new MemoryQuest))
    val d_result_to_wb = Valid(UInt(32.W))

    // TODO : info from Data Cache

    val mem_din  = Input(UInt(8.W))   // 1-byte dara we asked for
    val mem_dout = Output(UInt(8.W))  // 1-byte data to write
    val mem_a    = Output(UInt(32.W)) // we hope to read data from / write data to here
    val mem_wr   = Output(Bool())     // write enable
  })

  val state = RegInit(0.U(3.W)) // Arbiter state : 0 -> idle, 1 -> reading instruction for ICache,
                                //                 2 -> reading data for LSQ, 3 -> writing data for WB,
                                //                 4 -> reading data for WB, 5 -> halt
  val just_finished_i = RegInit(false.B)     // whether just finished reading 4 bytes of instruction for ICache
  val just_finished_d_lsq = RegInit(false.B) // whether just finished reading 4 bytes of data for LSQ
  val just_finished_d_wb = RegInit(false.B)  // whether just finished reading 4 bytes of data for WB

  val index = RegInit(0.U(32.W))             // a register that keeps memory address
  val tmp_array = RegInit(VecInit(Seq.fill(4)(0.U(8.W)))) // place the data together

  // output to ICache
  io.i_result.bits := 0.U
  io.i_result.valid := false.B
  just_finished_i := false.B

  // output to LSQ
  io.d_result_to_lsq.bits := 0.U
  io.d_result_to_lsq.valid := false.B
  just_finished_d_lsq := false.B

  // output to WB
  io.d_result_to_wb.bits := 0.U
  io.d_result_to_wb.valid := false.B
  just_finished_d_wb := false.B

  io.mem_dout := 0.U
  io.mem_a := 0.U
  io.mem_wr := false.B
  
  // printf("(MA) : state = %d\n", state)
  // printf("     : index = %d\n", index)
  // printf("     : just_finished_i = %d\n", just_finished_i)
  // printf("     : just_finished_d_lsq = %d\n", just_finished_d_lsq)
  // printf("     : just_finished_d_wb = %d\n", just_finished_d_wb)
  // printf("     : tmp_array(0) = %d\n", tmp_array(0))
  // printf("     : tmp_array(1) = %d\n", tmp_array(1))
  // printf("     : tmp_array(2) = %d\n", tmp_array(2))
  // printf("     : tmp_array(3) = %d\n", tmp_array(3))
  // printf("     : io.mem_din = %d\n", io.mem_din)
  // printf("     : io.mem_dout = %d\n", io.mem_dout)
  // printf("     : io.mem_a = %d\n", io.mem_a)
  // printf("     : io.mem_wr = %d\n", io.mem_wr)

  val lsq_max_index = Wire(UInt(2.W))
  lsq_max_index := 0.U
  when (io.d_quest_from_lsq.valid) {
    switch (io.d_quest_from_lsq.bits.size) {
      is (0.U) { lsq_max_index := 0.U }
      is (1.U) { lsq_max_index := 1.U }
      is (2.U) { lsq_max_index := 3.U }
    }
  }

  val wb_max_index = Wire(UInt(2.W))
  wb_max_index := 0.U
  when (io.d_quest_from_wb.valid) {
    switch (io.d_quest_from_wb.bits.size) {
      is (0.U) { wb_max_index := 0.U }
      is (1.U) { wb_max_index := 1.U }
      is (2.U) { wb_max_index := 3.U }
    }
  }

  when (io.predict_failed && state =/= 5.U) {
    state := 0.U(3.W)
  } .otherwise {
    switch (state) {
      is (0.U) { // idle
        when (just_finished_i) {
          io.i_result.bits := io.mem_din ## tmp_array(2) ## tmp_array(1) ## tmp_array(0)
          io.i_result.valid := true.B
        } .elsewhen(just_finished_d_lsq) {
          switch (lsq_max_index) {
            is (0.U) { io.d_result_to_lsq.bits := io.mem_din }
            is (1.U) { io.d_result_to_lsq.bits := io.mem_din ## tmp_array(0) }
            is (3.U) { io.d_result_to_lsq.bits := io.mem_din ## tmp_array(2) ## tmp_array(1) ## tmp_array(0) }
          }
          io.d_result_to_lsq.valid := true.B
        } .elsewhen(just_finished_d_wb) {
          switch (wb_max_index) {
            is (0.U) { io.d_result_to_wb.bits := io.mem_din }
            is (1.U) { io.d_result_to_wb.bits := io.mem_din ## tmp_array(0) }
            is (3.U) { io.d_result_to_wb.bits := io.mem_din ## tmp_array(2) ## tmp_array(1) ## tmp_array(0) }
          }
          io.d_result_to_wb.valid := true.B
        }

        // priority: WB/LSQ > ICache
        when(io.d_quest_from_wb.valid) {
          when (io.d_quest_from_wb.bits.wr_en) { // write
            when (io.d_quest_from_wb.bits.addr === 0xffffffff.U &&
                wb_max_index === 0.U && io.d_quest_from_wb.bits.value(7, 0) === 0.U) {
              state := 5.U(3.W)
              io.i_result.valid := false.B
              io.d_result_to_lsq.valid := false.B
              io.d_result_to_wb.valid := false.B
            } .otherwise {
              state := 3.U(3.W)
              tmp_array(0) := io.d_quest_from_wb.bits.value(7, 0)
              tmp_array(1) := io.d_quest_from_wb.bits.value(15, 8)
              tmp_array(2) := io.d_quest_from_wb.bits.value(23, 16)
              tmp_array(3) := io.d_quest_from_wb.bits.value(31, 24)
              io.mem_wr := true.B
              io.mem_a := io.d_quest_from_wb.bits.addr
              index := io.d_quest_from_wb.bits.addr
              io.mem_dout := io.d_quest_from_wb.bits.value(7, 0)
              when (wb_max_index === 0.U) {
                state := 0.U(3.W)
                io.d_result_to_wb.valid := true.B
              }
            }
          } .otherwise { // read
            state := 4.U(3.W)
            io.mem_a := io.d_quest_from_wb.bits.addr
            index := io.d_quest_from_wb.bits.addr
            when (wb_max_index === 0.U) {
              state := 0.U(3.W)
              just_finished_d_wb := true.B
            }
          }
        } .elsewhen (io.d_quest_from_lsq.valid) {
          state := 2.U(3.W)
          io.mem_a := io.d_quest_from_lsq.bits.addr
          index := io.d_quest_from_lsq.bits.addr
          when (lsq_max_index === 0.U) {
            state := 0.U(3.W)
            just_finished_d_lsq := true.B
          }
        } .elsewhen(io.i_quest.valid) {
          state := 1.U(3.W)
          io.mem_a := io.i_quest.bits
          index := io.i_quest.bits
        }
      }

      is (1.U) { // reading instruction for ICache
        val next_index = index + 1.U
        tmp_array(index(1, 0) & 3.U) := io.mem_din
        io.mem_a := next_index
        index := next_index
        when ((next_index(1, 0) & 3.U) === 3.U) {
          state := 0.U
          just_finished_i := true.B
        }
      }

      is (2.U) { // reading data for LSQ
        val next_index = index + 1.U
        tmp_array(index(1, 0) & 3.U) := io.mem_din
        io.mem_a := next_index
        index := next_index
        when ((next_index(1, 0) & 3.U) === lsq_max_index) {
          state := 0.U
          just_finished_d_lsq := true.B
        }
      }

      is (3.U) { // writing data for WB
        val next_index = index + 1.U
        io.mem_wr := true.B
        io.mem_a := next_index
        index := next_index
        io.mem_dout := tmp_array(next_index(1, 0) & 3.U)
        when ((next_index(1, 0) & 3.U) === wb_max_index) {
          state := 0.U
          io.d_result_to_wb.valid := true.B
        }
      }

      is (4.U) { // reading data for WB
        val next_index = index + 1.U
        tmp_array(index(1, 0) & 3.U) := io.mem_din
        io.mem_a := next_index
        index := next_index
        when ((next_index(1, 0) & 3.U) === wb_max_index) {
          state := 0.U
          just_finished_d_wb := true.B
        }
      }

      is (5.U) { // halt
        io.i_result.valid := false.B
        io.d_result_to_lsq.valid := false.B
        io.d_result_to_wb.valid := false.B
      }

    }
  }
}