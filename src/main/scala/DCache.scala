import chisel3._
import chisel3.util._

class Dcache(val log_size : Int = 10) extends Module {
  val size = 1 << log_size

  val io = IO(new Bundle {
    val predict_failed = Input(Bool())

    // with LSQ
    val lsq_quest = Flipped(Valid(new MemoryQuest))
    val lsq_result_hit = Valid(UInt(32.W))
    val lsq_result_mem = Valid(UInt(32.W))

    // with WB
    val wb_quest = Flipped(Valid(new MemoryQuest))
    val wb_result_hit = Valid(UInt(32.W))
    val wb_result_mem = Valid(UInt(32.W))

    // with MA
    val mem_quest = Valid(new MemoryQuest)
    val mem_result = Flipped(Valid(UInt(32.W)))
  })

  val special_address_for_io = Wire(UInt(32.W))
  special_address_for_io := 196608.U
  val origin_value = RegInit(0.U(32.W))
  val state = RegInit(0.U(3.W)) // cache state : 0 -> idle,
                                //               1 -> input/output address shouldn't be cached
                                //               2 -> mem busy to read
                                //               3 -> ?
                                //               4 -> modify and write back (wb -- sb/sh)

  // entries
  val data_array = SyncReadMem(size, UInt(32.W))
  val tag_array = RegInit(VecInit(Seq.fill(size)(0.U(20.W))))
  val write_flag_array = RegInit(VecInit(Seq.fill(size)(false.B)))
  val valid_flag_array = RegInit(VecInit(Seq.fill(size)(false.B)))

  val lsq_index = io.lsq_quest.bits.addr(log_size + 1, 2)
  val lsq_quest_tag = io.lsq_quest.bits.addr(31, log_size + 2)
  val wb_index = io.wb_quest.bits.addr(log_size + 1, 2)
  val wb_quest_tag = io.wb_quest.bits.addr(31, log_size + 2)

  val lsq_hit_result_valid = RegInit(false.B)
  val lsq_mem_result_valid = RegInit(false.B)
  val lsq_mem_result_bits = RegInit(0.U(32.W))

  val wb_hit_result_valid = RegInit(false.B)
  val wb_mem_result_valid = RegInit(false.B)
  val wb_mem_result_bits = RegInit(0.U(32.W))

  io.mem_quest.valid := false.B
  io.mem_quest.bits := 0.U

  lsq_hit_result_valid := false.B
  lsq_mem_result_valid := false.B
  lsq_mem_result_bits := 0.U

  wb_hit_result_valid := false.B
  wb_mem_result_valid := false.B
  wb_mem_result_bits := 0.U

  when (io.predict_failed) {
    state := 0.U
  } .otherwise {
    switch (state) {
      is (0.U) { // idle
        when (io.wb_quest.valid) {
          when (io.wb_quest.bits.addr === special_address_for_io) { // read input / write output, cannot be cached
            state := 1.U
            io.mem_quest := io.wb_quest
          } .elsewhen (io.wb_quest.bits.wr_en) { // write
            when (valid_flag_array(wb_index)) { // either hit or crashed
              when (tag_array(wb_index) === wb_quest_tag) { // hit
                switch (io.wb_quest.bits.size) {
                  is (0.U) { // sb
                    origin_value := data_array.read(wb_index)
                    state := 4.U
                  }
                  is (1.U) { // sh
                    origin_value := data_array.read(wb_index)
                    state := 4.U
                  }
                  is (2.U) { // sw
                    data_array.write(wb_index, io.wb_quest.bits.value)
                    write_flag_array(wb_index) := true.B
                    state := 0.U
                  }
                }
              } .otherwise { // crashed
                when (write_flag_array(wb_index)) { // write back

                } .otherwise { // overwrite
                  // todo
                  tag_array(wb_index) := wb_quest_tag
                }
              }
            } .otherwise {
              // todo: empty place, just write in
            }
          } .otherwise { // read
            // todo
          }
        } .elsewhen (io.lsq_quest.valid) {
          // todo
        }
      }

      is (1.U) { // input/output address shouldn't be cached, pass the quest to MA directly
        when (io.mem_result.valid) {
          when (io.lsq_quest.valid) {
            io.lsq_result_mem := io.mem_result
          } .otherwise {
            io.wb_result_mem := io.mem_result
          }
          state := 0.U
        } .otherwise {
          io.mem_quest := Mux(io.lsq_quest.valid, io.lsq_quest, io.wb_quest)
        }
      }

      is (2.U) { // mem busy to read (write result in cache)
        when (io.mem_result.valid) {
          state := 0.U
          when (io.lsq_quest.valid) { // for lsq
            lsq_mem_result_valid := true.B
            lsq_mem_result_bits := io.mem_result.bits
            valid_flag_array(lsq_index) := true.B
            tag_array(lsq_index) := lsq_quest_tag
            data_array.write(lsq_index, io.mem_result.bits)
            write_flag_array(lsq_index) := true.B
          } .otherwise { // for wb
            wb_mem_result_valid := true.B
            wb_mem_result_bits := io.mem_result.bits
            valid_flag_array(wb_index) := true.B
            tag_array(wb_index) := wb_quest_tag
            data_array.write(wb_index, io.mem_result.bits)
            write_flag_array(wb_index) := true.B
          }
        } .otherwise {
          io.mem_quest.valid := true.B
          io.mem_quest.bits
        }
      }

      is (3.U) { //

      }

      is (4.U) { // modify and write back (wb -- sb/sh)
        val to_store = Wire(UInt(32.W))
        when (io.wb_quest.bits.size === 0.U) { // sb
          switch (io.wb_quest.bits.addr(1, 0)) {
            is (0.U) { // origin(31, 8) ## wb_val(7, 0)
              to_store := origin_value(31, 8) ## io.wb_quest.bits.value(7, 0)
            }
            is (1.U) { // origin(31, 16) ## wb_val(7, 0) ## origin(7, 0)
              to_store := origin_value(31, 16) ## io.wb_quest.bits.value(7, 0) ## origin_value(7, 0)
            }
            is (2.U) { // origin(31, 24) ## wb_val(7, 0) ## origin(15, 0)
              to_store := origin_value(31, 24) ## io.wb_quest.bits.value(7, 0) ## origin_value(7, 0)
            }
            is (3.U) { // wb_val(7, 0) ## origin(23, 0)
              to_store := io.wb_quest.bits.value(7, 0) ## origin_value(23, 0)
            }
          }
        } .otherwise { // sh
          switch (io.wb_quest.bits.addr(1, 0)) {
            is (0.U) { // origin(31, 16) ## wb_val(15, 0)
              to_store := origin_value(31, 16) ## io.wb_quest.bits.value(15, 0)
            }
            is (2.U) { // wb_val(15, 0) ## origin(15, 0)
              to_store := io.wb_quest.bits.value(15, 0) ## origin_value(15, 0)
            }
          }
        }
        data_array.write(wb_index, to_store)
        write_flag_array(wb_index) := true.B
        state := 0.U
      }
    }
  }

  io.lsq_result_hit.valid := lsq_hit_result_valid
  io.lsq_result_hit.bits := Mux(io.lsq_quest.bits.addr === special_address_for_io, 0.U, data_array.read(lsq_index))
  io.wb_result_hit.valid := wb_hit_result_valid
  io.wb_result_hit.bits := Mux(io.wb_quest.bits.addr === special_address_for_io, 0.U, data_array.read(wb_index))
  io.lsq_result_mem.valid := lsq_mem_result_valid
  io.lsq_result_mem.bits := lsq_mem_result_bits
  io.wb_result_mem.valid := wb_mem_result_valid
  io.wb_result_mem.bits := wb_mem_result_bits
}