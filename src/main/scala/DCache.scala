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
  val data_in_crash = RegInit(0.U(32.W))
  val need_mem_write_bytes = RegInit(0.U(4.W))
  val write_back_task = RegInit(0.U.asTypeOf(Valid(new MemoryQuest)))
  val read_task = RegInit(0.U.asTypeOf(Valid(new MemoryQuest)))
  val state = RegInit(0.U(4.W)) // cache state : 0 -> idle,
                                //               1 -> input/output address shouldn't be cached, pass the quest to MA directly
                                //               2 -> ask memory for data, when we get data, complete the quest and store it in cache
                                //               3 -> write data crashed by wb-write back to memory (begin)
                                //               4 -> modify and write back (wb write)
                                //               5 ->
                                //               6 -> write data crashed by wb-write back to memory (byte 3)
                                //               7 -> write data crashed by wb-write back to memory (byte 2)
                                //               8 -> write data crashed by wb-write back to memory (byte 1)
                                //               9 -> write data crashed by wb-write back to memory (byte 0)
                                //               10 ->

  // entries
  val data_array = SyncReadMem(size, UInt(32.W))
  val tag_array = RegInit(VecInit(Seq.fill(size)(0.U(20.W))))
  val write_flag_array = RegInit(VecInit(Seq.fill(size)(0.U(4.W)))) // show whether each byte should be written back
  val valid_flag_array = RegInit(VecInit(Seq.fill(size)(false.B)))

  // quest info
  val lsq_index = io.lsq_quest.bits.addr(log_size + 1, 2)
  val lsq_quest_tag = io.lsq_quest.bits.addr(31, log_size + 2)
  val wb_index = io.wb_quest.bits.addr(log_size + 1, 2)
  val wb_quest_tag = io.wb_quest.bits.addr(31, log_size + 2)

  val lsq_hit_result_valid = RegInit(false.B)
  val wb_hit_result_valid = RegInit(false.B)

  def WriteBackCrashed (crashed_index: UInt, crashed_data: UInt, target_byte_index: Int): Unit = {
    val quest = Wire(new MemoryQuest)
    quest.wr_en := true.B
    quest.size := 0.U
    quest.addr := tag_array(crashed_index) ## crashed_index ## target_byte_index.U(2.W)
    quest.value := crashed_data(8 * target_byte_index + 7, 8 * target_byte_index)

    write_back_task.valid := true.B
    write_back_task.bits := quest

    io.mem_quest.valid := true.B
    io.mem_quest.bits := quest
  }

  def CancelMemWritingMission () : Unit = {
    write_back_task.valid := false.B
    write_back_task.bits.value := 0.U
    write_back_task.bits.addr := 0.U
    write_back_task.bits.size := 0.U
    write_back_task.bits.wr_en := false.B
    io.mem_quest.valid := false.B
    io.mem_quest.bits.value := 0.U
    io.mem_quest.bits.addr := 0.U
    io.mem_quest.bits.size := 0.U
    io.mem_quest.bits.wr_en := false.B
  }

  def SetReadingMission (target_address: UInt): Unit = {
    val quest = Wire(new MemoryQuest)
    quest.wr_en := false.B
    quest.size  := 2.U
    quest.addr  := target_address
    quest.value := 0.U

    read_task.valid := true.B
    read_task.bits := quest

    io.mem_quest.valid := true.B
    io.mem_quest.bits := quest
  }

  def CancelMemReadingMission () : Unit = {
    read_task.valid := false.B
    read_task.bits.value := 0.U
    read_task.bits.addr := 0.U
    read_task.bits.size := 0.U
    read_task.bits.wr_en := false.B
    io.mem_quest.valid := false.B
    io.mem_quest.bits.value := 0.U
    io.mem_quest.bits.addr := 0.U
    io.mem_quest.bits.size := 0.U
    io.mem_quest.bits.wr_en := false.B
  }

  io.mem_quest.valid := false.B
  io.mem_quest.bits.addr := 0.U
  io.mem_quest.bits.value := 0.U
  io.mem_quest.bits.size := 0.U
  io.mem_quest.bits.wr_en := false.B
  io.lsq_result_mem.valid := false.B
  io.lsq_result_mem.bits := 0.U
  io.wb_result_mem.valid := false.B
  io.wb_result_mem.bits := 0.U

  lsq_hit_result_valid := false.B
  wb_hit_result_valid := false.B

  when (io.predict_failed) {
    state := 0.U
  } .otherwise {
    switch (state) {
      is (0.U) { // idle
        when (io.wb_quest.valid) { // wb
          when (io.wb_quest.bits.addr === special_address_for_io) { // read input / write output, cannot be cached
            state := 1.U
            io.mem_quest := io.wb_quest
          } .elsewhen (io.wb_quest.bits.wr_en) { // write
            when (valid_flag_array(wb_index)) { // either hit or crashed
              when (tag_array(wb_index) === wb_quest_tag) { // hit
                when (io.wb_quest.bits.size === 2.U) {
                  data_array.write(wb_index, io.wb_quest.bits.value)
                  write_flag_array(wb_index) := 15.U
                  state := 0.U
                } .otherwise {
                  origin_value := data_array.read(wb_index)
                  state := 4.U
                }
              } .otherwise { // crashed
                data_in_crash := data_array.read(wb_index)
                need_mem_write_bytes := write_flag_array(wb_index)
                tag_array(wb_index) := wb_quest_tag
                state := 3.U
              }
            } .otherwise { // not valid
              valid_flag_array(wb_index) := true.B
              tag_array(wb_index) := wb_quest_tag
              when (io.wb_quest.bits.size === 2.U) {
                data_array.write(wb_index, io.wb_quest.bits.value)
                write_flag_array(wb_index) := 15.U
                state := 0.U
              } .otherwise {
                origin_value := data_array.read(wb_index)
                state := 4.U
              }
            }
          } .otherwise { // read
            when (valid_flag_array(wb_index)) { // either hit or crashed
              when (tag_array(wb_index) === wb_quest_tag) { // hit
                wb_hit_result_valid := true.B
              } .otherwise { // crashed
                when (write_flag_array(wb_index) === 0.U) { // not dirty, ask memory for data and store it in cache
                  SetReadingMission(io.wb_quest.bits.addr(31, 2) ## 0.U(2.W))
                  state := 2.U
                } .otherwise { // dirty, write the dirty bytes back into memory, the ask memory for data and store it in cache
                  // todo
                }
              }
            } .otherwise { // not valid
              valid_flag_array(wb_index) := true.B
              SetReadingMission(io.wb_quest.bits.addr(31, 2) ## 0.U(2.W))
              state := 2.U
            }
          }
        } .elsewhen (io.lsq_quest.valid) { // lsq
          when (io.lsq_quest.bits.addr === special_address_for_io) { // read input / write output, cannot be cached
            state := 1.U
            io.mem_quest := io.lsq_quest
          } .otherwise { // read
            when (valid_flag_array(lsq_index)) { // either hit or crashed
              when (tag_array(lsq_index) === lsq_quest_tag) { // hit
                lsq_hit_result_valid := true.B
              } .otherwise { // crashed
                when (write_flag_array(lsq_index) === 0.U) { // not dirty, ask memory for data and store it in cache
                  SetReadingMission(io.lsq_quest.bits.addr(31, 2) ## 0.U(2.W))
                  state := 2.U
                } .otherwise { // dirty, write the dirty bytes back into memory, the ask memory for data and store it in cache
                  // todo
                }
              }
            } .otherwise { // not valid
              valid_flag_array(lsq_index) := true.B
              SetReadingMission(io.lsq_quest.bits.addr(31, 2) ## 0.U(2.W))
              state := 2.U
            }
          }
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

      is (2.U) { // ask memory for data, when we get data, complete the quest and store it in cache
        when (io.mem_result.valid) { // get data (the whole 4 bytes)
          CancelMemReadingMission()
          state := 0.U
          when (io.lsq_quest.valid) { // complete lsq quest, update cache
            io.lsq_result_mem.valid := true.B
            switch (io.lsq_quest.bits.size) {
              is (0.U) { // lb
                switch (io.lsq_quest.bits.addr(1, 0)) {
                  is (0.U) {
                    io.lsq_result_mem.bits := 0.U(24.W) ## io.mem_result.bits(7, 0)
                  }
                  is (1.U) {
                    io.lsq_result_mem.bits := 0.U(24.W) ## io.mem_result.bits(15, 8)
                  }
                  is (2.U) {
                    io.lsq_result_mem.bits := 0.U(24.W) ## io.mem_result.bits(23, 16)
                  }
                  is (3.U) {
                    io.lsq_result_mem.bits := 0.U(24.W) ## io.mem_result.bits(31, 24)
                  }
                }
              }
              is (1.U) { // lh
                when (io.lsq_quest.bits.addr(1, 0) === 0.U) {
                  io.lsq_result_mem.bits := 0.U(16.W) ## io.mem_result.bits(15, 0)
                } .otherwise {
                  io.lsq_result_mem.bits := 0.U(16.W) ## io.mem_result.bits(31, 16)
                }
              }
              is (2.U) { // lw
                io.lsq_result_mem.bits := io.mem_result.bits
              }
            }
            data_array.write(lsq_index, io.mem_result.bits)
            tag_array(lsq_index) := lsq_quest_tag
            write_flag_array(lsq_index) := 0.U
          } .otherwise { // complete wb quest, update cache
            io.wb_result_mem.valid := true.B
            switch (io.wb_quest.bits.size) {
              is (0.U) { // lb
                switch (io.wb_quest.bits.addr(1, 0)) {
                  is (0.U) {
                    io.wb_result_mem.bits := 0.U(24.W) ## io.mem_result.bits(7, 0)
                  }
                  is (1.U) {
                    io.wb_result_mem.bits := 0.U(24.W) ## io.mem_result.bits(15, 8)
                  }
                  is (2.U) {
                    io.wb_result_mem.bits := 0.U(24.W) ## io.mem_result.bits(23, 16)
                  }
                  is (3.U) {
                    io.wb_result_mem.bits := 0.U(24.W) ## io.mem_result.bits(31, 24)
                  }
                }
              }
              is (1.U) { // lh
                when (io.wb_quest.bits.addr(1, 0) === 0.U) {
                  io.wb_result_mem.bits := 0.U(16.W) ## io.mem_result.bits(15, 0)
                } .otherwise {
                  io.wb_result_mem.bits := 0.U(16.W) ## io.mem_result.bits(31, 16)
                }
              }
              is (2.U) { // lw
                io.wb_result_mem.bits := io.mem_result.bits
              }
            }
            data_array.write(wb_index, io.mem_result.bits)
            tag_array(wb_index) := wb_quest_tag
            write_flag_array(wb_index) := 0.U
          }
        } .otherwise { // keep asking for data
          io.mem_quest := Mux(io.lsq_quest.valid, io.lsq_quest, io.wb_quest)
        }
      }

      is (3.U) { // write data crashed by wb-write back to memory (begin)
        when (need_mem_write_bytes(3)) {
          WriteBackCrashed(wb_index, data_in_crash, 3)
          state := 6.U
        } .elsewhen (need_mem_write_bytes(2)) {
          WriteBackCrashed(wb_index, data_in_crash, 2)
          state := 7.U
        } .elsewhen (need_mem_write_bytes(1)) {
          WriteBackCrashed(wb_index, data_in_crash, 1)
          state := 8.U
        } .elsewhen (need_mem_write_bytes(0)) {
          WriteBackCrashed(wb_index, data_in_crash, 0)
          state := 9.U
        } .otherwise { // finished write-into-memory task
          origin_value := data_array.read(wb_index)
          state := 4.U
        }
      }

      is (4.U) { // modify and write back (wb write)
        val to_store = Wire(UInt(32.W))
        when (io.wb_quest.bits.size === 0.U) { // sb
          switch (io.wb_quest.bits.addr(1, 0)) {
            is (0.U) { // origin(31, 8) ## wb_val(7, 0)
              to_store := origin_value(31, 8) ## io.wb_quest.bits.value(7, 0)
              write_flag_array(wb_index) := write_flag_array(wb_index)(3, 1) ## 1.U(1.W)
            }
            is (1.U) { // origin(31, 16) ## wb_val(7, 0) ## origin(7, 0)
              to_store := origin_value(31, 16) ## io.wb_quest.bits.value(7, 0) ## origin_value(7, 0)
              write_flag_array(wb_index) := write_flag_array(wb_index)(3, 2) ## 1.U(1.W) ## write_flag_array(wb_index)(0)
            }
            is (2.U) { // origin(31, 24) ## wb_val(7, 0) ## origin(15, 0)
              to_store := origin_value(31, 24) ## io.wb_quest.bits.value(7, 0) ## origin_value(15, 0)
              write_flag_array(wb_index) := write_flag_array(wb_index)(3) ## 1.U(1.W) ## write_flag_array(wb_index)(1, 0)
            }
            is (3.U) { // wb_val(7, 0) ## origin(23, 0)
              to_store := io.wb_quest.bits.value(7, 0) ## origin_value(23, 0)
              write_flag_array(wb_index) := 1.U(1.W) ## write_flag_array(wb_index)(2, 0)
            }
          }
        } .elsewhen (io.wb_quest.bits.size === 1.U) { // sh
          switch (io.wb_quest.bits.addr(1, 0)) {
            is (0.U) { // origin(31, 16) ## wb_val(15, 0)
              to_store := origin_value(31, 16) ## io.wb_quest.bits.value(15, 0)
              write_flag_array(wb_index) := write_flag_array(wb_index)(3, 2) ## 3.U(2.W)
            }
            is (2.U) { // wb_val(15, 0) ## origin(15, 0)
              to_store := io.wb_quest.bits.value(15, 0) ## origin_value(15, 0)
              write_flag_array(wb_index) := 3.U(2.W) ## write_flag_array(wb_index)(1, 0)
            }
          }
        } .otherwise { // sw
          to_store := io.wb_quest.bits.value
          write_flag_array(wb_index) := 15.U(4.W)
        }
        data_array.write(wb_index, to_store)
        state := 0.U
      }

      is (5.U) { //
      }

      is (6.U) { // write data crashed by wb-write back to memory (byte 3)
        when (io.mem_result.valid) { // byte 3 has been written back
          need_mem_write_bytes(3) := 0.U(1.W)
          when (need_mem_write_bytes(2)) {
            WriteBackCrashed(wb_index, data_in_crash, 2)
            state := 7.U
          } .elsewhen (need_mem_write_bytes(1)) {
            WriteBackCrashed(wb_index, data_in_crash, 1)
            state := 8.U
          } .elsewhen (need_mem_write_bytes(0)) {
            WriteBackCrashed(wb_index, data_in_crash, 0)
            state := 9.U
          } .otherwise { // finished write-into-memory task
            CancelMemWritingMission()
            origin_value := data_array.read(wb_index)
            state := 4.U
          }
        } .otherwise { // keep throwing quest
          io.mem_quest := write_back_task
        }
      }

      is (7.U) { // write data crashed by wb-write back to memory (byte 2)
        when (io.mem_result.valid) { // byte 2 has been written back
          need_mem_write_bytes(2) := 0.U(1.W)
          when (need_mem_write_bytes(1)) {
            WriteBackCrashed(wb_index, data_in_crash, 1)
            state := 8.U
          } .elsewhen (need_mem_write_bytes(0)) {
            WriteBackCrashed(wb_index, data_in_crash, 0)
            state := 9.U
          } .otherwise { // finished write-into-memory task
            CancelMemWritingMission()
            origin_value := data_array.read(wb_index)
            state := 4.U
          }
        } .otherwise { // keep throwing quest
          io.mem_quest := write_back_task
        }
      }

      is (8.U) { // write data crashed by wb-write back to memory (byte 1)
        when (io.mem_result.valid) { // byte 1 has been written back
          need_mem_write_bytes(1) := 0.U(1.W)
          when (need_mem_write_bytes(0)) {
            WriteBackCrashed(wb_index, data_in_crash, 0)
            state := 9.U
          } .otherwise { // finished write-into-memory task
            CancelMemWritingMission()
            origin_value := data_array.read(wb_index)
            state := 4.U
          }
        } .otherwise { // keep throwing quest
          io.mem_quest := write_back_task
        }
      }

      is (9.U) { // write data crashed by wb-write back to memory (byte 0)
        when (io.mem_result.valid) { // byte 0 has been written back
          need_mem_write_bytes(0) := 0.U(1.W)
          CancelMemWritingMission()
          origin_value := data_array.read(wb_index)
          state := 4.U
        } .otherwise { // keep throwing quest
          io.mem_quest := write_back_task
        }
      }

    }
  }

  io.lsq_result_hit.valid := lsq_hit_result_valid
  when (io.lsq_quest.bits.addr === special_address_for_io) {
    io.lsq_result_hit.bits := 0.U
  } .otherwise {
    switch (io.lsq_quest.bits.size) {
      is (0.U) { // lb
        switch (io.lsq_quest.bits.addr(1, 0)) {
          is (0.U) {
            io.lsq_result_hit.bits := 0.U(24.W) ## data_array.read(lsq_index)(7, 0)
          }
          is (1.U) {
            io.lsq_result_hit.bits := 0.U(16.W) ## data_array.read(lsq_index)(15, 8) ## 0.U(8.W)
          }
          is (2.U) {
            io.lsq_result_hit.bits := 0.U(8.W) ## data_array.read(lsq_index)(23, 16) ## 0.U(16.W)
          }
          is (3.U) {
            io.lsq_result_hit.bits := data_array.read(lsq_index)(31, 24) ## 0.U(24.W)
          }
        }
      }
      is (1.U) { // lh
        when (io.lsq_quest.bits.addr(1, 0) === 2.U) {
          io.lsq_result_hit.bits := data_array.read(lsq_index)(31, 16) ## 0.U(16.W)
        } .otherwise {
          io.lsq_result_hit.bits := 0.U(16.W) ## data_array.read(lsq_index)(15, 0)
        }
      }
      is (2.U) { // lw
        io.lsq_result_hit.bits := data_array.read(lsq_index)
      }
    }
  }
  io.wb_result_hit.valid := wb_hit_result_valid
  when (io.wb_quest.bits.addr === special_address_for_io) {
    io.wb_result_hit.bits := 0.U
  } .otherwise {
    switch (io.wb_quest.bits.size) {
      is (0.U) { // lb
        switch (io.wb_quest.bits.addr(1, 0)) {
          is (0.U) {
            io.wb_result_hit.bits := 0.U(24.W) ## data_array.read(wb_index)(7, 0)
          }
          is (1.U) {
            io.wb_result_hit.bits := 0.U(16.W) ## data_array.read(wb_index)(15, 8) ## 0.U(8.W)
          }
          is (2.U) {
            io.wb_result_hit.bits := 0.U(8.W) ## data_array.read(wb_index)(23, 16) ## 0.U(16.W)
          }
          is (3.U) {
            io.wb_result_hit.bits := data_array.read(wb_index)(31, 24) ## 0.U(24.W)
          }
        }
      }
      is (1.U) { // lh
        when (io.wb_quest.bits.addr(1, 0) === 2.U) {
          io.wb_result_hit.bits := data_array.read(wb_index)(31, 16) ## 0.U(16.W)
        } .otherwise {
          io.wb_result_hit.bits := 0.U(16.W) ## data_array.read(wb_index)(15, 0)
        }
      }
      is (2.U) { // lw
        io.wb_result_hit.bits := data_array.read(wb_index)
      }
    }
  }
}