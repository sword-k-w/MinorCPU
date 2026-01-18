import chisel3._
import chisel3.util._

class DCache(val log_size : Int = 8) extends Module {

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

  val lsq_quest_reg = Reg(Valid(new MemoryQuest))
  val wb_quest_reg = Reg(Valid(new MemoryQuest))
  lsq_quest_reg := io.lsq_quest
  wb_quest_reg := io.wb_quest

  val special_address_for_io_1 = Wire(UInt(32.W))
  val special_address_for_io_2 = Wire(UInt(32.W))
  special_address_for_io_1 := 196608.U
  special_address_for_io_2 := 196612.U
  val data_in_crash = RegInit(0.U(32.W))
  val write_back_task = RegInit(0.U.asTypeOf(Valid(new MemoryQuest)))
  val read_task = RegInit(0.U.asTypeOf(Valid(new MemoryQuest)))
  val state = RegInit(0.U(4.W)) // cache state : 0 -> idle
  //               1 -> input/output address shouldn't be cached, pass the quest to MA directly
  //               2 -> ask memory for data, when we get data, complete the quest and store it in cache
  //               3 -> write crashed data back to memory
  //               4 -> modify and write back (wb write hit)
  //               5 -> a 1-cycle pause for hit
  //               6 -> write crashed data back to memory

  // entries
  val data_array = SyncReadMem(size, UInt(32.W))
  val tag_array = RegInit(VecInit(Seq.fill(size)(0.U(20.W))))
  val write_flag_array = RegInit(VecInit(Seq.fill(size)(false.B))) // show whether the word should be written back
  val valid_flag_array = RegInit(VecInit(Seq.fill(size)(false.B)))

  // quest info
  val lsq_index = lsq_quest_reg.bits.addr(log_size + 1, 2)
  val lsq_quest_tag = lsq_quest_reg.bits.addr(31, log_size + 2)
  val wb_index = wb_quest_reg.bits.addr(log_size + 1, 2)
  val wb_quest_tag = wb_quest_reg.bits.addr(31, log_size + 2)

  val lsq_hit_result_valid = RegInit(false.B)
  val wb_hit_result_valid = RegInit(false.B)

  val hit_array_data_lsq = Reg(UInt(32.W))
  val hit_array_data_wb = Reg(UInt(32.W))

  when (tag_array(wb_index) === wb_quest_tag) {
    hit_array_data_wb := data_array.read(wb_index)
  } .otherwise {
    hit_array_data_lsq := data_array.read(lsq_index)
  }

  val origin_value = data_array.read(wb_index)

  def WriteBackCrashed (crashed_index: UInt, crashed_data: UInt): Unit = {
    val quest = Wire(new MemoryQuest)
    quest.wr_en := true.B
    quest.size := 2.U
    quest.addr := tag_array(crashed_index) ## crashed_index ## 0.U(2.W)
    quest.value := crashed_data

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
        when (wb_quest_reg.valid) { // wb
          when (wb_quest_reg.bits.addr === special_address_for_io_1 ||
            wb_quest_reg.bits.addr === special_address_for_io_2) { // read input/clocks or write output/program stop,
            // cannot be cached
            state := 1.U
            io.mem_quest := wb_quest_reg
          } .elsewhen (wb_quest_reg.bits.wr_en) { // write
            when (valid_flag_array(wb_index)) { // either hit or crashed
              when (tag_array(wb_index) === wb_quest_tag) { // hit
                when (wb_quest_reg.bits.size === 2.U) {
                  wb_hit_result_valid := true.B
                  data_array.write(wb_index, wb_quest_reg.bits.value)
                  write_flag_array(wb_index) := true.B
                  state := 5.U
                } .otherwise {
                  state := 4.U
                }
              } .otherwise { // crashed
                when (write_flag_array(wb_index)) { // dirty, write back, read from memory then write modified data into cache
                  data_in_crash := data_array.read(wb_index)
                  state := 3.U
                } .otherwise { // clean, read from memory then write modified data into cache
                  when (wb_quest_reg.bits.size === 2.U) {
                    wb_hit_result_valid := true.B
                    data_array.write(wb_index, wb_quest_reg.bits.value)
                    write_flag_array(wb_index) := true.B
                    state := 5.U
                  } .otherwise {
                    SetReadingMission(wb_quest_reg.bits.addr(31, 2) ## 0.U(2.W))
                    state := 2.U
                  }
                }
              }
            } .otherwise { // not valid
              valid_flag_array(wb_index) := true.B
              tag_array(wb_index) := wb_quest_tag
              when (wb_quest_reg.bits.size === 2.U) {
                wb_hit_result_valid := true.B
                data_array.write(wb_index, wb_quest_reg.bits.value)
                write_flag_array(wb_index) := true.B
                state := 5.U
              } .otherwise {
                SetReadingMission(wb_quest_reg.bits.addr(31, 2) ## 0.U(2.W))
                state := 2.U
              }
            }
          } .otherwise { // read
            when (valid_flag_array(wb_index)) { // either hit or crashed
              when (tag_array(wb_index) === wb_quest_tag) { // hit
                wb_hit_result_valid := true.B
                state := 5.U
              } .otherwise { // crashed
                when (write_flag_array(wb_index) === false.B) { // not dirty, ask memory for data and store it in cache
                  SetReadingMission(wb_quest_reg.bits.addr(31, 2) ## 0.U(2.W))
                  state := 2.U
                } .otherwise { // dirty, write the dirty bytes back into memory, then ask memory for data and store it in cache
                  data_in_crash := data_array.read(wb_index)
                  state := 3.U
                }
              }
            } .otherwise { // not valid
              valid_flag_array(wb_index) := true.B
              SetReadingMission(wb_quest_reg.bits.addr(31, 2) ## 0.U(2.W))
              state := 2.U
            }
          }
        } .elsewhen (lsq_quest_reg.valid) { // lsq
          when (lsq_quest_reg.bits.addr === special_address_for_io_1 ||
            lsq_quest_reg.bits.addr === special_address_for_io_2) { // read input/clocks or write output/program stop,
            // cannot be cached
            state := 1.U
            io.mem_quest := lsq_quest_reg
          } .otherwise { // read
            when (valid_flag_array(lsq_index)) { // either hit or crashed
              when (tag_array(lsq_index) === lsq_quest_tag) { // hit
                lsq_hit_result_valid := true.B
                state := 5.U
              } .otherwise { // crashed
                when (write_flag_array(lsq_index) === false.B) { // not dirty, ask memory for data and store it in cache
                  SetReadingMission(lsq_quest_reg.bits.addr(31, 2) ## 0.U(2.W))
                  state := 2.U
                } .otherwise { // dirty, write the dirty bytes back into memory, then ask memory for data and store it in cache
                  data_in_crash := data_array.read(lsq_index)
                  state := 3.U
                }
              }
            } .otherwise { // not valid
              valid_flag_array(lsq_index) := true.B
              SetReadingMission(lsq_quest_reg.bits.addr(31, 2) ## 0.U(2.W))
              state := 2.U
            }
          }
        }
      }

      is (1.U) { // input/output address shouldn't be cached, pass the quest to MA directly
        when (io.mem_result.valid) {
          when (lsq_quest_reg.valid) {
            io.lsq_result_mem := io.mem_result
          } .otherwise {
            io.wb_result_mem := io.mem_result
          }
          state := 0.U
        } .otherwise {
          io.mem_quest := Mux(lsq_quest_reg.valid, lsq_quest_reg, wb_quest_reg)
        }
      }

      is (2.U) { // ask memory for data, when we get data, complete the quest and store it in cache
        when (io.mem_result.valid) { // get data (the whole 4 bytes)
          CancelMemReadingMission()
          state := 0.U
          when (lsq_quest_reg.valid) { // complete lsq quest, update cache
            io.lsq_result_mem.valid := true.B
            switch (lsq_quest_reg.bits.size) {
              is (0.U) { // lb
                switch (lsq_quest_reg.bits.addr(1, 0)) {
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
                when (lsq_quest_reg.bits.addr(1, 0) === 0.U) {
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
            write_flag_array(lsq_index) := false.B
          } .otherwise { // complete wb quest, update cache
            when (wb_quest_reg.bits.wr_en === true.B) { // write into crashed/invalid space
              val to_store = Wire(UInt(32.W))
              to_store := 0.U
              switch (wb_quest_reg.bits.size) {
                is (0.U) { // sb
                  switch (wb_quest_reg.bits.addr(1, 0)) {
                    is (0.U) {
                      to_store := io.mem_result.bits(31, 8) ## wb_quest_reg.bits.value(7, 0)
                    }
                    is (1.U) {
                      to_store := io.mem_result.bits(31, 16) ## wb_quest_reg.bits.value(7, 0) ## io.mem_result.bits(7, 0)
                    }
                    is (2.U) {
                      to_store := io.mem_result.bits(31, 24) ## wb_quest_reg.bits.value(7, 0) ## io.mem_result.bits(15, 0)
                    }
                    is (3.U) {
                      to_store := wb_quest_reg.bits.value(7, 0) ## io.mem_result.bits(23, 0)
                    }
                  }
                }
                is (1.U) { // sh
                  when (wb_quest_reg.bits.addr(1, 0) === 0.U) {
                    to_store := io.mem_result.bits(31, 16) ## wb_quest_reg.bits.value(15, 0)
                  } .otherwise {
                    to_store := wb_quest_reg.bits.value(15, 0) ## io.mem_result.bits(15, 0)
                  }
                }
                is (2.U) { // sw
                  to_store := wb_quest_reg.bits.value
                }
              }
              data_array.write(wb_index, to_store)
              tag_array(wb_index) := wb_quest_tag
              write_flag_array(wb_index) := true.B
              wb_hit_result_valid := true.B
              state := 5.U
            } .otherwise { // wb read
              io.wb_result_mem.valid := true.B
              switch (wb_quest_reg.bits.size) {
                is (0.U) { // lb
                  switch (wb_quest_reg.bits.addr(1, 0)) {
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
                  when (wb_quest_reg.bits.addr(1, 0) === 0.U) {
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
              write_flag_array(wb_index) := false.B
            }
          }
        } .otherwise { // keep asking for data
          io.mem_quest := read_task
        }
      }

      is (3.U) { // write crashed data back to memory
        when (wb_quest_reg.valid) {
          WriteBackCrashed(wb_index, data_in_crash)
        } .otherwise { // crashed by lsq-read
          WriteBackCrashed(lsq_index, data_in_crash)
        }
        state := 6.U
      }

      is (4.U) { // modify and write back (wb write hit)
        val to_store = Wire(UInt(32.W))
        to_store := 0.U
        when (wb_quest_reg.bits.size === 0.U) { // sb
          switch (wb_quest_reg.bits.addr(1, 0)) {
            is (0.U) { // origin(31, 8) ## wb_val(7, 0)
              to_store := origin_value(31, 8) ## wb_quest_reg.bits.value(7, 0)
            }
            is (1.U) { // origin(31, 16) ## wb_val(7, 0) ## origin(7, 0)
              to_store := origin_value(31, 16) ## wb_quest_reg.bits.value(7, 0) ## origin_value(7, 0)
            }
            is (2.U) { // origin(31, 24) ## wb_val(7, 0) ## origin(15, 0)
              to_store := origin_value(31, 24) ## wb_quest_reg.bits.value(7, 0) ## origin_value(15, 0)
            }
            is (3.U) { // wb_val(7, 0) ## origin(23, 0)
              to_store := wb_quest_reg.bits.value(7, 0) ## origin_value(23, 0)
            }
          }
        } .elsewhen (wb_quest_reg.bits.size === 1.U) { // sh
          switch (wb_quest_reg.bits.addr(1, 0)) {
            is (0.U) { // origin(31, 16) ## wb_val(15, 0)
              to_store := origin_value(31, 16) ## wb_quest_reg.bits.value(15, 0)
            }
            is (2.U) { // wb_val(15, 0) ## origin(15, 0)
              to_store := wb_quest_reg.bits.value(15, 0) ## origin_value(15, 0)
            }
          }
        } .otherwise { // sw
          to_store := wb_quest_reg.bits.value
        }
        write_flag_array(wb_index) := true.B
        data_array.write(wb_index, to_store)
        wb_hit_result_valid := true.B
        state := 5.U
      }

      is (5.U) { // a 1-cycle pause for hit
        state := 0.U
      }

      is (6.U) { // write crashed data back to memory
        when (io.mem_result.valid) { // dirty word has been written back
          CancelMemWritingMission()
          when (wb_quest_reg.valid) {
            SetReadingMission(wb_quest_reg.bits.addr(31, 2) ## 0.U(2.W))
          } .otherwise { // crashed by lsq-read
            SetReadingMission(lsq_quest_reg.bits.addr(31, 2) ## 0.U(2.W))
          }
          state := 2.U
        } .otherwise { // keep throwing quest
          io.mem_quest := write_back_task
        }
      }

    }
  }

  io.lsq_result_hit.valid := lsq_hit_result_valid
  io.lsq_result_hit.bits := 0.U
  when (lsq_quest_reg.bits.addr =/= special_address_for_io_1 && lsq_quest_reg.bits.addr =/= special_address_for_io_2) {
    switch (lsq_quest_reg.bits.size) {
      is (0.U) { // lb
        switch (lsq_quest_reg.bits.addr(1, 0)) {
          is (0.U) {
            io.lsq_result_hit.bits := 0.U(24.W) ## hit_array_data_lsq(7, 0)
          }
          is (1.U) {
            io.lsq_result_hit.bits := 0.U(24.W) ## hit_array_data_lsq(15, 8)
          }
          is (2.U) {
            io.lsq_result_hit.bits := 0.U(24.W) ## hit_array_data_lsq(23, 16)
          }
          is (3.U) {
            io.lsq_result_hit.bits := 0.U(24.W) ## hit_array_data_lsq(31, 24)
          }
        }
      }
      is (1.U) { // lh
        when (lsq_quest_reg.bits.addr(1, 0) === 2.U) {
          io.lsq_result_hit.bits := 0.U(16.W) ## hit_array_data_lsq(31, 16)
        } .otherwise {
          io.lsq_result_hit.bits := 0.U(16.W) ## hit_array_data_lsq(15, 0)
        }
      }
      is (2.U) { // lw
        io.lsq_result_hit.bits := hit_array_data_lsq
      }
    }
  }
  io.wb_result_hit.valid := wb_hit_result_valid
  io.wb_result_hit.bits := 0.U
  when (wb_quest_reg.bits.addr =/= special_address_for_io_1 && wb_quest_reg.bits.addr =/= special_address_for_io_2) {
    switch (wb_quest_reg.bits.size) {
      is (0.U) { // lb
        switch (wb_quest_reg.bits.addr(1, 0)) {
          is (0.U) {
            io.wb_result_hit.bits := 0.U(24.W) ## hit_array_data_wb(7, 0)
          }
          is (1.U) {
            io.wb_result_hit.bits := 0.U(24.W) ## hit_array_data_wb(15, 8)
          }
          is (2.U) {
            io.wb_result_hit.bits := 0.U(24.W) ## hit_array_data_wb(23, 16)
          }
          is (3.U) {
            io.wb_result_hit.bits := 0.U(24.W) ## hit_array_data_wb(31, 24)
          }
        }
      }
      is (1.U) { // lh
        when (wb_quest_reg.bits.addr(1, 0) === 2.U) {
          io.wb_result_hit.bits := 0.U(16.W) ## hit_array_data_wb(31, 16)
        } .otherwise {
          io.wb_result_hit.bits := 0.U(16.W) ## hit_array_data_wb(15, 0)
        }
      }
      is (2.U) { // lw
        io.wb_result_hit.bits := hit_array_data_wb
      }
    }
  }
}