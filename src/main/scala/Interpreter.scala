import chisel3._
import chisel3.util._

class Interpreter extends Module {
  val io = IO(new Bundle {
    val mem_din        = Input(UInt(8.W))
    val mem_dout       = Output(UInt(8.W))
    val mem_a          = Output(UInt(32.W))
    val mem_wr         = Output(Bool())

    val dbgreg_dout    = Output(UInt(32.W))
  })

  // default
  io.mem_dout := 0.U
  io.mem_a := 0.U
  io.mem_wr := false.B
  io.dbgreg_dout := 0.U

  val pc = RegInit(0.U(32.W))
  val rf = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))
  val instruction_reg = RegInit(0.U(32.W))

  val stage = RegInit(0.U(32.W)) // 0 -> ask memory for instruction from current pc
                                 // 1 -> get instruction from memory

  switch (stage) {

    is (0.U) { // get instruction from current pc
      io.mem_a := pc
      pc := pc + 4.U // update pc
      stage := 1.U
    }

    is (1.U) { // get instruction from memory
      instruction_reg := io.mem_din
      val instruction = io.mem_din
      switch (instruction(6, 0)) {
        is (51.U) { // R
          val rs1 = instruction(19, 15)
          val rs2 = instruction(24, 20)
          val rd = instruction(11, 7)
          switch (instruction(14, 12)) { // switch by func3
            is (0.U) { // add / sub
              when (instruction(30)) { // sub
                rf(rd) := rf(rs1) - rf(rs2)
              } .otherwise { // add
                rf(rd) := rf(rs1) + rf(rs2)
              }
            }
            is (7.U) { // and
              rf(rd) := rf(rs1) & rf(rs2)
            }
            is (6.U) { // or
              rf(rd) := rf(rs1) | rf(rs2)
            }
            is (4.U) { // xor
              rf(rd) := rf(rs1) ^ rf(rs2)
            }
            is (1.U) { // sll
              rf(rd) := rf(rs1) << rf(rs2)
            }
            is (5.U) { // srl / sra
              val offset = rf(rs2)(4, 0)
              when (instruction(30)) { // sra
                rf(rd) := (rf(rs1).asSInt >> offset).asUInt
              } .otherwise { // srl
                rf(rd) := rf(rs1) >> offset
              }
            }
            is (2.U) { // slt
              when (rf(rs1).asSInt < rf(rs2).asSInt) {
                rf(rd) := 1.U
              } .otherwise {
                rf(rd) := 0.U
              }
            }
            is (3.U) { // sltu
              when (rf(rs1) < rf(rs2)) {
                rf(rd) := 1.U
              } .otherwise {
                rf(rd) := 0.U
              }
            }
          }
        }
        is (19.U) { // Arithmetic I
        }
        is (3.U) { // Memory I
        }
        is (35.U) { // S
        }
        is (99.U) { // B
        }
        is (111.U) { // jal
        }
        is (103.U) { // Control I
        }
        is (23.U) { // auipc
        }
        is (55.U) { // lui
        }
        is (115.U) { // Other I
        }
      }
    }
  }
}

object Main extends App {
  emitVerilog(new Interpreter(), Array("--target-dir", "generated"))
}