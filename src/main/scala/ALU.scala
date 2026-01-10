import chisel3._
import chisel3.util._

class ALUQuest extends Bundle {
  val in1 = UInt(32.W)
  val in2 = UInt(32.W)
  val in3 = UInt(32.W)
  val op = UInt(5.W)
  val funct = UInt(4.W) // funct7[1] | funct3
  val is_zero = Bool()
  val dest = UInt(5.W)
}

class ALU extends Module {
  val io = IO(new Bundle {
    val predict_failed = Input(Bool())

    val quest = Flipped(Valid(new ALUQuest))

    val broadcast_to_rob = Valid(new ALUToRoBResult)

    val broadcast_to_rs = Valid(new ToRSResult)

    val broadcast_to_lsq = Valid(new ALUBroadcastResult)
  })

  val broadcast_to_rob = Reg(new ALUToRoBResult)
  val broadcast_to_rob_valid = Reg(Bool())
  val broadcast_to_rs = Reg(new ToRSResult)
  val broadcast_to_rs_valid = Reg(Bool())
  val broadcast_to_lsq = Reg(new ALUBroadcastResult)
  val broadcast_to_lsq_valid = Reg(Bool())

  broadcast_to_rob_valid := false.B
  broadcast_to_rs_valid := false.B
  broadcast_to_lsq_valid := false.B
  broadcast_to_rob.mmio := false.B
  broadcast_to_lsq.mmio := false.B
  when (io.quest.valid && !io.predict_failed) {
    val res = Wire(UInt(5.W))
    res := 0.U
    when (io.quest.bits.op(2) === 1.U) {
      switch (io.quest.bits.funct) {
        is ("b0_000".U) { res := io.quest.bits.in1 + io.quest.bits.in2 }
        is ("b1_000".U) { res := io.quest.bits.in1 - io.quest.bits.in2 }
        is ("b0_111".U) { res := io.quest.bits.in1 & io.quest.bits.in2 }
        is ("b0_110".U) { res := io.quest.bits.in1 | io.quest.bits.in2 }
        is ("b0_100".U) { res := io.quest.bits.in1 ^ io.quest.bits.in2 }
        is ("b0_001".U) { res := io.quest.bits.in1 << io.quest.bits.in2(4, 0) }
        is ("b0_101".U) { res := io.quest.bits.in1 >> io.quest.bits.in2(4, 0) }
        is ("b1_101".U) { res := (io.quest.bits.in1.asSInt >> io.quest.bits.in2(4, 0)).asUInt }
        is ("b0_010".U) { res := (io.quest.bits.in1.asSInt < io.quest.bits.in2.asSInt).asUInt }
        is ("b0_011".U) { res := (io.quest.bits.in1 < io.quest.bits.in2).asUInt }
      }
    } .elsewhen (io.quest.bits.op(4) === 1.U && io.quest.bits.op(0) === 0.U) {
      switch (io.quest.bits.funct(2, 0)) {
        is ("b000".U) { res := (io.quest.bits.in1 === io.quest.bits.in2).asUInt }
        is ("b101".U) { res := (io.quest.bits.in1.asSInt >= io.quest.bits.in2.asSInt).asUInt }
        is ("b111".U) { res := (io.quest.bits.in1 >= io.quest.bits.in2).asUInt }
        is ("b100".U) { res := (io.quest.bits.in1.asSInt <= io.quest.bits.in2.asSInt).asUInt }
        is ("b110".U) { res := (io.quest.bits.in1 <= io.quest.bits.in2).asUInt }
        is ("b001".U) { res := (io.quest.bits.in1 =/= io.quest.bits.in2).asUInt }
      }
    } .otherwise {
      res := io.quest.bits.in1 + io.quest.bits.in2
    }
    when (io.quest.bits.op === "b00000".U) {
      when (res(17, 16) === "b11".U) { // mmio
        broadcast_to_rob_valid := true.B
        broadcast_to_rob.dest := io.quest.bits.dest
        broadcast_to_rob.addr := res
        broadcast_to_rob.mmio := true.B
        broadcast_to_lsq_valid := true.B
        broadcast_to_lsq.dest := io.quest.bits.dest
        broadcast_to_lsq.addr := res
        broadcast_to_lsq.mmio := true.B
      } .otherwise {
        broadcast_to_lsq_valid := true.B
        broadcast_to_lsq.dest := io.quest.bits.dest
        broadcast_to_lsq.addr := res
      }
    } .otherwise {
      when (io.quest.bits.op =/= "b01000".U) {
        broadcast_to_rs_valid := true.B
        broadcast_to_rs.dest := io.quest.bits.dest
        when (io.quest.bits.op === "b11001".U) {
          broadcast_to_rs.value := Mux(io.quest.bits.is_zero, 0.U, io.quest.bits.in3)
        } .otherwise {
          broadcast_to_rs.value := Mux(io.quest.bits.is_zero && io.quest.bits.op(1) === 1.U, 0.U, res)
        }
      }
      broadcast_to_rob_valid := true.B
      broadcast_to_rob.dest := io.quest.bits.dest
      broadcast_to_rob.addr := res
      when (io.quest.bits.op =/= "b01000".U && io.quest.bits.op =/= "b11001".U) {
        broadcast_to_rob.value := Mux(io.quest.bits.is_zero && io.quest.bits.op(1) === 1.U, 0.U, res)
      } .otherwise {
        broadcast_to_rob.value := io.quest.bits.in3
      }
    }
  }

  io.broadcast_to_rob.bits := broadcast_to_rob
  io.broadcast_to_rob.valid := broadcast_to_rob_valid
  io.broadcast_to_rs.bits := broadcast_to_rs
  io.broadcast_to_rs.valid := broadcast_to_rs_valid
  io.broadcast_to_lsq.bits := broadcast_to_lsq
  io.broadcast_to_lsq.valid := broadcast_to_lsq_valid
}