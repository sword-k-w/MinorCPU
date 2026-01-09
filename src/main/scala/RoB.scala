import chisel3._
import chisel3.util._

class ALUToRoBResult extends Bundle {
  val dest = UInt(5.W)
  val value = UInt(32.W)
  val addr = UInt(32.W)
}

class LSQToRoBResult extends Bundle {
  val dest = UInt(5.W)
  val value = UInt(32.W)
}

class RoBEntry extends Bundle {
  val ready = Bool()
  val instruction = new Instruction
  val value = UInt(32.W)
  val addr = UInt(32.W)
  // TODO : other info
}

// Reorder Buffer
class RoB extends Module {
  val io = IO(new Bundle {
    val predict_failed = Output(Bool())

    val new_instruction = Flipped(Decoupled(new Instruction))

    // remember delay a cycle in ALU
    val alu_broadcast_result = Flipped(Valid(new ALUToRoBResult))

    // remember delay a cycle in LSQ
    val lsq_broadcast_result = Flipped(Valid(new LSQToRoBResult))

    val broadcast_to_lsq = Valid(new RoBBroadcastResult)

    val commit_to_rf = Valid(new CommitInfo)

    val modified_pc = Valid(UInt(32.W))

    val qry1_index = Input(UInt(5.W))
    val qry1_ready = Output(Bool())
    val qry1_value = Output(UInt(32.W))

    val qry2_index = Input(UInt(5.W))
    val qry2_ready = Output(Bool())
    val qry2_value = Output(UInt(32.W))

    val rob_tail = Output(UInt(5.W))
  })

  val entry = Reg(Vec(32, new RoBEntry))
  val head = RegInit(0.U(5.W))
  val tail = RegInit(0.U(5.W))
  val predict_failed = RegInit(false.B)

  val broadcast_to_lsq_valid = Reg(Bool())
  val broadcast_to_lsq = Reg(new RoBBroadcastResult)

  broadcast_to_lsq_valid := false.B
  broadcast_to_lsq.addr := 0.U
  broadcast_to_lsq.value := 0.U
  broadcast_to_lsq.dest := 0.U

  val commit_to_rf_valid = Reg(Bool())
  val commit_to_rf = Reg(new CommitInfo)

  commit_to_rf_valid := false.B
  commit_to_rf.value := 0.U
  commit_to_rf.reg_id := 0.U
  commit_to_rf.rob_id := 0.U

  when (predict_failed) {
    head := 0.U
    tail := 0.U
    predict_failed := false.B
    io.new_instruction.ready := true.B
  } .otherwise {
    when (io.alu_broadcast_result.valid) {
      entry(io.alu_broadcast_result.bits.dest).ready := true.B
      entry(io.alu_broadcast_result.bits.dest).value := io.alu_broadcast_result.bits.value
      entry(io.alu_broadcast_result.bits.dest).addr := io.alu_broadcast_result.bits.addr
    }
    when (io.lsq_broadcast_result.valid) {
      entry(io.lsq_broadcast_result.bits.dest).ready := true.B
      entry(io.lsq_broadcast_result.bits.dest).value := io.lsq_broadcast_result.bits.value
    }

    // commit.
    // because the update of entry has one cycle latency, add special judge for commit condition
    when (head =/= tail && (entry(head).ready
      || (io.alu_broadcast_result.valid && io.alu_broadcast_result.bits.dest === head)
      || (io.lsq_broadcast_result.valid && io.lsq_broadcast_result.bits.dest === head))) {
      when (entry(head).instruction.op === "b11000".U || entry(head).instruction.op === "b11001".U) { // branch or jalr
        when (entry(head).instruction.predict_address =/= entry(head).value(31, 2)) {
          predict_failed := true.B
          io.predict_failed := true.B
          io.modified_pc.valid := true.B
          io.modified_pc.bits := entry(head).value
          // maybe something else need to do?
        }
      } .elsewhen(entry(head).instruction.op === "b01000".U) { // S
        broadcast_to_lsq_valid := true.B
        broadcast_to_lsq.addr := entry(head).addr
        broadcast_to_lsq.value := entry(head).value
        broadcast_to_lsq.dest := head
      } .otherwise {
        commit_to_rf_valid := true.B
        commit_to_rf.rob_id := head
        commit_to_rf.reg_id := entry(head).instruction.rd
        commit_to_rf.value := entry(head).value
      }
      head := head + 1.U
    }
    when (io.new_instruction.valid) {
      entry(tail).instruction := io.new_instruction.bits
      entry(tail).ready := false.B
      tail := tail + 1.U
    }
    io.new_instruction.ready := head + 2.U === tail
  }

  io.broadcast_to_lsq.bits := broadcast_to_lsq
  io.broadcast_to_lsq.valid := broadcast_to_lsq_valid
  io.commit_to_rf.bits := commit_to_rf
  io.commit_to_rf.valid := commit_to_rf_valid

  io.qry1_ready := entry(io.qry1_index).ready
  io.qry1_value := entry(io.qry1_index).value

  io.qry2_ready := entry(io.qry2_index).ready
  io.qry2_value := entry(io.qry2_index).value

  io.rob_tail := tail
}
