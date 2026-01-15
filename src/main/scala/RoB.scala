import chisel3._
import chisel3.util._

class ALUToRoBResult extends Bundle {
  val dest = UInt(5.W)
  val value = UInt(32.W)
  val addr = UInt(32.W)
  val mmio = Bool()
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
  val mmio_ready = Bool()
  // TODO : other info
}

// Reorder Buffer
class RoB extends Module {
  val io = IO(new Bundle {
    val wb_is_full = Input(Bool())

    val new_instruction = Flipped(Decoupled(new Instruction))

    // remember delay a cycle in ALU
    val alu_broadcast_result = Flipped(Valid(new ALUToRoBResult))

    // remember delay a cycle in LSQ
    val lsq_broadcast_result = Flipped(Valid(new LSQToRoBResult))

    val wb_broadcast_result = Flipped(Valid(new LSQToRoBResult))

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

    val predict_feedback = ValidIO(new UpdateInfo)
  })

  io.modified_pc.valid := false.B
  io.modified_pc.bits := 0.U

  val entry = RegInit(VecInit(Seq.fill(32)(0.U.asTypeOf(new RoBEntry))))

  val head = RegInit(0.U(5.W))
  val tail = RegInit(0.U(5.W))
  val predict_failed = RegInit(false.B)

  val broadcast_to_lsq_valid = Reg(Bool())
  val broadcast_to_lsq = Reg(new RoBBroadcastResult)

  val new_head = Wire(UInt(5.W))
  val new_tail = Wire(UInt(5.W))
  val new_entry = Wire(Vec(32, new RoBEntry))

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

  val modified_pc = Reg(UInt(32.W))

  io.predict_feedback.valid := false.B
  io.predict_feedback.bits.hashed_pc := 0.U
  io.predict_feedback.bits.actual_result := 0.U

  for (i <- 0 until 32) {
    new_entry(i) := entry(i)
  }
  new_head := head
  new_tail := tail

  when (predict_failed) {
    new_head := 0.U
    new_tail := 0.U
    predict_failed := false.B
  } .otherwise {
    when (io.new_instruction.valid) {
      new_tail := tail + 1.U
    }
    for (i <- 0 until 32) {
      when (i.U === io.alu_broadcast_result.bits.dest && io.alu_broadcast_result.valid) {
        new_entry(i).ready := true.B // pretend to commit if mmio
        new_entry(i).value := io.alu_broadcast_result.bits.value
        new_entry(i).addr := io.alu_broadcast_result.bits.addr
        new_entry(i).instruction.mmio := io.alu_broadcast_result.bits.mmio
      } .elsewhen (i.U === io.lsq_broadcast_result.bits.dest && io.lsq_broadcast_result.valid) {
        new_entry(i).ready := true.B
        new_entry(i).value := io.lsq_broadcast_result.bits.value
      } .elsewhen (i.U === io.wb_broadcast_result.bits.dest && io.wb_broadcast_result.valid) {
        new_entry(i).mmio_ready := true.B // real commit
        new_entry(i).value := io.wb_broadcast_result.bits.value
      } .elsewhen (i.U === tail && io.new_instruction.valid) {
        new_entry(i).instruction := io.new_instruction.bits
        new_entry(i).ready := false.B
        new_entry(i).mmio_ready := false.B
      }
    }

    val frozen = RegInit(false.B) // for mmio
    val commit_entry = TreeMux.TreeMux(head, new_entry)
    // commit.
    // because the update of entry has one cycle latency, add special judge for commit condition
    when (head =/= new_tail) {
      when (commit_entry.mmio_ready) {
        frozen := false.B
        commit_to_rf_valid := true.B
        commit_to_rf.rob_id := head
        commit_to_rf.reg_id := commit_entry.instruction.rd
        commit_to_rf.value := commit_entry.value
        new_head := head + 1.U
      } .elsewhen (commit_entry.ready && !frozen) {
        when (commit_entry.instruction.op === "b11000".U) { // branch
          io.predict_feedback.valid := true.B
          io.predict_feedback.bits.actual_result := commit_entry.value
          io.predict_feedback.bits.hashed_pc := commit_entry.instruction.hashed_address
          when (commit_entry.value =/= commit_entry.instruction.predict_taken) {
            predict_failed := true.B
            modified_pc := commit_entry.instruction.immediate
            // maybe something else need to do?
            // frozen := false.B (I don't think this is needed)
          }
          new_head := head + 1.U
        } .elsewhen (commit_entry.instruction.op === "b11001".U) { // jalr
          when (commit_entry.instruction.predict_address =/= commit_entry.addr(31, 2)) {
            predict_failed := true.B
            modified_pc := commit_entry.addr
            // maybe something else need to do?
            // frozen := false.B (I don't think this is needed)
          }
          new_head := head + 1.U
        } .elsewhen(commit_entry.instruction.op === "b01000".U || commit_entry.instruction.mmio) { // S
          when (!io.wb_is_full) {
            broadcast_to_lsq_valid := true.B
            broadcast_to_lsq.addr := commit_entry.addr
            broadcast_to_lsq.value := commit_entry.value
            broadcast_to_lsq.dest := head
            when (commit_entry.instruction.mmio) {
              frozen := true.B // wait until mmio is finished
            } .otherwise {
              new_head := head + 1.U
            }
          }
        } .otherwise {
          commit_to_rf_valid := true.B
          commit_to_rf.rob_id := head
          commit_to_rf.reg_id := commit_entry.instruction.rd
          commit_to_rf.value := commit_entry.value
          new_head := head + 1.U
        }
      }
    }
  }
  io.new_instruction.ready := new_tail + 1.U =/= new_head

  for (i <- 0 until 32) {
    entry(i) := new_entry(i)
  }
  head := new_head
  tail := new_tail

  io.broadcast_to_lsq.bits := broadcast_to_lsq
  io.broadcast_to_lsq.valid := broadcast_to_lsq_valid
  io.commit_to_rf.bits := commit_to_rf
  io.commit_to_rf.valid := commit_to_rf_valid

  io.qry1_ready := entry(io.qry1_index).ready &&
    (!entry(io.qry1_index).instruction.mmio || entry(io.qry1_index).mmio_ready)
  io.qry1_value := entry(io.qry1_index).value

  io.qry2_ready := entry(io.qry2_index).ready &&
    (!entry(io.qry2_index).instruction.mmio || entry(io.qry2_index).mmio_ready)
  io.qry2_value := entry(io.qry2_index).value

  io.rob_tail := tail

  io.modified_pc.valid := predict_failed
  io.modified_pc.bits := modified_pc
}
