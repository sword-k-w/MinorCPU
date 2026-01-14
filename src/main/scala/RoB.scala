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

  for (i <- 0 until 32) {
    new_entry(i.U) := entry(i.U)
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
        new_entry(i.U).ready := true.B // pretend to commit if mmio
        new_entry(i.U).value := io.alu_broadcast_result.bits.value
        new_entry(i.U).addr := io.alu_broadcast_result.bits.addr
        new_entry(i.U).instruction.mmio := io.alu_broadcast_result.bits.mmio
      } .elsewhen (i.U === io.lsq_broadcast_result.bits.dest && io.lsq_broadcast_result.valid) {
        new_entry(i.U).ready := true.B
        new_entry(i.U).value := io.lsq_broadcast_result.bits.value
      } .elsewhen (i.U === io.wb_broadcast_result.bits.dest && io.wb_broadcast_result.valid) {
        new_entry(i.U).mmio_ready := true.B // real commit
        new_entry(i.U).value := io.wb_broadcast_result.bits.value
      } .elsewhen (i.U === tail && io.new_instruction.valid) {
        new_entry(i.U).instruction := io.new_instruction.bits
        new_entry(i.U).ready := false.B
      }
    }

    val frozen = RegInit(false.B) // for mmio
    // commit.
    // because the update of entry has one cycle latency, add special judge for commit condition
    when (head =/= new_tail) {
      when (new_entry(head).mmio_ready) {
        frozen := false.B
        commit_to_rf_valid := true.B
        commit_to_rf.rob_id := head
        commit_to_rf.reg_id := new_entry(head).instruction.rd
        commit_to_rf.value := new_entry(head).value
        new_head := head + 1.U
      } .elsewhen (new_entry(head).ready && !frozen) {
        when (new_entry(head).instruction.op === "b11000".U || new_entry(head).instruction.op === "b11001".U) { // branch or jalr
          when (new_entry(head).instruction.predict_address =/= new_entry(head).value(31, 2)) {
            predict_failed := true.B
            io.modified_pc.valid := true.B
            io.modified_pc.bits := new_entry(head).value
            new_head := head + 1.U
            // maybe something else need to do?
            // frozen := false.B (I don't think this is needed)
          }
        } .elsewhen(new_entry(head).instruction.op === "b01000".U || new_entry(head).instruction.mmio) { // S
          when (!io.wb_is_full) {
            broadcast_to_lsq_valid := true.B
            broadcast_to_lsq.addr := new_entry(head).addr
            broadcast_to_lsq.value := new_entry(head).value
            broadcast_to_lsq.dest := head
            when (new_entry(head).instruction.mmio) {
              frozen := true.B // wait until mmio is finished
            } .otherwise {
              new_head := head + 1.U
            }
          }
        } .otherwise {
          commit_to_rf_valid := true.B
          commit_to_rf.rob_id := head
          commit_to_rf.reg_id := new_entry(head).instruction.rd
          commit_to_rf.value := new_entry(head).value
          new_head := head + 1.U
        }
      }
    }
  }
  io.new_instruction.ready := new_tail + 1.U =/= new_head

  for (i <- 0 until 32) {
    entry(i.U) := new_entry(i.U)
  }
  head := new_head
  tail := new_tail

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
