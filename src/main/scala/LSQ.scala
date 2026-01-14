import chisel3._
import chisel3.util._

class RoBBroadcastResult extends Bundle {
  val dest = UInt(5.W)
  val value = UInt(32.W)
  val addr = UInt(32.W)
}

class ALUBroadcastResult extends Bundle {
  val dest = UInt(5.W)
  val addr = UInt(32.W)
  val mmio = Bool()
}

class MemoryQuest extends Bundle {
  val addr = UInt(32.W)
  val value = UInt(32.W)
  val size = UInt(2.W)
  val wr_en = Bool()
}

class LSQEntry extends Bundle {
  val instruction = new Instruction
  val ready = Bool()
  val dest = UInt(5.W)
  val address = UInt(32.W)
  val value = UInt(32.W)
}

class LSQ extends Module {
  val io = IO(new Bundle {
    val predict_failed = Input(Bool())

    val rob_tail = Input(UInt(5.W))

    val new_instruction = Flipped(Decoupled(new Instruction))

    // remember delay a cycle in RoB
    val rob_broadcast_result = Flipped(Valid(new RoBBroadcastResult))

    // remember delay a cycle in ALU
    val alu_broadcast_result = Flipped(Valid(new ALUBroadcastResult))

    val broadcast_to_rs = Valid(new ToRSResult)

    val broadcast_to_rob = Valid(new LSQToRoBResult)

    val memory_quest = Valid(new MemoryQuest)

    val memory_result = Flipped(Valid(UInt(32.W)))

    val wb_is_empty = Input(Bool())
    val wb_is_full = Input(Bool())

    val store_to_wb = Valid(new AddrValue)
  })

  val entry = RegInit(VecInit(Seq.fill(32)(0.U.asTypeOf(new LSQEntry))))
  val head = RegInit(0.U(5.W))
  val tail = RegInit(0.U(5.W))

  val memory_quest = Reg(new MemoryQuest)
  val memory_quest_valid = RegInit(false.B)

  val broadcast_to_rs = Reg(new ToRSResult)
  val broadcast_to_rs_valid = RegInit(false.B)

  val broadcast_to_rob = Reg(new LSQToRoBResult)
  val broadcast_to_rob_valid = RegInit(false.B)

  val store_to_wb = Reg(new AddrValue)
  val store_to_wb_valid = RegInit(false.B)

  val new_head = Wire(UInt(5.W))
  val new_tail = Wire(UInt(5.W))
  val new_entry = Wire(Vec(32, new LSQEntry))

  memory_quest_valid := false.B
  broadcast_to_rs_valid := false.B
  broadcast_to_rob_valid := false.B
  store_to_wb_valid := false.B

  new_head := head
  new_tail := tail
  when (io.predict_failed) {
    when (head =/= tail && entry(head).instruction.op === "b01000".U && entry(head).ready) {
      new_tail := head + 1.U
    } .otherwise {
      new_tail := head
    }
    for (i <- 0 until 32) {
      new_entry(i.U) := entry(i.U)
    }
  } .otherwise {
    when (io.new_instruction.valid) {
      new_tail := tail + 1.U
    }
    for (i <- 0 until 32) {
      new_entry(i.U) := entry(i.U)
      when (i.U === tail) {
        when (io.new_instruction.valid) {
          new_entry(i.U).instruction := io.new_instruction.bits
          new_entry(i.U).ready := false.B
          new_entry(i.U).dest := io.rob_tail
        }
      } .elsewhen (io.rob_broadcast_result.valid && io.rob_broadcast_result.bits.dest === entry(i.U).dest) {
        new_entry(i.U).ready := true.B
        new_entry(i.U).address := io.rob_broadcast_result.bits.addr
        new_entry(i.U).value := io.rob_broadcast_result.bits.value
      } .elsewhen (io.alu_broadcast_result.valid && io.alu_broadcast_result.bits.dest === entry(i.U).dest) {
        new_entry(i.U).ready := !io.alu_broadcast_result.bits.mmio
        new_entry(i.U).address := io.alu_broadcast_result.bits.addr
        new_entry(i.U).instruction.mmio := io.alu_broadcast_result.bits.mmio
      }
    }
  }
  when (head =/= new_tail && new_entry(head).ready) {
    when (new_entry(head).instruction.op === "b01000".U || new_entry(head).instruction.mmio) { // S
      when (!io.wb_is_full) {
        store_to_wb_valid := true.B
        store_to_wb.addr := new_entry(head).address
        store_to_wb.value := new_entry(head).value
        store_to_wb.size := new_entry(head).instruction.funct(1, 0)
        store_to_wb.mmio := new_entry(head).instruction.mmio
        store_to_wb.dest := new_entry(head).dest
        new_head := head + 1.U
      }
    } .otherwise {
      when (io.memory_result.valid) {
        // TODO sign-extended / zero-extended
        broadcast_to_rs_valid := true.B
        broadcast_to_rs.value := io.memory_result.bits
        broadcast_to_rs.dest := new_entry(head).dest
        broadcast_to_rob_valid := true.B
        broadcast_to_rob.value := io.memory_result.bits
        broadcast_to_rob.dest := new_entry(head).dest
        new_head := head + 1.U
        when (new_head =/= new_tail && new_entry(new_head).ready) {
          assert(new_entry(new_head).instruction.op === "b00000".U, "a committed store occurs behind a uncommitted load!")
          when (io.wb_is_empty) {
            memory_quest_valid := true.B
            memory_quest.addr := new_entry(new_head).address
            memory_quest.size := new_entry(new_head).instruction.funct(1, 0)
            memory_quest.wr_en := false.B
          }
        }
      } .otherwise {
        when (io.wb_is_empty) {
          memory_quest_valid := true.B
          memory_quest.addr := new_entry(head).address
          memory_quest.size := new_entry(head).instruction.funct(1, 0)
          memory_quest.wr_en := false.B
        }
      }
    }
  }
  head := new_head
  tail := new_tail
  io.new_instruction.ready := new_tail + 1.U =/= new_head
  for (i <- 0 until 32) {
    entry(i.U) := new_entry(i.U)
  }
  io.memory_quest.valid := memory_quest_valid
  io.memory_quest.bits := memory_quest
  io.broadcast_to_rs.valid := broadcast_to_rs_valid
  io.broadcast_to_rs.bits := broadcast_to_rs
  io.broadcast_to_rob.valid := broadcast_to_rob_valid
  io.broadcast_to_rob.bits := broadcast_to_rob
  io.store_to_wb.valid := store_to_wb_valid
  io.store_to_wb.bits := store_to_wb
}
