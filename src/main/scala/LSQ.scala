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

//  val memory_quest = Reg(new MemoryQuest)
//  val memory_quest_valid = RegInit(false.B)

  val broadcast_to_rs = Reg(new ToRSResult)
  val broadcast_to_rs_valid = RegInit(false.B)

  val broadcast_to_rob = Reg(new LSQToRoBResult)
  val broadcast_to_rob_valid = RegInit(false.B)

  val store_to_wb = Reg(new AddrValue)
  val store_to_wb_valid = RegInit(false.B)

  val new_head = Wire(UInt(5.W))
  val new_tail = Wire(UInt(5.W))
  val new_entry = Wire(Vec(32, new LSQEntry))

  io.memory_quest.valid := false.B
  io.memory_quest.bits.value := 0.U
  io.memory_quest.bits.addr := 0.U
  io.memory_quest.bits.size := 0.U
  io.memory_quest.bits.wr_en := 0.U
//  memory_quest_valid := false.B
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
      new_entry(i) := entry(i)
      when (i.U === tail) {
        when (io.new_instruction.valid) {
          new_entry(i).instruction := io.new_instruction.bits
          new_entry(i).ready := false.B
          new_entry(i).dest := io.rob_tail
        }
      } .elsewhen (io.rob_broadcast_result.valid && io.rob_broadcast_result.bits.dest === entry(i).dest
        && !entry(i).ready) {
        new_entry(i).ready := true.B
        new_entry(i).address := io.rob_broadcast_result.bits.addr
        new_entry(i).value := io.rob_broadcast_result.bits.value
      } .elsewhen (io.alu_broadcast_result.valid && io.alu_broadcast_result.bits.dest === entry(i).dest
        && !entry(i).ready) {
        new_entry(i).ready := !io.alu_broadcast_result.bits.mmio
        new_entry(i).address := io.alu_broadcast_result.bits.addr
        new_entry(i).instruction.mmio := io.alu_broadcast_result.bits.mmio
      }
    }
  }
  val head_entry = TreeMux.TreeMux(head, new_entry.toSeq)
  when (head =/= new_tail && head_entry.ready) {
    when (head_entry.instruction.op === "b01000".U || head_entry.instruction.mmio) { // S
      when (!io.wb_is_full) {
        store_to_wb_valid := true.B
        store_to_wb.addr := head_entry.address
        store_to_wb.value := head_entry.value
        store_to_wb.size := head_entry.instruction.funct(1, 0)
        store_to_wb.mmio := head_entry.instruction.mmio
        store_to_wb.dest := head_entry.dest
        new_head := head + 1.U
      }
    } .otherwise {
      when (io.memory_result.valid) {
        val res = Wire(UInt(32.W))
        res := io.memory_result.bits
        when (head_entry.instruction.funct(2, 0) === 0.U) { // lb (sign-extended)
          when (io.memory_result.bits(7) === 1.U) {
            res := 16777215.U(24.W) ## io.memory_result.bits(7, 0)
          }
        } .elsewhen (head_entry.instruction.funct(2, 0) === 1.U) { // lh (sign-extended)
          when (io.memory_result.bits(15) === 1.U) {
            res := 65535.U(16.W) ## io.memory_result.bits(15, 0)
          }
        }
        broadcast_to_rs_valid := true.B
        broadcast_to_rs.value := res
        broadcast_to_rs.dest := head_entry.dest
        broadcast_to_rob_valid := true.B
        broadcast_to_rob.value := res
        broadcast_to_rob.dest := head_entry.dest
        new_head := head + 1.U
        val new_head_entry = TreeMux.TreeMux(new_head, new_entry.toSeq)
        when (new_head =/= new_tail && new_head_entry.ready) {
          assert(new_head_entry.instruction.op === "b00000".U, "a committed store occurs behind a uncommitted load!")
          when (io.wb_is_empty) {
            io.memory_quest.valid := true.B
            io.memory_quest.bits.addr := new_head_entry.address
            io.memory_quest.bits.size := new_head_entry.instruction.funct(1, 0)
            io.memory_quest.bits.wr_en := false.B
//            memory_quest_valid := true.B
//            memory_quest.addr := new_entry(new_head).address
//            memory_quest.size := new_entry(new_head).instruction.funct(1, 0)
//            memory_quest.wr_en := false.B
          }
        }
      } .otherwise {
        when (io.wb_is_empty) {
          io.memory_quest.valid := true.B
          io.memory_quest.bits.addr := head_entry.address
          io.memory_quest.bits.size := head_entry.instruction.funct(1, 0)
          io.memory_quest.bits.wr_en := false.B
        }
      }
    }
  }
  head := new_head
  tail := new_tail
  io.new_instruction.ready := new_tail + 1.U =/= new_head
  for (i <- 0 until 32) {
    entry(i) := new_entry(i)
  }
//  io.memory_quest.valid := memory_quest_valid
//  io.memory_quest.bits := memory_quest
  io.broadcast_to_rs.valid := broadcast_to_rs_valid
  io.broadcast_to_rs.bits := broadcast_to_rs
  io.broadcast_to_rob.valid := broadcast_to_rob_valid
  io.broadcast_to_rob.bits := broadcast_to_rob
  io.store_to_wb.valid := store_to_wb_valid
  io.store_to_wb.bits := store_to_wb
}
