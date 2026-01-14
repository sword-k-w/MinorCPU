import chisel3._
import chisel3.util._

class AddrValue extends Bundle {
  val addr = UInt(32.W)
  val value = UInt(32.W)
  val size = UInt(2.W)
  val dest = UInt(5.W)
  val mmio = Bool()
}

class WB extends Module {
  val io = IO(new Bundle {
    val is_empty = Output(Bool())
    val is_full = Output(Bool())

    val new_instruction = Flipped(Valid(new AddrValue))

    val broadcast_to_rs = Valid(new ToRSResult)

    val broadcast_to_rob = Valid(new LSQToRoBResult)

    val memory_quest = Valid(new MemoryQuest)

    val memory_result = Flipped(Valid(UInt(32.W)))
  })

  val head = RegInit(0.U(3.W))
  val tail = RegInit(0.U(3.W))
  val entry = Reg(Vec(8, new AddrValue))

  val new_head = Wire(UInt(3.W))
  val new_tail = Wire(UInt(3.W))
  val new_entry = Wire(Vec(8, new AddrValue))

//  val memory_quest = Reg(new MemoryQuest)
//  val memory_quest_valid = RegInit(false.B)

  val broadcast_to_rs = Reg(new ToRSResult)
  val broadcast_to_rs_valid = RegInit(false.B)

  val broadcast_to_rob = Reg(new LSQToRoBResult)
  val broadcast_to_rob_valid = RegInit(false.B)

  io.memory_quest.valid := false.B
  io.memory_quest.bits.addr := 0.U
  io.memory_quest.bits.value := 0.U
  io.memory_quest.bits.size := 0.U
  io.memory_quest.bits.wr_en := 0.U
//  memory_quest_valid := false.B
  broadcast_to_rs_valid := false.B
  broadcast_to_rob_valid := false.B
  new_head := head

  when (io.new_instruction.valid) {
    for (i <- 0 until 8) {
      when (i.U === tail) {
        new_entry(i.U) := io.new_instruction.bits
      } .otherwise {
        new_entry(i.U) := entry(i.U)
      }
    }
    new_tail := tail + 1.U
  } .otherwise {
    for (i <- 0 until 8) {
      new_entry(i.U) := entry(i.U)
    }
    new_tail := tail
  }

  when (head =/= new_tail) {
    when (io.memory_result.valid) {
      when (new_entry(head).mmio) {
        broadcast_to_rs_valid := true.B
        broadcast_to_rs.value := io.memory_result.bits
        broadcast_to_rs.dest := new_entry(head).dest
        broadcast_to_rob_valid := true.B
        broadcast_to_rob.value := io.memory_result.bits
        broadcast_to_rob.dest := new_entry(head).dest
      }
      new_head := head + 1.U
    } .otherwise {
      io.memory_quest.valid := true.B
      io.memory_quest.bits.addr := new_entry(head).addr
      io.memory_quest.bits.value := new_entry(head).value
      io.memory_quest.bits.size := new_entry(head).size
      io.memory_quest.bits.wr_en := !new_entry(head).mmio
//      memory_quest_valid := true.B
//      memory_quest.addr := new_entry(head).addr
//      memory_quest.value := new_entry(head).value
//      memory_quest.size := new_entry(head).size
//      memory_quest.wr_en := !new_entry(head).mmio
    }
  }

  head := new_head
  tail := new_tail
  for (i <- 0 until 8) {
    entry(i.U) := new_entry(i.U)
  }

  io.is_empty := new_head === new_tail
  io.is_full := new_tail + 1.U === new_head
//  io.memory_quest.valid := memory_quest_valid
//  io.memory_quest.bits := memory_quest
  io.broadcast_to_rs.valid := broadcast_to_rs_valid
  io.broadcast_to_rs.bits := broadcast_to_rs
  io.broadcast_to_rob.valid := broadcast_to_rob_valid
  io.broadcast_to_rob.bits := broadcast_to_rob
}
