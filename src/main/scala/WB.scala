import chisel3._
import chisel3.util._

class AddrValue extends Bundle {
  val addr = UInt(32.W)
  val value = UInt(32.W)
  val size = UInt(2.W)
}

class WB extends Module {
  val io = IO(new Bundle {
    val is_empty = Output(Bool())
    val is_full = Output(Bool())

    val new_instruction = Flipped(Valid(new AddrValue))

    val memory_quest = Valid(new MemoryQuest)

    val memory_result = Flipped(Valid(UInt(32.W)))
  })

  val head = RegInit(0.U(3.W))
  val tail = RegInit(0.U(3.W))
  val entry = Reg(Vec(8, new AddrValue))

  val new_head = Wire(UInt(3.W))
  val new_tail = Wire(UInt(3.W))
  val new_entry = Wire(Vec(8, new AddrValue))

  val memory_quest = Reg(new MemoryQuest)
  val memory_quest_valid = RegInit(false.B)

  memory_quest_valid := false.B
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
      new_head := head + 1.U
    } .otherwise {
      memory_quest_valid := true.B
      memory_quest.addr := new_entry(head).addr
      memory_quest.value := new_entry(head).value
      memory_quest.size := new_entry(head).size
      memory_quest.wr_en := true.B
    }
  }

  head := new_head
  tail := new_tail
  for (i <- 0 until 8) {
    entry(i.U) := new_entry(i.U)
  }

  io.is_empty := new_head === new_tail
  io.is_full := new_tail + 1.U === new_head
  io.memory_quest.valid := memory_quest_valid
  io.memory_quest.bits := memory_quest
}
