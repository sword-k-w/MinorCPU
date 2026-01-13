import chisel3._
import chisel3.util._

class ICache(val log_size : Int = 10) extends Module {
  val size = 1 << log_size

  val io = IO(new Bundle {
    val predict_failed = Input(Bool())

    // between IF
    val quest = Input(UInt(32.W))

    val quest_result = Valid(UInt(32.W))
    val quest_result2 = Valid(UInt(32.W)) // data from memory 

    // between MA
    val mem_quest = Valid(UInt(32.W))
    
    val mem_result = Flipped(Valid(UInt(32.W)))
  })

  val state = RegInit(false.B) // cache state : false is idle, true is reading from memory
  
  val data_array = SyncReadMem(size, UInt(32.W))
  val tag_array = RegInit(VecInit(Seq.fill(size)(0.U(20.W))))
  val valid_array = RegInit(VecInit(Seq.fill(size)(false.B)))

  val mem_quest_bits = RegInit(0.U(32.W))
  val mem_quest_valid = RegInit(false.B)
  val quest_result = RegInit(false.B)
  val quest_result2_valid = RegInit(false.B)
  val quest_result2_bits = RegInit(0.U(32.W))
  
  // printf("(ICache) : state = %d\n", state)
  // printf("         : quest_result = %d\n", quest_result)
  // printf("         : io.mem_result.valid = %d\n", io.mem_result.valid)
  // printf("         : io.mem_result.bits = %d\n", io.mem_result.bits)
  // printf("         : io.predict_failed = %d\n", io.predict_failed)

  val index = io.quest(log_size + 1, 2)
  val quest_tag = io.quest(31, log_size + 2)

  mem_quest_valid := false.B

  quest_result := false.B
  quest_result2_bits := 0.U
  quest_result2_valid := false.B

  when (io.predict_failed) {
    state := false.B
  } .otherwise {
    when (state) {
      when (io.mem_result.valid) {
        state := false.B
        quest_result2_bits := io.mem_result.bits
        quest_result2_valid := io.mem_result.valid
        // printf("pass result!\n")

        // update cache with data from memory
        valid_array(index) := true.B
        tag_array(index) := quest_tag
        data_array.write(index, io.mem_result.bits) 
      }
    } .otherwise {
      val hit = (valid_array(index) && tag_array(index) === quest_tag)
      when (hit) {
        quest_result := true.B
      } .otherwise {
        state := true.B
        mem_quest_valid := true.B
        mem_quest_bits := io.quest
      }
    }
  }

  io.quest_result.bits := data_array.read(index)
  io.quest_result.valid := quest_result
  io.mem_quest.valid := mem_quest_valid
  io.mem_quest.bits := mem_quest_bits
  io.quest_result2.valid := quest_result2_valid
  io.quest_result2.bits := quest_result2_bits
}