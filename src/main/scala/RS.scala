import chisel3._
import chisel3.util._

class ToRSResult extends Bundle {
  val dest = UInt(5.W)
  val value = UInt(32.W)
}

class RSEntry extends Bundle {
  val op = UInt(5.W)
  val funct = UInt(4.W)
  val busy = Bool()
  val value1 = UInt(32.W)
  val valid1 = Bool()
  val depend1 = UInt(5.W)
  val value2 = UInt(32.W)
  val valid2 = Bool()
  val depend2 = UInt(5.W)
  val immediate_s = UInt(32.W)
  val is_zero = Bool()
}

class RS extends Module {
  val io = IO(new Bundle {
    val predict_failed = Input(Bool())

    val new_instruction = Flipped(Valid(new Instruction))

    val rob_tail = Input(UInt(5.W))

    // remember delay a cycle in ALU
    val alu_broadcast_result = Flipped(Valid(new ToRSResult))

    // remember delay a cycle in LSQ
    val lsq_broadcast_result = Flipped(Valid(new ToRSResult))

    val alu_quest = Valid(new ALUQuest)

    // query RF
    val qry1_addr           = Output(UInt(5.W))
    val qry1_val            = Input(UInt(32.W))
    val qry1_has_dependence = Input(Bool())
    val qry1_dependence     = Input(UInt(5.W))

    val qry2_addr           = Output(UInt(5.W))
    val qry2_val            = Input(UInt(32.W))
    val qry2_has_dependence = Input(Bool())
    val qry2_dependence     = Input(UInt(5.W))

    // query RoB
    val qry1_index = Output(UInt(5.W))
    val qry1_ready = Input(Bool())
    val qry1_value = Input(UInt(32.W))

    val qry2_index = Output(UInt(5.W))
    val qry2_ready = Input(Bool())
    val qry2_value = Input(UInt(32.W))
  })

  val entry = Reg(Vec(32, new RSEntry))
  val new_entry = Vec(32, new RSEntry)
  val merge_entry = Vec(31, new RSEntry)
  val merge_index = Vec(31, UInt(5.W))

  val alu_quest = Reg(new ALUQuest)
  val alu_quest_valid = Reg(Bool())

  alu_quest_valid := false.B
  alu_quest.is_zero := false.B

  when (io.predict_failed) {
    for (member <- entry) {
      member.busy := false.B
    }
  } .otherwise {
    for (i <- 0 until 32) {
      def CheckDependence1() : Unit = {
        io.qry1_addr := io.new_instruction.bits.rs1
        io.qry1_index := io.qry1_dependence
        when (io.new_instruction.bits.rs1 === 0.U || !io.qry1_has_dependence) {
          new_entry(i.U).valid1 := true.B
          new_entry(i.U).value1 := io.qry1_val
          new_entry(i.U).depend1 := 0.U
        } .elsewhen (io.qry1_ready) {
          new_entry(i.U).valid1 := true.B
          new_entry(i.U).value1 := io.qry1_value
          new_entry(i.U).depend1 := 0.U
        } .otherwise {
          new_entry(i.U).valid1 := false.B
          new_entry(i.U).value1 := 0.U
          new_entry(i.U).depend1 := io.qry1_dependence
        }
      }
      def CheckDependence2() : Unit = {
        io.qry2_addr := io.new_instruction.bits.rs2
        io.qry2_index := io.qry2_dependence
        when (io.new_instruction.bits.rs2 === 0.U || !io.qry2_has_dependence) {
          new_entry(i.U).valid2 := true.B
          new_entry(i.U).value2 := io.qry2_val
          new_entry(i.U).depend2 := 0.U
        } .elsewhen (io.qry2_ready) {
          new_entry(i.U).valid2 := true.B
          new_entry(i.U).value2 := io.qry2_value
          new_entry(i.U).depend2 := 0.U
        } .otherwise {
          new_entry(i.U).valid2 := false.B
          new_entry(i.U).value2 := 0.U
          new_entry(i.U).depend2 := io.qry2_dependence
        }
      }
      // new instruction
      when (io.new_instruction.valid && i.U === io.rob_tail) {
        new_entry(i.U).is_zero := io.new_instruction.bits.rd === 0.U && io.new_instruction.bits.op(0) === 1.U
        new_entry(i.U).busy := true.B
        new_entry(i.U).op := io.new_instruction.bits.op
        new_entry(i.U).funct := io.new_instruction.bits.funct
        when (io.new_instruction.bits.op === "b11001".U) { // jalr
          new_entry(i.U).immediate_s := io.new_instruction.bits.immediate
          new_entry(i.U).valid2 := true.B
          new_entry(i.U).value2 := io.new_instruction.bits.rs2
          new_entry(i.U).depend2 := 0.U
          CheckDependence1()
        } .elsewhen (io.new_instruction.bits.op === "b11011".U || io.new_instruction.bits.op === "b00101".U
          || io.new_instruction.bits.op === "b01101".U) { // jal, auipc, lui
          new_entry(i.U).immediate_s := 0.U
          new_entry(i.U).valid1 := true.B
          new_entry(i.U).value1 := io.new_instruction.bits.immediate
          new_entry(i.U).depend1 := 0.U
          new_entry(i.U).valid2 := true.B
          new_entry(i.U).value2 := 0.U
          new_entry(i.U).depend2 := 0.U
        } .elsewhen (io.new_instruction.bits.op === "b01000".U || io.new_instruction.bits.op === "b01100".U
          || io.new_instruction.bits.op === "b11000".U) { // S, R, B
          new_entry(i.U).immediate_s := io.new_instruction.bits.immediate
          CheckDependence1()
          CheckDependence2()
        } .otherwise {
          new_entry(i.U).immediate_s := 0.U
          new_entry(i.U).valid2 := true.B
          new_entry(i.U).value2 := io.new_instruction.bits.immediate
          new_entry(i.U).depend2 := 0.U
          CheckDependence1()
        }
      } .otherwise {
        new_entry(i.U).op := entry(i.U).busy
        new_entry(i.U).busy := entry(i.U).busy
        new_entry(i.U).immediate_s := entry(i.U).immediate_s
        new_entry(i.U).is_zero := entry(i.U).is_zero

        // update dependence according to broadcast info
        when (io.alu_broadcast_result.valid && entry(i.U).depend1 === io.alu_broadcast_result.bits.dest) {
          new_entry(i.U).valid1 := true.B
          new_entry(i.U).value1 := io.alu_broadcast_result.bits.value
          new_entry(i.U).depend1 := 0.U
        } .elsewhen (io.lsq_broadcast_result.valid && entry(i.U).depend1 === io.lsq_broadcast_result.bits.dest) {
          new_entry(i.U).valid1 := true.B
          new_entry(i.U).value1 := io.lsq_broadcast_result.bits.value
          new_entry(i.U).depend1 := 0.U
        } .otherwise {
          new_entry(i.U).valid1 := entry(i.U).valid1
          new_entry(i.U).value1 := entry(i.U).value1
          new_entry(i.U).depend1 := entry(i.U).depend1
        }
        when (io.alu_broadcast_result.valid && entry(i.U).depend2 === io.alu_broadcast_result.bits.dest) {
          new_entry(i.U).valid2 := true.B
          new_entry(i.U).value2 := io.alu_broadcast_result.bits.value
          new_entry(i.U).depend2 := 0.U
        } .elsewhen (io.lsq_broadcast_result.valid && entry(i.U).depend2 === io.lsq_broadcast_result.bits.dest) {
          new_entry(i.U).valid2 := true.B
          new_entry(i.U).value2 := io.lsq_broadcast_result.bits.value
          new_entry(i.U).depend2 := 0.U
        } .otherwise {
          new_entry(i.U).valid2 := entry(i.U).valid2
          new_entry(i.U).value2 := entry(i.U).value2
          new_entry(i.U).depend2 := entry(i.U).depend2
        }
      }
    }
    for (i <- 0 until 16) {
      merge_entry(i.U) :=
        Mux(new_entry((2 * i).U).busy && new_entry((2 * i + 1).U).valid1 && new_entry((2 * i + 1).U).valid2,
          new_entry((2 * i).U), new_entry((2 * i + 1).U))
      merge_index(i.U) :=
        Mux(new_entry((2 * i).U).busy && new_entry((2 * i + 1).U).valid1 && new_entry((2 * i + 1).U).valid2,
          (2 * i).U, (2 * i + 1).U)
    }
    for (i <- 0 until 8) {
      merge_entry((i + 16).U) :=
        Mux(merge_entry((2 * i).U).busy && merge_entry((2 * i + 1).U).valid1 && merge_entry((2 * i + 1).U).valid2,
          merge_entry((2 * i).U), merge_entry((2 * i + 1).U))
      merge_index((i + 16).U) :=
        Mux(merge_entry((2 * i).U).busy && merge_entry((2 * i + 1).U).valid1 && merge_entry((2 * i + 1).U).valid2,
          merge_index((2 * i).U), merge_index((2 * i + 1).U))
    }
    for (i <- 0 until 4) {
      merge_entry((i + 24).U) :=
        Mux(merge_entry((2 * i).U).busy && merge_entry((2 * i + 1).U).valid1 && merge_entry((2 * i + 1).U).valid2,
          merge_entry((2 * i).U), merge_entry((2 * i + 1).U))
      merge_index((i + 24).U) :=
        Mux(merge_entry((2 * i).U).busy && merge_entry((2 * i + 1).U).valid1 && merge_entry((2 * i + 1).U).valid2,
          merge_index((2 * i).U), merge_index((2 * i + 1).U))
    }
    for (i <- 0 until 2) {
      merge_entry((i + 28).U) :=
        Mux(merge_entry((2 * i).U).busy && merge_entry((2 * i + 1).U).valid1 && merge_entry((2 * i + 1).U).valid2,
          merge_entry((2 * i).U), merge_entry((2 * i + 1).U))
      merge_index((i + 28).U) :=
        Mux(merge_entry((2 * i).U).busy && merge_entry((2 * i + 1).U).valid1 && merge_entry((2 * i + 1).U).valid2,
          merge_index((2 * i).U), merge_index((2 * i + 1).U))
    }
    merge_entry(30.U) := Mux(merge_entry(28.U).busy && merge_entry(28.U).valid1 && merge_entry(28.U).valid2,
      merge_entry(28.U), merge_entry(29.U))
    merge_index(30.U) := Mux(merge_entry(28.U).busy && merge_entry(28.U).valid1 && merge_entry(28.U).valid2,
      merge_index(28.U), merge_index(29.U))

    when (merge_entry(30.U).busy && merge_entry(30.U).valid1 && merge_entry(30.U).valid2) {
      alu_quest_valid := true.B
      alu_quest.op := merge_entry(30.U).op
      alu_quest.funct := merge_entry(30.U).funct
      alu_quest.dest := merge_index(30.U)
      alu_quest.in1 := merge_entry(30.U).value1
      alu_quest.is_zero := merge_entry(30.U).is_zero
      // maybe can be reduced here
      when (merge_entry(30.U).op === "b01000".U) { // S
        alu_quest.in2 := merge_entry(30.U).immediate_s
        alu_quest.in3 := merge_entry(30.U).value2
      } .otherwise {
        alu_quest.in2 := merge_entry(30.U).value2
        alu_quest.in3 := merge_entry(30.U).immediate_s
      }
    }

    for (i <- 0 until 32) {
      entry(i.U) := new_entry(i.U)
      when (merge_index(30.U) === i.U && merge_entry(30.U).valid1 && merge_entry(30.U).valid2) {
        entry(i.U).busy := false.B
      }
    }
  }
  io.alu_quest := alu_quest
}
