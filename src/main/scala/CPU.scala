import chisel3._
import chisel3.util._

class CPU extends Module {
  val io = IO(new Bundle {
    val rdy_in         = Input(Bool())
    val mem_din        = Input(UInt(8.W))
    val mem_dout       = Output(UInt(8.W))
    val mem_a          = Output(UInt(32.W))
    val mem_wr         = Output(Bool())

    val io_buffer_full = Input(Bool())
    val dbgreg_dout    = Output(UInt(32.W))
  })
  assert(io.rdy_in, "ready in is false")
  // default
  io.mem_dout := 0.U
  io.mem_a := 0.U
  io.mem_wr := 0.U
  io.dbgreg_dout := 0.U

  val if_ = Module(new IF)
  val icache = Module(new ICache)
  val dcache = Module(new DCache)
  val ma = Module(new MA)
  val iq = Module(new IQ)
  val rf = Module(new RF)
  val rs = Module(new RS)
  val rob = Module(new RoB)
  val lsq = Module(new LSQ)
  val wb = Module(new WB)
  val alu = Module(new ALU)
  val predictor = Module(new Predictor)
  val ras = Module(new RAS)

  if_.io.quest <> icache.io.quest
  if_.io.quest_result <> icache.io.quest_result
  if_.io.quest_result2 <> icache.io.quest_result2
  if_.io.instruction <> iq.io.new_instruction
  if_.io.modified_pc <> rob.io.modified_pc
  if_.io.query_pc <> predictor.io.queried_pc
  if_.io.predict_result <> predictor.io.predict_result
  if_.io.new_address <> ras.io.new_address
  if_.io.pop_valid <> ras.io.pop_valid
  if_.io.jalr_result <> ras.io.top

  icache.io.predict_failed <> rob.io.modified_pc.valid
  icache.io.mem_quest <> ma.io.i_quest
  icache.io.mem_result <> ma.io.i_result

  dcache.io.lsq_result_hit <> lsq.io.hit_result
  dcache.io.lsq_result_mem <> lsq.io.miss_result
  dcache.io.wb_result_hit <> wb.io.hit_result
  dcache.io.wb_result_mem <> wb.io.miss_result
  dcache.io.mem_quest <> ma.io.d_quest
  dcache.io.predict_failed <> rob.io.modified_pc.valid
  dcache.io.lsq_quest <> lsq.io.memory_quest
  dcache.io.wb_quest <> wb.io.memory_quest
  dcache.io.mem_result <> ma.io.d_result

  ma.io.predict_failed <> rob.io.modified_pc.valid
  ma.io.mem_din <> io.mem_din
  ma.io.mem_dout <> io.mem_dout
  ma.io.mem_a <> io.mem_a
  ma.io.mem_wr <> io.mem_wr

  iq.io.predict_failed <> rob.io.modified_pc.valid
  iq.io.instruction_to_rob <> rob.io.new_instruction
  iq.io.instruction_to_rs <> rs.io.new_instruction
  iq.io.instruction_to_lsq <> lsq.io.new_instruction
  iq.io.new_dependence_valid <> rf.io.new_dependence_valid
  iq.io.new_reg_id <> rf.io.new_reg_id

  rf.io.predict_failed <> rob.io.modified_pc.valid
  rf.io.commit_info <> rob.io.commit_to_rf
  rf.io.new_dependence <> rob.io.rob_tail
  rf.io.qry1_addr <> rs.io.qry1_addr
  rf.io.qry1_val <> rs.io.qry1_val
  rf.io.qry1_has_dependence <> rs.io.qry1_has_dependence
  rf.io.qry1_dependence <> rs.io.qry1_dependence
  rf.io.qry2_addr <> rs.io.qry2_addr
  rf.io.qry2_val <> rs.io.qry2_val
  rf.io.qry2_has_dependence <> rs.io.qry2_has_dependence
  rf.io.qry2_dependence <> rs.io.qry2_dependence

  rs.io.predict_failed <> rob.io.modified_pc.valid
  rs.io.rob_tail <> rob.io.rob_tail
  rs.io.alu_broadcast_result <> alu.io.broadcast_to_rs
  rs.io.lsq_broadcast_result <> lsq.io.broadcast_to_rs
  rs.io.wb_broadcast_result <> wb.io.broadcast_to_rs
  rs.io.alu_quest <> alu.io.quest
  rs.io.qry1_index <> rob.io.qry1_index
  rs.io.qry1_ready <> rob.io.qry1_ready
  rs.io.qry1_value <> rob.io.qry1_value
  rs.io.qry2_index <> rob.io.qry2_index
  rs.io.qry2_ready <> rob.io.qry2_ready
  rs.io.qry2_value <> rob.io.qry2_value

  rob.io.wb_is_full <> wb.io.is_full
  rob.io.alu_broadcast_result <> alu.io.broadcast_to_rob
  rob.io.lsq_broadcast_result <> lsq.io.broadcast_to_rob
  rob.io.broadcast_to_lsq <> lsq.io.rob_broadcast_result
  rob.io.wb_broadcast_result <> wb.io.broadcast_to_rob
  rob.io.predict_feedback <> predictor.io.update_info

  lsq.io.predict_failed <> rob.io.modified_pc.valid
  lsq.io.rob_tail <> rob.io.rob_tail
  lsq.io.alu_broadcast_result <> alu.io.broadcast_to_lsq
  lsq.io.wb_is_empty <> wb.io.is_empty
  lsq.io.wb_is_full <> wb.io.is_full
  lsq.io.store_to_wb <> wb.io.new_instruction

  alu.io.predict_failed <> rob.io.modified_pc.valid
}


object Main extends App {
  emitVerilog(new CPU(), Array("--target-dir", "generated"))
  emitVerilog(new ALU(), Array("--target-dir", "generated"))
  emitVerilog(new ICache(), Array("--target-dir", "generated"))
  emitVerilog(new IF(), Array("--target-dir", "generated"))
  emitVerilog(new IQ(), Array("--target-dir", "generated"))
  emitVerilog(new LSQ(), Array("--target-dir", "generated"))
  emitVerilog(new MA(), Array("--target-dir", "generated"))
  emitVerilog(new Predictor(), Array("--target-dir", "generated"))
  emitVerilog(new RAS(), Array("--target-dir", "generated"))
  emitVerilog(new RF(), Array("--target-dir", "generated"))
  emitVerilog(new RoB(), Array("--target-dir", "generated"))
  emitVerilog(new WB(), Array("--target-dir", "generated"))
}