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
  
  // default
  io.mem_dout := 0.U
  io.mem_a := 0.U
  io.mem_wr := 0.U
  io.dbgreg_dout := 0.U
}

object CPUMain extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new CPU(), args = args)
}