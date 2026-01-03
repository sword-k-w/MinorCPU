import chisel3._

class Instruction extends Bundle {
  val rd = UInt(5.W)
  val rs1 = UInt(5.W)
  val rs2 = UInt(5.W)
  val op = UInt(5.W) // use first five bits
  val funct = UInt(4.W) // funct7[1] | funct3
  val immediate = UInt(20.W)
  val predict_address = UInt(30.W) // for jalr and branch, store the speculation result to check whether need to restart in RoB
}

class Decoder extends Module {

}
