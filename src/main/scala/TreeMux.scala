import chisel3._
import chisel3.util._

object TreeMux {
  def TreeMux[T <: Data](index : UInt, value: Seq[T]) : T = {
    val depth = log2Ceil(value.size)
    var cur_dep_output : Seq[T] = value
    for (i <- 0 until depth) {
      val next_dep_output = Seq.newBuilder[T]
      for (j <- 0 until cur_dep_output.size / 2) {
        next_dep_output += Mux(index(i), cur_dep_output(2 * j + 1), cur_dep_output(2 * j))
      }
      cur_dep_output = next_dep_output.result()
    }
    cur_dep_output.head
  }
}