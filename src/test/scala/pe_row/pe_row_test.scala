package pe_row

import java.io._

import chisel3._
import chisel3.experimental.FixedPoint

import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}

// import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

// class PE_Array_Tester(c: PE_Array, bits: Int, width: Int, height: Int)
//     extends PeekPokeTester(c) {
//   step(1)
// }

object PERowMain extends App {
  // iotesters.Driver.execute(
  //   args,
  //   () => new PE_Array(32, 4, 4, 2, 2)
  // ) { c =>
  //   new PE_Array_Tester(c, 32, 4, 4)
  // }

  val verilogString =
    (new chisel3.stage.ChiselStage).emitVerilog(new PE_Array(32, 4, 4, 2, 2))
  val writer = new PrintWriter(new File("PE_Row_Array.v"))

  writer.write(verilogString)
  writer.close()
  // println(verilogString)
}
