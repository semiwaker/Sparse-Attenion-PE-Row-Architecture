package pe_row

import chisel3._
import chisel3.util._

import chisel3.experimental.FixedPoint

import exp_unit.ExpUnitFixPoint

// class Mac_Unit(bits: Int) extends Module {
//   val io = IO(new Bundle {
//     val a = Input(UInt(bits.W))
//     val b = Input(UInt(bits.W))
//     val c_in = Input(UInt(bits.W))
//     val c_out = Output(UInt(bits.W))
//     val en = Input(Bool())
//     val clr = Input(Bool())
//     val dump = Input(Bool())
//   })
//   val r = Reg(UInt(bits.W))
//   when(io.clr) {
//     r := 0.U
//   }.otherwise {
//     when(io.en) {
//       when(io.dump) {
//         r := io.c_in
//       }.otherwise {
//         r := io.a * io.b + r
//       }
//     }.otherwise {
//       r := r
//     }
//   }
//   io.c_out := r
// }

object SeqSwitch {
  def apply[T <: Data](sel: UInt, out: T, cases: IndexedSeq[Tuple2[UInt, T]]) {
    var w = when(sel === cases.head._1) {
      out := cases.head._2
    }
    for ((u, v) <- cases.tail) {
      w = w.elsewhen(sel === u) {
        out := v
      }
    }
  }
}

class PE(
    bits: Int,
    point: Int,
    width: Int,
    buf_size: Int,
    c1_bits: Int,
    c2_bits: Int,
    c4_bits: Int
) extends Module {
  val fpType = FixedPoint(bits.W, point.BP)
  val io = IO(new Bundle {
    val row_in = Input(Vec(width, fpType))
    val col_in = Input(Vec(width, fpType))
    val o_in = Input(fpType)
    val o_out = Output(fpType)
    val score_in = Output(fpType)
    val score_out = Output(fpType)

    val clr = Input(Bool())
    val c1 = Input(UInt(c1_bits.W))
    val c2 = Input(UInt(c2_bits.W))
    val c3 = Input(UInt(2.W))
    val c4 = Input(UInt(c4_bits.W))
    val c5 = Input(UInt(2.W))
  })

  val exp_unit = Module(new ExpUnitFixPoint(bits, point, 6, 2))

  val a = Wire(fpType)
  val b = Wire(fpType)
  val c = Wire(fpType)
  val acc = Reg(fpType)
  val score_exp = Reg(fpType)

  // Buffer
  val buf = Reg(Vec(buf_size, fpType))
  val buf_vec = Vec(buf_size + 1, fpType)
  buf_vec(0) := acc
  for (i <- 0 until buf_size) buf_vec(i + 1) := buf(i)

  // MAC
  val a_vec = Vec(width + 1, fpType)
  for (i <- 0 until width) a_vec(i) := io.row_in(i)
  a_vec(width) := score_exp
  SeqSwitch(
    io.c1,
    a,
    ((width + 1).U +: Range(0, width).map(_.U)).zip(a_vec)
  )
  SeqSwitch(
    io.c2,
    b,
    Range(0, width).map(_.U).zip(io.col_in)
  )
  c := a * b

  // Score
  exp_unit.io.in_value := acc
  io.score_out := score_exp

  // Control
  when(io.clr) {
    acc := fpType(0)
    for (i <- 0 until buf.size)
      buf(i) := fpType(0)
  }.otherwise {
    switch(io.c3) {
      is(0.U) {
        acc := acc
      }
      is(1.U) {
        acc := acc + c
      }
      is(2.U) {
        acc := io.o_in + c
        buf(0) := acc
        for (i <- 0 until buf_size - 1)
          buf(i + 1) := buf(i)
        SeqSwitch(
          io.c4,
          io.o_out,
          Range(0, buf_size + 1).map(_.U).zip(buf_vec)
        )
      }
    }

    switch(io.c5) {
      is(0.U) {
        score_exp := score_exp
      }
      is(1.U) {
        score_exp := exp_unit.io.out_exp
      }
      is(2.U) {
        score_exp := io.score_in
      }
    }
  }
}

class PE_Row(
    pe_n: Int,
    bits: Int,
    point: Int,
    width: Int,
    c1_bits: Int,
    c2_bits: Int,
    c4_bits: Int
) extends Module {
  val fpType = FixedPoint(bits.W, point.BP)
  val io = IO(new Bundle {
    val left_in = Input(fpType)
    val top_in = Input(Vec(width, fpType))
    val bot_out = Output(Vec(width, fpType))
    val o_out = Output(fpType)
    val score_sum = Output(fpType)

    val clr = Input(Bool())
    val c1 = Input(UInt(c1_bits.W))
    val c2 = Input(UInt(c2_bits.W))
    val c3 = Input(UInt(2.W))
    val c4 = Input(UInt(c4_bits.W))
    val c5 = Input(UInt(2.W))
  })

  val pes =
    for (i <- 0 until pe_n)
      yield Module(
        new PE(
          bits,
          point,
          width,
          width - pe_n,
          c1_bits,
          c2_bits,
          c4_bits
        )
      )

  val v_regs = Reg(Vec(width, fpType))
  val h_regs = Reg(Vec(width, fpType))
  val ssum = Reg(fpType)

  for (i <- 0 until pe_n) {
    pes(i).io.row_in := v_regs
    pes(i).io.col_in := h_regs
    pes(i).io.clr := io.clr
    pes(i).io.c1 := io.c1
    pes(i).io.c2 := io.c2
    pes(i).io.c3 := io.c3
    pes(i).io.c4 := io.c4
    pes(i).io.c5 := io.c5
  }
  for (i <- 0 until pe_n - 1) {
    pes(i + 1).io.score_in := pes(i).io.score_out
    pes(i + 1).io.o_in := pes(i).io.o_out
  }
  pes(0).io.score_in := fpType(0)
  pes(0).io.o_in := fpType(0)

  for (i <- 0 until width)
    io.bot_out(i) := h_regs(i)

  io.o_out := pes(pe_n - 1).io.o_out

  when(io.clr) {
    for (i <- 0 until width) {
      v_regs(i) := 0.U
      h_regs(i) := 0.U
    }
    ssum := 0.U
  }.otherwise {
    for (i <- 0 until width - 1)
      v_regs(i + 1) := v_regs(i)
    v_regs(0) := io.left_in

    h_regs := io.top_in
    io.bot_out := h_regs

    when(io.c5 === 2.U) {
      ssum := ssum + pes(pe_n - 1).io.score_out
    }.otherwise {
      ssum := ssum
    }
  }
}

class PE_Array(
    pe_n: Int,
    bits: Int,
    point: Int,
    width: Int,
    height: Int,
    c1_bits: Int,
    c2_bits: Int,
    c4_bits: Int
) extends Module {
  val fpType = FixedPoint(bits.W, point.BP)
  val io = IO(new Bundle {
    val left_in = Input(Vec(height, fpType))
    val top_in = Input(Vec(width, fpType))
    val out = Output(Vec(height, fpType))
    val score_sum = Output(Vec(height, fpType))
    val clr = Input(Bool())
    val c1 = Input(UInt(c1_bits.W))
    val c2 = Input(UInt(c2_bits.W))
    val c3 = Input(UInt(2.W))
    val c4 = Input(UInt(c4_bits.W))
    val c5 = Input(UInt(2.W))
  })
  val rows =
    for (i <- 0 until height)
      yield Module(
        new PE_Row(
          pe_n,
          bits,
          point,
          width,
          c1_bits,
          c2_bits,
          c4_bits
        )
      )

  for (i <- 0 until height) {
    rows(i).io.clr := io.clr
    rows(i).io.c1 := io.c1
    rows(i).io.c2 := io.c2
    rows(i).io.c3 := io.c3
    rows(i).io.c4 := io.c4
    rows(i).io.c5 := io.c5
    rows(i).io.left_in := io.left_in(i)
    io.out(i) := rows(i).io.o_out
    io.score_sum(i) := rows(i).io.score_sum
  }
  rows(0).io.top_in := io.top_in
  for (i <- 0 until height - 1)
    rows(i + 1).io.top_in := rows(i).io.bot_out
}
