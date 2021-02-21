package pe_row

import chisel3._
import chisel3.util._
import Chisel.experimental.dump

class Mac_Unit(bits: Int) extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(bits.W))
    val b = Input(UInt(bits.W))
    val c_in = Input(UInt(bits.W))
    val c_out = Output(UInt(bits.W))
    val en = Input(Bool())
    val clr = Input(Bool())
    val dump = Input(Bool())
  })
  val r = Reg(UInt(bits.W))
  when(io.clr) {
    r := 0.U
  }.otherwise {
    when(io.en) {
      when(io.dump) {
        r := io.c_in
      }.otherwise {
        r := io.a * io.b + r
      }
    }.otherwise {
      r := r
    }
  }
  io.c_out := r
}

class PE_Row(bits: Int, width: Int, mac_n: Int, sel_bits: Int) extends Module {
  val io = IO(new Bundle {
    val left_in = Input(UInt(bits.W))
    // val right_out = Output(UInt(bits.W))
    val top_in = Input(Vec(width, UInt(bits.W)))
    val bot_out = Output(Vec(width, UInt(bits.W)))
    // val o_in = Input(UInt(bits.W))
    val o_out = Output(UInt(bits.W))
    val pos = Input(Vec(mac_n, UInt(sel_bits.W)))
    val en = Input(Bool())
    val clr = Input(Bool())
    val dump = Input(Bool())
  })

  val macs = for (i <- 0 until mac_n) yield Module(new Mac_Unit(bits))

  val v_regs = Reg(Vec(width, UInt(bits.W)))
  val h_regs = Reg(Vec(width, UInt(bits.W)))
  val dump_regs = Reg(Vec(mac_n, Bool()))

  for (i <- 0 until mac_n) {
    macs(i).io.en := io.en
    macs(i).io.clr := io.clr
    macs(i).io.dump := dump_regs(i)
    var w = when(io.pos(i) === 0.U) {
      macs(i).io.a := v_regs(0)
      macs(i).io.b := h_regs(0)
    }
    for (j <- 1 until width) {
      w = w.elsewhen(io.pos(i) === j.U) {
        macs(i).io.a := v_regs(j)
        macs(i).io.b := h_regs(j)
      }
    }
    w.otherwise {
      macs(i).io.a := DontCare
      macs(i).io.b := DontCare
    }
  }
  macs(0).io.c_in := 0.U
  io.o_out := macs(mac_n - 1).io.c_out
  for (i <- 0 until mac_n - 1)
    macs(i + 1).io.c_in := macs(i).io.c_out

  for (i <- 0 until width)
    io.bot_out(i) := h_regs(i)

  when(io.clr) {
    for (i <- 0 until width) {
      v_regs(i) := 0.U
      h_regs(i) := 0.U
    }
    for (i <- 0 until mac_n) {
      dump_regs(i) := 0.B
    }
  }.otherwise {
    when(io.en) {
      v_regs(0) := io.left_in
      // io.right_out := v_regs(width - 1)
      for (i <- 0 until width - 1)
        v_regs(i + 1) := v_regs(i)
      for (i <- 0 until width) {
        h_regs(i) := io.top_in(i)
      }
      when(io.dump) {
        for (i <- 0 until mac_n)
          dump_regs(i) := 1.B
      }.otherwise {
        dump_regs(0) := 0.B
        for (i <- 0 until mac_n - 1)
          dump_regs(i + 1) := dump_regs(i)
      }
    }.otherwise {
      for (i <- 0 until width) {
        v_regs(i) := v_regs(i)
        h_regs(i) := h_regs(i)
      }
      for (i <- 0 until mac_n)
        dump_regs(i) := dump_regs(i)
    }
  }
}

class PE_Array(bits: Int, width: Int, height: Int, mac_n: Int, sel_bits: Int)
    extends Module {
  val io = IO(new Bundle {
    val left_in = Input(Vec(height, UInt(bits.W)))
    // val right_out = Output(Vec(height, UInt(bits.W)))
    val top_in = Input(Vec(width, UInt(bits.W)))
    // val bot_out = Output(Vec(width, UInt(bits.W)))
    val out = Output(Vec(height, UInt(bits.W)))
    val pos = Input(Vec(height, Vec(mac_n, UInt(sel_bits.W))))
    val en = Input(Bool())
    val clr = Input(Bool())
    val dump = Input(Bool())
  })
  val rows =
    for (i <- 0 until height)
      yield Module(new PE_Row(bits, width, mac_n, sel_bits))

  for (i <- 0 until height) {
    rows(i).io.en := io.en
    rows(i).io.clr := io.clr
    rows(i).io.dump := io.dump
    rows(i).io.pos := io.pos(i)
    rows(i).io.left_in := io.left_in(i)
    io.out(i) := rows(i).io.o_out
  }
  rows(0).io.top_in := io.top_in
  for (i <- 0 until height - 1)
    rows(i + 1).io.top_in := rows(i).io.bot_out
}
