package rift2Core.diff

import chisel3._
import chisel3.util._

import rift2Core.define._

import rift2Core.frontend._
import rift2Core.backend._


class Info_abi_reg extends Bundle {
  val zero = UInt(64.W)
  val ra   = UInt(64.W)
  val sp   = UInt(64.W)
  val gp   = UInt(64.W)
  val tp   = UInt(64.W)
  val t0   = UInt(64.W)
  val t1   = UInt(64.W)
  val t2   = UInt(64.W)
  val s0   = UInt(64.W)
  val s1   = UInt(64.W)
  val a0   = UInt(64.W)
  val a1   = UInt(64.W)
  val a2   = UInt(64.W)
  val a3   = UInt(64.W)
  val a4   = UInt(64.W)
  val a5   = UInt(64.W)
  val a6   = UInt(64.W)
  val a7   = UInt(64.W)
  val s2   = UInt(64.W)
  val s3   = UInt(64.W)
  val s4   = UInt(64.W)
  val s5   = UInt(64.W)
  val s6   = UInt(64.W)
  val s7   = UInt(64.W)
  val s8   = UInt(64.W)
  val s9   = UInt(64.W)
  val s10  = UInt(64.W)
  val s11  = UInt(64.W)
  val t3   = UInt(64.W)
  val t4   = UInt(64.W)
  val t5   = UInt(64.W)
  val t6   = UInt(64.W)
}

class Info_cmm_diff extends Bundle {
  val pc = Vec(2, UInt(64.W))
  val comfirm = Vec(2, Bool())
  val abort = Vec(2, Bool())
  val priv_lvl = UInt(2.W)
  val is_ecall_M = Bool()
  val is_ecall_S = Bool()
  val is_ecall_U = Bool()
}


class diff extends Module {
  val io = IO(new Bundle{
    val register = Input(new Info_abi_reg)
    val commit = Input(new Info_cmm_diff)
  })


  dontTouch(io)

}

