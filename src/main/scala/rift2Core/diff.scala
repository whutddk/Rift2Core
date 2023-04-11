/*
  Copyright (c) 2020 - 2023 Wuhan University of Technology <295054118@whut.edu.cn>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

package rift2Core.diff

import chisel3._
import base._

import rift2Core.backend._

import rift2Chip._
import org.chipsalliance.cde.config._


class Info_cmm_diff(implicit p: Parameters) extends RiftBundle {
  val pc = Vec(cm_chn, UInt(64.W))
  val comfirm = Vec(cm_chn, Bool())
  val abort = Vec(cm_chn, Bool())
  val priv_lvl = UInt(2.W)
  val is_ecall_M = Bool()
  val is_ecall_S = Bool()
  val is_ecall_U = Bool()
}

class Info_csr_reg(implicit p: Parameters) extends RiftBundle {
	val mstatus = UInt(64.W)
	val mtvec = UInt(64.W)
	val mscratch = UInt(64.W)
	val mepc = UInt(64.W)
	val mcause = UInt(64.W)
	val mtval = UInt(64.W)

  val mvendorid = UInt(64.W)
  val marchid = UInt(64.W)
  val mimpid = UInt(64.W)
  val mhartid = UInt(64.W)
  val misa = UInt(64.W)
  val mie = UInt(64.W)
  val mip = UInt(64.W)
  val medeleg = UInt(64.W)
  val mideleg = UInt(64.W)
  // val mcounteren = UInt(64.W)
  // val mcountinhibit = UInt(64.W)
  // val tselect = UInt(64.W)
  // val tdata1[MAX_TRIGGERS] = UInt(64.W)
  // val tdata2[MAX_TRIGGERS] = UInt(64.W)
  // val tdata3[MAX_TRIGGERS] = UInt(64.W)
  // val mhpmevent[32] = UInt(64.W)

  val pmpcfg  = (if( pmpNum == 0 ) { Vec(1, UInt(64.W))}        else {Vec(pmpNum, UInt(64.W))})
	val pmpaddr = (if( pmpNum == 0 ) { Vec(8, UInt(64.W))} else {Vec(8*pmpNum, UInt(64.W))})


  val stvec = UInt(64.W)
  val sscratch = UInt(64.W)
  val sepc = UInt(64.W)
  val scause = UInt(64.W)
  val stval = UInt(64.W)
  val satp = UInt(64.W)

  // val scounteren = UInt(64.W)
  // val dcsr = UInt(64.W)
  // val dpc = UInt(64.W)
  // val dscratch;  = UInt(64.W)

  val fflags = UInt(32.W)
  val frm = UInt(8.W)

  val mcycle      = UInt(64.W)
  val minstret    = UInt(64.W)
  val mhpmcounter = Vec( 32, UInt(64.W))
}

class diff(implicit p: Parameters) extends RiftModule with HasFPUParameters{

  class DiffIO extends Bundle{
    val diffXReg = Input(Vec(32, UInt(64.W)))
    val diffFReg = Input(Vec(32, UInt(65.W)))

    val commit = Input(new Info_cmm_diff)
    val csr = Input(new Info_csr_reg)    
  }
  val io = IO(new DiffIO)


  dontTouch(io)


  class Info_abi_Xreg extends Bundle {
    val zero = UInt(64.W)
    val ra   = UInt(64.W)
    val sp   = UInt(64.W)
    val gp   = UInt(64.W)
    val tp   = UInt(64.W)
    val t    = Vec( 7, UInt(64.W) )
    val s    = Vec( 12, UInt(64.W) )
    val a    = Vec( 8, UInt(64.W) )

  }

  val XReg = Wire(new Info_abi_Xreg)
  dontTouch(XReg)

  XReg.zero := io.diffXReg(0)
  XReg.ra   := io.diffXReg(1)
  XReg.sp   := io.diffXReg(2)
  XReg.gp   := io.diffXReg(3)
  XReg.tp   := io.diffXReg(4)
  XReg.t(0)   := io.diffXReg(5)
  XReg.t(1)   := io.diffXReg(6)
  XReg.t(2)   := io.diffXReg(7)
  XReg.s(0)   := io.diffXReg(8)
  XReg.s(1)   := io.diffXReg(9)
  XReg.a(0)   := io.diffXReg(10)
  XReg.a(1)   := io.diffXReg(11)
  XReg.a(2)   := io.diffXReg(12)
  XReg.a(3)   := io.diffXReg(13)
  XReg.a(4)   := io.diffXReg(14)
  XReg.a(5)   := io.diffXReg(15)
  XReg.a(6)   := io.diffXReg(16)
  XReg.a(7)   := io.diffXReg(17)
  XReg.s(2)   := io.diffXReg(18)
  XReg.s(3)   := io.diffXReg(19)
  XReg.s(4)   := io.diffXReg(20)
  XReg.s(5)   := io.diffXReg(21)
  XReg.s(6)   := io.diffXReg(22)
  XReg.s(7)   := io.diffXReg(23)
  XReg.s(8)   := io.diffXReg(24)
  XReg.s(9)   := io.diffXReg(25)
  XReg.s(10)  := io.diffXReg(26)
  XReg.s(11)  := io.diffXReg(27)
  XReg.t(3)   := io.diffXReg(28)
  XReg.t(4)   := io.diffXReg(29)
  XReg.t(5)   := io.diffXReg(30)
  XReg.t(6)   := io.diffXReg(31)

  class Info_abi_Freg extends Bundle {
    val ft = Vec(12, UInt(64.W))
    val fs = Vec(12, UInt(64.W))
    val fa = Vec(8, UInt(64.W))
  }

  val FReg1 = Wire(new Info_abi_Freg)
  val FReg2 = Wire(new Info_abi_Freg)
  dontTouch(FReg1)
  dontTouch(FReg2)

  for ( i <- 0 until 8 )  yield { FReg1.ft(i)   := {val unbx = unbox(io.diffFReg(i)   , 0.U, Some(FType.S)); val ie = ieee(unbx, t = FType.S); sextXTo(ie(31,0),64)} }
  for ( i <- 0 until 2 )  yield { FReg1.fs(i)   := {val unbx = unbox(io.diffFReg(8+i) , 0.U, Some(FType.S)); val ie = ieee(unbx, t = FType.S); sextXTo(ie(31,0),64)} }
  for ( i <- 0 until 8 )  yield { FReg1.fa(i)   := {val unbx = unbox(io.diffFReg(10+i), 0.U, Some(FType.S)); val ie = ieee(unbx, t = FType.S); sextXTo(ie(31,0),64)} }
  for ( i <- 0 until 10 ) yield { FReg1.fs(2+i) := {val unbx = unbox(io.diffFReg(18+i), 0.U, Some(FType.S)); val ie = ieee(unbx, t = FType.S); sextXTo(ie(31,0),64)} }
  for ( i <- 0 until 4 )  yield { FReg1.ft(8+i) := {val unbx = unbox(io.diffFReg(28+i), 0.U, Some(FType.S)); val ie = ieee(unbx, t = FType.S); sextXTo(ie(31,0),64)} }

  for ( i <- 0 until 8 )  yield { FReg2.ft(i)   := {val unbx = unbox(io.diffFReg(i)   , 1.U, None); ieee(unbx, t = FType.D)} }
  for ( i <- 0 until 2 )  yield { FReg2.fs(i)   := {val unbx = unbox(io.diffFReg(8+i) , 1.U, None); ieee(unbx, t = FType.D)} }
  for ( i <- 0 until 8 )  yield { FReg2.fa(i)   := {val unbx = unbox(io.diffFReg(10+i), 1.U, None); ieee(unbx, t = FType.D)} }
  for ( i <- 0 until 10 ) yield { FReg2.fs(2+i) := {val unbx = unbox(io.diffFReg(18+i), 1.U, None); ieee(unbx, t = FType.D)} }
  for ( i <- 0 until 4 )  yield { FReg2.ft(8+i) := {val unbx = unbox(io.diffFReg(28+i), 1.U, None); ieee(unbx, t = FType.D)} }
}

