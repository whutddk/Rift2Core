/*
  Copyright (c) 2020 - 2022 Wuhan University of Technology <295054118@whut.edu.cn>

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

class Info_csr_reg extends Bundle {
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

  val pmpcfg = Vec(4, UInt(64.W))
	val pmpaddr = Vec(16, UInt(64.W))


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
}

class diff extends Module {
  val io = IO(new Bundle{
    val register = Input(new Info_abi_reg)
    val commit = Input(new Info_cmm_diff)
    val csr = Input(new Info_csr_reg)
  })


  dontTouch(io)

}

