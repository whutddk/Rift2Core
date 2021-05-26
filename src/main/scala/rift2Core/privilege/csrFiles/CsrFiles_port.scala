

/*
  Copyright (c) 2020 - 2021 Ruige Lee <wut.ruigeli@gmail.com>

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

package rift2Core.privilege.csrFiles

import chisel3._
import chisel3.util._
import rift2Core.define._


class Exe_Port extends Bundle {
  val addr = UInt(12.W)
  val dat_i = UInt(64.W)
  val op_rw = Bool()
  val op_rs = Bool()
  val op_rc = Bool()
}


object Reg_Exe_Port {

  def apply( csr_reg: UInt, addr: UInt, ep: Exe_Port ): (Bool, UInt) = {
    val enable = (ep.addr === addr) & (ep.op_rw | ep.op_rs | ep.op_rc)
    val dnxt = Mux1H(Seq(
        ep.op_rw -> ( ep.dat_i),
        ep.op_rs -> (csr_reg | ep.dat_i),
        ep.op_rc -> (csr_reg & ~ep.dat_i),
      ))
    return (enable, dnxt)
  }
}

abstract class CsrFiles_port extends Module{
  val exe_port = Wire(new Exe_Port)
  val is_trap = Wire(Bool())
  // val is_exception = Wire(Bool())
  val is_mRet = Wire(Bool())
  val is_sRet = Wire(Bool())
  // val is_uRet: Bool
  // lazy val is_xRet = is_mRet | is_sRet //| is_uRet

  val commit_pc = Wire(UInt(64.W))
  val ill_instr = Wire(UInt(64.W))
  val ill_vaddr = Wire(UInt(64.W))


  val is_instr_accessFault      = Wire(Bool())
  val is_instr_illeage          = Wire(Bool())
  val is_breakPoint             = Wire(Bool())
  val is_load_misAlign          = Wire(Bool())
  val is_load_accessFault       = Wire(Bool())
  val is_storeAMO_misAlign      = Wire(Bool())
  val is_storeAMO_accessFault   = Wire(Bool())
  val is_ecall                  = Wire(Bool())
  val is_instr_pageFault        = Wire(Bool())
  val is_load_pageFault         = Wire(Bool())
  val is_storeAMO_pageFault     = Wire(Bool())


  val retired_cnt = Wire( UInt(2.W))

  val clint_sw_m = Wire(Bool())
  val clint_sw_s = Wire(Bool())
  val clint_tm_m = Wire(Bool())
  val clint_tm_s = Wire(Bool())
  val clint_ex_m = Wire(Bool())
  val clint_ex_s = Wire(Bool())

  val rtc_clock = Wire(Bool())


  // val ustatus = Wire(UInt(64.W))
  // val uie = Wire(UInt(64.W))
  // val utvec = Wire(UInt(64.W))
  // val uscratch = Wire(UInt(64.W))
  // val uepc = Wire(UInt(64.W))
  // val ucause = Wire(UInt(64.W))
  // val utval = Wire(UInt(64.W))
  // val uip = Wire(UInt(64.W))
  val fflags = Wire(UInt(64.W))
  val frm = Wire(UInt(64.W))
  val fcsr = Wire(UInt(64.W))
  val cycle = Wire(UInt(64.W))
  val time = Wire(UInt(64.W))
  val instret = Wire(UInt(64.W))
  val sstatus = Wire(UInt(64.W))
  // val sedeleg = Wire(UInt(64.W))
  // val sideleg = Wire(UInt(64.W))
  val sie = Wire(UInt(64.W))
  val stvec = Wire(UInt(64.W))
  val scounteren = Wire(UInt(64.W))
  val sscratch = Wire(UInt(64.W))
  val sepc = Wire(UInt(64.W))
  val scause = Wire(UInt(64.W))
  val stval = Wire(UInt(64.W))
  val sip = Wire(UInt(64.W))
  val satp = Wire(UInt(64.W))
  val hstatus = Wire(UInt(64.W))
  val hedeleg = Wire(UInt(64.W))
  val hideleg = Wire(UInt(64.W))
  val hie = Wire(UInt(64.W))
  val hcounteren = Wire(UInt(64.W))
  val hgeie = Wire(UInt(64.W))
  val htval = Wire(UInt(64.W))
  val hip = Wire(UInt(64.W))
  val hvip = Wire(UInt(64.W))
  val htinst = Wire(UInt(64.W))
  val hgeip = Wire(UInt(64.W))
  val hgatp = Wire(UInt(64.W))
  val htimedelta = Wire(UInt(64.W))
  val vsstatus = Wire(UInt(64.W))
  val vsie = Wire(UInt(64.W))
  val vstvec = Wire(UInt(64.W))
  val vsscratch = Wire(UInt(64.W))
  val vsepc = Wire(UInt(64.W))
  val vscause = Wire(UInt(64.W))
  val vstval = Wire(UInt(64.W))
  val vsip = Wire(UInt(64.W))
  val vsatp = Wire(UInt(64.W))
  val mvendorid = Wire(UInt(64.W))
  val marchid = Wire(UInt(64.W))
  val mimpid = Wire(UInt(64.W))
  val mhartid = Wire(UInt(64.W))
  val mstatus = Wire(UInt(64.W))
  val misa = Wire(UInt(64.W))
  val medeleg = Wire(UInt(64.W))
  val mideleg = Wire(UInt(64.W))
  val mie = Wire(UInt(64.W))
  val mtvec = Wire(UInt(64.W))
  val mcounteren = Wire(UInt(64.W))
  val mscratch = Wire(UInt(64.W))
  val mepc = Wire(UInt(64.W))
  val mcause = Wire(UInt(64.W))
  val mtval = Wire(UInt(64.W))
  val mip = Wire(UInt(64.W))
  val mtinst = Wire(UInt(64.W))
  val mtval2 = Wire(UInt(64.W))
  val mcycle = Wire(UInt(64.W))
  val minstret = Wire(UInt(64.W))
  val mcountinhibit = Wire(UInt(64.W))
  val tselect = Wire(UInt(64.W))
  val tdata1 = Wire(UInt(64.W))
  val tdata2 = Wire(UInt(64.W))
  val tdata3 = Wire(UInt(64.W))
  val dcsr = Wire(UInt(64.W))
  val dpc = Wire(UInt(64.W))
  val dscratch0 = Wire(UInt(64.W))
  val dscratch1 = Wire(UInt(64.W))


  val pmpcfg  = WireDefault(VecInit( Seq.fill(16)(0.U(64.W)) ))
  val pmpaddr = WireDefault(VecInit( Seq.fill(64)(0.U(64.W)) ))


  val hpmcounter = WireDefault(VecInit( Seq.fill(32)(0.U(64.W)) ))
  val mhpmcounter = WireDefault(VecInit( Seq.fill(32)(0.U(64.W)) ))
  val mhpmevent = WireDefault(VecInit( Seq.fill(32)(0.U(64.W)) ))


  val priv_lvl_dnxt = Wire(UInt(2.W))
  val priv_lvl_qout = RegNext(priv_lvl_dnxt, "b11".U(2.W))

}

