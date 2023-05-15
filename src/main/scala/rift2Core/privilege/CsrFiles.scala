
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

package rift2Core.privilege


import chisel3._
import chisel3.util._
import rift2Core.backend._

import rift2Chip._
import org.chipsalliance.cde.config._

class CsrOperator extends Bundle{
  val dat_i = UInt(64.W)
  val op_rw = Bool()
  val op_rs = Bool()
  val op_rc = Bool()
}

class Exe_Port extends CsrOperator {
  val addr = UInt(12.W)
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


// abstract class  BaseCsrFiles extends BaseCommit

class FCSRBundle extends Bundle{
  val frm      = UInt(3.W)
  val fflags   = UInt(5.W)
}

class VCSRBundle extends Bundle{
  val vxrm     = UInt(2.W)
  val vxsat    = UInt(1.W)
}

class VConfigBundle(implicit p: Parameters) extends RiftBundle{
  require( log2Ceil(vParams.vlmax) <= 55 )
  val vill     = UInt(1.W)
  val vl       = UInt(55.W)
  val vtype    = UInt(8.W)
}

/**
  * Machine Status Registers
  * @param SD (63) whether either fs or xs is dirty
  * @param MBE (37) endian of M-mode, when 0, is little-endian, hard-wire to 0.U(64.bits)
  * @param SBE (36) endian of S-mode, when 0, is little-endian, hard-wire to 0.U(64.bits)
  * @param SXL (35,34) XLEN of S-mode, hard-wire to 2.U(64.bits)
  * @param UXL (33,32) XLEN of U-mode, hard-wire to 2.U(64.bits)
  * @param TSR (22) trap sret, when 1, sret in s-mode cause illegal instruction exception
  * @param TW (21) Time out wait for WFI, when 1 wfi cause illegal instruction exception in U\S-MODE
  * @param TVM (20) Trap Virtual Memory, when 1, access satp or Sfence.vma will cause illegal instruction exception
  * @param MXR (19) Make executable Readable. When 0, only loads form pages marked readable
  * @param SUM (18) permit Supervisor User Memory access. When 0, S-mode accesses to page.U === 1 will fault.
  * @param MPRV (17) Memory Privilege; When 1, load store using trans&protect in M-mode; When MRET or SRet to S\U-mode, set to 0.
  * @param XS (16,15) aditional user-mode extension and associated state
  * @param FS (14,13) float point statuw=s
  * @param MPP (12,11) Previous Mode is U-mode or S-mode or M-mode? When MRet, privilege mode update to MPP, MPP set to "U"
  * @param SPP (8) Previous Mode is U-mode or S-mode? When SRet, privilege mode update to SPP, SPP set to "U"
  * @param MPIE (7)  When MRet, MPIE set to 1
  * @param UBE (6) endian of U-mode, when 0, is little-endian, hard-wire to 0.U(64.bits)
  * @param SPIE (5) When SRet, SPIE set to 1
  * @param MIE (3) M-mode Interrupt Enable; When MRet, update to MPIE
  * @param SIE (1) S-mode Interrupt Enable; When SRet, update to SPIE
  */
class MStatusBundle extends Bundle{
  val sd = UInt(1.W)
  val reserved0 = UInt(25.W)
  val mbe = UInt(1.W)
  val sbe = UInt(1.W)
  val sxl = UInt(2.W)
  val uxl = UInt(2.W)
  val reserved1 = UInt(9.W)
  val tsr = UInt(1.W)
  val tw = UInt(1.W)
  val tvm = UInt(1.W)
  val mxr = UInt(1.W)
  val sum = UInt(1.W)
  val mprv = UInt(1.W)
  val xs = UInt(2.W)
  val fs = UInt(2.W)
  val mpp = UInt(2.W)
  val reserved2 = UInt(2.W)
  val spp = UInt(1.W)
  val mpie = UInt(1.W)
  val ube = UInt(1.W)
  val spie = UInt(1.W)
  val reserved3 = UInt(1.W)
  val mie = UInt(1.W)
  val reserved4 = UInt(1.W)
  val sie = UInt(1.W)
  val reserved5 = UInt(1.W)
}

class MSIntBundle extends Bundle{
  val reserved0 = UInt(4.W)
  val mei       = UInt(1.W)
  val reserved1 = UInt(1.W)
  val sei       = UInt(1.W)
  val reserved2 = UInt(1.W)
  val mti       = UInt(1.W)
  val reserved3 = UInt(1.W)
  val sti       = UInt(1.W)
  val reserved4 = UInt(1.W)
  val msi       = UInt(1.W)
  val reserved5 = UInt(1.W)
  val ssi       = UInt(1.W)
  val reserved6 = UInt(1.W)
}

class TVecBundle extends Bundle{
  val base = UInt(62.W)
  val mode = UInt(2.W)
}

class CounterenBundle extends Bundle{
  val hpm = UInt(32.W)
  def ir = hpm(2)
  def tm = hpm(1)
  def cy = hpm(0)
}

class CauseBundle extends Bundle{
  val interrupt = UInt(1.W)
  val exception_code = UInt(63.W)
}

class PmpcfgBundle extends Bundle{
  val L = UInt(1.W)
  val reserved0 = UInt(2.W)
  val A = UInt(2.W)
  val X = UInt(1.W)
  val W = UInt(1.W)
  val R = UInt(1.W)
}

class SatpBundle extends Bundle{
  val mode = UInt(4.W)
  val asid = UInt(16.W)
  val ppn = UInt(44.W)
}

class DcsrBundle extends Bundle{
  val xdebugver = UInt(4.W)
  val reserved0 = UInt(12.W)
  val ebreakm   = UInt(1.W)
  val reserved1 = UInt(1.W)
  val ebreaks   = UInt(1.W)
  val ebreaku   = UInt(1.W)
  val stepie    = UInt(1.W)
  val stopcount = UInt(1.W)
  val stoptime  = UInt(1.W)
  val cause     = UInt(3.W)
  val reserved2 = UInt(1.W)
  val mprven    = UInt(1.W)
  val nmip      = UInt(1.W)
  val step      = UInt(1.W)
  val prv       = UInt(2.W)
}

class CSR_Bundle(implicit p: Parameters) extends RiftBundle {

  val priv_lvl = UInt(2.W)
  // val new_priv = UInt(2.W)
  val DMode    = Bool()

  val vstart  = UInt( (log2Ceil(vParams.vlmax)).W )
  val vcsr    = new VCSRBundle
  val vConfig = new VConfigBundle
  val vlenb   = UInt((log2Ceil(vParams.vlen/8)).W)

  // val ustatus  = UInt(64.W)
  // val uie      = UInt(64.W)
  // val utvec    = UInt(64.W)
  // val uscratch = UInt(64.W)
  // val uepc     = UInt(64.W)
  // val ucause   = UInt(64.W)
  // val utval    = UInt(64.W)
  // val uip      = UInt(64.W)
  val fflags   = UInt(64.W)
  val frm      = UInt(64.W)
  val fcsr        = new FCSRBundle
  /** Hardware Performance Monitor -- time (read-only) @return a count of the number of ***rtc*** cycles executed by the ***processor core*** on which the hart is running from an arbitrary start time in the past*/
  val time        = UInt(64.W)
  val sstatus     = new MStatusBundle
  // val sedeleg  = UInt(64.W)
  // val sideleg  = UInt(64.W)
  val sie         = new MSIntBundle
  val stvec       = new TVecBundle
  val scounteren  = new CounterenBundle
  val sscratch    = UInt(64.W)
  val sepc        = UInt(64.W)
  val scause      = new CauseBundle
  val stval       = UInt(64.W)
  val sip         = new MSIntBundle
  val satp        = new SatpBundle
  // val hstatus     = UInt(64.W)
  // val hedeleg     = UInt(64.W)
  // val hideleg     = UInt(64.W)
  // val hie         = UInt(64.W)
  // val hcounteren  = UInt(64.W)
  // val hgeie       = UInt(64.W)
  // val htval       = UInt(64.W)
  // val hip         = UInt(64.W)
  // val hvip        = UInt(64.W)
  // val htinst      = UInt(64.W)
  // val hgeip       = UInt(64.W)
  // val hgatp       = UInt(64.W)
  // val htimedelta  = UInt(64.W)
  // val vsstatus    = UInt(64.W)
  // val vsie        = UInt(64.W)
  // val vstvec      = UInt(64.W)
  // val vsscratch   = UInt(64.W)
  // val vsepc       = UInt(64.W)
  // val vscause     = UInt(64.W)
  // val vstval      = UInt(64.W)
  // val vsip        = UInt(64.W)
  // val vsatp       = UInt(64.W)
  val mvendorid   = UInt(64.W)
  val marchid     = UInt(64.W)
  val mimpid      = UInt(64.W)
  val mhartid     = UInt(64.W)
  val mstatus     = new MStatusBundle
  val misa        = UInt(64.W)
  val medeleg     = UInt(64.W)
  val mideleg     = UInt(64.W)
  val mie         = new MSIntBundle
  val mtvec       = new TVecBundle
  val mcounteren  = new CounterenBundle
  val mscratch    = UInt(64.W)
  val mepc        = UInt(64.W)
  val mcause      = new CauseBundle
  val mtval       = UInt(64.W)
  val mip         = new MSIntBundle
  val mtinst      = UInt(64.W)
  val mtval2      = UInt(64.W)
  val mcycle      = UInt(64.W)
  val minstret    = UInt(64.W)
  val mcountinhibit = UInt(64.W)
  val tselect     = UInt(64.W)
  val tdata1      = UInt(64.W)
  val tdata2      = UInt(64.W)
  val tdata3      = UInt(64.W)
  val dcsr        = new DcsrBundle
  val dpc         = UInt(64.W)
  val dscratch0   = UInt(64.W)
  val dscratch1   = UInt(64.W)
  val dscratch2   = UInt(64.W)

  val pmpcfg  = (if(pmpNum==0) { Vec( 1, Vec(8, new PmpcfgBundle) ) } else {Vec( pmpNum, Vec(8, new PmpcfgBundle) )})
  val pmpaddr = (if(pmpNum==0) { Vec( 8, UInt(64.W)) }      else {Vec( 8*pmpNum, UInt(64.W))})

  val mhpmcounter = Vec( 32, UInt(64.W))
  val mhpmevent   = Vec( 32, UInt(64.W))


  val cycle      = UInt(64.W)
  val instret    = UInt(64.W)
  val hpmcounter = Vec( 32, UInt(64.W))

  def is_ssi: Bool = { 
    val is_ssi = mip.ssi & mie.ssi & mstatus.sie & ~( priv_lvl === "b11".U & mideleg(1) )
    return is_ssi.asBool
  }
  def is_msi: Bool = { 
    val is_msi = mip.msi & mie.msi & mstatus.mie
    return is_msi.asBool
  }
  def is_sti: Bool = { 
    val is_sti = mip.sti & mie.sti & mstatus.sie & ~( priv_lvl === "b11".U & mideleg(5) )
    return is_sti.asBool
  }
  def is_mti: Bool = { 
    val is_mti = mip.mti & mie.mti & mstatus.mie
    return is_mti.asBool
  }
  def is_sei: Bool = { 
    val is_sei = mip.sei & mie.sei & mstatus.sie & ~( priv_lvl === "b11".U & mideleg(9) )
    return is_sei.asBool
  }
  def is_mei: Bool = { 
    val is_mei = mip.mei & mie.mei & mstatus.mie
    return is_mei.asBool
  }
  def is_m_interrupt: Bool = {
    val is_m_interrupt = is_msi | is_mti | is_mei
    return is_m_interrupt
  }
  def is_s_interrupt: Bool = {
    val is_s_interrupt = is_ssi | is_sti | is_sei
    return is_s_interrupt
  }


  def isViolateCSRR(addr: UInt): Bool = {
    return MuxCase( true.B, //non-exist csr
      Seq( //special situation has higher priority
        ( addr === "h180".U ) -> ~((priv_lvl === "b11".U) | (priv_lvl === "b01".U & mstatus.tvm === 0.U)), //TVM IN S-MODE
        ( addr === "hB00".U ) -> ~((priv_lvl === "b11".U) | (priv_lvl === "b01".U & mcounteren.cy)),
        ( addr === "hB02".U ) -> ~((priv_lvl === "b11".U) | (priv_lvl === "b01".U & mcounteren.ir)),
      ) ++
      CSRInfoTable.CSRGroup.map{ csr =>
        (addr === (csr.address).U) -> 
          ( (priv_lvl < (csr.priv).U) | //wrong priv
            (if( csr.isDMode ) { ~DMode } else {false.B})
          )
      }
    )
  }

  def isViolateCSRW(addr: UInt): Bool = {
    return CSRInfoTable.CSRGroup.map{ csr =>
      (addr === (csr.address).U) & ((csr.isRO).B)
    }.reduce(_|_)
  }

}

trait UpdateCsrFilesFun { this: BaseCommit =>




  def update_fcsr( in: CMMState_Bundle): FCSRBundle = {
    val fcsr = WireDefault( in.csrfiles.fcsr )

    val (enable0, dnxt0) = Reg_Exe_Port( in.csrfiles.fcsr.fflags, "h001".U, in.csrExe )
    val (enable1, dnxt1) = Reg_Exe_Port( in.csrfiles.fcsr.frm,    "h002".U, in.csrExe )
    val (enable2, dnxt2) = Reg_Exe_Port( in.csrfiles.fcsr.asUInt, "h003".U, in.csrExe )  
        

    when(enable0)      { fcsr.fflags := dnxt0 }
    .elsewhen(enable1) { fcsr.frm    := dnxt1 }
    .elsewhen(enable2) { fcsr.fflags := dnxt2(4,0); fcsr.frm := dnxt2(7,5) }   

    return fcsr
  }






 
      





  //machine information register
  def update_mvendorid( in: CMMState_Bundle ) = 0.U
  def update_marchid  ( in: CMMState_Bundle ) = 29.U
  def update_mimpid   ( in: CMMState_Bundle ) = 0.U
  def update_mhartid  ( in: CMMState_Bundle ) = 0.U


  def update_priv_lvl( in: CMMState_Bundle ): UInt = {
    val priv_lvl = WireDefault( in.csrfiles.priv_lvl )

    when(in.exint.emu_reset) { priv_lvl := "b11".U}

    when( in.csrfiles.DMode ) {
      when(in.is_dRet) { priv_lvl := in.csrfiles.dcsr.prv }
    } .otherwise {
      when(in.is_mRet) { priv_lvl := in.csrfiles.mstatus.mpp }
      when(in.is_sRet) { priv_lvl := in.csrfiles.mstatus.spp }

      when(in.csrfiles.is_ssi) { priv_lvl := Mux( in.csrfiles.priv_lvl === "b11".U, "b11".U, Mux( ~in.csrfiles.mideleg(1), "b11".U, "b01".U ) ) }
      when(in.csrfiles.is_msi) { priv_lvl := "b11".U }
      when(in.csrfiles.is_sti) { priv_lvl := Mux( in.csrfiles.priv_lvl === "b11".U, "b11".U, Mux( ~in.csrfiles.mideleg(5), "b11".U, "b01".U ) ) }
      when(in.csrfiles.is_mti) { priv_lvl := "b11".U }
      when(in.csrfiles.is_sei) { priv_lvl := Mux( in.csrfiles.priv_lvl === "b11".U, "b11".U, Mux( ~in.csrfiles.mideleg(9), "b11".U, "b01".U ) ) }
      when(in.csrfiles.is_mei) { priv_lvl := "b11".U }


      when(in.is_instr_misAlign       ) { priv_lvl := Mux( in.csrfiles.priv_lvl === "b11".U, "b11".U, Mux(~in.csrfiles.medeleg(0),  "b11".U, "b01".U) )}
      when(in.is_instr_access_fault   ) { priv_lvl := Mux( in.csrfiles.priv_lvl === "b11".U, "b11".U, Mux(~in.csrfiles.medeleg(1),  "b11".U, "b01".U) )}
      when(in.is_illeage              ) { priv_lvl := Mux( in.csrfiles.priv_lvl === "b11".U, "b11".U, Mux(~in.csrfiles.medeleg(2),  "b11".U, "b01".U) )}
      when(in.is_ebreak_exc           ) { priv_lvl := Mux( in.csrfiles.priv_lvl === "b11".U, "b11".U, Mux(~in.csrfiles.medeleg(3),  "b11".U, "b01".U) )}
      when(in.is_load_misAlign        ) { priv_lvl := Mux( in.csrfiles.priv_lvl === "b11".U, "b11".U, Mux(~in.csrfiles.medeleg(4),  "b11".U, "b01".U) )}
      when(in.is_load_accessFault     ) { priv_lvl := Mux( in.csrfiles.priv_lvl === "b11".U, "b11".U, Mux(~in.csrfiles.medeleg(5),  "b11".U, "b01".U) )}
      when(in.is_store_misAlign       ) { priv_lvl := Mux( in.csrfiles.priv_lvl === "b11".U, "b11".U, Mux(~in.csrfiles.medeleg(6),  "b11".U, "b01".U) )}
      when(in.is_store_accessFault    ) { priv_lvl := Mux( in.csrfiles.priv_lvl === "b11".U, "b11".U, Mux(~in.csrfiles.medeleg(7),  "b11".U, "b01".U) )}
      when(in.is_ecall_U              ) { priv_lvl := Mux(~in.csrfiles.medeleg(8), "b11".U, "b01".U) }
      when(in.is_ecall_S              ) { priv_lvl := Mux(~in.csrfiles.medeleg(9), "b11".U, "b01".U) }
      when(in.is_ecall_M              ) { priv_lvl :=  "b11".U }
      when(in.is_instr_paging_fault   ) { priv_lvl := Mux( in.csrfiles.priv_lvl === "b11".U, "b11".U, Mux(~in.csrfiles.medeleg(12), "b11".U, "b01".U) )}
      when(in.is_load_pagingFault     ) { priv_lvl := Mux( in.csrfiles.priv_lvl === "b11".U, "b11".U, Mux(~in.csrfiles.medeleg(13), "b11".U, "b01".U) )}
      when(in.is_store_pagingFault    ) { priv_lvl := Mux( in.csrfiles.priv_lvl === "b11".U, "b11".U, Mux(~in.csrfiles.medeleg(15), "b11".U, "b01".U) )}

    }
    
    return priv_lvl
  }


  def update_mstatus( in: CMMState_Bundle ): MStatusBundle = {
    val mstatus = WireDefault( in.csrfiles.mstatus )

    mstatus.sd  := ((mstatus.xs === 3.U) || (mstatus.fs === 3.U)).asUInt
    mstatus.mbe := 0.U(1.W)
    mstatus.sbe := 0.U(1.W)
    mstatus.sxl := 2.U(2.W)
    mstatus.uxl := 2.U(2.W)
    mstatus.xs  := 0.U(2.W)
    mstatus.ube := 0.U(1.W)
      
    val (enable0, dnxt0) = Reg_Exe_Port( in.csrfiles.mstatus.asUInt, "h100".U, in.csrExe )
    val (enable1, dnxt1) = Reg_Exe_Port( in.csrfiles.mstatus.asUInt, "h300".U, in.csrExe )

    when( (in.isInterrupt | in.isException) & ~in.csrfiles.DMode & ~in.isNomaskInterrupt ) {
      when( update_priv_lvl(in) === "b11".U ) {
        mstatus.mpie := in.csrfiles.mstatus.mie
        mstatus.mie  := 0.U
        mstatus.mpp  := in.csrfiles.priv_lvl
      }
      when( update_priv_lvl(in) === "b01".U ) {
        mstatus.spp  := Mux( (in.csrfiles.priv_lvl === "b00".U), 0.U, 1.U )
        mstatus.spie := in.csrfiles.mstatus.sie
        mstatus.sie  := 0.U
      }
    }
    .elsewhen( in.is_mRet & ~in.csrfiles.DMode & ~in.isNomaskInterrupt ) {
      mstatus.mie  := in.csrfiles.mstatus.mpie
      mstatus.mpie := 1.U
      mstatus.mpp  := "b00".U

      mstatus.mprv := Mux( update_priv_lvl(in) =/= "b11".U, 0.U, in.csrfiles.mstatus.mprv )
    }
    .elsewhen( in.is_sRet & ~in.csrfiles.DMode & ~in.isNomaskInterrupt  ) {
      mstatus.spie := 1.U
      mstatus.sie  := in.csrfiles.mstatus.spie

      mstatus.mprv := Mux( update_priv_lvl(in) =/= "b11".U, 0.U, in.csrfiles.mstatus.mprv )
    }
    .elsewhen(enable0) {

      mstatus.mxr  := dnxt0(19)
      mstatus.sum  := dnxt0(18)
      mstatus.fs   := dnxt0(14,13)
      mstatus.spp  := dnxt0(8)
      mstatus.spie := dnxt0(5)
      mstatus.sie  := dnxt0(1)
    }
    .elsewhen(enable1) {
      mstatus.tsr  := dnxt1(22)
      mstatus.tw   := dnxt1(21)
      mstatus.tvm  := dnxt1(20)
      mstatus.mxr  := dnxt1(19)
      mstatus.sum  := dnxt1(18)
      mstatus.mprv := dnxt1(17)
      mstatus.fs   := dnxt1(14,13)
      mstatus.mpp  := dnxt1(12,11)
      mstatus.spp  := dnxt1(8)
      mstatus.mpie := dnxt1(7)
      mstatus.spie := dnxt1(5)
      mstatus.mie  := dnxt1(3)
      mstatus.sie  := dnxt1(1)
    }

    when(in.is_fpu_state_change & in.csrfiles.mstatus.fs =/= 0.U) {
      mstatus.fs := 3.U
    }
    return mstatus
  }


  /**
    * Machine ISA register 
    * @param MXL (63,62) is 2.U for XLEN of RiftCore is 64 
    * @param Extensions (25:0) 
    * @note U(20): User mode implement S(18): Supervisor mode implemented N(13): User-level interrupts supported
    * @note M(12): Integer Multiply/Divide extension I(8): RV64I base ISA C(2): Compressed extension
    * 
    */
  def update_misa( in: CMMState_Bundle ) = {
    val mxl = WireDefault(2.U(2.W))
    val extensions = {
      if ( fpuNum > 0 ) { //fpu
        WireDefault("b00000101000001000100101101".U(26.W))  
      } else if (true) { //none fpu, has S-mode U-mode
        WireDefault("b00000101000001000100000101".U(26.W))
      } else {
        WireDefault("b00000000000001000100000101".U(26.W))
      }
    
    }
    Cat(mxl, 0.U(36.W), extensions)
  }

  
  

  
  /**
    * Machine Trap Delegation Register
    * 
    * By default, the exception will be handled in M-mode, when the bits set, it's handled in S-mode
    */
  def update_medeleg( in: CMMState_Bundle ): UInt = {
    val medeleg = WireDefault( in.csrfiles.medeleg )
    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.medeleg, "h302".U, in.csrExe )
    when(enable) { medeleg := dnxt }
    return medeleg
  } 
  
  /**
    * Machine Trap Delegation Register
    * 
    * By default, the interrupt will be handled in M-mode, when the bits set, it's handled in S-mode
    */
  def update_mideleg( in: CMMState_Bundle ): UInt = {
    val mideleg = WireDefault( in.csrfiles.mideleg )
    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.mideleg, "h303".U, in.csrExe )
    when(enable) { mideleg := dnxt }
    return mideleg
  }

  /**
    * Machine Interrupt Registers
    *
    * @param meie (11)
    * @param seie (9)
    * @param mtie (7)
    * @param stie (5)
    * @param msie (3)
    * @param ssie (1)
    */
  def update_mie( in: CMMState_Bundle ): MSIntBundle = {
    val mie = WireDefault( in.csrfiles.mie )

    val (enable0, dnxt0) = Reg_Exe_Port( in.csrfiles.mie.asUInt, "h304".U, in.csrExe )
    val (enable1, dnxt1) = Reg_Exe_Port( in.csrfiles.sie.asUInt, "h104".U, in.csrExe )

    when(enable0) {
      mie.mei := dnxt0(11)
      mie.sei := dnxt0(9)
      mie.mti := dnxt0(7)
      mie.sti := dnxt0(5)
      mie.msi := dnxt0(3)
      mie.ssi := dnxt0(1)
    }
    .elsewhen(enable1) {
      mie.sei := dnxt1(9)
      mie.sti := dnxt1(5)
      mie.ssi := dnxt1(1)
    }      
    return mie
  }



  /**
    * Machine Trap-Vector Base-Address Register
    * holds trap vector configuration, consisting of a vector of a vector base address and a bector mode 
    * @param base (63,2)
    * @param mode (1,0) read only in this version hardwire to 0.U
    */

  def update_mtvec( in: CMMState_Bundle ): TVecBundle = {
    val mtvec = WireDefault( in.csrfiles.mtvec )

    mtvec.mode := 0.U
    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.mtvec.asUInt, "h305".U, in.csrExe )
    when(enable) { mtvec.base := dnxt(63,2) }    

    return mtvec
  }




  /**
    * Machine Counter-Enable Register -- mcounteren
    *
    * @param HPM (31,3) Whether is allowed to access hpmcounter(x) in S-mode
    * @param IR (2) Whether is allowed to access instret in S-mode
    * @param TM (1) Whether is allowed to access time in S-mode
    * @param CY (0) Whether is allowed to access cycle in S-mode
    */

  def update_mcounteren( in: CMMState_Bundle ): CounterenBundle = {
    val mcounteren = WireDefault( in.csrfiles.mcounteren )

    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.mcounteren.asUInt, "h306".U, in.csrExe )
    when(enable) { mcounteren.hpm := dnxt }
    return mcounteren
  }


  //Machine Trap Handling
  /**
    * Machine Scratch Register -- mscratch
    *
    * it's used to hold a pointer to a M-mode hart-local context space and swapped with a user register upon entry to an M-mode trap handler
    */
  def update_mscratch( in: CMMState_Bundle ): UInt = {
    val mscratch = WireDefault( in.csrfiles.mscratch )
    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.mscratch, "h340".U, in.csrExe )
    when(enable) { mscratch := dnxt }
    return mscratch
  }


  /**
    * Machine Exception Program Counter
    * @note hold all valid virtual addresses 
    * when a ***trap*** is taken into ***M-mode***, update to the ***virtual address*** that was interrupted or encountered the exception 
    * @note we are only considering 2 condition: 1) 1 trap outsize the DMode; 2) trap inside the DMode; we will not consider normal trap + step, for step-interrupt has one cycle latency
    */
  def update_mepc( in: CMMState_Bundle ): UInt = {
    val mepc = WireDefault( in.csrfiles.mepc )
    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.mepc, "h341".U, in.csrExe )

    when( (in.isInterrupt | in.isException) & update_priv_lvl(in) === "b11".U & ~in.csrfiles.DMode & ~in.isNomaskInterrupt  ){ mepc := in.commit_pc }
    .elsewhen(enable) { mepc := dnxt }
    return mepc
  }

  /**
    * Machine Cause Register
    * 
    * when a ***trap*** is taken into ***M-mode***, Indicating the event that caused the trap
    * @param interrupt
    * @param exception_code
    */
  def update_mcause( in: CMMState_Bundle ): CauseBundle = {
    val mcause = WireDefault( in.csrfiles.mcause )
    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.mcause.asUInt, "h342".U, in.csrExe )

    when( in.csrfiles.is_m_interrupt & update_priv_lvl(in) === "b11".U & ~in.csrfiles.DMode & ~in.isNomaskInterrupt ) {
      mcause.interrupt := 1.U
      mcause.exception_code := Mux1H( Seq(
        // is_ssi -> 1.U,
        in.csrfiles.is_msi -> 3.U,
        // is_sti -> 5.U,
        in.csrfiles.is_mti -> 7.U,
        // is_sei -> 9.U,
        in.csrfiles.is_mei -> 11.U
      ))
    }
    .elsewhen( in.isException & update_priv_lvl(in) === "b11".U & ~in.csrfiles.DMode & ~in.isNomaskInterrupt ) {
      mcause.interrupt := 0.U
      mcause.exception_code := Mux1H( Seq(
        in.is_instr_misAlign        -> 0.U,
        in.is_instr_access_fault    -> 1.U,
        in.is_illeage               -> 2.U,
        in.is_ebreak_exc            -> 3.U,
        in.is_load_misAlign         -> 4.U,
        in.is_load_accessFault      -> 5.U,
        in.is_store_misAlign        -> 6.U,
        in.is_store_accessFault     -> 7.U,
        in.is_ecall_U               -> 8.U,
        in.is_ecall_S               -> 9.U,
        in.is_ecall_M               -> 11.U,
        in.is_instr_paging_fault    -> 12.U,
        in.is_load_pagingFault      -> 13.U,
        in.is_store_pagingFault     -> 15.U,
      ))
    }
    .elsewhen(enable) {
      mcause.interrupt      := dnxt(63)
      mcause.exception_code := dnxt(62,0)
    }
    return mcause
  }

  

  /**
    * Machine Trap Value Register
    * 
    * When a trap is taken into ***M-mode***, update to ***virtual address*** or ***faulting instruction***
    */
  def update_mtval( in: CMMState_Bundle ): UInt = {
    val mtval = WireDefault( in.csrfiles.mtval )

    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.mtval, "h343".U, in.csrExe )

    when( (in.isInterrupt | in.isException) & update_priv_lvl(in) === "b11".U & ~in.csrfiles.DMode & ~in.isNomaskInterrupt ) {
      mtval := Mux1H( Seq(
        in.is_instr_access_fault    -> in.ill_ivaddr,
        in.is_instr_paging_fault    -> in.ill_ivaddr,
        in.is_illeage         -> 0.U,
        in.is_ebreak_exc            -> 0.U,
        in.is_load_misAlign         -> in.ill_dvaddr,
        in.is_load_accessFault      -> in.ill_dvaddr,
        in.is_store_misAlign        -> in.ill_dvaddr,
        in.is_store_accessFault     -> in.ill_dvaddr,
        in.is_load_pagingFault      -> in.ill_dvaddr,
        in.is_store_pagingFault     -> in.ill_dvaddr    
      ))
    }
    .elsewhen(enable) { mtval := dnxt }
    return mtval
  }


  /**
    * Machine Interrupt Registers
    * 
    * @note implemented in read-only mode
    *
    * @param meip (11)
    * @param seip (9)
    * @param mtip (7)
    * @param stip (5)
    * @param msip (3)
    * @param ssip (1)
    */

  def update_mip( in: CMMState_Bundle ): MSIntBundle = {
    val mip = WireDefault( in.csrfiles.mip )
    
    mip.mei := in.exint.mei
    mip.sei := in.exint.sei
    mip.mti := in.exint.mti
    mip.sti := in.exint.sti
    mip.msi := in.exint.msi
    mip.ssi := in.exint.ssi

    return mip
  }


  def update_mtinst( in: CMMState_Bundle ): UInt = {
    val mtinst = WireDefault( in.csrfiles.mtinst )

    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.mtinst, "h34A".U, in.csrExe )
    when(enable) { mtinst := dnxt }    

    return mtinst
  }

  def update_mtval2( in: CMMState_Bundle ): UInt = {
    val mtval2 = WireDefault( in.csrfiles.mtval2 )
    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.mtval2, "h34B".U, in.csrExe )
    when(enable) { mtval2 := dnxt }
    return mtval2
  }


  //Machine Memory Protection
  def update_pmpcfg( in: CMMState_Bundle ): Vec[Vec[PmpcfgBundle]] = {
    val pmpcfg = WireDefault( in.csrfiles.pmpcfg )

    for ( i <- 0 until pmpNum ) {
      for ( j <- 0 until 8 ) {
        if ( i % 2 == 0 ) {
          val (enable, dnxt) = Reg_Exe_Port( Cat(in.csrfiles.pmpcfg(i).map{_.asUInt}.reverse), "h3A0".U + (2*i).U, in.csrExe )
          
          when( enable & ~in.csrfiles.pmpcfg(i)(j).L.asBool ) {
              pmpcfg(i)(j).L := dnxt(8*j+7)
              pmpcfg(i)(j).A := dnxt(8*j+4,8*j+3)
              pmpcfg(i)(j).X := dnxt(8*j+2)
              pmpcfg(i)(j).W := dnxt(8*j+1)
              pmpcfg(i)(j).R := dnxt(8*j+0)
          }
        } else {
          pmpcfg(i)(j) := 0.U.asTypeOf(new PmpcfgBundle)
        }        
      }
    }
    return pmpcfg
  }


  def update_pmpaddr( in: CMMState_Bundle ): Vec[UInt] = {
    val pmpaddr = WireDefault(in.csrfiles.pmpaddr)

    for( i <- 0 until 8*pmpNum ) {

      val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.pmpaddr(i), "h3B0".U + i.U, in.csrExe )

      val lock = in.csrfiles.pmpcfg(i/8*2)(i%8).L
      when(enable & lock =/= 1.U ) { pmpaddr(i) := dnxt(37,0) } // for dromajo diff-test, support sv39 only
    }

    return pmpaddr
  }



  //Machine Counter/Timer

  //0xb00
  /**
    * Hardware Performance Monitor -- mcycle
    *
    * @return the number of clock cycles executed by the processor core
    */ 
  def update_mcycle(in: CMMState_Bundle): UInt = {
    val mcycle = Wire(UInt(64.W))

    val (enable, dnxt) = Reg_Exe_Port( csrfiles.mcycle, "hB00".U, in.csrExe )
      when(enable) { mcycle := dnxt }
      .otherwise{ mcycle := csrfiles.mcycle + 1.U }
    return mcycle
  }


  /**
    * Hardware Performance Monitor -- minstret
    *
    * @return the number of instructions the hart has retired
    */
  def update_minstret( in: CMMState_Bundle ): UInt = {
    val minstret = WireDefault( in.csrfiles.minstret )

    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.minstret, "hB02".U, in.csrExe )
    when(enable) { minstret := dnxt }
    .otherwise { minstret := in.csrfiles.minstret + 1.U } //we don't need to know whether it's retired here

    return minstret  
  }



  /**
    * Hardware Performance Monitor -- mhpmcounter 3~31
    *
    * @return 
    */
  def update_mhpmcounter( in: CMMState_Bundle ): Vec[UInt] = {
    val mhpmcounter = WireDefault( in.csrfiles.mhpmcounter )
    for ( i <- 0 until 32 ) yield {
      val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.mhpmcounter(i), "hB00".U + i.U, in.csrExe )

      if ( ( hpmNum == 4 ) && (i == 3 || i == 4 || i == 5 || i == 6) ) {
        when(enable) { mhpmcounter(i) := dnxt } //other counter will be hardwire to 0
      }

      if ( ( hpmNum == 4 ) && (i == 3) ) {
        //branch success
        when(in.rod.is_branch & ~io.bctq.bits.isMisPredict) {
          mhpmcounter(i) := in.csrfiles.mhpmcounter(i) + 1.U
        }
      }
      if ( ( hpmNum == 4 ) && (i == 4) ) {
        //branch mispredict
        when(in.rod.is_branch & io.bctq.bits.isMisPredict) {
          mhpmcounter(i) := in.csrfiles.mhpmcounter(i) + 1.U          
        }
      }
      if ( ( hpmNum == 4 ) && (i == 5) ) {
        //branch success
        when(in.rod.is_jalr & ~io.jctq.bits.isMisPredict) {
          mhpmcounter(i) := in.csrfiles.mhpmcounter(i) + 1.U
        }
      }
      if ( ( hpmNum == 4 ) && (i == 6) ) {
        //branch mispredict
        when(in.rod.is_jalr & io.jctq.bits.isMisPredict) {
          mhpmcounter(i) := in.csrfiles.mhpmcounter(i) + 1.U          
        }
      }
    }

    return mhpmcounter
  }


  //Machine Counter Setup
  /**
    * 
    * when set, the counter will not increase, all hard-wire to 0 in this version
    * 
    */
  def update_mcountinhibit( in: CMMState_Bundle ) = 0.U(64.W)


  def update_mhpmevent( in: CMMState_Bundle ): Vec[UInt]= {
    val mhpmevent = WireDefault(in.csrfiles.mhpmevent)

    for ( i <- 0 until 32 ) yield {
      // if ( i == 0 || i == 1 || i == 2 ) { mhpmevent(i) := 0.U }
      // else {
      //   val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.mhpmevent(i), "hB20".U + i.U, in.csrExe )
      //   when(enable) { mhpmevent(i) := dnxt }
      // }
      if ( ( hpmNum == 4 ) && (i == 3 || i == 4 || i == 5 || i == 6) ) {
        mhpmevent(i) := 1.U
      } else {
        mhpmevent(i) := 0.U
      }
    }

    return mhpmevent
  }

  //supervisor trap setup
  
  /**
    * Supervisor Status Register -- sstatus
    * 
    * @param SD (63) whether either fs or xs is dirty
    * @param UXL (33,32) XLEN of U-mode, hard-wire to 2.U(64.bits)
    * @param MXR (19) Make executable Readable. When 0, only loads form pages marked readable
    * @param SUM (18) permit Supervisor User Memory access. When 0, S-mode accesses to page.U === 1 will fault.
    * @param XS (16,15) aditional user-mode extension and associated state
    * @param FS (14,13) float point statuw=s
    * @param SPP (8) Previous Mode is U-mode or S-mode? When SRet, privilege mode update to SPP, SPP set to "U"
    * @param UBE (6) endian of U-mode, when 0, is little-endian
    * @param SPIE (5) When SRet, SPIE set to 1
    * @param SIE (1) S-mode Interrupt Enable; When SRet, update to SPIE
    * 
    */

  // val sstatus = {
  //   mstatus & Cat( "b1".U, 0.U(29.W), "b11".U, 0.U(12.W), "b11011110000101100010".U )
  // }



/**
  * Supervisor Interrupt Register -- sie (enable)
  * @note read-only, meie(11), mtie(7), msie(3) is visible and maskable when mideleg(x) set
  * 
  */
  // val (meie, meie_dnxt) = SuperscalarReg( init = 0.U(1.W), is_reitred = is_retired_v )
  // val (seie, seie_dnxt) = SuperscalarReg( init = 0.U(1.W), is_reitred = is_retired_v )
  // val (mtie, mtie_dnxt) = SuperscalarReg( init = 0.U(1.W), is_reitred = is_retired_v )
  // val (stie, stie_dnxt) = SuperscalarReg( init = 0.U(1.W), is_reitred = is_retired_v )
  // val (msie, msie_dnxt) = SuperscalarReg( init = 0.U(1.W), is_reitred = is_retired_v )
  // val (ssie, ssie_dnxt) = SuperscalarReg( init = 0.U(1.W), is_reitred = is_retired_v )

  // val sie = mie
  //     Cat(
  //       0.U(4.W), meie & mideleg(11), 0.U(1.W), seie,
  //       0.U(1.W), mtie & mideleg(7),  0.U(1.W), stie,
  //       0.U(1.W), msie & mideleg(3),  0.U(1.W), ssie, 0.U(1.W) )

  // val sie_dnxt = ( 0 until cm ).map{ t =>
  //   Cat(
  //     0.U(4.W), meie_dnxt(t) & mideleg_dnxt(t)(11), 0.U(1.W), seie_dnxt(t),
  //     0.U(1.W), mtie_dnxt(t) & mideleg_dnxt(t)(7),  0.U(1.W), stie_dnxt(t),
  //     0.U(1.W), msie_dnxt(t) & mideleg_dnxt(t)(3),  0.U(1.W), ssie_dnxt(t), 0.U(1.W) )    
  // }

  // ( 0 until cm ).map{ t => {
  //   val value = if ( t == 0 ) sie else sie_dnxt(t-1)
  //   val (enable0, dnxt0) = Reg_Exe_Port( value, "h104".U, exe_port(t) )
  //   val (enable1, dnxt1) = Reg_Exe_Port( value, "h304".U, exe_port(t) )

  //   when(enable0) {
  //     meie_dnxt(t) := dnxt0(11)
  //     seie_dnxt(t) := dnxt0(9)
  //     mtie_dnxt(t) := dnxt0(7)
  //     stie_dnxt(t) := dnxt0(5)
  //     msie_dnxt(t) := dnxt0(3)
  //     ssie_dnxt(t) := dnxt0(1)

  //   }
  //   .elsewhen(enable1) {
  //     seie_dnxt(t) := dnxt1(9)
  //     stie_dnxt(t) := dnxt1(5)
  //     ssie_dnxt(t) := dnxt1(1)
  //   }
  // }}


  /**
    * Supervisor Trap Vector Base Address Register --stvec
    *
    * @note holdstrap vector configuration
    * @param base (63,2) vector base address, either va or pa
    * @param mode (1,0) vector mode,hard-wire to 0 in this version
    */
  def update_stvec( in: CMMState_Bundle ): TVecBundle = {
    val stvec = WireDefault( in.csrfiles.stvec )

    stvec.mode := 0.U(2.W)

    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.stvec.asUInt, "h105".U, in.csrExe )
    when(enable) { stvec.base := dnxt(63,2) }

    return stvec
  }


  /**
    * Supervisor Timers and Performance Counters -- Counter-Enable Register -- scounteren
    * 
    * @note controls the availability of the hardware performance monitoring counters to U-mode
    * @param HPM (31,3) Whether is allowed to access hpmcounter(x) in U-mode
    * @param IR (2) Whether is allowed to access instret in U-mode
    * @param TM (1) Whether is allowed to access time in U-mode
    * @param CY (0) Whether is allowed to access cycle in U-mode
    */

  def update_scounteren( in: CMMState_Bundle ): CounterenBundle = {
    val scounteren = WireDefault( in.csrfiles.scounteren )
 
    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.scounteren.asUInt, "h306".U, in.csrExe )
      when(enable) { scounteren.hpm := dnxt }    


    return scounteren
  }

  //supervisor trap handling

  /**
    * Supervisor Scratch Register -- sscratch
    *
    * @note used to hold a pointer to the hart-local supervisor context while the hart is executing user code
    */
  def update_sscratch( in: CMMState_Bundle ): UInt = {
    val sscratch = WireDefault( in.csrfiles.sscratch )
    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.sscratch, "h140".U, in.csrExe )
    when(enable) { sscratch := dnxt }
    return sscratch
  }


  /**
    * Supervisor Exception Program Counter -- sepc
    * 
    * hold virtual addresses: when a trap is taken into S-mode, sepc is written with the virtual address of
    * the instruction that was interrupted or that encountered the exception
    */
  def update_sepc( in: CMMState_Bundle ): UInt = {
    val sepc = WireDefault( in.csrfiles.sepc )
    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.sepc, "h141".U, in.csrExe )

    when( (in.isInterrupt | in.isException) & update_priv_lvl(in) === "b01".U & ~in.csrfiles.DMode & ~in.isNomaskInterrupt ) { sepc := in.commit_pc }
    .elsewhen(enable) { sepc := dnxt }
    return sepc
  }

  /**
    * Supervisor Cause Register -- scause
    * 
    * when a trap is taken into S-mode, scause is written with a code indicating the event that cause the trap
    * @return
    */
  def update_scause( in: CMMState_Bundle ): CauseBundle = {
    val scause = WireDefault( in.csrfiles.scause )
    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.scause.asUInt, "h142".U, in.csrExe )

    when( ( in.csrfiles.is_m_interrupt | in.csrfiles.is_s_interrupt ) & update_priv_lvl(in) === "b01".U & ~in.csrfiles.DMode & ~in.isNomaskInterrupt ) {
      scause.interrupt := 1.U
      scause.exception_code := Mux1H( Seq(
        in.csrfiles.is_ssi -> 1.U,
        in.csrfiles.is_msi -> 3.U,
        in.csrfiles.is_sti -> 5.U,
        in.csrfiles.is_mti -> 7.U,
        in.csrfiles.is_sei -> 9.U,
        in.csrfiles.is_mei -> 11.U
      ))
    }
    .elsewhen( in.isException & update_priv_lvl(in) === "b01".U & ~in.csrfiles.DMode & ~in.isNomaskInterrupt ) {
      scause.interrupt := 0.U
      scause.exception_code := Mux1H( Seq(
        in.is_instr_misAlign        -> 0.U,
        in.is_instr_access_fault    -> 1.U,
        in.is_illeage               -> 2.U,
        in.is_ebreak_exc            -> 3.U,
        in.is_load_misAlign         -> 4.U,
        in.is_load_accessFault     -> 5.U,
        in.is_store_misAlign        -> 6.U,
        in.is_store_accessFault     -> 7.U,
        in.is_ecall_U               -> 8.U,
        in.is_ecall_S               -> 9.U,
        in.is_ecall_M               -> 11.U,
        in.is_instr_paging_fault    -> 12.U,
        in.is_load_pagingFault     -> 13.U,
        in.is_store_pagingFault    -> 15.U,
      ))
    }
    .elsewhen(enable) {
      scause.interrupt      := dnxt(63)
      scause.exception_code := dnxt(62,0)
    }
    return scause
  }

  /**
    * Supervisor Trap Value Register -- stval
    * 
    * when a trap is taken into S-mode, stval is written with exception-specific information to assist softwave in handling the trap
    *
    * @return
    */
  def update_stval( in: CMMState_Bundle ): UInt = {
    val stval = WireDefault( in.csrfiles.stval )
    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.stval, "h143".U, in.csrExe )

    when( (in.isInterrupt | in.isException) & update_priv_lvl(in) === "b01".U & ~in.csrfiles.DMode & ~in.isNomaskInterrupt ) {
      stval := Mux1H( Seq(
        in.is_instr_access_fault    -> in.ill_ivaddr,
        in.is_instr_paging_fault    -> in.ill_ivaddr,
        in.is_illeage         -> 0.U,
        in.is_ebreak_exc            -> 0.U,
        in.is_load_misAlign         -> in.ill_dvaddr,
        in.is_load_accessFault     -> in.ill_dvaddr,
        in.is_store_misAlign     -> in.ill_dvaddr,
        in.is_store_accessFault -> in.ill_dvaddr,
        in.is_load_pagingFault     -> in.ill_dvaddr,
        in.is_store_pagingFault -> in.ill_dvaddr       
      ))
    }
    .elsewhen(enable) { stval := dnxt }
    return stval
  }



/**
  * Supervisor Interrupt Register -- sip
  * @note read-only, meip(11), mtip(7), msip(3) is visible when mideleg(x) set
  * 
  */

  //   val meip = clint_ex_m
  //   val seip = clint_ex_s
  //   val mtip = clint_tm_m
  //   val stip = clint_tm_s
  //   val msip = clint_sw_m
  //   val ssip = clint_sw_s

  // val sip =
  //   Cat(
  //     0.U(4.W), meip & mideleg(11), 0.U(1.W), seip,
  //     0.U(1.W), mtip & mideleg(7),  0.U(1.W), stip,
  //     0.U(1.W), msip & mideleg(3),  0.U(1.W), ssip, 0.U(1.W) )

  // val sip_dnxt = ( 0 until cm ).map{ t => 
  //   Cat(
  //     0.U(4.W), meip & mideleg_dnxt(t)(11), 0.U(1.W), seip,
  //     0.U(1.W), mtip & mideleg_dnxt(t)(7),  0.U(1.W), stip,
  //     0.U(1.W), msip & mideleg_dnxt(t)(3),  0.U(1.W), ssip, 0.U(1.W) )
  // }


  /**
    * Supervisor Address protection and translation Register -- satp
    *
    * @param mode (63,60) select the current address-translation scheme  
    * @param asid (59,44) address space identifier, which facilitates address-translation fences on a per-address-space basis
    * @param PPN (43,0) physical page number (ppn) of the root page table
    */

  def update_satp( in: CMMState_Bundle ): SatpBundle = {
    val satp = WireDefault( in.csrfiles.satp )

    val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.satp.asUInt, "h180".U, in.csrExe )
    when(enable) {
      /** @note only sv39 supportted */
      satp.mode := dnxt(63,60) & ("b1000".U)(4.W)
      satp.asid := dnxt(59,44)
      satp.ppn  := dnxt(43,0)
    }

    return satp
  }


  //user trap setup
  // ustatus := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h000".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // uie := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h004".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // utvec := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h005".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  //user trap handling
  // uscratch := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h040".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // uepc := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h041".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // ucause := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h042".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // utval := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h043".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // uip := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h044".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // sedeleg := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h102".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // sideleg := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h103".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }



  // //hypervisor trap setup
  // hstatus := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h600".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // hedeleg := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h602".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // hideleg := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h603".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // hie := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h604".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // hcounteren := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h606".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // hgeie := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h607".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // //hypervisor trap handling
  // htval := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h643".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // hip := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h644".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // hvip := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h645".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // htinst := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h64A".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // hgeip := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "hE12".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // //hypervisor protection and translation
  // hgatp := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h680".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // //hypervisor counter timer virtualization registers
  // htimedelta := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h605".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // //virtual supervisor registers
  // vsstatus := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h200".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // vsie := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h204".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // vstvec := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h205".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // vsscratch := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h240".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // vsepc := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h241".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // vscause := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h242".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // vstval := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h243".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // vsip := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h244".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }

  // vsatp := {
  //   val value = RegInit(0.U(64.W))
  //   val (enable, dnxt) = Reg_Exe_Port( value, "h280".U, exe_port )
  //   when(enable) { value := dnxt }
  //   value 
  // }






  def update_vstart( in: CMMState_Bundle ): UInt = {
    val vstart = WireDefault( in.csrfiles.vstart )

    val (enable0, dnxt0) = Reg_Exe_Port( in.csrfiles.vstart, "h008".U, in.csrExe )
    // val (enable1, dnxt1) = Reg_Exe_Port( in.csrfiles.vstart, "hFFF".U, in.csrExe ) //vlsu exception

    when( enable0 ){ //csr write vstart
      vstart := dnxt0
    }.elsewhen( in.rod.isVector ){  //vector commit will auto reset vstart to 0
      vstart := 
        Mux1H(Seq(
          -> 0.U,
          -> vstart + vl
          -> vstart + exceptionIdx
        ))
    }

    return vstart
  }


  def update_vcsr( in: CMMState_Bundle ): VCSRBundle = {
    val vcsr = WireDefault( in.csrfiles.vcsr )

    val (enable0, dnxt0) = Reg_Exe_Port( in.csrfiles.vcsr.asUInt, "h00F".U, in.csrExe )
    val (enable1, dnxt1) = Reg_Exe_Port( in.csrfiles.vcsr.vxsat,  "h009".U, in.csrExe )
    val (enable2, dnxt2) = Reg_Exe_Port( in.csrfiles.vcsr.vxrm,   "h00A".U, in.csrExe )

    when( enable0 ){ //csr write vxsat, vxrm will redirect to vcsr in Module csr
      vcsr.vxsat := dnxt0(2,1)
      vcsr.vxrm := dnxt0(0)
    } .elsewhen( enable1 ){
      vcsr.vxsat := dnxt1
    } .elsewhen( enable2 ){
      vcsr.vxrm  := dnxt2
    }

    return vcsr
  }

  def update_vConfig( in: CMMState_Bundle ): VConfigBundle = {
    val vConfig = WireDefault( in.csrfiles.vConfig )

    val (enable0, dnxt0) = Reg_Exe_Port( in.csrfiles.vConfig.asUInt, "hFFE".U, in.csrExe ) //vset config from csr
    val (enable1, dnxt1) = Reg_Exe_Port( in.csrfiles.vConfig.asUInt, "hFFF".U, in.csrExe ) //vlsu FOF none exception

    when( enable0 ){     //vset mux to csr to write vl and vtype
      vConfig.vill  := dnxt0.extract(63)
      vConfig.vl    := dnxt0.apply(62,8)
      vConfig.vtype := dnxt0.apply(7,0)
    }.elsewhen(enable1){ //vlsu FOF none exception
      when( dnxt0(60) ){ //trigger fof
        vConfig.vl    := dnxt0.apply((log2Ceil(vParams.vlmax)-1), 0)        
      }

    }

    return vConfig
  }

  def update_vlenb( in: CMMState_Bundle ): UInt = {
    val vlenb = ((vParams.vlen)/8).U
    return vlenb
  }


  //Debug/Trace Register
  def update_tselect( in: CMMState_Bundle): UInt = {
    val tselect = WireDefault( in.csrfiles.tselect )

    if (hasDebugger) {
      val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.tselect, "h7A0".U, in.csrExe )
      when(enable) { tselect := Mux( dnxt >= 0.U, ~dnxt, dnxt ) }      
    }

    return tselect
  }

  def update_tdata1( in: CMMState_Bundle): UInt = {
    val tdata1 = WireDefault( in.csrfiles.tdata1 )

    if (hasDebugger) {
      val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.tdata1, "h7A1".U, in.csrExe )
      when(enable) { tdata1 := dnxt }
    }
    return tdata1
  }

  def update_tdata2( in: CMMState_Bundle): UInt = {
    val tdata2 = WireDefault( in.csrfiles.tdata2 )

    if (hasDebugger) {
      val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.tdata2, "h7A2".U, in.csrExe )
      when(enable) { tdata2 := dnxt }
    }
    return tdata2
  }

  def update_tdata3( in: CMMState_Bundle ): UInt = {
    val tdata3 = WireDefault( in.csrfiles.tdata3 )

    if (hasDebugger) {
      val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.tdata3, "h7A3".U, in.csrExe )
      when(enable) { tdata3 := dnxt }
    }
    return tdata3
  }

  def update_dcsr( in: CMMState_Bundle ): DcsrBundle = {
    val dcsr = WireDefault( in.csrfiles.dcsr )
    dcsr.xdebugver := 4.U(4.W)
    dcsr.reserved0 := 0.U
    dcsr.reserved1 := 0.U
    dcsr.stepie    := 0.U(1.W) 
    dcsr.stopcount := 0.U(1.W)
    dcsr.stoptime  := 0.U(1.W)
    dcsr.reserved2 := 0.U
    dcsr.mprven    := 0.U(1.W)

    if (hasDebugger) {
      val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.dcsr.asUInt, "h7B0".U, in.csrExe )
      when(false.B) {}
      .elsewhen( (in.csrfiles.DMode === false.B) & (update_DMode(in) === true.B) ){
        dcsr.prv   := in.csrfiles.priv_lvl
        dcsr.cause := MuxCase( 0.U, Seq(
          in.exint.is_trigger     -> 2.U,
          in.isEbreakDM           -> 1.U,
          in.exint.hartHaltReq    -> 3.U,
          in.exint.is_single_step -> 4.U,
        ))
      }
      .elsewhen(enable) {
        dcsr.ebreakm   := dnxt(15)
        dcsr.ebreaks   := dnxt(13)
        dcsr.ebreaku   := dnxt(12)
        dcsr.step := dnxt(2)
        dcsr.prv  := dnxt(1,0)
      }
    }

    return dcsr
  } 

  def update_dpc( in: CMMState_Bundle ): UInt = {
    val dpc  = WireDefault( in.csrfiles.dpc )

    if (hasDebugger) {
      val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.dpc, "h7B1".U, in.csrExe )
      when(enable) { dpc := dnxt }
      .elsewhen( (in.csrfiles.DMode === false.B) & (update_DMode(in) === true.B) ) {
        dpc := Mux1H(Seq(
          in.isEbreakDM            -> in.commit_pc,
          in.exint.is_single_step    -> in.commit_pc,
          in.exint.is_trigger        -> 0.U,
          in.exint.hartHaltReq       -> in.commit_pc,
        ))
      }
    }

    return dpc
  }

  def update_dscratch0( in: CMMState_Bundle ): UInt = {
    val dscratch0 = WireDefault( in.csrfiles.dscratch0 )

    if (hasDebugger) {
      val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.dscratch0, "h7B2".U, in.csrExe )
      when(enable) { dscratch0 := dnxt }
    }
    return dscratch0
  }

  def update_dscratch1( in: CMMState_Bundle ): UInt = {
    val dscratch1 = WireDefault( in.csrfiles.dscratch1 )
    if (hasDebugger) {
      val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.dscratch1, "h7B3".U, in.csrExe )
      when(enable) { dscratch1 := dnxt }
    }
    return dscratch1
  }

  def update_dscratch2( in: CMMState_Bundle ): UInt = {
    val dscratch2 = WireDefault( in.csrfiles.dscratch2 )
    if (hasDebugger) {
      val (enable, dnxt) = Reg_Exe_Port( in.csrfiles.dscratch2, "h7B4".U, in.csrExe )
      when(enable) { dscratch2 := dnxt }
    }
    return dscratch2
  }

  def update_DMode( in: CMMState_Bundle ): Bool = {
    val DMode = WireDefault( in.csrfiles.DMode )
    if (hasDebugger) {
      when( in.isDebugInterrupt ) { DMode := true.B }
      when( in.isEbreakDM )       { DMode := true.B }
      .elsewhen( in.is_dRet )     { DMode := false.B }
    }
    return DMode
  }
  


  def update_csrfiles( in: CMMState_Bundle ): CSR_Bundle = {
    val csrfiles = Wire( new CSR_Bundle )
    csrfiles.priv_lvl      := update_priv_lvl(in)
    csrfiles.DMode         := update_DMode(in)
    csrfiles.fcsr          := update_fcsr(in)
    csrfiles.stvec         := update_stvec(in)
    csrfiles.scounteren    := update_scounteren(in)
    csrfiles.sscratch      := update_sscratch(in)
    csrfiles.sepc          := update_sepc(in)
    csrfiles.scause        := update_scause(in)
    csrfiles.stval         := update_stval(in)
    // csrfiles.sip           := update_sip(in)
    csrfiles.satp          := update_satp(in)
    csrfiles.mvendorid     := update_mvendorid(in)
    csrfiles.marchid       := update_marchid(in)
    csrfiles.mimpid        := update_mimpid(in)
    csrfiles.mhartid       := update_mhartid(in)
    csrfiles.mstatus       := update_mstatus(in)
    csrfiles.misa          := update_misa(in)
    csrfiles.medeleg       := update_medeleg(in)
    csrfiles.mideleg       := update_mideleg(in)
    csrfiles.mie           := update_mie(in)
    csrfiles.mtvec         := update_mtvec(in)
    csrfiles.mcounteren    := update_mcounteren(in)
    csrfiles.mscratch      := update_mscratch(in)
    csrfiles.mepc          := update_mepc(in)
    csrfiles.mcause        := update_mcause(in)
    csrfiles.mtval         := update_mtval(in)
    csrfiles.mip           := update_mip(in)
    csrfiles.mtinst        := update_mtinst(in)
    csrfiles.mtval2        := update_mtval2(in)
    csrfiles.mcycle        := update_mcycle(in)
    csrfiles.minstret      := update_minstret(in)
    csrfiles.mcountinhibit := update_mcountinhibit(in)
    csrfiles.tselect       := update_tselect (in)
    csrfiles.tdata1        := update_tdata1 (in)
    csrfiles.tdata2        := update_tdata2 (in)
    csrfiles.tdata3        := update_tdata3(in)
    csrfiles.dcsr          := update_dcsr (in)
    csrfiles.dpc           := update_dpc(in)
    csrfiles.dscratch0     := update_dscratch0 (in)
    csrfiles.dscratch1     := update_dscratch1 (in)
    csrfiles.dscratch2     := update_dscratch2 (in)
    csrfiles.pmpcfg        := update_pmpcfg(in)
    csrfiles.pmpaddr       := update_pmpaddr(in)
    csrfiles.mhpmcounter   := update_mhpmcounter(in)
    csrfiles.mhpmevent     := update_mhpmevent(in)

    csrfiles.time := DontCare

    if(hasVector){
      csrfiles.vstart  := update_vstart(in)
      csrfiles.vcsr    := update_vcsr(in)
      csrfiles.vConfig := update_vConfig(in)
      csrfiles.vlenb   := update_vlenb(in)      
    } else{
      csrfiles.vstart  := DontCare
      csrfiles.vcsr    := DontCare
      csrfiles.vConfig := DontCare
      csrfiles.vlenb   := DontCare
    }

    csrfiles.sstatus    := (csrfiles.mstatus.asUInt & Cat( "b1".U, 0.U(29.W), "b11".U, 0.U(12.W), "b11011110000101100010".U )).asTypeOf(new MStatusBundle)
    csrfiles.sie        := csrfiles.mie
    csrfiles.sip        := csrfiles.mip
    csrfiles.cycle      := csrfiles.mcycle
    csrfiles.instret    := csrfiles.minstret
    csrfiles.hpmcounter := csrfiles.mhpmcounter

    csrfiles.fflags     := csrfiles.fcsr.fflags
    csrfiles.frm        := csrfiles.fcsr.frm

    return csrfiles
  }
}


