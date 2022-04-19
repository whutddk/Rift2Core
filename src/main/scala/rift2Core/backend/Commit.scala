
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


package rift2Core.backend

import chisel3._
import chisel3.util._

import rift2Core.define._
import rift2Core.frontend._
import rift2Core.L1Cache._
import rift2Core.backend._
import rift2Core.privilege._
import rift2Core.privilege.csrFiles._
import debug._
import chisel3.experimental._

import rift2Core.diff._

class ExInt_Bundle extends Bundle {
  val is_single_step = Bool()
  val is_trigger = Bool()

  val emu_reset = Bool()
	val clint_sw_m = Bool()
	val clint_sw_s = Bool()
	val clint_tm_m = Bool()
	val clint_tm_s = Bool()
	val clint_ex_m = Bool()
	val clint_ex_s = Bool()  
}


@chiselName
class CMMState_Bundle extends Bundle{
  val rod = new Info_reorder_i
  val csrfiles = new CSR_Bundle
  val lsu_cmm = new Info_lsu_cmm

  val csrExe = new Exe_Port
  val fcsrExe = new Exe_Port
  val is_wb = Bool()
  val ill_ivaddr = UInt(64.W)
	val ill_dvaddr = UInt(64.W)
  val rtc_clock = Bool()
  val exint = new ExInt_Bundle

  def is_load_accessFault: Bool = {
    val is_load_accessFault = lsu_cmm.is_access_fault & rod.is_lu & ~is_wb
    return is_load_accessFault
  }

  def is_store_accessFault: Bool = {
    val is_store_accessFault = lsu_cmm.is_access_fault & ( rod.is_su | rod.is_amo ) & ~is_wb
    return is_store_accessFault
  }

  def is_load_pagingFault: Bool = {
    val is_load_pagingFault = lsu_cmm.is_paging_fault & rod.is_lu & ~is_wb
    return is_load_pagingFault
  }

  def is_store_pagingFault: Bool = {
    val is_store_pagingFault = lsu_cmm.is_paging_fault & ( rod.is_su | rod.is_amo ) & ~is_wb
    return is_store_pagingFault
  }

  def is_load_misAlign: Bool = {
    val is_load_misAlign = lsu_cmm.is_misAlign & rod.is_lu & ~is_wb
    return is_load_misAlign
  }

  def is_store_misAlign: Bool = {
    val is_store_misAlign = lsu_cmm.is_misAlign & (rod.is_su | rod.is_amo) & ~is_wb
    return is_store_misAlign
  }

  def is_ecall: Bool = {
    val is_ecall = rod.privil.ecall
    return is_ecall
  }

  def is_ebreak_exc: Bool = {
    val is_ebreak_exc = rod.privil.ebreak & ~is_ebreak_breakpointn
    return is_ebreak_exc
  }

  def is_instr_access_fault: Bool = {
    val is_instr_access_fault = rod.privil.is_access_fault
    return is_instr_access_fault
  }

  def is_instr_paging_fault: Bool = {
    val is_instr_paging_fault = rod.privil.is_paging_fault
    return is_instr_paging_fault
  }

  def is_csrw_illegal: Bool = {
    val is_csrw_illegal = ( csrExe.op_rc | csrExe.op_rs | csrExe.op_rw ) & ( csrfiles.csr_write_denied(csrFiles.DMode) | csrfiles.csr_read_prilvl(csrFiles.priv_lvl, csrFiles.DMode) )
    return is_csrw_illegal
  }

  def is_csrr_illegal: Bool = {
    val is_csrr_illegal = csrfiles.csr_read_prilvl(csrFiles.priv_lvl, csrFiles.DMode)
    return is_csrr_illegal
  }

  def is_fcsrw_illegal: Bool = {
    val is_fcsrw_illegal = ( fcsrExe.op_rc | fcsrExe.op_rs | fcsrExe.op_rw ) & (csrFiles.mstatus.fs === 0.U)
    return is_fcsrw_illegal
  }

      

  def is_illeage: Bool = {
    val is_csr_illegal = 
      (is_csrr_illegal  & rod.is_csr & ~is_wb) |
      (is_csrw_illegal  & rod.is_csr &  is_wb) |
      (is_fcsrw_illegal & rod.is_fpu &  is_wb)

    val is_ill_sfence = is_wb & rod.is_sfence_vma & ( (csrFiles.mstatus.tvm & csrFiles.priv_lvl === "b01".U) | csrFiles.priv_lvl === "b00".U)
    val is_ill_wfi_v  = is_wb & rod.is_wfi        & (  csrFiles.mstatus.tw & csrFiles.priv_lvl < "b11".U )

    val is_ill_mRet_v = rod.privil.mret & csrFiles.priv_lvl =/= "b11".U
    val is_ill_sRet_v = rod.privil.sret & ( csrFiles.priv_lvl === "b00".U | ( csrFiles.priv_lvl === "b01".U & csrFiles.mstatus.tsr) )
    val is_ill_dRet_v = rod.privil.dret & ~csrFiles.DMode
    val is_ill_fpus_v = (is_wb & (rod.is_fcmm | rod.is_fpu) & csrFiles.mstatus.fs === 0.U)

    val is_illeage = rod.is_illeage | is_csr_illegal | is_ill_sfence | is_ill_wfi | is_ill_mRet | is_ill_sRet | is_ill_dRet | is_ill_fpus
    return is_illeage
  }
     
  def is_mRet: Bool = {
    val is_mRet = rod.privil.mret & csrFiles.priv_lvl === "b11".U
    return is_mRet
  }

  def is_sRet: Bool = {
    val is_sRet = rod.privil.sret & ( csrFiles.priv_lvl === "b11".U | ( csrFiles.priv_lvl === "b01".U & ~csrFiles.mstatus.tsr) )
    return is_sRet
  }

  def is_dRet: Bool = {
    val is_dRet = rod.privil.dret & csrFiles.DMode
    return is_dRet
  }

  def is_fence_i: Bool = {
    val is_fence_i = rod.is_fence_i & is_wb
    return is_fence_i
  }

  def is_sfence_vma: Bool = {
    val is_sfence_vma = rod.is_sfence_vma & is_wb & ( (~csrFiles.mstatus.tvm & csrFiles.priv_lvl === "b01".U) | csrFiles.priv_lvl === "b11".U)
    return is_sfence_vma
  }


  def is_exception: Bool = {
    val is_exception = 
      is_ecall                 |
      is_ebreak_exc            |
      is_instr_access_fault    |
      is_instr_paging_fault    |
      is_illeage               |
      is_load_accessFault      |
      is_store_accessFault     |
      is_load_misAlign         |
      is_store_misAlign        |
      is_load_pagingFault      |
      is_store_pagingFault
    return is_exception
  }

  def is_interrupt: Bool = {
    val is_interrupt = ((csrfiles.is_m_interrupt | csrfiles.is_s_interrupt) & ~is_step_int_block) | is_nomask_interrupt
    return is_interrupt
  }

	def is_trap: Bool = {
    val is_trap = is_interrupt | is_exception
    return is_trap
  } 

  def is_xRet: Bool = {
    val is_xRet = is_mRet | is_sRet | is_dRet
    return is_xRet
  }

  def is_fpu_state_change: Bool = {
    val is_fpu_state_change = ~is_trap & (rod.is_fcmm | rod.is_fpu)
    return is_fpu_state_change
  }


  def is_ebreak_breakpointn: Bool = {
    val is_ebreak_breakpointn = 
      Mux1H(Seq(
        ( csrfiles.priv_lvl === "b11".U) -> csrfiles.dcsr(15),
        ( csrfiles.priv_lvl === "b01".U) -> csrfiles.dcsr(13),
        ( csrfiles.priv_lvl === "b00".U) -> csrfiles.dcsr(12),
      ))
    return is_ebreak_breakpointn
  }

  def is_step_int_block: Bool = {
    val is_step_int_block = ~csrfiles.dcsr.stepie & csrfiles.DMode
    return is_step_int_block
  }

  def is_step: Bool = {
    val is_step = csrfiles.dcsr.step & DMode
    return is_step
  }

  def commit_pc: UInt = {
    val commit_pc = rod.pc
    return commit_pc
  } 

  def is_ebreak_dm: Bool = {
    val is_ebreak_dm = rod.privil.ebreak & is_ebreak_breakpointn
    return is_ebreak_dm
  }

  def is_debug_interrupt: Bool = {
    val is_debug_interrupt = ~csrFiles.DMode & (
      exint.is_single_step |
      exint.is_trigger |
      exint.hartHaltReq |
      is_ebreak_dm      
    )    
    return is_debug_interrupt
  }

  def is_nomask_interrupt: Bool = {
    val is_nomask_interrupt = is_debug_interrupt | exint.emu_reset
    return is_nomask_interrupt
  }


  


}





abstract class BaseCommit extends RawModule




/** commit
  * @note for every commit-chn, it can be:
  * comfirm: commit at this tick
  * abort: cancel and flush at this tick
  * cancel: the perivious chn abort
  * idle: empty line or waitting to check whether is comfirm or abort
  *
  * @note new feature
  * 1. abort can only emmit at chn0 -> abort can emmit at any chn
  */
class Commit(cm: Int=2) extends CsrFiles with BaseCommit {
  val io = IO(new Bundle{
    val cm_op = Vec(cm, new Info_commit_op(64))
    val rod = Vec(cm, Flipped(new DecoupledIO( new Info_reorder_i ) ))

    val cmm_lsu = Output(new Info_cmm_lsu)
    val lsu_cmm = Input( new Info_lsu_cmm )

    val cmm_bru_ilp = Output(Bool())

    val csr_addr = Flipped(ValidIO(UInt(12.W)))
    val csr_data = ValidIO(UInt(64.W))
    val csr_cmm_op = Flipped(DecoupledIO( new Exe_Port ) )

    val is_misPredict = Input(Bool())
    val is_commit_abort = Vec(cm, Output( Bool() ))

    val cmm_pc = new ValidIO(new Info_cmm_pc)
    val if_cmm = Input( new Info_if_cmm )

    val ifence = Output(Bool())

    val cmm_mmu = Output( new Info_cmm_mmu )


    val fcsr = Output(UInt(24.W))
    val fcsr_cmm_op = Vec(cm, Flipped(DecoupledIO( new Exe_Port ) ))

    val dm = Flipped(new Info_DM_cmm)

    val rtc_clock = Input(Bool())

    val diff_commit = Output(new Info_cmm_diff)
    val diff_csr = Output(new Info_csr_reg)
  })


  val commit_state = Wire( Vec( cm, UInt(2.W)) )
  val commit_state_is_comfirm = for ( i <- 0 until cm ) yield commit_state(i) === 3.U
  val commit_state_is_abort   = for ( i <- 0 until cm ) yield commit_state(i) === 2.U
  val commit_state_is_cancel  = for ( i <- 0 until cm ) yield commit_state(i) === 1.U
  val commit_state_is_idle    = for ( i <- 0 until cm ) yield commit_state(i) === 0.U

  val is_retired = ( 0 until cm ).map{ i => {commit_state_is_comfirm(i) | commit_state_is_abort(i)} }

  ( 1 until cm ).map{ i =>  assert( ~(is_retired(i) & ~is_retired(i-1)) ) }


  val csrfiles = Reg(new CSR_Bundle)
  for( i <- 0 until cm ) yield { when( is_retired(i) { csrfiles := csr_state(i) }  ) }
  when( reset.asBool ) { resetToDefault(csrfiles) }

  val emptyExePort = {
    val res = Wire(Decoupled( new Exe_Port ))
    res.valid := false.B
    res.bits  := 0.U.asTypeOf(new Exe_Port)
    res.ready := false.B
    res
  }
  val csrExe  = ReDirect( VecInit(Seq(io.csr_cmm_op) ++ Seq.fill(cm-1)(emptyExePort), VecInit( io.rod.map{_.bits.is_csr} ) ))
  val fcsrExe = ReDirect( io.fcsr_cmm_op, VecInit( io.rod.map{_.bits.is_fcsr} ) )

  val is_single_step = RegNext(next = is_retired(0) & cmm_state(0).is_step, init = false.B)
  val is_trigger = false.B

  val emu_reset = RegInit( false.B )
  when( io.dm.hartResetReq ) { emu_reset := true.B }
  .elsewhen( emu_reset ) { emu_reset := false.B }
  io.dm.hartIsInReset := emu_reset




  val cmm_state = Wire( cm, new CMMState_Bundle )
  val csr_state = Wire( cm, new CSR_Bundle )
  ( 0 until cm ).map{ i => {
    cmm_state(i).rod      := io.rod(i).bits
    if ( i == 0 ) { cmm_state(i).csrFiles := csrfiles } else { cmm_state(i).csrFiles := csr_state(i-1) }
    
    cmm_state(i).lsu_cmm := io.lsu_cmm
    cmm_state(i).csrExe  := csrExe(i).bits
    cmm_state(i).fcsrExe := fcsrExe(i).bits
    cmm_state(i).is_wb   := io.cm_op(i).is_writeback
    cmm_state(i).ill_ivaddr               := io.if_cmm.ill_vaddr
    cmm_state(i).ill_dvaddr               := io.lsu_cmm.trap_addr
    cmm_state(i).rtc_clock               := io.rtc_clock

    exint.is_single_step := is_single_step
    exint.is_trigger := false.B
    exint.emu_reset  := emu_reset
	  exint.clint_sw_m := false.B
	  exint.clint_sw_s := false.B
	  exint.clint_tm_m := false.B
	  exint.clint_tm_s := false.B
	  exint.clint_ex_m := false.B
	  exint.clint_ex_s := false.B 

    csr_state(i) := update_csrfiles(in = cmm_state(i))


  }}

  val abort_chn = Wire(UInt(log2Ceil(cm).W)) abort_chn := DontCare
  ( 1 until cm ).map{ i => assert( commit_state(i) <= commit_state(i-1) ) }
  
  for ( i <- 0 until cm ) yield {
    when( ~io.rod(i).valid ) {
      commit_state := 0.U //IDLE
    }
    
    .elsewhen( {if( i == 0 ) { false.B } else {commit_state_is_abort(i-1)}} ) {
      commit_state === 1.U //abort override following as cancel
    }
    .otherwise {
      when(
          (io.rod(i).bits.is_branch & io.is_misPredict ) | //1) it is the 1st branch, 2) a branch in-front and mis-predict, this will cancel 
          is_xRet | is_trap | is_fence_i | is_sfence_vma
        ) {
        commit_state := 2.U //abort
        abort_chn := i.U
      } .elsewhen( is_wb & ~is_step ) { //when writeback and no-step
        commit_state := 3.U
      } .otherwise {
        commit_state := 0.U //idle
      }
    }    
  }
















  for ( i <- 0 until cm ) yield {
    io.cm_op(i).phy := io.rod(i).rd0_phy
    io.cm_op(i).raw := io.rod(i).rd0_raw
    io.cm_op(i).toX := io.rod(i).is_xcmm
    io.cm_op(i).toF := io.rod(i).is_fcmm 
  }

  //bru commit ilp
  io.cmm_bru_ilp :=
    (io.rod(0).valid) & io.rod(0).bits.is_branch & (~io.cm_op(0).is_writeback)
  printf("Warning, preformance enhance require: 1. branch pending only resolved in chn0 -> only one branch pending can be resolved, in any chn.")

  io.cmm_lsu.is_amo_pending := {
    io.rod(0).valid & io.rod(0).bits.is_amo & ~io.cm_op(0).is_writeback //only pending amo in rod0 is send out
  }
  printf("Warning, amo_pending can only emmit at chn0")
  


    


  ( 0 until cm ).map{ i =>
    io.cm_op(i).is_comfirm := commit_state_is_comfirm(i)
    io.cm_op(i).is_abort   := commit_state_is_abort(i)
  }


  ( 0 until cm ).map{ i =>
    io.rod(i).ready := is_reitred(i)
  }


  ( 0 until cm ).map{ i =>
    io.cmm_lsu.is_store_commit(i) := io.rod(i).bits.is_su & commit_state_is_comfirm(i)
  }


  io.cmm_pc.valid := false.B
  io.cmm_pc.bits.addr := 0.U
  for ( i <- 0 until cm ) yield {
    when( commit_state_is_abort(i) ) {
      when( cmm_state(i).is_mRet ) {
        io.cmm_pc.valid := true.B
        io.cmm_pc.bits.addr := cmm_state(i).csrfiles.mepc
      } 
      when( cmm_state(i).is_sRet ) {
        io.cmm_pc.valid := true.B
        io.cmm_pc.bits.addr := cmm_state(i).csrfiles.sepc
      } 
      when( cmm_state(i).is_dRet ) {
        io.cmm_pc.valid := true.B
        io.cmm_pc.bits.addr := cmm_state(i).csrfiles.dpc
      } 
      when( cmm_state(i).is_trap ) {
        io.cmm_pc.valid := true.B
        io.cmm_pc.bits.addr := Mux1H(Seq(
            emu_reset                                   -> "h80000000".U,
            cmm_state(i).is_debug_interrupt             -> "h00000000".U,
            (update_priv_lvl(cmm_state(i)) === "b11".U) -> cmm_state(i).csrfiles.mtvec,
            (update_priv_lvl(cmm_state(i)) === "b01".U) -> cmm_state(i).csrfiles.stvec
          )),
      } 
      when( cmm_state(i).is_fence_i | cmm_state(i).is_sfence_vma ) {
        io.cmm_pc.valid := true.B
        io.cmm_pc.bits.addr := (io.rod(i).bits.pc + 4.U)
      }
    }

    assert( PopCount(Seq( cmm_state(i).is_xRet, cmm_state(i).is_trap, cmm_state(i).is_fence_i, cmm_state(i).is_sfence_vma)) <= 1.U )
  }









  io.csr_data.bits := csrfiles.csr_read_res(io.csr_addr.bits)
  io.csr_data.valid := io.csr_addr.valid & ~csrfiles.csr_read_prilvl(io.csr_addr.bits)
  
  
  io.csr_cmm_op.ready := ( 0 until cm ).map{ i =>
    commit_state_is_comfirm(i) & io.rod(i).bits.is_csr
  }.reduce(_|_)

  assert( ~(io.csr_cmm_op.ready & ~io.csr_cmm_op.valid) )
  printf("Warning, csr can only execute one by one")
    // io.csr_cmm_op.valid & (
    //   (is_commit_comfirm(0) & io.rod_i(0).bits.is_csr) | 
    //   (is_commit_comfirm(1) & io.rod_i(1).bits.is_csr)		
    // )


  ( 0 until cm ).map{ i => {
    io.fcsr_cmm_op(i).ready :=
      commit_state_is_comfirm(i) & io.rod_i(i).bits.is_fpu

    assert( ~(io.fcsr_cmm_op(i).ready & ~io.fcsr_cmm_op(i).valid) )
  }}
  printf("Warning, fcsr can only execute one by one")

  /** @note fcsr-read will request after cmm_op_fifo clear, frm will never change until fcsr-write */
  io.fcsr := csrfiles.fcsr 







  io.cmm_mmu.satp := csrfiles.satp
  for ( i <- 0 until 16 ) yield io.cmm_mmu.pmpcfg(i) := csrfiles.pmpcfg(i)
  for ( i <- 0 until 64 ) yield io.cmm_mmu.pmpaddr(i) := csrfiles.pmpaddr(i)
  io.cmm_mmu.priv_lvl_if   := csrfiles.priv_lvl
  io.cmm_mmu.priv_lvl_ls   := Mux( csrfiles.mstatus.mprv, csrfiles.mstatus.mpp, csrfiles.priv_lvl )
  io.cmm_mmu.mstatus    := csrfiles.mstatus
  io.cmm_mmu.sstatus    := csrfiles.sstatus
  io.cmm_mmu.sfence_vma := ( 0 until cm ).map{ i => 
    commit_state_is_abort(i) & cmm_state(i).is_sfence_vma 
  }.reduce(_|_)
    // ( io.rod_i(0).valid & is_sfence_vma_v(0))  


  io.ifence := ( 0 until cm ).map{ i => 
    commit_state_is_abort(i) & cmm_state(i).is_fence_i
  }.reduce(_|_)
    // ( io.rod_i(0).valid & is_fence_i_v(0))


  ( 0 until cm ).map{ i => {
    io.is_commit_abort(i) := commit_state_is_abort(i)
  }}
  








  

  def resetToDefault(csrfiles: CSR_Bundle) = {

    csrfiles.priv_lvl      := "b11".U
    csrfiles.DMode         := false.B
    csrfiles.fcsr          := 0.U.asTypeOf(new FCSRBundle)
    csrfiles.cycle         := 0.U
    csrfiles.time          := 0.U
    csrfiles.instret       := 0.U
    csrfiles.stvec         := 0.U.asTypeOf(new TVecBundle)
    csrfiles.scounteren    := 0.U.asTypeOf(new CounterenBundle)
    csrfiles.sscratch      := 0.U.asTypeOf(UInt(64.W))
    csrfiles.sepc          := 0.U.asTypeOf(UInt(64.W))
    csrfiles.scause        := 0.U.asTypeOf(new CauseBundle)
    csrfiles.stval         := 0.U.asTypeOf(UInt(64.W))
    csrfiles.sip           := 0.U.asTypeOf(new MSIntBundle)
    csrfiles.satp          := 0.U.asTypeOf(new SatpBundle)
    csrfiles.mvendorid     := 0.U
    csrfiles.marchid       := 0.U
    csrfiles.mimpid        := 0.U
    csrfiles.mhartid       := 0.U

    csrfiles.mstatus.mbe  := 0.U(1.W)
    csrfiles.mstatus.sbe  := 0.U(1.W)
    csrfiles.mstatus.sxl  := 2.U(2.W)
    csrfiles.mstatus.uxl  := 2.U(2.W)
    csrfiles.mstatus.tsr  := 0.U(1.W)
    csrfiles.mstatus.tw   := 0.U(1.W)
    csrfiles.mstatus.tvm  := 0.U(1.W)
    csrfiles.mstatus.mxr  := 0.U(1.W)
    csrfiles.mstatus.sum  := 0.U(1.W)
    csrfiles.mstatus.mprv := 0.U(1.W)
    csrfiles.mstatus.xs   := 0.U(2.W)
    csrfiles.mstatus.fs   := 0.U(2.W)
    csrfiles.mstatus.mpp  := "b11".U(2.W)
    csrfiles.mstatus.spp  := 0.U(1.W)
    csrfiles.mstatus.mpie := 0.U(1.W)
    csrfiles.mstatus.ube  := 0.U(1.W)
    csrfiles.mstatus.spie := 0.U(1.W)
    csrfiles.mstatus.mie  := 0.U(1.W)
    csrfiles.mstatus.sie  := 0.U(1.W)
    csrfiles.mstatus.sd   := 0.U(1.W)

    csrfiles.misa          := Cat(2.U(2.W), 0.U(36.W), "b00000101000001000100101101".U(26.W))
    csrfiles.medeleg       := 0.U
    csrfiles.mideleg       := 0.U
    csrfiles.mie           := 0.U.asTypeOf(new MSIntBundle)
    csrfiles.mtvec         := 0.U.asTypeOf(new TVecBundle)
    csrfiles.mcounteren    := 0.U.asTypeOf(new CounterenBundle)
    csrfiles.mscratch      := 0.U
    csrfiles.mepc          := 0.U
    csrfiles.mcause        := 0.U.asTypeOf(new CauseBundle)
    csrfiles.mtval         := 0.U
    csrfiles.mip           := 0.U.asTypeOf(new MSIntBundle)
    csrfiles.mtinst        := 0.U
    csrfiles.mtval2        := 0.U
    csrfiles.mcycle        := 0.U
    csrfiles.minstret      := 0.U
    csrfiles.mcountinhibit := 0.U
    csrfiles.tselect       := 0.U
    csrfiles.tdata1        := 0.U
    csrfiles.tdata2        := 0.U
    csrfiles.tdata3        := 0.U
              
    csrfiles.dcsr.xdebugver := 4.U(4.W)
    csrfiles.dcsr.ebreakm   := 1.U(1.W)
    csrfiles.dcsr.ebreaks   := 1.U(1.W)
    csrfiles.dcsr.ebreaku   := 1.U(1.W)
    csrfiles.dcsr.stepie    := 0.U(1.W)
    csrfiles.dcsr.stopcount := 0.U(1.W)
    csrfiles.dcsr.stoptime  := 0.U(1.W)
    csrfiles.dcsr.cause     := 0.U(3.W)
    csrfiles.dcsr.mprven    := 0.U(1.W)
    csrfiles.dcsr.nmip      := 0.U(1.W)
    csrfiles.dcsr.step      := 0.U(1.W)
    csrfiles.dcsr.prv       := 3.U(2.W)

    csrfiles.dpc           := 0.U.asTypeOf(new DcsrBundle)
    csrfiles.dscratch0     := 0.U
    csrfiles.dscratch1     := 0.U
    csrfiles.dscratch2     := 0.U
    csrfiles.pmpcfg        := VecInit( Seq.fill(16)(VecInit( Seq.fill(8)(0.U.asTypeOf( new PmpcfgBundle) ))))
    csrfiles.pmpaddr       := VecInit( Seq.fill(64)(0.U(64.W)) )
    csrfiles.hpmcounter    := VecInit( Seq.fill(32)(0.U(64.W)) )
    csrfiles.mhpmcounter   := VecInit( Seq.fill(32)(0.U(64.W)) )
    csrfiles.mhpmevent     := VecInit( Seq.fill(32)(0.U(64.W)) )    
  }













  io.diff_commit.pc(0) := io.rod_i(0).bits.pc
  io.diff_commit.pc(1) := io.rod_i(1).bits.pc
  io.diff_commit.comfirm(0) := is_commit_comfirm(0)
  io.diff_commit.comfirm(1) := is_commit_comfirm(1)
  io.diff_commit.abort(0) := io.is_commit_abort(0)
  io.diff_commit.abort(1) := io.is_commit_abort(1)
  io.diff_commit.priv_lvl := priv_lvl_qout
  io.diff_commit.is_ecall_M := is_ecall_M
  io.diff_commit.is_ecall_S := is_ecall_S
  io.diff_commit.is_ecall_U := is_ecall_U


	io.diff_csr.mstatus   := mstatus
	io.diff_csr.mtvec     := mtvec
	io.diff_csr.mscratch  := mscratch
	io.diff_csr.mepc      := mepc
	io.diff_csr.mcause    := mcause
	io.diff_csr.mtval     := mtval
  io.diff_csr.mvendorid := mvendorid
  io.diff_csr.marchid   := marchid
  io.diff_csr.mimpid    := mimpid
  io.diff_csr.mhartid   := mhartid
  io.diff_csr.misa      := misa
  io.diff_csr.mie       := mie
  io.diff_csr.mip       := mip
  io.diff_csr.medeleg   := medeleg
  io.diff_csr.mideleg   := mideleg
  // io.diff_csr.mcounteren           = mcounteren
  // io.diff_csr.mcountinhibit        = mcountinhibit
  // io.diff_csr.tselect              = tselect
  // io.diff_csr.tdata1[MAX_TRIGGERS] = tdata1
  // io.diff_csr.tdata2[MAX_TRIGGERS] = tdata2
  // io.diff_csr.tdata3[MAX_TRIGGERS] = tdata3
  // io.diff_csr.mhpmevent[32]        = mhpmevent
  for ( i <- 0 until 4 ) yield {
    io.diff_csr.pmpcfg(i) := pmpcfg(i)
  }
  for ( i <- 0 until 16 ) yield {
	  io.diff_csr.pmpaddr(i)  := pmpaddr(i)  
  }

  io.diff_csr.stvec    := stvec
  io.diff_csr.sscratch := sscratch
  io.diff_csr.sepc     := sepc
  io.diff_csr.scause   := scause
  io.diff_csr.stval    := stval
  io.diff_csr.satp     := satp
  // io.diff_csr.scounteren := scounteren
  // io.diff_csr.dcsr       := dcsr
  // io.diff_csr.dpc        := dpc
  // io.diff_csr.dscratch   := dscratch
  io.diff_csr.fflags  := fcsr(4,0)
  io.diff_csr.frm     := fcsr(7,5)



}
