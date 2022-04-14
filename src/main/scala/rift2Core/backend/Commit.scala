
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

import rift2Core.diff._

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
class Commit(cm: Int=2) extends Privilege with Superscalar {
  val io = IO(new Bundle{
    val cm_op = Vec(cm, new Info_commit_op(64))
    val rod_i = Vec(cm, Flipped(new DecoupledIO( new Info_reorder_i ) ))

    val cmm_lsu = Output(new Info_cmm_lsu)
    val lsu_cmm = Input( new Info_lsu_cmm )

    val cmm_bru_ilp = Output(Bool())

    val csr_addr = Flipped(ValidIO(UInt(12.W)))
    val csr_data = ValidIO(UInt(64.W))
    val csr_cmm_op = Flipped(DecoupledIO( new Exe_Port ) )

    val is_misPredict = Input(Bool())
    val is_commit_abort = Vec(2, Output( Bool() ))

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




  val commit_state = Wire(Vec(cm, UInt(2.W)))
  val commit_state_is_comfirm = ( 0 until cm ).map{ commit_state(i) === 3.U }
  val commit_state_is_abort   = ( 0 until cm ).map{ commit_state(i) === 2.U }
  val commit_state_is_cancel = ( 0 until cm ).map{ commit_state(i) === 1.U }
  val commit_state_is_idle    = ( 0 until cm ).map{ commit_state(i) === 0.U }
  val abort_chn = Wire(UInt(log2Ceil(cm).W))

  ( 1 until cm ).map{ i => assert( commit_state(i) <= commit_state(i-1) ) }


  val rd0_raw = io.rod_i.map{ _.bits.rd0_raw }
  val rd0_phy = io.rod_i.map{ _.bits.rd0_phy }
  val is_xcmm = io.rod_i.map{ _.bits.is_xcmm }
  val is_fcmm = io.rod_i.map{ _.bits.is_fcmm }

  for ( i <- 0 until cm ) yield {
    io.cm_op(i).phy := rd0_phy(i)
    io.cm_op(i).raw := rd0_raw(i)
    io.cm_op(i).toX := is_xcmm(i)
    io.cm_op(i).toF := is_fcmm(i) 
  }

  val is_wb_v = io.cm_op.map{ _ => _.is_writeback }
  


  //bru commit ilp
  io.cmm_bru_ilp := (io.rod_i(0).valid) & io.rod_i(0).bits.is_branch & (~io.cm_op(0).ready)
  printf("Warning, preformance enhance require: 1. branch pending only resolved in chn0 -> only one branch pending can be resolved, in any chn.")

  // val is_1st_solo = io.is_commit_abort(0) | io.rod_i(0).bits.is_csr | io.rod_i(0).bits.is_su | ~is_wb_v(0) | (io.rod_i(0).bits.rd0_raw === io.rod_i(1).bits.rd0_raw)
  // val is_2nd_solo = io.is_commit_abort(1) | io.rod_i(1).bits.is_csr | io.rod_i(1).bits.is_su

    val is_load_accessFault_ack_v = ( 0 until cm ).map{ i =>
      io.lsu_cmm.is_access_fault & io.rod_i(i).bits.is_lu & ~is_wb_v(i)
    }


    val is_store_accessFault_ack_v = ( 0 until cm ).map{ i =>
      io.lsu_cmm.is_access_fault & ( io.rod_i(i).bits.is_su | io.rod_i(i).bits.is_amo ) & ~is_wb_v(i),
    }

    val is_load_pagingFault_ack_v = ( 0 until cm ).map{ i =>
      io.lsu_cmm.is_paging_fault & io.rod_i(i).bits.is_lu & ~is_wb_v(i),
    }

    val is_store_pagingFault_ack_v = ( 0 until cm ).map{ i =>
      io.lsu_cmm.is_paging_fault & ( io.rod_i(i).bits.is_su | io.rod_i(i).bits.is_amo ) & ~is_wb_v(i)
    }

      
    val is_load_misAlign_ack_v = ( 0 until cm ).map{ i =>
      io.lsu_cmm.is_misAlign & io.rod_i(i).bits.is_lu & ~is_wb_v(i)
    }

    val is_store_misAlign_ack_v = ( 0 until cm ).map{ i =>
      io.lsu_cmm.is_misAlign & (io.rod_i(i).bits.is_su | io.rod_i(i).bits.is_amo) & ~is_wb_v(i)
    }

    val is_ecall_v = io.rod_i.map{ _.bits.privil.ecall }


    val is_ebreak_v = io.rod_i.map{ _.bits.privil.ebreak }
 
    val is_instr_access_fault_v = io.rod_i.map{ _.bits.privil.is_access_fault }
    val is_instr_paging_fault_v = io.rod_i.map{ _.bits.privil.is_paging_fault }


    val is_illeage_v = ( 0 until cm ).map{ i =>
      val is_csr_illegal = 
        (is_csrr_illegal  & io.rod_i(i).valid & io.rod_i(i).bits.is_csr  & ~is_wb_v(i)) |
        (is_csrw_illegal  & io.rod_i(i).valid & io.rod_i(i).bits.is_csr  &  is_wb_v(i))  |
        (is_fcsrw_illegal(i) & io.rod_i(i).valid & io.rod_i(i).bits.is_fpu &  is_wb_v(i))

      val is_ill_sfence = is_wb_v(i) & io.rod_i(i).bits.is_sfence_vma & ( (mstatus(i)(20) & priv_lvl(i) === "b01".U) | priv_lvl(i) === "b00".U)

      val is_ill_wfi_v = is_wb_v(i) & io.rod_i(i).bits.is_wfi & (mstatus(i)(21) & priv_lvl_qout < "b11".U)

      val is_ill_mRet_v = io.rod_i(i).bits.privil.mret & priv_lvl(i) =/= "b11".U
      val is_ill_sRet_v = io.rod_i(i).bits.privil.sret & ( priv_lvl(i) === "b00".U | ( priv_lvl(i) === "b01".U & mstatus(i)(22)) )
      val is_ill_dRet_v = io.rod_i(i).bits.privil.dret & ~is_inDebugMode
      val is_ill_fpus_v = (is_wb_v(i) & (io.rod_i(i).bits.is_fcmm | io.rod_i(i).bits.is_fpu) & mstatus(i)(14,13) === 0.U)

      io.rod_i(i).bits.is_illeage | is_csr_illegal | is_ill_sfence | is_ill_wfi_v | is_ill_mRet_v | is_ill_sRet_v | is_ill_dRet_v | is_ill_fpus_v
    }
    
    

    val is_mRet_v = ( 0 until cm ).map{ i =>
      io.rod_i(i).bits.privil.mret & priv_lvl(i) === "b11".U,
    }

    val is_sRet_v = ( 0 until cm ).map{ i =>
      io.rod_i(i).bits.privil.sret & ( priv_lvl(i) === "b11".U | ( priv_lvl(i) === "b01".U & ~mstatus(i)(22)) ),
    }

    val is_dRet_v = ( 0 until cm ).map{ i =>
      io.rod_i(i).bits.privil.dret & is_inDebugMode(i)
    }

    val is_fence_i_v = ( 0 until cm ).map{ i =>
      io.rod_i(i).bits.is_fence_i & is_wb_v(i)
    }


    val is_sfence_vma_v = ( 0 until cm ).map{ i =>
      io.rod_i(i).bits.is_sfence_vma & is_wb_v(i) & ( (~mstatus(i)(20) & priv_lvl(i) === "b01".U) | priv_lvl(i) === "b11".U)
    }


	val is_exception_v = ( 0 until cm ).map{ i =>
    is_ecall_v(i)  |
    is_ebreak_v(i)  |
    is_instr_access_fault_v(i)  |
    is_instr_paging_fault_v(i)  |
    is_illeage_v(i) |
    is_load_accessFault_ack_v(i)  |
    is_store_accessFault_ack_v(i) |
    is_load_misAlign_ack_v(i)  |
    is_store_misAlign_ack_v(i) |
    is_load_pagingFault_ack_v(i) |
    is_store_pagingFault_ack_v(i)         
  }




	val is_trap_v = ( 0 until cm ).map{ i =>
    is_interrupt | is_exception_v(i)
  }
  

  val is_xRet_v = ( 0 until cm ).map{ i =>
    is_mRet_v(i) | is_sRet_v(i) | is_dRet_v(i)
  }

  abort_chn := DontCare
  for ( i <- 0 until cm ) yield {
    when( ~io.rod_i(i).valid ) {
      commit_state(i) := 0.U //IDLE
    } .otherwise {
      when(
          (io.rod_i(i).bits.is_branch & io.is_misPredict ) |
          is_xRet_v(i) |
          is_trap_v(i) |
          is_fence_i_v(i) |
          is_sfence_vma_v(i)
        ) {
        commit_state(i) := 2.U //abort
        abort_chn := i.U
      } .elsewhen(
        is_wb_v(i) & {
          if ( i =/= 0 ) {~is_step}
          else { true.B }          
        }) { //when writeback and no-step
        commit_state(i) := 3.U
      } .otherwise {
        commit_state(i) := 0.U //idle
      }
      if ( i =/= 0 ) {commit_state(i-1) === 1.U} //abort override following as cancel
    }
  }
    


  // io.is_commit_abort(1) :=
  //   (io.rod_i(1).valid) & ( ( (io.rod_i(1).bits.is_branch) & io.is_misPredict ) | is_xRet_v(1) | is_trap_v(1) | is_fence_i_v(1) | is_sfence_vma_v(1)  ) & ~is_1st_solo 

  // io.is_commit_abort(0) :=
  //   (io.rod_i(0).valid) & ( ( (io.rod_i(0).bits.is_branch) & io.is_misPredict ) | is_xRet_v(0) | is_trap_v(0) | is_fence_i_v(0) | is_sfence_vma_v(0) )


  //only one privilege can commit once
  // val is_commit_comfirm = VecInit(
  //   is_wb_v(0) & ~io.is_commit_abort(0),
  //   is_wb_v(1) & ~io.is_commit_abort(1) & ~is_1st_solo & ~is_step
  // )



  ( 0 until cm ).map{ i =>
    io.cm_op(i).is_comfirm := commit_state_is_comfirm(i)
    io.cm_op(i).is_abort   := commit_state_is_abort(i)

  }
  // io.cm_op(0).valid := is_commit_comfirm(0) | io.is_commit_abort(0)
  // io.cm_op(1).valid := is_commit_comfirm(1) | io.is_commit_abort(1)

  // io.cm_op(0).bits.is_abort := io.is_commit_abort(0)
  // io.cm_op(1).bits.is_abort := io.is_commit_abort(1)

  ( 0 until cm ).map{ i =>
    io.rod_i(i).ready := commit_state_is_comfirm(i) | commit_state_is_abort(i)
  }


  io.cmm_lsu.is_amo_pending := {
    io.rod_i(0).valid & io.rod_i(0).bits.is_amo & ~is_wb_v(0) //only pending amo in rod0 is send out
  }
  printf("Warning, amo_pending can only emmit at chn0")
  

  ( 0 until cm ).map{ i =>
    io.cmm_lsu.is_store_commit(i) := io.rod_i(i).bits.is_su & commit_state_is_comfirm(i)
  }


  io.cmm_pc.valid := false.B
  io.cmm_pc.bits.addr := 0.U
  for ( i <- 0 until cm ) yield {
    when( commit_state_is_abort(i) ) {
      when( is_mRet_v(i) ) {
        io.cmm_pc.valid := true.B
        io.cmm_pc.bits.addr := mepc
      } 
      when( is_sRet_v(i) ) {
        io.cmm_pc.valid := true.B
        io.cmm_pc.bits.addr := sepc
      } 
      when( is_dRet_v(i) ) {
        io.cmm_pc.valid := true.B
        io.cmm_pc.bits.addr := dpc
      } 
      when( is_trap_v(i) ) {
        io.cmm_pc.valid := true.B
        io.cmm_pc.bits.addr := Mux1H(Seq(
            emu_reset                              -> "h80000000".U,
            is_debug_interrupt                     -> "h00000000".U,
            (priv_lvl_dnxt(abort_chn) === "b11".U) -> mtvec,
            (priv_lvl_dnxt(abort_chn) === "b01".U) -> stvec
          )),
      } 
      when( is_fence_i_v(i) | is_sfence_vma_v(i) ) {
        io.cmm_pc.valid := true.B
        io.cmm_pc.bits.addr := (io.rod_i(i).bits.pc + 4.U)
      }
    }

    assert( PopCount(Seq( is_xRet_v(i), is_trap_v(i), is_fence_i_v(i), is_sfence_vma_v(i))) <= 1.U )
  }





  // io.cmm_pc.valid := ( 0 until cm ).map{
  //    commit_state_is_abort(i) & (
  //     is_mRet_v(i) |
  //     is_sRet_v(i) |
  //     is_dRet_v(i) |
  //     is_trap_v(i) |
  //     is_fence_i_v(i) |
  //     is_sfence_vma_v(i)
  //   )
  // }.reduce(_|_)

  // io.cmm_pc.bits.addr := Mux1H(Seq(
  //   ( is_mRet_v(abort_chn) ) -> mepc,
  //   ( is_sRet_v(abort_chn) ) -> sepc,
  //   ( is_dRet_v(abort_chn) ) -> dpc ,
  //   ( is_trap_v(abort_chn) ) -> MuxCase(0.U, Array(
  //     emu_reset -> "h80000000".U,
  //     is_debug_interrupt -> "h00000000".U,
  //     (priv_lvl_dnxt(abort_chn) === "b11".U) -> mtvec,
  //     (priv_lvl_dnxt(abort_chn) === "b01".U) -> stvec)
  //   ),
  //   (is_fence_i_v(abort_chn)) ->    (io.rod_i(abort_chn).bits.pc + 4.U),
  //   (is_sfence_vma_v(abort_chn)) -> (io.rod_i(abort_chn).bits.pc + 4.U),

  // ))



  // assert(
  //   PopCount(Seq(
  //     ( commit_state_is_abort(i) & is_xRet_v(0)),
  //     (is_trap_v(0)),
  //     (is_fence_i_v(0)),
  //     (is_sfence_vma_v(0)),
  //   )) <= 1.U
  // )

  // assert( ~(io.cmm_pc.valid & ~io.is_commit_abort(0) & ~io.is_commit_abort(1)) )


    val is_retired_v = ( 0 until cm ).map{ i => 
      commit_state_is_comfirm(i) | commit_state_is_abort(i)
    }

    ( 1 until cm ).map{ i => 
      assert( ~(is_retired_v(i) & ~is_retired_v(i-1)) )
    }


      // VecInit(
      //   is_commit_comfirm(0) | io.is_commit_abort(0),
      //   is_commit_comfirm(1),
      // )








  is_csrw_illegal := 
    io.csr_cmm_op.valid &
    ( io.csr_cmm_op.bits.op_rc | io.csr_cmm_op.bits.op_rs | io.csr_cmm_op.bits.op_rw ) &
    (
      csr_write_denied(io.csr_cmm_op.bits.addr) | csr_read_prilvl(io.csr_cmm_op.bits.addr)
    )  

  is_csrr_illegal := 
    io.csr_addr.valid & csr_read_prilvl(io.csr_addr.bits)



  io.csr_data.bits := csr_read_res(io.csr_addr.bits)
  io.csr_data.valid := io.csr_addr.valid & ~is_csrr_illegal
  
  
  io.csr_cmm_op.ready := ( 0 until cm ).map{ i =>
    commit_state_is_comfirm(i) & io.rod_i(i).bits.is_csr
  }.reduce(_|_)

  assert( ~(io.csr_cmm_op.ready & ~io.csr_cmm_op.valid) )
  printf("Warning, csr can only execute one by one")
    // io.csr_cmm_op.valid & (
    //   (is_commit_comfirm(0) & io.rod_i(0).bits.is_csr) | 
    //   (is_commit_comfirm(1) & io.rod_i(1).bits.is_csr)		
    // )


  ( 0 until cm ).map{ i =>
  
    is_fcsrw_illegal(i) := 
      io.fcsr_cmm_op(i).valid & 
      ( io.fcsr_cmm_op(i).bits.op_rc | io.fcsr_cmm_op(i).bits.op_rs | io.fcsr_cmm_op(i).bits.op_rw ) &
      (
        mstatus(i)(14,13) === 0.U
      )

    io.fcsr_cmm_op(i).ready :=
      commit_state_is_comfirm(i) & io.rod_i(i).bits.is_fpu

    assert( ~(io.fcsr_cmm_op(i).ready & ~io.fcsr_cmm_op(i).valid) )
  }

  /** @note fcsr-read will request after cmm_op_fifo clear, frm will never change until fcsr-write */
  io.fcsr := fcsr 









	rtc_clock               := io.rtc_clock	
	exe_port                := Mux( io.csr_cmm_op.fire, io.csr_cmm_op.bits, 0.U.asTypeOf(new Exe_Port))

  ( 0 until cm ).map{ i => 
    exe_fport(i) := Mux( io.fcsr_cmm_op(i).fire, io.fcsr_cmm_op(i).bits, 0.U.asTypeOf(new Exe_Port))
  }

	// is_trap                 := io.rod_i(0).valid & is_trap_v(0)
	// is_mRet                 := io.rod_i(0).valid & is_mRet_v(0)
	// is_sRet                 := io.rod_i(0).valid & is_sRet_v(0)
	// is_dRet                 := io.rod_i(0).valid & is_dRet_v(0)
  ( 0 until cm ).map{ i => 
    commit_pc(i) := io.rod_i(i).bits.pc
  }


	ill_instr               := 0.U
  ill_ivaddr               := io.if_cmm.ill_vaddr
	ill_dvaddr               := io.lsu_cmm.trap_addr

	// is_instr_access_fault    := io.rod_i(0).valid & is_instr_access_fault_v(0)
	// is_instr_paging_fault    := io.rod_i(0).valid & is_instr_paging_fault_v(0)
	// is_instr_illeage        := io.rod_i(0).valid & is_illeage_v(0)
	// is_breakPoint           := io.rod_i(0).valid & is_ebreak_v(0) & ~is_ebreak_breakpointn
	// is_load_misAlign        := io.rod_i(0).valid & is_load_misAlign_ack_v(0)
	// is_load_access_fault     := io.rod_i(0).valid & is_load_accessFault_ack_v(0)
	// is_storeAMO_misAlign    := io.rod_i(0).valid & is_store_misAlign_ack_v(0)
	// is_storeAMO_access_fault := io.rod_i(0).valid & is_store_accessFault_ack_v(0)
	// is_storeAMO_paging_fault := io.rod_i(0).valid & is_store_pagingFault_ack_v(0)
	// is_ecall_M                := io.rod_i(0).valid & is_ecall_v(0) & priv_lvl_qout === "b11".U
	// is_ecall_S                := io.rod_i(0).valid & is_ecall_v(0) & priv_lvl_qout === "b01".U
	// is_ecall_U                := io.rod_i(0).valid & is_ecall_v(0) & priv_lvl_qout === "b00".U
	// is_load_paging_fault       := io.rod_i(0).valid & is_load_pagingFault_ack_v(0)

	retired_cnt :=
    MuxCase( 0.U, Array(
      ( cm-1 to 0 ).map{ i => (is_retired_v(i) -> i.U) }
    ))

	clint_sw_m              := false.B
	clint_sw_s              := false.B
	clint_tm_m              := false.B
	clint_tm_s              := false.B
	clint_ex_m              := false.B
	clint_ex_s              := false.B



  io.cmm_mmu.satp := satp
  for ( i <- 0 until 16 ) yield io.cmm_mmu.pmpcfg(i) := pmpcfg(i)
  for ( i <- 0 until 64 ) yield io.cmm_mmu.pmpaddr(i) := pmpaddr(i)
  io.cmm_mmu.priv_lvl_if   := priv_lvl_if
  io.cmm_mmu.priv_lvl_ls   := priv_lvl_ls
  io.cmm_mmu.mstatus    := mstatus
  io.cmm_mmu.sstatus    := sstatus
  io.cmm_mmu.sfence_vma := ( 0 until cm ).map{ i => 
    is_commit_abort(i) & is_sfence_vma_v(i) 
  }
    // ( io.rod_i(0).valid & is_sfence_vma_v(0))  


  io.ifence := ( 0 until cm ).map{ i => 
    is_commit_abort(i) & is_fence_i_v(i)
  }
    // ( io.rod_i(0).valid & is_fence_i_v(0))


  is_fpu_state_change := ( 0 until cm ).map{ i => 
    is_commit_comfirm(i) & (io.rod_i(i).bits.is_fcmm | io.rod_i(i).bits.is_fpu)
  }.reduce(_|_)
    




  is_single_step    := RegNext(next = is_retired_v(0) & is_step, init = false.B)
  is_trigger        := false.B
  is_halt_int   := io.dm.hartHaltReq
  is_ebreak_retired := ( 0 until cm ).map{ i =>
      is_retired_v(i) & is_ebreak_v(i) & is_ebreak_breakpointn
    }.reduce(_|_)



  is_debug_interrupt :=
    ~is_inDebugMode & (
      is_single_step |
      is_trigger |
      is_halt_int |
      is_ebreak_retired      
    )



  is_nomask_interrupt := is_debug_interrupt | emu_reset

  io.dm.hartIsInReset := emu_reset
  ResetReq := io.dm.hartResetReq

  is_inDebugMode_en := 
    (io.rod_i(0).valid & is_trap_v(0) & is_debug_interrupt) |
    is_dRet

  is_inDebugMode_dnxt := Mux1H(Seq(
    ( io.rod_i(0).valid & is_trap_v(0) & is_debug_interrupt ) -> true.B,
    is_dRet -> false.B,
  ))














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
