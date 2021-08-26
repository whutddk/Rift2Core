
/*
* @Author: Ruige Lee
* @Date:   2021-04-14 11:24:59
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-05-17 17:02:17
*/

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


package rift2Core.backend

import chisel3._
import chisel3.util._

import rift2Core.define._
import rift2Core.frontend._
import rift2Core.L1Cache._
import rift2Core.backend._
import rift2Core.privilege._
import rift2Core.privilege.csrFiles._

import rift2Core.diff._

/** commit
  * @author Ruige Lee
  * 
  * 
  */
class Commit extends Privilege with Superscalar {
  val io = IO(new Bundle{

    val cm_op = Vec(2, ValidIO( new Info_commit_op ))
    val log = Input(Vec(64, UInt(2.W)))


    val rod_i = Vec(2, Flipped(new DecoupledIO( new Info_reorder_i ) ))

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



    val cmm_mmu = Output( new Info_cmm_mmu )

    val rtc_clock = Input(Bool())

    val diff_commit = Output(new Info_cmm_diff)
  })


  val rd0_raw = VecInit( io.rod_i(0).bits.rd0_raw, io.rod_i(1).bits.rd0_raw )
  val rd0_phy = VecInit( io.rod_i(0).bits.rd0_phy, io.rod_i(1).bits.rd0_phy )


  val is_wb_v =
    VecInit(
      (io.log(rd0_phy(0)) === 3.U) & (io.rod_i(0).valid),
      (io.log(rd0_phy(1)) === 3.U) & (io.rod_i(1).valid)
    )

  //bru commit ilp
  io.cmm_bru_ilp := (io.rod_i(0).valid) & io.rod_i(0).bits.is_branch & (io.log(rd0_phy(0)) =/= 3.U)

  val is_1st_solo = io.is_commit_abort(0) | io.rod_i(0).bits.is_csr | io.rod_i(0).bits.is_su | ~is_wb_v(0) | (io.rod_i(0).bits.rd0_raw === io.rod_i(1).bits.rd0_raw)
  val is_2nd_solo = io.is_commit_abort(1) | io.rod_i(1).bits.is_csr | io.rod_i(1).bits.is_su

    val is_load_accessFault_ack_v =
      VecInit(
        io.lsu_cmm.is_access_fault & io.rod_i(0).bits.is_lu & ~is_wb_v(0),
        io.lsu_cmm.is_access_fault & io.rod_i(1).bits.is_lu & ~is_wb_v(1) & ~is_1st_solo
      )

    val is_store_accessFault_ack_v =
      VecInit(
        io.lsu_cmm.is_access_fault & ( io.rod_i(0).bits.is_su | io.rod_i(0).bits.is_amo ) & ~is_wb_v(0),
        io.lsu_cmm.is_access_fault & ( io.rod_i(1).bits.is_su | io.rod_i(1).bits.is_amo ) & ~is_wb_v(1) & ~is_1st_solo
      )

    val is_load_pagingFault_ack_v =
      VecInit(
        io.lsu_cmm.is_paging_fault & io.rod_i(0).bits.is_lu & ~is_wb_v(0),
        io.lsu_cmm.is_paging_fault & io.rod_i(1).bits.is_lu & ~is_wb_v(1) & ~is_1st_solo
      )

    val is_store_pagingFault_ack_v =
      VecInit(
        io.lsu_cmm.is_paging_fault & ( io.rod_i(0).bits.is_su | io.rod_i(0).bits.is_amo ) & ~is_wb_v(0),
        io.lsu_cmm.is_paging_fault & ( io.rod_i(1).bits.is_su | io.rod_i(1).bits.is_amo ) & ~is_wb_v(1) & ~is_1st_solo
      )

      
    val is_load_misAlign_ack_v =
      VecInit(
        io.lsu_cmm.is_misAlign & io.rod_i(0).bits.is_lu & ~is_wb_v(0),
        io.lsu_cmm.is_misAlign & io.rod_i(1).bits.is_lu & ~is_wb_v(1) & ~is_1st_solo
      )

    val is_store_misAlign_ack_v =
      VecInit(
        io.lsu_cmm.is_misAlign & (io.rod_i(0).bits.is_su | io.rod_i(0).bits.is_amo) & ~is_wb_v(0),
        io.lsu_cmm.is_misAlign & (io.rod_i(1).bits.is_su | io.rod_i(1).bits.is_amo) & ~is_wb_v(1) & ~is_1st_solo
      )




    val is_ecall_v  =
      VecInit(
        io.rod_i(0).bits.privil.ecall,
        io.rod_i(1).bits.privil.ecall & ~is_1st_solo
      )

    val is_ebreak_v =
      VecInit(
        io.rod_i(0).bits.privil.ebreak,
        io.rod_i(1).bits.privil.ebreak & ~is_1st_solo
      )
 
    val is_instr_access_fault_v =
      VecInit(
        io.rod_i(0).bits.privil.is_access_fault,
        io.rod_i(1).bits.privil.is_access_fault & ~is_1st_solo
      )

    val is_instr_paging_fault_v =
      VecInit(
        io.rod_i(0).bits.privil.is_paging_fault,
        io.rod_i(1).bits.privil.is_paging_fault & ~is_1st_solo
      )

    val is_illeage_v = {
      val is_csr_illegal = VecInit(
        (is_csrr_illegal & io.rod_i(0).valid & io.rod_i(0).bits.is_csr & ~is_wb_v(0)) |
        (is_csrw_illegal & io.rod_i(0).valid & io.rod_i(0).bits.is_csr & is_wb_v(0)) ,
        (is_csrr_illegal & io.rod_i(1).valid & io.rod_i(1).bits.is_csr & ~is_wb_v(1)) |
        (is_csrw_illegal & io.rod_i(1).valid & io.rod_i(1).bits.is_csr & is_wb_v(1))
      )

      val is_ill_sfence = VecInit(
        is_wb_v(0) & io.rod_i(0).bits.is_sfence_vma & (mstatus(20) | priv_lvl_qout === "b00".U),
        is_wb_v(1) & io.rod_i(1).bits.is_sfence_vma & (mstatus(20) | priv_lvl_qout === "b00".U)
      )

      val is_ill_wfi_v = VecInit(
        is_wb_v(0) & io.rod_i(0).bits.is_wfi & (mstatus(21) & priv_lvl_qout < "b11".U),
        is_wb_v(1) & io.rod_i(1).bits.is_wfi & (mstatus(21) & priv_lvl_qout < "b11".U)
      )

      val is_ill_mRet_v = VecInit(
        io.rod_i(0).bits.privil.mret & priv_lvl_qout =/= "b11".U,
        io.rod_i(1).bits.privil.mret & priv_lvl_qout =/= "b11".U
      )

      val is_ill_sRet_v = VecInit(
        io.rod_i(0).bits.privil.sret & ( priv_lvl_qout === "b00".U | ( priv_lvl_qout === "b10".U & mstatus(22)) ),
        io.rod_i(1).bits.privil.sret & ( priv_lvl_qout === "b00".U | ( priv_lvl_qout === "b10".U & mstatus(22)) )
      
      )



      VecInit(
        (io.rod_i(0).bits.is_illeage | is_csr_illegal(0) | is_ill_sfence(0) | is_ill_wfi_v(0) | is_ill_mRet_v(0) | is_ill_sRet_v(0) ),
        (io.rod_i(1).bits.is_illeage | is_csr_illegal(1) | is_ill_sfence(1) | is_ill_wfi_v(1) | is_ill_mRet_v(1) | is_ill_sRet_v(1)) & ~is_1st_solo
      )
    }

    val is_mRet_v =
      VecInit(
        io.rod_i(0).bits.privil.mret & priv_lvl_qout === "b11".U,
        io.rod_i(1).bits.privil.mret & priv_lvl_qout === "b11".U & ~is_1st_solo
      )

    val is_sRet_v =
      VecInit(
        io.rod_i(0).bits.privil.sret & ( priv_lvl_qout === "b11".U | ( priv_lvl_qout === "b10".U & ~mstatus(22)) ),
        io.rod_i(1).bits.privil.sret & ( priv_lvl_qout === "b11".U | ( priv_lvl_qout === "b10".U & ~mstatus(22)) ) & ~is_1st_solo
      )

    val is_fence_i_v =
      VecInit(
        io.rod_i(0).bits.is_fence_i & is_wb_v(0),
        io.rod_i(1).bits.is_fence_i & is_wb_v(1) & ~is_1st_solo
      )

    val is_sfence_vma_v =
      VecInit(
        io.rod_i(0).bits.is_sfence_vma & is_wb_v(0),
        io.rod_i(1).bits.is_sfence_vma & is_wb_v(1) & ~is_1st_solo
      )







	val is_exception_v = 
    VecInit( for (i <- 0 until 2) yield {
      (
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
      )

    })

	// val is_trap_v = Wire(Vec(2, Bool()))
  // val is_xRet_v = Wire(Vec(2, Bool()))

	val is_trap_v = VecInit( is_interrupt | is_exception_v(0), is_interrupt | is_exception_v(1) )
  val is_xRet_v = VecInit( is_mRet_v(0) | is_sRet_v(0), is_mRet_v(1) | is_sRet_v(1) )



  io.is_commit_abort(1) :=
    (io.rod_i(1).valid) & ( ( (io.rod_i(1).bits.is_branch) & io.is_misPredict ) | is_xRet_v(1) | is_trap_v(1) | is_fence_i_v(1) | is_sfence_vma_v(1) ) & ~is_1st_solo
  // assert( io.is_commit_abort(1) === false.B )


  io.is_commit_abort(0) :=
    (io.rod_i(0).valid) & ( ( (io.rod_i(0).bits.is_branch) & io.is_misPredict ) | is_xRet_v(0) | is_trap_v(0) | is_fence_i_v(0) | is_sfence_vma_v(0) )


  //only one privilege can commit once
  val is_commit_comfirm = VecInit(
    is_wb_v(0) & ~io.is_commit_abort(0),
    is_wb_v(1) & ~io.is_commit_abort(1) & ~is_1st_solo
  )

  // when( is_commit_comfirm(0) ) { printf("comfirm pc=%x\n", io.rod_i(0).bits.pc) }
  // when( is_commit_comfirm(1) ) { printf("comfirm pc=%x\n", io.rod_i(1).bits.pc) }


  io.cm_op(0).valid := is_commit_comfirm(0)
  io.cm_op(1).valid := is_commit_comfirm(1)

  io.cm_op(0).bits.phy := rd0_phy(0)
  io.cm_op(1).bits.phy := rd0_phy(1)

  io.cm_op(0).bits.raw := rd0_raw(0)
  io.cm_op(1).bits.raw := rd0_raw(1)



  io.rod_i(0).ready := is_commit_comfirm(0) | io.is_commit_abort(0)
  io.rod_i(1).ready := is_commit_comfirm(1)

  io.cmm_lsu.is_amo_pending := io.rod_i(0).valid & io.rod_i(0).bits.is_amo & ~is_commit_comfirm(0) //only pending amo in rod0 is send out
  io.cmm_lsu.is_lr_clear := io.is_commit_abort(1) | io.is_commit_abort(0)

    //  |
    // (io.rod_i(1).bits.is_amo & ~is_commit_comfirm(1) ~is_1st_solo )

  io.cmm_lsu.is_store_commit(0) := io.rod_i(0).bits.is_su & is_commit_comfirm(0)
  io.cmm_lsu.is_store_commit(1) := io.rod_i(1).bits.is_su & is_commit_comfirm(1)




  



  io.cmm_pc.valid :=
  (    
    (io.rod_i(0).valid & is_mRet_v(0)) |
    (io.rod_i(0).valid & is_sRet_v(0)) |
    (io.rod_i(0).valid & is_trap_v(0)) |
    (io.rod_i(0).valid & is_fence_i_v(0)) |
    (io.rod_i(0).valid & is_sfence_vma_v(0))
  ) 
  // |
  // (

  //   (io.rod_i(1).valid & is_mRet_v(1)) |
  //   (io.rod_i(1).valid & is_sRet_v(1)) |
  //   (io.rod_i(1).valid & is_trap_v(1)) |
  //   (io.rod_i(1).valid & is_fence_i_v(1)) |    
  //   (io.rod_i(1).valid & is_sfence_vma_v(1))  

  // )
    
  io.cmm_pc.bits.addr := Mux1H(Seq(
    (io.rod_i(0).valid & is_mRet_v(0)) -> mepc,
    (io.rod_i(0).valid & is_sRet_v(0)) -> sepc,
    (io.rod_i(0).valid & is_trap_v(0)) -> Mux1H(Seq( (priv_lvl_dnxt === "b11".U) -> mtvec, (priv_lvl_dnxt === "b01".U) -> stvec)),
    (io.rod_i(0).valid & is_fence_i_v(0)) -> (io.rod_i(0).bits.pc + 4.U),
    (io.rod_i(0).valid & is_sfence_vma_v(0)) -> (io.rod_i(0).bits.pc + 4.U),

    // (io.rod_i(1).valid & is_mRet_v(1)) -> mepc,
    // (io.rod_i(1).valid & is_sRet_v(1)) -> sepc,
    // (io.rod_i(1).valid & is_trap_v(1)) -> mtvec,
    // (io.rod_i(1).valid & is_fence_i_v(1)) -> (io.rod_i(1).bits.pc + 4.U),
    // (io.rod_i(1).valid & is_sfence_vma_v(1)) -> (io.rod_i(1).bits.pc + 4.U),
  ))

  assert(
    PopCount(Seq(
      (io.rod_i(0).valid & is_xRet_v(0)),
      (io.rod_i(0).valid & is_trap_v(0)),
      (io.rod_i(0).valid & is_fence_i_v(0)),
      (io.rod_i(0).valid & is_sfence_vma_v(0)),
      // (io.rod_i(1).valid & is_xRet_v(1)),
      // (io.rod_i(1).valid & is_trap_v(1)),
      // (io.rod_i(1).valid & is_fence_i_v(1)),    
      // (io.rod_i(1).valid & is_sfence_vma_v(1)),    
    )) <= 1.U
  )

  assert( ~(io.cmm_pc.valid & ~io.is_commit_abort(0) & ~io.is_commit_abort(1)) )


    val is_retired_v = 
      VecInit(
        is_commit_comfirm(0) | io.is_commit_abort(0),
        is_commit_comfirm(1),
      )












  is_csrw_illegal := 
    io.csr_cmm_op.valid & (
      csr_write_denied(io.csr_cmm_op.bits.addr) |
      csr_read_prilvl(io.csr_cmm_op.bits.addr)
    )

  is_csrr_illegal := 
    io.csr_addr.valid & csr_read_prilvl(io.csr_addr.bits)



  io.csr_data.bits := csr_read_res(io.csr_addr.bits)
  io.csr_data.valid := io.csr_addr.valid & ~is_csrr_illegal


  
  io.csr_cmm_op.ready :=
    io.csr_cmm_op.valid & (
      (is_commit_comfirm(0) & io.rod_i(0).bits.is_csr) | 
      (is_commit_comfirm(1) & io.rod_i(1).bits.is_csr)		
    )











	rtc_clock               := io.rtc_clock	
	exe_port                := Mux( io.csr_cmm_op.ready, io.csr_cmm_op.bits, 0.U.asTypeOf(new Exe_Port))
	is_trap                 := io.rod_i(0).valid & is_trap_v(0)
	is_mRet                 := io.rod_i(0).valid & is_mRet_v(0)
	is_sRet                 := io.rod_i(0).valid & is_sRet_v(0)
	commit_pc               := Mux(is_1st_solo, io.rod_i(0).bits.pc, io.rod_i(1).bits.pc)
	ill_instr               := 0.U
  ill_ivaddr               := io.if_cmm.ill_vaddr
	ill_dvaddr               := io.lsu_cmm.trap_addr
	is_instr_access_fault    := io.rod_i(0).valid & is_instr_access_fault_v(0)
	is_instr_paging_fault    := io.rod_i(0).valid & is_instr_paging_fault_v(0)
	is_instr_illeage        := io.rod_i(0).valid & is_illeage_v(0)
	is_breakPoint           := io.rod_i(0).valid & is_ebreak_v(0)
	is_load_misAlign        := io.rod_i(0).valid & is_load_misAlign_ack_v(0)
	is_load_access_fault     := io.rod_i(0).valid & is_load_accessFault_ack_v(0)
	is_storeAMO_misAlign    := io.rod_i(0).valid & is_store_misAlign_ack_v(0)
	is_storeAMO_access_fault := io.rod_i(0).valid & is_store_accessFault_ack_v(0)
	is_storeAMO_paging_fault := io.rod_i(0).valid & is_store_pagingFault_ack_v(0)
	is_ecall_M                := io.rod_i(0).valid & is_ecall_v(0) & priv_lvl_qout === "b11".U
	is_ecall_S                := io.rod_i(0).valid & is_ecall_v(0) & priv_lvl_qout === "b01".U
	is_ecall_U                := io.rod_i(0).valid & is_ecall_v(0) & priv_lvl_qout === "b00".U
	is_load_paging_fault       := io.rod_i(0).valid & is_load_pagingFault_ack_v(0)
	retired_cnt             := Mux( is_retired_v(1), 2.U, Mux(is_retired_v(0), 1.U, 0.U) )
	clint_sw_m              := false.B
	clint_sw_s              := false.B
	clint_tm_m              := false.B
	clint_tm_s              := false.B
	clint_ex_m              := false.B
	clint_ex_s              := false.B



  io.cmm_mmu.satp := satp
  for ( i <- 0 until 16 ) yield io.cmm_mmu.pmpcfg(i) := pmpcfg(i)
  for ( i <- 0 until 64 ) yield io.cmm_mmu.pmpaddr(i) := pmpaddr(i)
  io.cmm_mmu.priv_lvl   := priv_lvl_qout
  io.cmm_mmu.mstatus    := mstatus
  io.cmm_mmu.sstatus    := sstatus
  io.cmm_mmu.sfence_vma := 
    ( io.rod_i(1).valid & is_sfence_vma_v(1) & ~is_1st_solo) |
    ( io.rod_i(0).valid & is_sfence_vma_v(0))








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


  assert( ~(is_wb_v(0) & ~io.rod_i(0).valid) )
  assert( ~(is_wb_v(1) & ~io.rod_i(1).valid) )
}
