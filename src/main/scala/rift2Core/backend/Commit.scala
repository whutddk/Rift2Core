
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
import rift2Core.backend._
import rift2Core.privilege._
import rift2Core.privilege.csrFiles._

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

    val csr_addr = Input(UInt(12.W))
    val csr_data = Output(UInt(64.W))
    val csr_cmm_op = Flipped(DecoupledIO( new Exe_Port ) )

    val is_misPredict = Input(Bool())
    val is_commit_abort = Vec(2, Output( Bool() ))

    val cmm_pc = new ValidIO(new Info_cmm_pc)

    val cmm_mmu = Output( new Info_cmm_mmu )

    val rtc_clock = Input(Bool())
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
        io.lsu_cmm.is_accessFault & io.rod_i(0).bits.is_lu & ~is_wb_v(0),
        io.lsu_cmm.is_accessFault & io.rod_i(1).bits.is_lu & ~is_wb_v(1) & ~is_1st_solo
      )

    val is_store_accessFault_ack_v =
      VecInit(
        io.lsu_cmm.is_accessFault & io.rod_i(0).bits.is_su & ~is_wb_v(0),
        io.lsu_cmm.is_accessFault & io.rod_i(1).bits.is_su & ~is_wb_v(1) & ~is_1st_solo
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
 
    val is_instr_accessFault_v =
      VecInit(
        io.rod_i(0).bits.is_accessFault,
        io.rod_i(1).bits.is_accessFault & ~is_1st_solo
      )

    val is_illeage_v =
      VecInit(
        io.rod_i(0).bits.is_illeage,
        io.rod_i(1).bits.is_illeage & ~is_1st_solo
      )

    val is_mRet_v =
      VecInit(
        io.rod_i(0).bits.privil.mret,
        io.rod_i(1).bits.privil.mret & ~is_1st_solo
      )

    val is_sRet_v =
      VecInit(
        io.rod_i(0).bits.privil.sret,
        io.rod_i(1).bits.privil.sret & ~is_1st_solo
      )

    val is_fence_i_v =
      VecInit(
        io.rod_i(0).bits.is_fence_i & is_wb_v(0),
        io.rod_i(1).bits.is_fence_i & is_wb_v(1) & ~is_1st_solo
      )



	val is_exception_v = 
    VecInit( for (i <- 0 until 2) yield {
      is_ecall_v(i)  |
      is_ebreak_v(i)  |
      is_instr_accessFault_v(i)  |
      is_illeage_v(i) |
      is_load_accessFault_ack_v(i)  |
      is_store_accessFault_ack_v(i) |
      is_load_misAlign_ack_v(i)  |
      is_store_misAlign_ack_v(i)
    })

	// val is_trap_v = Wire(Vec(2, Bool()))
  // val is_xRet_v = Wire(Vec(2, Bool()))

	val is_trap_v = VecInit( is_interrupt | is_exception_v(0), is_interrupt | is_exception_v(1) )
  val is_xRet_v = VecInit( is_mRet_v(0) | is_sRet_v(0), is_mRet_v(1) | is_sRet_v(1) )



  io.is_commit_abort(1) :=
    (io.rod_i(1).valid) & ( ( (io.rod_i(1).bits.is_branch) & io.is_misPredict ) | is_xRet_v(1) | is_trap_v(1) | is_fence_i_v(1) ) & ~is_1st_solo
  
  io.is_commit_abort(0) :=
    (io.rod_i(0).valid) & ( ( (io.rod_i(0).bits.is_branch) & io.is_misPredict ) | is_xRet_v(0) | is_trap_v(0) | is_fence_i_v(0) )


  //only one privilege can commit once
  val is_commit_comfirm = VecInit(
    is_wb_v(0) & ~io.is_commit_abort(0),
    is_wb_v(1) & ~io.is_commit_abort(1) & ~is_1st_solo
  )


  io.cm_op(0).valid := is_commit_comfirm(0)
  io.cm_op(1).valid := is_commit_comfirm(1)

  io.cm_op(0).bits.phy := rd0_phy(0)
  io.cm_op(1).bits.phy := rd0_phy(1)

  io.cm_op(0).bits.raw := rd0_raw(0)
  io.cm_op(1).bits.raw := rd0_raw(1)



  io.rod_i(0).ready := is_commit_comfirm(0)
  io.rod_i(1).ready := is_commit_comfirm(1)

  io.cmm_lsu.is_amo_pending := io.rod_i(0).valid & io.rod_i(0).bits.is_amo & ~is_commit_comfirm(0) //only pending amo in rod0 is send out
  io.cmm_lsu.is_lr_clear := io.is_commit_abort(1) | io.is_commit_abort(0)

    //  |
    // (io.rod_i(1).bits.is_amo & ~is_commit_comfirm(1) ~is_1st_solo )

  io.cmm_lsu.is_store_commit(0) := io.rod_i(0).bits.is_su & is_commit_comfirm(0)
  io.cmm_lsu.is_store_commit(1) := io.rod_i(1).bits.is_su & is_commit_comfirm(1)




  



  io.cmm_pc.valid :=
  (    
    (io.rod_i(0).valid & is_xRet_v(0)) |
    (io.rod_i(0).valid & is_trap_v(0)) |
    (io.rod_i(0).valid & is_fence_i_v(0)) 
  ) |
  (

    (io.rod_i(1).valid & is_xRet_v(1)) |
    (io.rod_i(1).valid & is_trap_v(1)) |
    (io.rod_i(1).valid & is_fence_i_v(1))      

  )
    
  io.cmm_pc.bits.addr := Mux1H(Seq(
    (io.rod_i(0).valid & is_xRet_v(0)) -> mepc,
    (io.rod_i(0).valid & is_trap_v(0)) -> mtvec,
    (io.rod_i(0).valid & is_fence_i_v(0)) -> (io.rod_i(0).bits.pc + 4.U),

    (io.rod_i(1).valid & is_xRet_v(1)) -> mepc,
    (io.rod_i(1).valid & is_trap_v(1)) -> mtvec,
    (io.rod_i(1).valid & is_fence_i_v(1)) -> (io.rod_i(1).bits.pc + 4.U)
  ))

  assert(
    PopCount(Seq(
      (io.rod_i(0).valid & is_xRet_v(0)),
      (io.rod_i(0).valid & is_trap_v(0)),
      (io.rod_i(0).valid & is_fence_i_v(0)),
      (io.rod_i(1).valid & is_xRet_v(1)),
      (io.rod_i(1).valid & is_trap_v(1)),
      (io.rod_i(1).valid & is_fence_i_v(1))     
    )) <= 1.U
  )

  assert( ~(io.cmm_pc.valid & ~io.is_commit_abort(0) & ~io.is_commit_abort(1)) )


    val is_retired_v = 
      VecInit(
        is_commit_comfirm(0) | io.is_commit_abort(0),
        is_commit_comfirm(1) 
      )















  io.csr_data := csr_read(io.csr_addr)

  
  io.csr_cmm_op.ready := io.csr_cmm_op.valid & (
                (is_commit_comfirm(0) & io.rod_i(0).bits.is_csr) | 
                (is_commit_comfirm(1) & io.rod_i(1).bits.is_csr)		
              )











	rtc_clock               := io.rtc_clock	
	exe_port                := Mux( io.csr_cmm_op.ready, io.csr_cmm_op.bits, 0.U.asTypeOf(new Exe_Port))
	is_trap                 := is_trap_v.contains(true.B)
	is_mRet                 := is_mRet_v.contains(true.B)
	is_sRet                 := is_sRet_v.contains(true.B)
	commit_pc               := Mux(is_1st_solo, io.rod_i(0).bits.pc, io.rod_i(1).bits.pc)
	ill_instr               := 0.U
	ill_vaddr               := io.lsu_cmm.trap_addr
	is_instr_accessFault    := is_instr_accessFault_v.contains(true.B)
	is_instr_illeage        := is_illeage_v.contains(true.B)
	is_breakPoint           := is_ebreak_v.contains(true.B)
	is_load_misAlign        := is_load_misAlign_ack_v.contains(true.B)
	is_load_accessFault     := is_load_accessFault_ack_v.contains(true.B)
	is_storeAMO_misAlign    := is_store_misAlign_ack_v.contains(true.B)
	is_storeAMO_accessFault := is_store_accessFault_ack_v.contains(true.B)
	is_ecall                := is_ecall_v.contains(true.B)
	is_instr_pageFault      := false.B
	is_load_pageFault       := false.B
	is_storeAMO_pageFault   := false.B
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
  io.cmm_mmu.priv_lvl := priv_lvl_qout
  io.cmm_mmu.mstatus := mstatus
  io.cmm_mmu.sstatus := sstatus
}
