
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

    val cm_op = Output(Vec(32, Vec(4, Bool())))
    val log = Input(Vec(32,Vec(4, UInt(2.W))) )

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

    val rtc_clock = Input(Bool())
  })


  val rd0_raw = VecInit( io.rod_i(0).bits.rd0_raw, io.rod_i(1).bits.rd0_raw )
  val rd0_idx = VecInit( io.rod_i(0).bits.rd0_idx, io.rod_i(1).bits.rd0_idx )

  val is_wb =
    VecInit(
      (io.log(rd0_raw(0))(rd0_idx(0)) === 3.U) & (io.rod_i(0).valid),
      (io.log(rd0_raw(1))(rd0_idx(1)) === 3.U) & (io.rod_i(1).valid)
    )

  //bru commit ilp
  io.cmm_bru_ilp := (io.rod_i(0).valid) & io.rod_i(0).bits.is_branch & (io.log(rd0_raw(0))(rd0_idx(0)) =/= 3.U)

  val is_1st_solo = io.is_commit_abort(0) | io.rod_i(0).bits.is_csr | io.rod_i(0).bits.is_su | ~is_wb(0) | (io.rod_i(0).bits.rd0_raw === io.rod_i(1).bits.rd0_raw)
  val is_2nd_solo = io.is_commit_abort(1) | io.rod_i(1).bits.is_csr | io.rod_i(1).bits.is_su


  io.is_commit_abort(1) :=
    (io.rod_i(1).valid) & ( ( (io.rod_i(1).bits.is_branch) & io.is_misPredict ) | is_xRet(1) | is_trap(1) ) & ~is_1st_solo
  
  io.is_commit_abort(0) :=
    (io.rod_i(0).valid) & ( ( (io.rod_i(0).bits.is_branch) & io.is_misPredict ) | is_xRet(0) | is_trap(0) )


  //only one privilege can commit once
  val is_commit_comfirm = VecInit(
    is_wb(0) & ~io.is_commit_abort(0),
    is_wb(1) & ~io.is_commit_abort(1) & ~is_1st_solo
  )

  for ( i <- 0 until 32; j <- 0 until 4 ) yield {
    io.cm_op(i)(j) := 
      (is_commit_comfirm(0) & rd0_raw(0) === i.U & rd0_idx(0) === j.U) | 
      (is_commit_comfirm(1) & rd0_raw(1) === i.U & rd0_idx(1) === j.U)
  }

  io.rod_i(0).ready := is_commit_comfirm(0)
  io.rod_i(1).ready := is_commit_comfirm(1)

  io.cmm_lsu.is_fence_commit := 	(io.rod_i(0).bits.is_fence & is_commit_comfirm(0)) |
                  (io.rod_i(1).bits.is_fence & is_commit_comfirm(1))

  io.cmm_lsu.is_store_commit := 	(io.rod_i(0).bits.is_su & is_commit_comfirm(0)) |
                  (io.rod_i(1).bits.is_su & is_commit_comfirm(1))




  

  val is_fence_i = VecInit( 	io.rod_i(0).bits.is_fence_i & is_commit_comfirm(0),
                io.rod_i(1).bits.is_fence_i & is_commit_comfirm(1)
              )

  io.cmm_pc.valid := is_xRet_v.contains(true.B) | is_trap_v.contains(true.B) | is_fence_i.contains(true.B)
  io.cmm_pc.bits.addr := MuxCase(0.U, Array(
    is_xRet(0) -> mepc,
    is_trap(0) -> mtvec,
    is_xRet(1) -> mepc,
    is_trap(1) -> mtvec,
    is_fence_i(0) -> (io.rod_i(0).bits.pc + 4.U),
    is_fence_i(1) -> (io.rod_i(1).bits.pc + 4.U)
  ))




















  io.csr_data := csr_read(io.csr_addr)

  
  io.csr_cmm_op.ready := io.csr_cmm_op.valid & (
                (is_commit_comfirm(0) & io.rod_i(0).bits.is_csr) | 
                (is_commit_comfirm(1) & io.rod_i(1).bits.is_csr)		
              )












    val is_load_accessFault_ack_v =
      VecInit(
        io.lsu_cmm.is_accessFault & io.rod_i(0).bits.is_lu & ~is_wb(0),
        io.lsu_cmm.is_accessFault & io.rod_i(1).bits.is_lu & ~is_wb(1) & ~is_1st_solo
      )

    val is_store_accessFault_ack_v =
      VecInit(
        io.lsu_cmm.is_accessFault & io.rod_i(0).bits.is_su & ~is_wb(0),
        io.lsu_cmm.is_accessFault & io.rod_i(1).bits.is_su & ~is_wb(1) & ~is_1st_solo
      )

    val is_load_misAlign_ack_v =
      VecInit(
        io.lsu_cmm.is_misAlign & io.rod_i(0).bits.is_lu & ~is_wb(0),
        io.lsu_cmm.is_misAlign & io.rod_i(1).bits.is_lu & ~is_wb(1) & ~is_1st_solo
      )

    val is_store_misAlign_ack_v =
      VecInit(
        io.lsu_cmm.is_misAlign & io.rod_i(0).bits.is_su & ~is_wb(0),
        io.lsu_cmm.is_misAlign & io.rod_i(1).bits.is_su & ~is_wb(1) & ~is_1st_solo
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

    val is_retired_v = 
      VecInit(
        is_commit_comfirm(0) | io.is_commit_abort(0),
        is_commit_comfirm(1) | io.is_commit_abort(1)
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


	val is_trap_v = VecInit( is_interrupt | is_exception(0), is_interrupt | is_exception(1) )
  val is_xRet_v = VecInit( is_mRet(0) | is_sRet(0), is_mRet(2) | is_sRet(1) )



	override val rtc_clock = io.rtc_clock	
	override val exe_port = Mux( io.csr_cmm_op.ready, io.csr_cmm_op.bits, 0.U.asTypeOf(new Exe_Port))
	override val is_trap                 = is_trap_v.contains(true.B)
	override val is_mRet                 = is_mRet_v.contains(true.B)
	override val is_sRet                 = is_sRet_v.contains(true.B)
	override val commit_pc = Mux(is_1st_solo, io.rod_i(0).bits.pc, io.rod_i(1).bits.pc)
	override val ill_instr               = 0.U
	override val ill_vaddr               = io.lsu_cmm.trap_addr
	override val is_instr_accessFault    = is_instr_accessFault_v.contains(true.B)
	override val is_instr_illeage        = is_illeage_v.contains(true.B)
	override val is_breakPoint           = is_ebreak_v.contains(true.B)
	override val is_load_misAlign        = is_load_misAlign_ack_v.contains(true.B)
	override val is_load_accessFault     = is_load_accessFault_ack_v.contains(true.B)
	override val is_storeAMO_misAlign    = is_store_misAlign_ack_v.contains(true.B)
	override val is_storeAMO_accessFault = is_store_accessFault_ack_v.contains(true.B)
	override val is_ecall                = is_ecall_v.contains(true.B)
	override val is_instr_pageFault      = false.B
	override val is_load_pageFault       = false.B
	override val is_storeAMO_pageFault   = false.B
	override val retired_cnt = Mux( is_retired_v(1), 2.U, Mux(is_retired_v(0), 1.U, 0.U) )
	override val clint_sw_m = false.B
	override val clint_sw_s = false.B
	override val clint_tm_m = false.B
	override val clint_tm_s = false.B
	override val clint_ex_m = false.B
	override val clint_ex_s = false.B



}
