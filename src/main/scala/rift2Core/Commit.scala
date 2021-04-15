
/*
* @Author: Ruige Lee
* @Date:   2021-04-14 11:24:59
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-14 11:25:13
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


package rift2Core

import chisel3._
import chisel3.util._

import rift2Core.basic._
import rift2Core.backend._

class Commit extends Module {
	val io = IO(new Bundle{

		val cm_op = Output(Vec(32, Vec(4, Bool())))
		val log = Input(Vec(32,Vec(4, UInt(2.W))) )

		val rod_i = Vec(2, Flipped(new DecoupledIO( new Info_reorder_i ) ))

		val cmm_lsu = Vec(2, Output(new Info_cmm_lsu))
		val lsu_cmm = Input( new Info_lsu_cmm )

		val cmm_bru_ilp = Output(Bool())

		val is_misPredict = Input(Bool())





		val cmm_pc = new DecoupledIO(new Info_cmm_pc)
	})



	val rd0_raw = VecInit( io.rod_i(1).bits.rd0_raw, io.rod_i(0).bits.rd0_raw )
	val rd0_idx = VecInit( io.rod_i(1).bits.rd0_idx, io.rod_i(0).bits.rd0_idx )

	val is_wb = VecInit(
						(io.log(rd0_raw(1))(rd0_idx(1)) === 3.U) & (io.rod_i(1).valid),
						(io.log(rd0_raw(0))(rd0_idx(0)) === 3.U) & (io.rod_i(0).valid)
					)

	//bru commit ilp
	io.cmm_bru_ilp := (io.rod_i(0).valid) & io.rod_i(0).bits.is_branch & (io.log(rd0_raw(0))(rd0_idx(0)) =/= 3.U)

	val is_commit_abort = VecInit(
		(io.rod_i(1).valid) & ( ( (io.rod_i(1).bits.is_branch) & io.is_misPredict ) | Privilege.is_xRet(1) | Privilege.is_trap(1) ),
		(io.rod_i(0).valid) & ( ( (io.rod_i(0).bits.is_branch) & io.is_misPredict ) | Privilege.is_xRet(0) | Privilege.is_trap(0) )
	)

	//only one privilege can commit once
	val is_commit_comfirm = VecInit(
		is_wb(1) & ~is_commit_abort(1) & is_wb(0) & ~is_commit_abort(0),
		is_wb(0) & ~is_commit_abort(0)
	)

	for ( i <- 0 until 32; j <- 0 until 4 ) yield {
		io.cm_op(i)(j) := 
			(is_commit_comfirm(0) & rd0_raw(0) === i.U & rd0_idx(0) === j.U) | 
			(is_commit_comfirm(1) & rd0_raw(1) === i.U & rd0_idx(1) === j.U)
	}

	io.rod_i(0).ready := is_commit_comfirm(0)
	io.rod_i(1).ready := is_commit_comfirm(1)

	io.cmm_lsu(0).is_fence_commit := io.rod_i(0).bits.is_fence & is_commit_comfirm(0)
	io.cmm_lsu(1).is_fence_commit := io.rod_i(1).bits.is_fence & is_commit_comfirm(1)
	io.cmm_lsu(0).is_store_commit := io.rod_i(0).bits.is_su & is_commit_comfirm(0)
	io.cmm_lsu(1).is_store_commit := io.rod_i(1).bits.is_su & is_commit_comfirm(1)




	val privilege = new Privilege

	for ( i <- 0 until 2 ) yield {
		privilege.commit_pc(i) := io.rod_i(i).bits.pc
		privilege.is_load_accessFault_ack(i) := io.lsu_cmm.is_accessFault & io.rod_i(i).bits.is_lu & ~is_wb(i)
		privilege.is_store_accessFault_ack(i) := io.lsu_cmm.is_accessFault & io.rod_i(i).bits.is_su & ~is_wb(i)
		privilege.is_load_misAlign_ack(i) := io.lsu_cmm.is_misAlign & io.rod_i(i).bits.is_lu & ~is_wb(i)
		privilege.is_store_misAlign_ack(i) := io.lsu_cmm.is_misAlign & io.rod_i(i).bits.is_su & ~is_wb(i)

		privilege.is_ecall(i) := io.rod_i(i).bits.privil.ecall
		privilege.is_ebreak(i) := io.rod_i(i).bits.privil.ebreak
		privilege.is_instr_accessFault(i) := io.rod_i(i).bits.is_accessFault
		privilege.is_illeage(i) := io.rod_i(i).bits.is_illeage
		privilege.is_Mret(i) := io.rod_i(i).bits.privil.mret
		privilege.is_Sret(i) := io.rod_i(i).bits.privil.sret
		privilege.is_Uret(i) := io.rod_i(i).bits.privil.uret
	}
	privilege.lsu_trap_addr := io.lsu_cmm.trap_addr

	io.cmm_pc.valid := Privilege.is_xRet.contains(true.B) | Privilege.is_trap.contains(true.B)

	io.cmm_pc.bits.addr := MuxCase(0.U, Array(
		Privilege.is_xRet(0) -> M_CsrFiles.mepc.value,
		Privilege.is_trap(0) -> M_CsrFiles.mtvec.value,
		Privilege.is_xRet(1) -> M_CsrFiles.mepc.value,
		Privilege.is_trap(1) -> M_CsrFiles.mtvec.value		
	))

}
