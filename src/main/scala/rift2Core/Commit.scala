
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

class Commit extends Privilege with Superscalar{
	val io = IO(new Bundle{

		val cm_op = Output(Vec(32, Vec(4, Bool())))
		val log = Input(Vec(32,Vec(4, UInt(2.W))) )

		val rod_i = Vec(2, Flipped(new DecoupledIO( new Info_reorder_i ) ))

		val cmm_lsu = Output(new Info_cmm_lsu)
		val lsu_cmm = Input( new Info_lsu_cmm )

		val cmm_bru_ilp = Output(Bool())

		val csr_addr = Input(UInt(12.W))
		val csr_data = Output(UInt(64.W))
		val csr_cmm_op = Flipped(DecoupledIO( new Csr_Port ) )

		val is_misPredict = Input(Bool())
		val is_commit_abort = Vec(2, Output( Bool() ))

		val cmm_pc = new ValidIO(new Info_cmm_pc)
	})

	override lazy val csrFiles = new CsrFiles

	val rd0_raw = VecInit( io.rod_i(1).bits.rd0_raw, io.rod_i(0).bits.rd0_raw )
	val rd0_idx = VecInit( io.rod_i(1).bits.rd0_idx, io.rod_i(0).bits.rd0_idx )

	val is_wb = VecInit(
						(io.log(rd0_raw(1))(rd0_idx(1)) === 3.U) & (io.rod_i(1).valid),
						(io.log(rd0_raw(0))(rd0_idx(0)) === 3.U) & (io.rod_i(0).valid)
					)

	//bru commit ilp
	io.cmm_bru_ilp := (io.rod_i(0).valid) & io.rod_i(0).bits.is_branch & (io.log(rd0_raw(0))(rd0_idx(0)) =/= 3.U)

	def is_1st_solo = io.is_commit_abort(0) | io.rod_i(0).bits.is_csr | ~io.rod_i(0).bits.is_su
	def is_2nd_solo = io.is_commit_abort(1)


	io.is_commit_abort(1) :=
		(io.rod_i(1).valid) & ( ( (io.rod_i(1).bits.is_branch) & io.is_misPredict ) | is_xRet(1) | is_trap(1) ) & is_1st_solo
	
	io.is_commit_abort(0) :=
		(io.rod_i(0).valid) & ( ( (io.rod_i(0).bits.is_branch) & io.is_misPredict ) | is_xRet(0) | is_trap(0) )


	//only one privilege can commit once
	val is_commit_comfirm = VecInit(
		is_wb(1) & ~io.is_commit_abort(1) & ~is_1st_solo,
		is_wb(0) & ~io.is_commit_abort(0)
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





	csrFiles.addr := io.csr_addr
	io.csr_data := csrFiles.read

	
	io.csr_cmm_op.ready := 
			(is_commit_comfirm(0) & io.rod_i(0).bits.is_csr) | 
			(is_commit_comfirm(1) & io.rod_i(1).bits.is_csr)

	csrFiles.port := io.csr_cmm_op.bits










	for ( i <- 0 until 2 ) yield {
		commit_pc(i) := io.rod_i(i).bits.pc
		is_load_accessFault_ack(i) := io.lsu_cmm.is_accessFault & io.rod_i(i).bits.is_lu & ~is_wb(i)
		is_store_accessFault_ack(i) := io.lsu_cmm.is_accessFault & io.rod_i(i).bits.is_su & ~is_wb(i)
		is_load_misAlign_ack(i) := io.lsu_cmm.is_misAlign & io.rod_i(i).bits.is_lu & ~is_wb(i)
		is_store_misAlign_ack(i) := io.lsu_cmm.is_misAlign & io.rod_i(i).bits.is_su & ~is_wb(i)

		is_ecall(i) := io.rod_i(i).bits.privil.ecall
		is_ebreak(i) := io.rod_i(i).bits.privil.ebreak
		is_instr_accessFault(i) := io.rod_i(i).bits.is_accessFault
		is_illeage(i) := io.rod_i(i).bits.is_illeage
		is_Mret(i) := io.rod_i(i).bits.privil.mret
		is_Sret(i) := io.rod_i(i).bits.privil.sret
		is_Uret(i) := io.rod_i(i).bits.privil.uret
	}
	lsu_trap_addr := io.lsu_cmm.trap_addr

	io.cmm_pc.valid := is_xRet.contains(true.B) | is_trap.contains(true.B)

	io.cmm_pc.bits.addr := MuxCase(0.U, Array(
		is_xRet(0) -> csrFiles.m_csrFiles.mepc.value,
		is_trap(0) -> csrFiles.m_csrFiles.mtvec.value,
		is_xRet(1) -> csrFiles.m_csrFiles.mepc.value,
		is_trap(1) -> csrFiles.m_csrFiles.mtvec.value		
	))

}
