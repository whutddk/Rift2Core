
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

import rift2Core.define._
import rift2Core.backend._
import chisel3.experimental.chiselName




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
		val csr_cmm_op = Flipped(DecoupledIO( new Exe_Port ) )

		val is_misPredict = Input(Bool())
		val is_commit_abort = Vec(2, Output( Bool() ))

		val cmm_pc = new ValidIO(new Info_cmm_pc)
	})

	val rd0_raw = VecInit( io.rod_i(0).bits.rd0_raw, io.rod_i(1).bits.rd0_raw )
	val rd0_idx = VecInit( io.rod_i(0).bits.rd0_idx, io.rod_i(1).bits.rd0_idx )

	val is_wb = VecInit(
						(io.log(rd0_raw(0))(rd0_idx(0)) === 3.U) & (io.rod_i(0).valid),
						(io.log(rd0_raw(1))(rd0_idx(1)) === 3.U) & (io.rod_i(1).valid)
					)

	//bru commit ilp
	io.cmm_bru_ilp := (io.rod_i(0).valid) & io.rod_i(0).bits.is_branch & (io.log(rd0_raw(0))(rd0_idx(0)) =/= 3.U)

	def is_1st_solo = io.is_commit_abort(0) | io.rod_i(0).bits.is_csr | io.rod_i(0).bits.is_su | ~is_wb(0) 
	def is_2nd_solo = io.is_commit_abort(1) | io.rod_i(1).bits.is_csr | io.rod_i(1).bits.is_su


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




	lsu_trap_addr := io.lsu_cmm.trap_addr

	val is_fence_i = VecInit( 	io.rod_i(0).bits.is_fence_i & is_commit_comfirm(0),
								io.rod_i(1).bits.is_fence_i & is_commit_comfirm(1)
							)

	io.cmm_pc.valid := is_xRet.contains(true.B) | is_trap.contains(true.B)
	io.cmm_pc.bits.addr := MuxCase(0.U, Array(
		is_xRet(0) -> m_csrFiles.mepc,
		is_trap(0) -> m_csrFiles.mtvec,
		is_xRet(1) -> m_csrFiles.mepc,
		is_trap(1) -> m_csrFiles.mtvec,
		is_fence_i(0) -> (io.rod_i(0).bits.pc + 4.U),
		is_fence_i(1) -> (io.rod_i(1).bits.pc + 4.U)
	))




















	io.csr_data := csr_read(io.csr_addr)

	
	io.csr_cmm_op.ready := io.csr_cmm_op.valid & (
								(is_commit_comfirm(0) & io.rod_i(0).bits.is_csr) | 
								(is_commit_comfirm(1) & io.rod_i(1).bits.is_csr)		
							)


	csr_access.exe_port := Mux( io.csr_cmm_op.ready, io.csr_cmm_op.bits, 0.U.asTypeOf(new Exe_Port))











		commit_pc := Mux(is_1st_solo, io.rod_i(0).bits.pc, io.rod_i(1).bits.pc)


		is_load_accessFault_ack(0) := io.lsu_cmm.is_accessFault & io.rod_i(0).bits.is_lu & ~is_wb(0)
		is_load_accessFault_ack(1) := io.lsu_cmm.is_accessFault & io.rod_i(1).bits.is_lu & ~is_wb(1) & ~is_1st_solo

		is_store_accessFault_ack(0) := io.lsu_cmm.is_accessFault & io.rod_i(0).bits.is_su & ~is_wb(0)
		is_store_accessFault_ack(1) := io.lsu_cmm.is_accessFault & io.rod_i(1).bits.is_su & ~is_wb(1) & ~is_1st_solo

		is_load_misAlign_ack(0) := io.lsu_cmm.is_misAlign & io.rod_i(0).bits.is_lu & ~is_wb(0)
		is_load_misAlign_ack(1) := io.lsu_cmm.is_misAlign & io.rod_i(1).bits.is_lu & ~is_wb(1) & ~is_1st_solo

		is_store_misAlign_ack(0) := io.lsu_cmm.is_misAlign & io.rod_i(0).bits.is_su & ~is_wb(0)
		is_store_misAlign_ack(1) := io.lsu_cmm.is_misAlign & io.rod_i(1).bits.is_su & ~is_wb(1) & ~is_1st_solo

		is_ecall(0) := io.rod_i(0).bits.privil.ecall
		is_ecall(1) := io.rod_i(1).bits.privil.ecall & ~is_1st_solo

		is_ebreak(0) := io.rod_i(0).bits.privil.ebreak
		is_ebreak(1) := io.rod_i(1).bits.privil.ebreak & ~is_1st_solo

		is_instr_accessFault(0) := io.rod_i(0).bits.is_accessFault
		is_instr_accessFault(1) := io.rod_i(1).bits.is_accessFault & ~is_1st_solo

		is_illeage(0) := io.rod_i(0).bits.is_illeage
		is_illeage(1) := io.rod_i(1).bits.is_illeage & ~is_1st_solo

		mp.is_Mret(0) := io.rod_i(0).bits.privil.mret
		mp.is_Mret(1) := io.rod_i(1).bits.privil.mret & ~is_1st_solo

		sp.is_Sret(0) := io.rod_i(0).bits.privil.sret
		sp.is_Sret(1) := io.rod_i(1).bits.privil.sret & ~is_1st_solo

		up.is_Uret(0) := io.rod_i(0).bits.privil.uret
		up.is_Uret(1) := io.rod_i(1).bits.privil.uret & ~is_1st_solo






}
