/*
* @Author: Ruige Lee
* @Date:   2021-03-18 16:11:48
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-03-23 19:17:01
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
import rift2Core.frontend._
import rift2Core.backend._
import rift2Core.cache._
import tilelink._

class Rift2Core extends Module {
	val io = IO(new Bundle{
		val il1_chn_a = new DecoupledIO(new TLchannel_a(128, 32))
		val il1_chn_d = Flipped(new DecoupledIO( new TLchannel_d(128) ))

		val dl1_chn_a = new DecoupledIO(new TLchannel_a(128, 32))
		val dl1_chn_d = Flipped(new DecoupledIO( new TLchannel_d(128) ))
		val sys_chn_a = new DecoupledIO(new TLchannel_a(64,32))
		val sys_chn_d = Flipped(new DecoupledIO(new TLchannel_d(64)))

		val l2c_fence_req = Output(Bool())
		val l3c_fence_req = Output(Bool())
		val l2c_fence_end = Input(Bool())
		val l3c_fence_end = Input(Bool())
	})

	lazy val pc_stage = Module(new Pc_gen)
	lazy val if_stage = Module(new Ifetch)
	lazy val iq_stage = Module(new Iqueue_ss)
	lazy val ib_stage = Module(new BranchPredict_ss)
	lazy val id_stage = Module(new Decode_ss)

	lazy val dpt_stage = Module(new Dispatch_ss)
	lazy val iss_stage = Module(new Issue)
	lazy val exe_stage = Module(new Execute)
	lazy val iwb_stage = Module(new WriteBack)
	lazy val cmm_stage = Module(new Commit)

	lazy val i_regfiles = Module(new Regfiles)

	if_stage.io.il1_chn_a <> io.il1_chn_a
	if_stage.io.il1_chn_d <> io.il1_chn_d

	exe_stage.io.dl1_chn_a <> io.dl1_chn_a
	exe_stage.io.dl1_chn_d <> io.dl1_chn_d
	exe_stage.io.sys_chn_a <> io.sys_chn_a
	exe_stage.io.sys_chn_d <> io.sys_chn_d

	exe_stage.io.l2c_fence_req <> io.l2c_fence_req
	exe_stage.io.l3c_fence_req <> io.l3c_fence_req
	exe_stage.io.l2c_fence_end <> io.l2c_fence_end
	exe_stage.io.l3c_fence_end <> io.l3c_fence_end

		pc_stage.io.ib_pc <> ib_stage.io.ib_pc  //is_jal | is_jalr | is_predict_taken | is_misPredict_taken | is_bru_iq_j_ack
		

		pc_stage.io.pc_iq <> iq_stage.io.pc_iq	//valid when flush for new pc 

		
		pc_stage.io.pc_if <> if_stage.io.pc_if
		if_stage.io.if_iq <> iq_stage.io.if_iq
		iq_stage.io.iq_ib <> ib_stage.io.iq_ib
		ib_stage.io.ib_id <> id_stage.io.ib_id
		id_stage.io.id_dpt <> dpt_stage.io.id_dpt

		dpt_stage.io.alu_dpt_iss <> iss_stage.io.alu_dpt_iss
		dpt_stage.io.bru_dpt_iss <> iss_stage.io.bru_dpt_iss
		dpt_stage.io.lsu_dpt_iss <> iss_stage.io.lsu_dpt_iss
		dpt_stage.io.csr_dpt_iss <> iss_stage.io.csr_dpt_iss
		dpt_stage.io.mul_dpt_iss <> iss_stage.io.mul_dpt_iss

		iss_stage.io.alu_iss_exe <> exe_stage.io.alu_iss_exe
		iss_stage.io.bru_iss_exe <> exe_stage.io.bru_iss_exe
		iss_stage.io.lsu_iss_exe <> exe_stage.io.lsu_iss_exe
		iss_stage.io.csr_iss_exe <> exe_stage.io.csr_iss_exe
		iss_stage.io.mul_iss_exe <> exe_stage.io.mul_iss_exe

		exe_stage.io.alu_exe_iwb <>	iwb_stage.io.exe_iwb(0)
		exe_stage.io.bru_exe_iwb <>	iwb_stage.io.exe_iwb(1)
		exe_stage.io.csr_exe_iwb <>	iwb_stage.io.exe_iwb(2)	
		exe_stage.io.lsu_exe_iwb <>	iwb_stage.io.exe_iwb(3)	
		exe_stage.io.mul_exe_iwb <>	iwb_stage.io.exe_iwb(4)


	if_stage.io.is_il1_fence_req := false.B
		

		ib_stage.io.bru_iq_b <> exe_stage.io.bru_iq_b
		ib_stage.io.bru_iq_j <> exe_stage.io.bru_iq_j

	ib_stage.io.fencei_end := false.B


	if_stage.io.flush  := cmm_stage.io.is_commit_abort(0) | cmm_stage.io.is_commit_abort(1) | ib_stage.io.ib_pc.valid
	ib_stage.io.flush  := cmm_stage.io.is_commit_abort(0) | cmm_stage.io.is_commit_abort(1)
	id_stage.io.flush  := cmm_stage.io.is_commit_abort(0) | cmm_stage.io.is_commit_abort(1)
	dpt_stage.io.flush := cmm_stage.io.is_commit_abort(0) | cmm_stage.io.is_commit_abort(1)
	iss_stage.io.flush := cmm_stage.io.is_commit_abort(0) | cmm_stage.io.is_commit_abort(1)
	exe_stage.io.flush := cmm_stage.io.is_commit_abort(0) | cmm_stage.io.is_commit_abort(1)
	i_regfiles.io.flush := cmm_stage.io.is_commit_abort(0) | cmm_stage.io.is_commit_abort(1)

	cmm_stage.io.is_misPredict := ib_stage.io.is_misPredict_taken



	cmm_stage.io.rod_i <> dpt_stage.io.rod_i
	cmm_stage.io.cmm_lsu <> exe_stage.io.cmm_lsu
	cmm_stage.io.lsu_cmm <> exe_stage.io.lsu_cmm
	cmm_stage.io.cmm_bru_ilp <> exe_stage.io.cmm_bru_ilp
	cmm_stage.io.csr_addr <> exe_stage.io.csr_addr
	cmm_stage.io.csr_data <> exe_stage.io.csr_data
	cmm_stage.io.csr_cmm_op <> exe_stage.io.csr_cmm_op

	cmm_stage.io.cmm_pc <> pc_stage.io.cmm_pc


	i_regfiles.io.wb_reg <> iwb_stage.io.wb_reg
	i_regfiles.io.rn_op <> dpt_stage.io.rn_op_i
	i_regfiles.io.cm_op <> cmm_stage.io.cm_op
	i_regfiles.io.files <> iss_stage.io.files
	i_regfiles.io.log <> cmm_stage.io.log
	i_regfiles.io.log <> iss_stage.io.log
	i_regfiles.io.log <> dpt_stage.io.log_i
	i_regfiles.io.rn_ptr <> dpt_stage.io.rn_ptr_i


	
}




