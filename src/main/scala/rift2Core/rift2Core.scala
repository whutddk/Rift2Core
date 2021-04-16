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

	val pc_stage = new Pc_gen
	val if_stage = new Ifetch
	val iq_stage = new Iqueue_ss
	val ib_stage = new BranchPredict_ss
	val id_stage = new Decode_ss

	val dpt_stage = new Dispatch_ss
	val iss_stage = new Issue
	val exe_stage = new Execute
	val iwb_stage = new WriteBack
	val cmm_stage = new Commit

	val i_regfiles = new Regfiles

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





		if_stage.io.is_il1_fence_req
		if_stage.io.flush

		ib_stage.io.bru_iq_b <> exe_stage.io.bru_iq_b
		ib_stage.io.bru_iq_j <> exe_stage.io.bru_iq_j

		ib_stage.io.flush

		id_stage.io.flush


		dpt_stage.io.rod_f = Vec(2,new DecoupledIO(new Info_reorder_f))
		dpt_stage.io.rn_ptr_f = Vec(32, Input(UInt(2.W))) 
		dpt_stage.io.log_f = Vec(32,Vec(4, Input(UInt(2.W))) )
		dpt_stage.io.rn_op_f = Vec(32, Vec(4, Output(Bool())))
		dpt_stage.io.fpu_dpt_iss = new DecoupledIO(new Fpu_dpt_info())
		dpt_stage.io.flush 


		iss_stage.io.regLog = Vec(32,Vec(4, Input(UInt(2.W))) )
		iss_stage.io.flush = Input(Bool())




	exe_stage.io.is_fence_commit = Input(Bool())
	exe_stage.io.is_store_commit = Input(Bool())
	exe_stage.io.flush = Input(Bool())



	cmm_stage.io.rod_i <> dpt_stage.io.rod_i
	cmm_stage.io.cmm_lsu = Vec(2, Output(new Info_cmm_lsu))
	cmm_stage.io.lsu_cmm = Input( new Info_lsu_cmm )
	cmm_stage.io.cmm_bru_ilp <> exe_stage.io.cmm_bru_ilp
	cmm_stage.io.csr_addr <> exe_stage.io.csr_addr
	cmm_stage.io.csr_data <> exe_stage.io.csr_data
	cmm_stage.io.csr_cmm_op <> exe_stage.io.csr_cmm_op
	cmm_stage.io.is_misPredict = Input(Bool())
	cmm_stage.io.cmm_pc <> pc_stage.io.cmm_pc


	i_regfiles.io.wb_reg <> iwb_stage.io.wb_reg
	i_regfiles.io.rn_op <> dpt_stage.io.rn_op_i
	i_regfiles.io.cm_op <> cmm_stage.io.cm_op
	i_regfiles.io.files <> iss_stage.io.files
	i_regfiles.io.log <> cmm_stage.io.log
	i_regfiles.io.log <> dpt_stage.io.log_i
	i_regfiles.io.rn_ptr <> dpt_stage.io.rn_ptr_i
	// i_regfiles.io.ar_ptr = Vec(32, Output(UInt(2.W))) 

	i_regfiles.io.flush = Input(Bool())
}




