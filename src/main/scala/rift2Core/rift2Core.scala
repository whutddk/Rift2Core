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

class Rift2Core extends Module {
	val io = IO(new Bundle{
		val il1_chn_a = new DecoupledIO(new TLchannel_a(128, 32))
		val il1_chn_d = Flipped(new DecoupledIO( new TLchannel_d(128) ))
	})

	val pc_stage = new Pc_gen
	val if_stage = new Ifetch
	val iq_stage = new Iqueue
	val ib_stage = new BranchPredict
	val id_stage = new Decode

	val dpt_stage = new Dispatch
	val iss_stage = new Issue
	val exe_stage = new Execute
	val iwb_stage = new WriteBack
	val cmm_stage = new Commit

	val i_regfiles = new Regfiles

	if_stage.io.il1_chn_a <> io.il1_chn_a
	if_stage.io.il1_chn_d <> io.il1_chn_d

		pc_stage.io.ib_pc <> ib_stage.io.ib_pc  //is_jal | is_jalr | is_predict_taken | is_misPredict_taken | is_bru_iq_j_ack
		pc_stage.io.cmm_pc

		pc_stage.io.pc_iq <> iq_stage.io.pc_iq	//valid when flush for new pc 

		
		pc_stage.io.pc_if <> if_stage.io.pc_if
		if_stage.io.if_iq <> iq_stage.io.if_iq
		iq_stage.io.iq_ib <> ib_stage.io.iq_ib




		if_stage.io.is_il1_fence_req
		if_stage.io.flush

		


		ib_stage.io.bru_iq_b
		ib_stage.io.bru_iq_j


		
		ib_stage.io.ib_id

		ib_stage.io.flush


}




