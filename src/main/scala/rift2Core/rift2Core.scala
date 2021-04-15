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

	})

	val pc_stage = new Pc_gen
	val if_stage = new Ifetch
	val iq_stage = new IQueue
	val ib_stage = new BranchPredict
	val id_stage = new Decode

	val dpt_stage = new Dispatch
	val iss_stage = new Issue
	val exe_stage = new Execute
	val iwb_stage = new WriteBack
	val cmm_stage = new Commit

	val i_regfiles = new Regfiles

}




