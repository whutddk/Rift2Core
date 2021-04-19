/*
* @Author: Ruige Lee
* @Date:   2021-04-19 14:43:41
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-19 14:49:46
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


package rift2Chip

import chisel3._
import chisel3.util._
import rift2Core._
import rift2Core.basic._
import rift2Core.frontend._
import rift2Core.backend._
import rift2Core.cache._
import tilelink._

class Rift2Chip extends Module {
	val io = IO( new Bundle{

	})


	val i_rift2Core = Module( new Rift2Core )


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

	
}



