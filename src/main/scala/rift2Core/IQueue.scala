/*
* @Author: Ruige Lee
* @Date:   2021-04-12 16:52:15
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-12 16:52:17
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
import chisel3.util.random._

import chisel3.experimental.ChiselEnum
import rift2Core.basic._
import rift2Core.frontend._

class IQueue extends BranchPredict with IBuf{
    override val io = IO(new Bundle {
        val bru_iq_b = new DecoupledIO( Bool() )
		val bru_iq_j = new DecoupledIO( new Info_JTB )

		val iq_pc_info = new DecoupledIO(UInt(64.W))

		val flush = Input(Bool())
    })


    override def ori_pc = 0.U
    override def is_1st16 = false.B // = (ibuf_pop(0).valid === true.B) & ibuf_pop(0).bits(1,0) =/= "b11".U
	override def is_1st32 = false.B
	override def is_2nd16 = false.B // = (ibuf_pop(idx_2nd).valid === true.B) & ibuf_pop(idx_2nd).bits(1,0) =/= "b11".U
	override def is_2nd32 = false.B

}


