/*
* @Author: Ruige Lee
* @Date:   2021-04-13 20:07:05
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-13 20:07:28
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

class Pc_gen extends Module {
	val io = IO(new Bundle{
		val ib_pc = Flipped(new DecoupledIO( new Info_ib_pc) )
		val cmm_pc = Flipped ( new DecoupledIO(new Info_cmm_pc))
		val pc_if = new DecoupledIO( new Info_pc_if )

	})

	io.cmm_pc.ready := true.B
	io.ib_pc.ready  := true.B
	io.pc_if.valid := true.B

	def is_cmm_pc_ack = io.cmm_pc.valid & io.cmm_pc.ready
	def is_ib_pc_ack = io.ib_pc.valid & io.ib_pc.ready
	def is_pc_if_ack = io.pc_if.valid & io.pc_if.ready

	val addr = RegInit("h80000000".U(32.W))


	when ( is_cmm_pc_ack | is_ib_pc_ack | is_pc_if_ack ) {
		when( is_cmm_pc_ack ){
			addr := io.cmm_pc.bits.addr
		}
		.elsewhen(is_ib_pc_ack ) {
			addr := io.ib_pc.bits.addr
		}
		.otherwise{
			addr := (addr + 16.U) & ~("b1111".U(32.W))
		}
	}

	io.pc_if.bits.addr := addr


}
