package rift2Core

/*
* @Author: Ruige Lee
* @Date:   2021-03-19 09:55:43
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-03-22 17:15:05
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


import chisel3._
import chisel3.util._
import rift2Core.basicElement._
import rift2Core.frontend._


class Decode extends Module {
	val io = IO( new Bundle {
		val iq_id = Flipped(new DecoupledIO(new Info_iq_id()))
		val id_dpt = new DecoupledIO(new Info_id_dpt())
		// val instr = Input(UInt(32.W))
		// val info = Output(UInt(32.W))
	})

	
	// io.id_dpt.bits.info := 
	// 	Mux( io.iq_id.bits.isrvc,
	// 	new decode16(io.iq_id.bits.instr).info,
	// 	new decode32(io.iq_id.bits.instr).info
	// 	)

	val d16 = Module(new Decode16())


	d16.io.instr := io.iq_id.bits.instr
	io.id_dpt.bits.info := d16.io.info
	io.id_dpt.valid := true.B
	io.iq_id.ready := true.B

	io.id_dpt.bits.isIFAccessFault := false.B
	io.id_dpt.bits.isIlleage := false.B
	io.id_dpt.bits.isRVC := io.iq_id.bits.isRVC
	io.id_dpt.bits.pc := io.iq_id.bits.pc


	// io.info := io.instr




}





