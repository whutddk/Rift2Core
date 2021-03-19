package rift2Core

/*
* @Author: Ruige Lee
* @Date:   2021-03-19 09:55:43
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-03-19 10:40:20
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


class decode extends Module {
	val io = IO (
		new Bundle {
			val iq_id = Flipped(new DecoupledIO(new Info_iq_id()))
			val id_dpt = new DecoupledIO(new Info_id_dpt())

		}
	)

	val instr32 = Wire(UInt(32.W))
	val instr16 = Wire(UInt(16.W))

	instr32 := io.iq_id.bits.iq_id_instr
	instr16 := io.iq_id.bits.iq_id_instr(15,0)


	












}





