

/*
* @Author: Ruige Lee
* @Date:   2021-04-01 15:48:26
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-01 15:49:12
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


package rift2Core.basic

import chisel3._
import chisel3.util._



class Gen_sram(dw: Int, aw: Int) extends BlackBox(Map("DW" -> dw, "AW" -> aw)) with HasBlackBoxResource {
	val io = IO(new Bundle{
		val data_w = Input(UInt(dw.W))
		val addr_w = Input(UInt(aw.W))
		val data_wstrb = Input(UInt((dw/8).W))
		val en_w   = Input(Bool())

		val data_r = Output(UInt(dw.W))
		val addr_r = Input(UInt(aw.W))
		val en_r   = Input(Bool())

		val clk    = Input(Clock())
	})

	setResource("/gen_sram.v")



}

