

/*
* @Author: Ruige Lee
* @Date:   2021-04-14 09:19:20
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-14 09:20:03
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


package rift2Core.backend

import chisel3._
import chisel3.util._

import rift2Core.define._

class WriteBack extends Module {
	val io = IO(new Bundle{
		val exe_iwb = Vec(5, (Flipped(new DecoupledIO(new Exe_iwb_info))))
		val wb_reg = Output(new Info_wb_reg)
	})


	val iwb_ack = { for ( i <- 0 until 5 ) yield { io.exe_iwb(i).valid & io.exe_iwb(i).ready} }
	for ( i <- 0 until 5 ) yield { io.exe_iwb(i).ready := true.B }

	val is_wb = Wire(Vec(32, Vec(4, Bool())))

	for ( i <- 0 until 32; j <- 0 until 4 ) yield {
		is_wb(i)(j) := 
			(iwb_ack(0) & (io.exe_iwb(0).bits.rd0_raw === i.U) & (io.exe_iwb(0).bits.rd0_idx === j.U)) |
			(iwb_ack(1) & (io.exe_iwb(1).bits.rd0_raw === i.U) & (io.exe_iwb(1).bits.rd0_idx === j.U)) |
			(iwb_ack(2) & (io.exe_iwb(2).bits.rd0_raw === i.U) & (io.exe_iwb(2).bits.rd0_idx === j.U)) |
			(iwb_ack(3) & (io.exe_iwb(3).bits.rd0_raw === i.U) & (io.exe_iwb(3).bits.rd0_idx === j.U)) |
			(iwb_ack(4) & (io.exe_iwb(4).bits.rd0_raw === i.U) & (io.exe_iwb(4).bits.rd0_idx === j.U))
	}


	for ( i <- 0 until 32; j <- 0 until 4 ) yield {
		io.wb_reg.enable(i)(j) := is_wb(i)(j)
		io.wb_reg.dnxt(i)(j) := MuxCase( DontCare, Array(
			(iwb_ack(0) & (io.exe_iwb(0).bits.rd0_raw === i.U) & (io.exe_iwb(0).bits.rd0_idx === j.U)) -> io.exe_iwb(0).bits.res,
			(iwb_ack(1) & (io.exe_iwb(1).bits.rd0_raw === i.U) & (io.exe_iwb(1).bits.rd0_idx === j.U)) -> io.exe_iwb(1).bits.res,
			(iwb_ack(2) & (io.exe_iwb(2).bits.rd0_raw === i.U) & (io.exe_iwb(2).bits.rd0_idx === j.U)) -> io.exe_iwb(2).bits.res,
			(iwb_ack(3) & (io.exe_iwb(3).bits.rd0_raw === i.U) & (io.exe_iwb(3).bits.rd0_idx === j.U)) -> io.exe_iwb(3).bits.res,
			(iwb_ack(4) & (io.exe_iwb(4).bits.rd0_raw === i.U) & (io.exe_iwb(4).bits.rd0_idx === j.U)) -> io.exe_iwb(4).bits.res
		) )
	}

	//alu, lsu and bru must always ready
}


