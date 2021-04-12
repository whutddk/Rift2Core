
/*
* @Author: Ruige Lee
* @Date:   2021-04-09 10:34:13
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-09 10:34:13
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


package rift2Core.frontend

import chisel3._
import chisel3.util._
import rift2Core.basic._

class IAlign{
	val io = new Bundle{
		val pc    = Wire(UInt(64.W))
		val instr = Wire(UInt(128.W))
		val align_instr = Wire(UInt(128.W))
		val align_cnt  = Wire(UInt(8.W))      // 16bits has a mask		
	}

	io.align_instr := Mux1H(Seq(
		(io.pc(3,1) === 0.U) -> io.instr(127,0),
		(io.pc(3,1) === 1.U) -> io.instr(127,16),
		(io.pc(3,1) === 2.U) -> io.instr(127,32),
		(io.pc(3,1) === 3.U) -> io.instr(127,48),
		(io.pc(3,1) === 4.U) -> io.instr(127,64),
		(io.pc(3,1) === 5.U) -> io.instr(127,80),
		(io.pc(3,1) === 6.U) -> io.instr(127,96),
		(io.pc(3,1) === 7.U) -> io.instr(127,112),
	))

	io.align_cnt := 8.U - io.pc(3,1)
	
	
}




trait IBuf {

	val ibuf_valid_i = Wire(Bool())
	val ibuf_ready_i = Wire(Bool())

	// val instr_buf  = RegInit(0.U(256.W))
	// val instr_cnt = RegInit(0.U(5.W))    //can be 0 ~16

	// val ibuf_valid_o = Wire(Vec(2,Bool()))
	// val ibuf_ready_o = Wire(Vec(2,Bool()))

	val ia = new IAlign()

	// def ibuf_o_ack(i: UInt) = ibuf_valid_o(i) & ibuf_ready_o(i)


	val instr_buf = new MultiPortFifo( UInt(16.W), 4, 8, 2 ) 







	// assert ( ~(ibuf_o_ack(1) === true.B & ibuf_o_ack(0) === false.B), "Assert Failed at IBuf" )
}


