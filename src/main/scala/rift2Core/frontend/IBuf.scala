
/*
* @Author: Ruige Lee
* @Date:   2021-04-09 10:34:13
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-13 10:12:11
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

	val pc    = Wire(UInt(64.W))
	val instr = Wire(UInt(128.W))
	val align_instr = Wire(UInt(128.W))
	val align_mask  = Wire( UInt(8.W) )



	align_instr := Mux1H(Seq(
		(pc(3,1) === 0.U) -> instr(127,0),
		(pc(3,1) === 1.U) -> instr(127,16),
		(pc(3,1) === 2.U) -> instr(127,32),
		(pc(3,1) === 3.U) -> instr(127,48),
		(pc(3,1) === 4.U) -> instr(127,64),
		(pc(3,1) === 5.U) -> instr(127,80),
		(pc(3,1) === 6.U) -> instr(127,96),
		(pc(3,1) === 7.U) -> instr(127,112),
	))

	align_mask := Mux1H(Seq(
		(pc(3,1) === 0.U) -> Fill(8,1.U),
		(pc(3,1) === 1.U) -> Fill(7,1.U),
		(pc(3,1) === 2.U) -> Fill(6,1.U),
		(pc(3,1) === 3.U) -> Fill(5,1.U),
		(pc(3,1) === 4.U) -> Fill(4,1.U),
		(pc(3,1) === 5.U) -> Fill(3,1.U),
		(pc(3,1) === 6.U) -> Fill(2,1.U),
		(pc(3,1) === 7.U) -> Fill(1,1.U),
	))

	def out(i: Int): UInt = {
		return align_instr( 16*i+15, 16*i)
	} 
	
	def is_valid(i: Int): Bool = return Mux( align_mask(i) === 1.U, true.B, false.B )
	
}




trait IBuf {

	val ibuf_valid_i = Wire(Bool())
	val ibuf_ready_i = Wire(Bool())

	val ia = new IAlign()


	val ibuf = Module(new MultiPortFifo( UInt(16.W), 4, 8, 4 ) )

	for ( i <- 0 until 8 ) yield ibuf.io.enq(i).bits  := ia.out(i)
	for ( i <- 0 until 8 ) yield ibuf.io.enq(i).valid := ia.is_valid(i) & ibuf_valid_i

	ibuf_ready_i := 
		ibuf.io.enq(0).ready & ibuf.io.enq(1).ready & ibuf.io.enq(2).ready & ibuf.io.enq(3).ready & 
		ibuf.io.enq(4).ready & ibuf.io.enq(5).ready & ibuf.io.enq(6).ready & ibuf.io.enq(7).ready
	

	// def is_ibuf_i_ack: Bool = (ibuf.io.enq(0).valid & ~ibuf.io.enq(0).ready)


	for ( i <- 0 until 8 ) yield {
		assert( ~(ibuf.io.enq(i).valid & ~ibuf.io.enq(i).ready), "Assert Failed at ibuf enq")
	}

}


