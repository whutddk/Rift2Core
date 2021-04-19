/*
* @Author: Ruige Lee
* @Date:   2021-03-26 17:41:24
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-03-26 17:46:47
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
import chisel3.experimental.chiselName





//dw:data type aw:address width, in: input port num, out: output port num 

@chiselName
class MultiPortFifo[T<:Data]( dw: T, aw: Int, in: Int, out: Int ) extends Module{
	val io = IO(new Bundle{
		val enq = Vec(in, Flipped(new DecoupledIO(dw)) )
		val deq  = Vec(out, new DecoupledIO(dw) )

		val flush = Input(Bool())
	})


	def dp: Int = { var res = 1; for ( i <- 0 until aw ) { res = res * 2 }; return res }

	
	
	assert( (in < dp && out < dp) , "dp = "+dp+", in = "+in+", out = "+ out )

	val buf = RegInit(VecInit(Seq.fill(dp)(0.U.asTypeOf(dw))))
	val buf_valid = Reg(Vec(dp, Bool()))

	val rd_ptr = RegInit(0.U(aw.W))
	val wr_ptr = RegInit(0.U(aw.W))


	for ( i <- 0 until in )  yield io.enq(i).ready := (buf_valid((wr_ptr + i.U)(aw-1,0)) === false.B)
	for ( i <- 0 until out ) yield io.deq(i).valid := (buf_valid((rd_ptr + i.U)(aw-1,0)) === true.B)

	def is_enq_ack(i: Int) = io.enq(i.U).valid & io.enq(i.U).ready
	def is_deq_ack(i: Int) =  io.deq(i.U).valid  & io.deq(i.U).ready

	def enq_cnt: UInt = {
		val port = for ( i <- 0 until in ) yield { is_enq_ack(in-1-i) === true.B }  //in-1 ~ 0
		val cnt  = for ( i <- 0 until in ) yield { (in-1-i).U } //in-1 ~ 0

		return MuxCase( 0.U, port zip cnt )
	}
	
	def pop_cnt: UInt = {
		val port = for ( i <- 0 until out ) yield { is_enq_ack(out-1-i) === true.B }  //out-1 ~ 0
		val cnt  = for ( i <- 0 until out ) yield { (out-1-i).U } //out-1 ~ 0

		return MuxCase( 0.U, port zip cnt )
	}






	when (io.flush) {
		for ( i <- 0 until dp ) yield 	buf_valid(i) := false.B
		rd_ptr := 0.U
		wr_ptr := 0.U
	}
	.otherwise{
		for ( i <- 0 until in; j <- 0 until out ) yield {
			val fifo_ptr_w = (wr_ptr + i.U)(aw-1,0)
			val fifo_ptr_r = (rd_ptr + j.U)(aw-1,0)

			buf_valid(fifo_ptr_w) := Mux(is_enq_ack(i), true.B,  buf_valid(fifo_ptr_w))
			buf_valid(fifo_ptr_r) := Mux(is_deq_ack(j), false.B, buf_valid(fifo_ptr_r))

			buf(fifo_ptr_w) := Mux(is_enq_ack(i), io.enq(i).bits, buf(fifo_ptr_w))
		}



		rd_ptr := rd_ptr + pop_cnt
		wr_ptr := wr_ptr + enq_cnt
	}


	for ( i <- 0 until out ) yield {
		val fifo_ptr_r = (rd_ptr + i.U)(aw-1,0)
		io.deq(i).bits := Mux(buf_valid(fifo_ptr_r), buf(fifo_ptr_r), DontCare)
	}



	for ( i <- 0 until in; j <- 0 until in ) yield 
		assert( !(io.enq(i).valid == true.B && io.enq(j).valid == false.B && i >= j), "Assert Fail! in port illegal")

	for ( i <- 0 until out; j <- 0 until out ) yield 
		assert( !(io.deq(i).valid == true.B && io.deq(j).valid == false.B && i >= j), "Assert Fail! out port illegal")


}

