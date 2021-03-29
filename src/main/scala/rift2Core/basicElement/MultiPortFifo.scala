/*
* @Author: Ruige Lee
* @Date:   2021-03-26 17:41:24
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-03-26 17:46:47
*/


package rift2Core.basicElement

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

// // package of input port
// class Fifo_port_i[T<:Data](private val dw: T) extends Bundle {
// 	val valid = Input(Bool())
// 	val data = Input(dw.cloneType)
// 	val ready = Output(Bool())
// }

// // package of output port
// class Fifo_port_o[T<:Data](private val dw: T) extends Bundle {
// 	val valid = Output(Bool())
// 	val data = Output(dw.cloneType)
// 	val ready = Input(Bool())
// }

//dw:data type aw:address width, in: input port num, out: output port num 
class MultiPortFifo[T<:Data]( dw: T, aw: Int, in: Int, out: Int ) extends Module {
	val io = IO(new Bundle{
		// val push = Vec(in,  new Fifo_port_i(dw.cloneType))
		// val pop  = Vec(out, new Fifo_port_o(dw.cloneType))


		val push = Vec(in, Flipped(new DecoupledIO(dw)) )
		val pop  = Vec(out, new DecoupledIO(dw) )

		val flush = Input(Bool())
	})


	def dp: Int = {
		var res = 1
		for ( i <- 0 until aw ) {
			res = res * 2
		}
		return res
	}
	
	scala.math.pow(2,aw).toInt
	assert( (in < dp && out < dp) , "dp = "+dp+", in = "+in+", out = "+ out )

	val buf = Reg(Vec(dp, dw))
	val buf_valid = Reg(Vec(dp, Bool()))

	val rd_ptr = Reg(UInt(aw.W))
	val wr_ptr = Reg(UInt(aw.W))


	for ( i <- 0 until in) yield  io.push(i).ready := (buf_valid((wr_ptr + i.U)(aw-1,0)) === false.B)
	for ( i <- 0 until out ) yield io.pop(i).valid := (buf_valid((rd_ptr + i.U)(aw-1,0)) === true.B)

	def push_ack(i: UInt) = io.push(i).valid & io.push(i).ready
	def pop_ack(i: UInt) =  io.pop(i).valid  & io.pop(i).ready

	def push_cnt: UInt = {
		var cnt = 0.U
		for ( i <- 0 until in ) {
			if ( push_ack(i.U) == true.B ) {
				cnt = i.U
			}
		}
		return cnt
	}

	def pop_cnt: UInt = {
		var cnt = 0.U
		for ( i <- 0 until out ) {
			if ( pop_ack(i.U) == true.B ) {
				cnt = i.U
			}
		}
		return cnt
	}






	when (reset.asBool() | io.flush) {
		for ( i <- 0 until dp ) yield 	buf_valid(i) := false.B
		rd_ptr := 0.U
		wr_ptr := 0.U
	}
	.otherwise{
		for ( i <- 0 until in; j <- 0 until out ) {
			val fifo_ptr_w = (wr_ptr + i.U)(aw-1,0)
			val fifo_ptr_r = (rd_ptr + j.U)(aw-1,0)

			buf_valid(fifo_ptr_w) := Mux(push_ack(i.U), true.B, buf_valid(fifo_ptr_w))
			buf_valid(fifo_ptr_r) := Mux(pop_ack(j.U),  false.B, buf_valid(fifo_ptr_r))

			buf(fifo_ptr_w) := Mux(push_ack(i.U), io.push(i.U).bits, buf(fifo_ptr_w))
		}






		// for ( i <- 0 until dp ) yield {
		// 	def is_push(i: UInt): Bool =  

		// 	buf_valid(i) :=
		// 	buf(i) := 
		// }



		rd_ptr := rd_ptr + pop_cnt
		wr_ptr := wr_ptr + push_cnt
	}


	for ( i <- 0 until out ) yield {
		val fifo_ptr_r = (rd_ptr + i.U)(aw-1,0)
		io.pop(i.U).bits := Mux(buf_valid(fifo_ptr_r), buf(fifo_ptr_r), DontCare)
	}



	for ( i <- 0 until in; j <- 0 until in ) yield 
		assert( !(io.push(i.U).valid == true.B && io.push(j.U).valid == false.B && i >= j), "Assert Fail! in port illegal")

	for ( i <- 0 until out; j <- 0 until out ) yield 
		assert( !(io.pop(i.U).valid == true.B && io.pop(j.U).valid == false.B && i >= j), "Assert Fail! out port illegal")


}

