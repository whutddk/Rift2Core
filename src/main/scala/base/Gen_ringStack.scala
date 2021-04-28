/*
* @Author: Ruige Lee
* @Date:   2021-04-12 11:52:53
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-28 11:19:02
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


package base

import chisel3._
import chisel3.util._





class Gen_ringStack[T<:Data]( dw: T, aw: Int ) extends Module {
	lazy val io = IO(new Bundle{
			val enq = Flipped(new DecoupledIO(dw))
			val deq  = new DecoupledIO(dw)

			val flush = Input(Bool())

	})

	def dp: Int = { var res = 1; for ( i <- 0 until aw ) { res = res * 2 }; return res }


	val buf = RegInit(VecInit(Seq.fill(dp)(0.U.asTypeOf(dw))))
	val btm_ptr = RegInit(0.U((aw+1).W))
	val top_ptr = RegInit(0.U((aw+1).W))

	def rd_idx = top_ptr(aw-1, 0) - 1.U
	def wr_idx = top_ptr(aw-1, 0)

	def is_empty = (btm_ptr === top_ptr)
	def is_full  = ((btm_ptr(aw-1, 1) === top_ptr(aw-1, 1)) & (btm_ptr(aw) =/= top_ptr(aw)))

	def is_enq_ack = io.enq.valid & io.enq.ready
	def is_deq_ack = io.deq.valid & io.deq.ready

	when( is_enq_ack ) {
		buf(wr_idx) := io.enq.bits
	}

	when(io.flush) {
		btm_ptr := 0.U
		top_ptr := 0.U
	}
	.otherwise{
		when( is_enq_ack ) {
			when(is_full) {
				btm_ptr := btm_ptr + 1.U			
			}
			top_ptr := top_ptr + 1.U
		}
		when( is_deq_ack ) {
			top_ptr := top_ptr - 1.U
		}
	}

	io.enq.ready := true.B
	io.deq.valid  := ~is_empty
	io.deq.bits   := buf(rd_idx)



	assert ( ~(is_enq_ack & is_deq_ack), "Assert Fail at RSA, RSA will never pop and push at the same times" )


}

