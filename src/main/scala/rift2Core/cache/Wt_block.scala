

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

package rift2Core.cache

import chisel3._
import chisel3.util._


class Write_info extends Bundle {
	val data = UInt(64.W)
	val addr = UInt(32.W)
	val wstrb = UInt(8.W)
}



class Wt_block( aw: Int ) {

		val data_i    = Wire( new Write_info )
		val data_o    = Wire( new Write_info )

		val commit    = Wire(Bool())
		val pop       = Wire(Bool())
		val push      = Wire(Bool())

		val full      = Wire(Bool())
		val empty     = Wire(Bool())

		val flush     = Wire(Bool())


	def dp: Int = { var res = 1; for ( i <- 0 until aw ) { res = res * 2 } ; return res } 
	println("Wt block created, dp is:"+dp);



	val info       = RegInit( VecInit( Seq.fill(dp)(0.U.asTypeOf(new Write_info) )))
	val is_valid   = RegInit( VecInit( Seq.fill(dp)(false.B)) )
	val is_commit  = RegInit( VecInit( Seq.fill(dp)(false.B)) ) 
	val rd_ptr     = RegInit(0.U((aw+1).W))
	val wr_ptr     = RegInit(0.U((aw+1).W))
	val cc_ptr     = RegInit(0.U((aw+1).W))


	when(flush) {
		wr_ptr := cc_ptr

		for ( i <- 0 until dp ) yield is_valid(i) := is_commit(i)
	}
	.elsewhen(pop) {
		rd_ptr := rd_ptr + 1.U

		is_valid(rd_ptr(aw-1,0))  := false.B
		is_commit(rd_ptr(aw-1,0)) := false.B
	}
	.elsewhen(push) {
		wr_ptr := wr_ptr + 1.U

		info(wr_ptr(aw-1,0)) := data_i
		is_valid(wr_ptr(aw-1,0))  := true.B
	}
	.elsewhen(commit) {
		cc_ptr := cc_ptr + 1.U

		is_commit(cc_ptr(aw-1,0)) := true.B
	}

	full  := (rd_ptr(aw-1,0) === wr_ptr(aw-1,0)) & ( rd_ptr(aw) =/= wr_ptr(aw) )
	empty := cc_ptr === rd_ptr

	data_o := info(rd_ptr(aw-1,0))

	def is_hazard(chk_addr: UInt, width_mask: UInt): Bool = {

		val cmp = Wire(Vec(dp, Bool()))

		for ( i <- 0 until dp ) yield {
			cmp(i) := ((info(i).addr & width_mask) === (chk_addr & width_mask)) & is_valid(i)
		}

		return cmp.contains(true.B)
	}

}

