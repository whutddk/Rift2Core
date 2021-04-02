

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
import chisel3.experimental.chiselName

@chiselName
class Wt_block( dw: Int, aw: Int ) extends Module {
	val io = IO(new Bundle{

		val data_i    = Input(UInt(dw.W))
		val data_o    = Output(UInt(dw.W))

		val commit    = Input(Bool())
		val pop       = Input(Bool())
		val push      = Input(Bool())

		val flush     = Input(Bool())
	})

	def dp: Int = { var res = 1; for ( i <- 0 until aw ) { res = res * 2 } ;println("dp is:"+res); return res } 


	val info       = Reg( Vec(dp, UInt(dw.W))) 
	val is_valid   = Reg( Vec(dp, Bool()))
	val is_commit  = Reg( Vec(dp, Bool())) 
	val rd_ptr     = Reg(UInt((aw+1).W))
	val wr_ptr     = Reg(UInt((aw+1).W))
	val cc_ptr     = Reg(UInt((aw+1).W))


	when( reset.asBool ) {
		rd_ptr := 0.U
		wr_ptr := 0.U
		cc_ptr := 0.U
		
		for ( i <- 0 until dp ) yield {
			info(i)      := 0.U
			is_valid(i)  := false.B
			is_commit(i) := false.B
		}
	}
	.elsewhen(io.flush) {
		wr_ptr := cc_ptr

		for ( i <- 0 until dp ) yield is_valid(i) := is_commit(i)
	}
	.elsewhen(io.pop) {
		rd_ptr := rd_ptr + 1.U

		is_valid(rd_ptr(aw-1,0))  := false.B
		is_commit(rd_ptr(aw-1,0)) := false.B
	}
	.elsewhen(io.push) {
		wr_ptr := wr_ptr + 1.U

		info(wr_ptr(aw-1,0)) := io.data_i
		is_valid(wr_ptr(aw-1,0))  := true.B
	}
	.elsewhen(io.commit) {
		cc_ptr := cc_ptr + 1.U

		is_commit(cc_ptr(aw-1,0)) := true.B
	}

	def full  = (rd_ptr(aw-1,0) === wr_ptr(aw-1,0)) & ( rd_ptr(aw) =/= wr_ptr(aw) )
	def empty = cc_ptr === rd_ptr

	io.data_o := info(rd_ptr(aw-1,0))

	def is_hazard(chk_addr: UInt, width_mask: UInt): Bool = {
		var res = false.B
		for ( i <- 0 until dp ) {
			if ( (info(i) & width_mask) == (chk_addr & width_mask) ) {
				res = true.B
			}
		}
		return res
	}

}

