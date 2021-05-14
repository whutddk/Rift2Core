

/*
* @Author: Ruige Lee
* @Date:   2021-03-24 11:59:20
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-28 11:19:33
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


abstract class Ppbuf[T <:Data](dw:T , dp: Int) extends Module {
	val io = IO(new Bundle{
		val push = Input(Bool())


		val pop  = Input(Bool())


		val data_i = Input(dw)
		val buf_o   = Vec(dp, Output(dw))
		val valid_o = Output(UInt(dp.W))

		val flush = Output(Bool())
	})

	def pop_idx: UInt

	val buf = Reg(Vec(dp, dw))
	val valid = Wire(UInt(dp.W))

	def full = valid.andR
	def empty = (~valid).andR


	def push_idx: UInt = {
		var i = 0
		while( i < dp && valid(i) == 1.U ){
			i = i + 1
		}
		return i.U
	}
	// MuxCase(0.U, {val pushArray = for ( i <- 0 until dp ) yield Array( valid(i) === true.B -> i.U)} )

	io.buf_o := buf
	io.valid_o := valid

	when( reset.asBool ) {
		for ( i <- 0 until dp) yield 
		{
			buf(i) := 0.U
			valid(i) := 0.U 			
		}

	}
	.elsewhen( io.flush ) {
		for ( i <- 0 until dp) yield {
			buf(i) := 0.U
			valid(i) := 0.U			
		}

	}
	.elsewhen( io.push ) {
		Reg(push_idx) := io.data_i
		valid(push_idx) := 1.U
	}
	.elsewhen( io.pop ) {
		valid(pop_idx) := 0.U
	}

	assert( ((io.push & ~full) || (io.pop & ~empty)), "Assert Fail at ppbuf" )
}

