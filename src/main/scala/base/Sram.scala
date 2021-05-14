

/*
* @Author: Ruige Lee
* @Date:   2021-04-01 15:48:26
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-28 10:52:27
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
import chisel3.experimental.chiselName
import chisel3.util.experimental.loadMemoryFromFileInline

// @chiselName
// class Gen_sram(dw: Int, aw: Int) extends BlackBox(Map("DW" -> dw, "AW" -> aw)) with HasBlackBoxResource {
// 	val io = IO(new Bundle{
// 		val data_w = Input(UInt(dw.W))
// 		val addr_w = Input(UInt(aw.W))
// 		val data_wstrb = Input(UInt(((dw+7)/8).W))
// 		val en_w   = Input(Bool())

// 		val data_r = Output(UInt(dw.W))
// 		val addr_r = Input(UInt(aw.W))
// 		val en_r   = Input(Bool())

// 		val clk    = Input(Clock())
// 	})

// 	addResource("/gen_sram.v")



// }


@chiselName
class Sram(dw: Int, aw: Int) extends Module {
	val io = IO(new Bundle{
		val data_w = Input(UInt(dw.W))
		val addr_w = Input(UInt(aw.W))
		val data_wstrb = Input(UInt(((dw+7)/8).W))
		val en_w   = Input(Bool())

		val data_r = Output(UInt(dw.W))
		val addr_r = Input(UInt(aw.W))
		val en_r   = Input(Bool())

		// val clk    = Input(Clock())
	})

	def dp: Int = { var res = 1; for ( i <- 0 until aw ) { res = res * 2 }; return res }
	def byte_cnt = (dw+7)/8

	val ram = Mem( dp, Vec( byte_cnt, UInt(8.W) ) )

	val data_i = Wire( Vec( byte_cnt, UInt(8.W) ) )
	val data_o = RegInit( VecInit( Seq.fill(byte_cnt)( 0.U(8.W) ) ))
	val mask   = Wire( Vec( byte_cnt, Bool() ) )

	for ( i <- 0 until byte_cnt-1 ) yield  data_i(i) := io.data_w(8*i+7, 8*i) 
	data_i(byte_cnt-1) := io.data_w(dw-1, 8*(byte_cnt-1))

	for ( i <- 0 until byte_cnt ) yield  mask(i) := io.data_wstrb(i).asBool


	when( io.en_w ) {
		ram.write(io.addr_w, data_i, mask)
	}

	when( io.en_r ){
		data_o := ram.read(io.addr_r)
	}

	io.data_r := Cat( for ( i <- 0 until byte_cnt) yield data_o(byte_cnt-i-1) )
	

	// loadMemoryFromFileInline(ram, "rv64mi-p-ma_fetch")
}


