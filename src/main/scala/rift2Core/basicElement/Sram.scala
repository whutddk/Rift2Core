

/*
* @Author: Ruige Lee
* @Date:   2021-04-01 15:48:26
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-01 15:49:12
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


package rift2Core.basicElement

import chisel3._
import chisel3.util._
import chisel3.experimental.chiselName

@chiselName
class Sram(dw: Int, aw: Int) extends Module {
	val io = IO(new Bundle{
		val data_w = Input(UInt(dw.W))
		val addr_w = Input(UInt(aw.W))
		val mask_w = Input(UInt((dw/8).W))
		val en_w   = Input(Bool())

		val data_r = Output(UInt(dw.W))
		val addr_r = Input(UInt(aw.W))
		val en_r   = Input(Bool())
	})

	def dp: Int = { var res = 1; for ( i <- 0 until aw ) { res = res * 2 } ;println("dp is:"+res); return res } 





	val sram_reg = Reg(Vec( (dw/8), UInt(8.W)) )
	val ram = SyncReadMem(dp, Vec( (dw/8), UInt(8.W)))

	val data_w_vec = Wire( Vec( (dw/8), UInt(8.W) ) )
	val mask_w_vec = Wire( Vec( (dw/8), Bool() ) )

	when ( io.en_w ) {
		ram.write( io.addr_w, data_w_vec, mask_w_vec )
	}

	when (io.en_r) {
		sram_reg := ram.read(io.addr_r)
	}

	for ( i <- 0 until dw/8 ) {
		data_w_vec(i) := io.data_w(8*i+7, 8*i)
		mask_w_vec(i) := io.mask_w(i)
	}


	io.data_r := Cat( for ( i <- 0 until dw/8 ) yield {sram_reg((dw/8)-i-1)}  )




}

