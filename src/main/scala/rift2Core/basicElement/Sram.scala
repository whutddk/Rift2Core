

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



class Sram(dw: Int, aw: Int) extends BlackBox(Map("DW" -> dw, "AW" -> aw)) with HasBlackBoxInline {
	val io = IO(new Bundle{
		val data_w = Input(UInt(dw.W))
		val addr_w = Input(UInt(aw.W))
		val mask_w = Input(UInt((dw/8).W))
		val en_w   = Input(Bool())

		val data_r = Output(UInt(dw.W))
		val addr_r = Input(UInt(aw.W))
		val en_r   = Input(Bool())

		val clk    = Input(Clock())
	})


	setInline("sram.v",
			"""
|			module sram #
|(
|	parameter DW = 32,
|	parameter AW = 14
|)
|(
|
|	input [DW-1:0] data_w,
|	input [AW-1:0] addr_w,
|	input [(DW+7)/8-1:0] mask_w,
|	input en_w,
|
|
|	output [DW-1:0] data_r,
|	input [AW-1:0] addr_r,
|	input en_r,
|
|	input clk
|
|);
|
|	localparam DP = 2**AW;
|	localparam DW_ZM = (DW+7)/8*8;
|
|	reg [DW_ZM-1:0] ram[0:DP-1];
|	reg [DW_ZM-1:0] data_r_reg;
|	wire [DW_ZM-1:0] data_w_zmask = {DW_ZM{1'b0}} | data_w;
|
|
|	generate
|		for ( genvar i = 0; i < (DW+7)/8; i = i + 1) begin
|			always @(posedge clk) begin
|				if (en_w) begin
|					if (mask_w[i]) begin
|						ram[addr_w][i*8+:8] <= #1 data_w_zmask[i*8+:8] ;					
|					end
|				end
|
|				if (en_r) begin
|					data_r_reg[i*8+:8] <= #1 ram[addr_r][i*8+:8];
|				end
|			end
|
|
|		end
|	endgenerate
|	
|	assign data_r = data_r_reg[DW-1:0];
|
|
|
|
|endmodule
			""".stripMargin)











	// def dp: Int = { var res = 1; for ( i <- 0 until aw ) { res = res * 2 } ;println("dp is:"+res); return res } 





	// val sram_reg = Reg(Vec( (dw/8), UInt(8.W)) )
	// val ram = SyncReadMem(dp, Vec( (dw/8), UInt(8.W)))

	// val data_w_vec = Wire( Vec( (dw/8), UInt(8.W) ) )
	// val mask_w_vec = Wire( Vec( (dw/8), Bool() ) )

	// when ( io.en_w ) {
	// 	ram.write( io.addr_w, data_w_vec, mask_w_vec )
	// }

	// when (io.en_r) {
	// 	sram_reg := ram.read(io.addr_r)
	// }

	// for ( i <- 0 until dw/8 ) {
	// 	data_w_vec(i) := io.data_w(8*i+7, 8*i)
	// 	mask_w_vec(i) := io.mask_w(i)
	// }


	// io.data_r := Cat( for ( i <- 0 until dw/8 ) yield {sram_reg((dw/8)-i-1)}  )




}

