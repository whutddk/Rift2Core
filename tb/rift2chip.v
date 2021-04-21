/*
* @File name: rift2chip
* @Author: Ruige Lee
* @Email: wut.ruigeli@gmail.com
* @Date:   2021-04-21 15:17:49
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-21 19:10:21
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


`timescale 1 ns / 1 ps




module rift2chip (

	
);

	reg CLK;
	reg RSTn;


	wire         io_il1_chn_a_ready;
	wire         io_il1_chn_a_valid;
	wire [2:0]   io_il1_chn_a_bits_opcode;
	wire [2:0]   io_il1_chn_a_bits_param;
	wire [7:0]   io_il1_chn_a_bits_size;
	wire [2:0]   io_il1_chn_a_bits_source;
	wire [31:0]  io_il1_chn_a_bits_address;
	wire [15:0]   io_il1_chn_a_bits_mask;
	wire [127:0] io_il1_chn_a_bits_data;
	wire         io_il1_chn_a_bits_corrupt;
	wire         io_il1_chn_d_ready;
	wire         io_il1_chn_d_valid;
	wire [2:0]   io_il1_chn_d_bits_opcode;
	wire [1:0]   io_il1_chn_d_bits_param;
	wire [7:0]   io_il1_chn_d_bits_size;
	wire [2:0]   io_il1_chn_d_bits_source;
	wire [2:0]   io_il1_chn_d_bits_sink;
	wire         io_il1_chn_d_bits_denied;
	wire [127:0] io_il1_chn_d_bits_data;
	wire         io_il1_chn_d_bits_corrupt;














Rift2Core i_rift2Core(
	.clock(CLK),
	.reset(~RSTn),

	.io_il1_chn_a_ready       (io_il1_chn_a_ready),
	.io_il1_chn_a_valid       (io_il1_chn_a_valid),
	.io_il1_chn_a_bits_opcode (io_il1_chn_a_bits_opcode),
	.io_il1_chn_a_bits_param  (io_il1_chn_a_bits_param),
	.io_il1_chn_a_bits_size   (io_il1_chn_a_bits_size),
	.io_il1_chn_a_bits_source (io_il1_chn_a_bits_source),
	.io_il1_chn_a_bits_address(io_il1_chn_a_bits_address),
	.io_il1_chn_a_bits_mask   (io_il1_chn_a_bits_mask),
	.io_il1_chn_a_bits_data   (io_il1_chn_a_bits_data),
	.io_il1_chn_a_bits_corrupt(io_il1_chn_a_bits_corrupt),
	.io_il1_chn_d_ready       (io_il1_chn_d_ready),
	.io_il1_chn_d_valid       (io_il1_chn_d_valid),
	.io_il1_chn_d_bits_opcode (io_il1_chn_d_bits_opcode),
	.io_il1_chn_d_bits_param  (io_il1_chn_d_bits_param),
	.io_il1_chn_d_bits_size   (io_il1_chn_d_bits_size),
	.io_il1_chn_d_bits_source (io_il1_chn_d_bits_source),
	.io_il1_chn_d_bits_sink   (io_il1_chn_d_bits_sink),
	.io_il1_chn_d_bits_denied (io_il1_chn_d_bits_denied),
	.io_il1_chn_d_bits_data   (io_il1_chn_d_bits_data),
	.io_il1_chn_d_bits_corrupt(io_il1_chn_d_bits_corrupt)

);


tl_mem i_tl_mem(

	.tlslv_a_opcode(io_il1_chn_a_bits_opcode),
	.tlslv_a_param(io_il1_chn_a_bits_param),
	.tlslv_a_size(io_il1_chn_a_bits_size),
	.tlslv_a_source(io_il1_chn_a_bits_source),
	.tlslv_a_address(io_il1_chn_a_bits_address),
	.tlslv_a_mask(io_il1_chn_a_bits_mask),
	.tlslv_a_data(io_il1_chn_a_bits_data),
	.tlslv_a_corrupt(io_il1_chn_a_bits_corrupt),

	.tlslv_a_valid(io_il1_chn_a_valid),
	.tlslv_a_ready(io_il1_chn_a_ready),

	.tlslv_d_opcode(io_il1_chn_d_bits_opcode),
	.tlslv_d_param(io_il1_chn_d_bits_param),
	.tlslv_d_size(io_il1_chn_d_bits_size),
	.tlslv_d_source(io_il1_chn_d_bits_source),
	.tlslv_d_sink(io_il1_chn_d_bits_sink),
	.tlslv_d_denied(io_il1_chn_d_bits_denied),
	.tlslv_d_data(io_il1_chn_d_bits_data),
	.tlslv_d_corrupt(),

	.tlslv_d_valid(io_il1_chn_d_valid),
	.tlslv_d_ready(io_il1_chn_d_ready),

	.clk(CLK),
	.rst(~RSTn)
	
);



initial begin
	CLK = 0;
	RSTn = 0;

	#20

	RSTn <= 1;

	#5000
			$display("Time Out !!!");
	$stop;
end


initial begin
	forever
	begin 
		 #5 CLK <= ~CLK;
	end
end



initial
begin
	$dumpfile("./build/wave.vcd"); //生成的vcd文件名称
	$dumpvars(0, rift2chip);//tb模块名称
end

// always @(negedge CLK)begin 
// 	if (isEcall) begin
// 		if ( x3 == 64'd1 ) begin
// 			$display("PASS");
// 			$finish;
// 		end
// 		else begin
// 			$display("Fail");
// 			$stop;
// 		end

// 	end

// end









endmodule






