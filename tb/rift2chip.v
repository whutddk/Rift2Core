/*
* @File name: rift2chip
* @Author: Ruige Lee
* @Email: wut.ruigeli@gmail.com
* @Date:   2021-04-21 15:17:49
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-05-10 17:48:06
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




	wire         io_dl1_chn_a_ready;
	wire         io_dl1_chn_a_valid;
	wire [2:0]   io_dl1_chn_a_bits_opcode;
	wire [2:0]   io_dl1_chn_a_bits_param;
	wire [7:0]   io_dl1_chn_a_bits_size;
	wire [2:0]   io_dl1_chn_a_bits_source;
	wire [31:0]  io_dl1_chn_a_bits_address;
	wire [15:0]  io_dl1_chn_a_bits_mask;
	wire [127:0] io_dl1_chn_a_bits_data;
	wire         io_dl1_chn_a_bits_corrupt;
	wire         io_dl1_chn_d_ready;
	wire         io_dl1_chn_d_valid;
	wire [2:0]   io_dl1_chn_d_bits_opcode;
	wire [1:0]   io_dl1_chn_d_bits_param;
	wire [7:0]   io_dl1_chn_d_bits_size;
	wire [2:0]   io_dl1_chn_d_bits_source;
	wire [2:0]   io_dl1_chn_d_bits_sink;
	wire         io_dl1_chn_d_bits_denied;
	wire [127:0] io_dl1_chn_d_bits_data;
	wire         io_dl1_chn_d_bits_corrupt;









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
	.io_il1_chn_d_bits_corrupt(io_il1_chn_d_bits_corrupt),

	.io_dl1_chn_a_ready       (io_dl1_chn_a_ready),
	.io_dl1_chn_a_valid       (io_dl1_chn_a_valid),
	.io_dl1_chn_a_bits_opcode (io_dl1_chn_a_bits_opcode),
	.io_dl1_chn_a_bits_param  (io_dl1_chn_a_bits_param),
	.io_dl1_chn_a_bits_size   (io_dl1_chn_a_bits_size),
	.io_dl1_chn_a_bits_source (io_dl1_chn_a_bits_source),
	.io_dl1_chn_a_bits_address(io_dl1_chn_a_bits_address),
	.io_dl1_chn_a_bits_mask   (io_dl1_chn_a_bits_mask),
	.io_dl1_chn_a_bits_data   (io_dl1_chn_a_bits_data),
	.io_dl1_chn_a_bits_corrupt(io_dl1_chn_a_bits_corrupt),
	.io_dl1_chn_d_ready       (io_dl1_chn_d_ready),
	.io_dl1_chn_d_valid       (io_dl1_chn_d_valid),
	.io_dl1_chn_d_bits_opcode (io_dl1_chn_d_bits_opcode),
	.io_dl1_chn_d_bits_param  (io_dl1_chn_d_bits_param),
	.io_dl1_chn_d_bits_size   (io_dl1_chn_d_bits_size),
	.io_dl1_chn_d_bits_source (io_dl1_chn_d_bits_source),
	.io_dl1_chn_d_bits_sink   (io_dl1_chn_d_bits_sink),
	.io_dl1_chn_d_bits_denied (io_dl1_chn_d_bits_denied),
	.io_dl1_chn_d_bits_data   (io_dl1_chn_d_bits_data),
	.io_dl1_chn_d_bits_corrupt(io_dl1_chn_d_bits_corrupt)

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

tl_mem d_tl_mem(

	.tlslv_a_opcode (io_dl1_chn_a_bits_opcode),
	.tlslv_a_param  (io_dl1_chn_a_bits_param),
	.tlslv_a_size   (io_dl1_chn_a_bits_size),
	.tlslv_a_source (io_dl1_chn_a_bits_source),
	.tlslv_a_address(io_dl1_chn_a_bits_address),
	.tlslv_a_mask   (io_dl1_chn_a_bits_mask),
	.tlslv_a_data   (io_dl1_chn_a_bits_data),
	.tlslv_a_corrupt(io_dl1_chn_a_bits_corrupt),

	.tlslv_a_valid  (io_dl1_chn_a_valid),
	.tlslv_a_ready  (io_dl1_chn_a_ready),

	.tlslv_d_opcode (io_dl1_chn_d_bits_opcode),
	.tlslv_d_param  (io_dl1_chn_d_bits_param),
	.tlslv_d_size   (io_dl1_chn_d_bits_size),
	.tlslv_d_source (io_dl1_chn_d_bits_source),
	.tlslv_d_sink   (io_dl1_chn_d_bits_sink),
	.tlslv_d_denied (io_dl1_chn_d_bits_denied),
	.tlslv_d_data   (io_dl1_chn_d_bits_data),
	.tlslv_d_corrupt(),

	.tlslv_d_valid  (io_dl1_chn_d_valid),
	.tlslv_d_ready  (io_dl1_chn_d_ready),

	.clk(CLK),
	.rst(~RSTn)
	
);





reg [255:0] testName;

initial begin
	if ( $value$plusargs("%s",testName[255:0]) ) begin
		$display("%s",testName);
	end
end



initial begin
	CLK = 0;
	RSTn = 0;

	#20

	RSTn <= 1;

	#50000
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

wire isEcall = 
	( i_rift2Core.cmm_stage.io_rod_i_0_bits_privil_ecall & i_rift2Core.cmm_stage.io_rod_i_0_valid ) | 
	( i_rift2Core.cmm_stage.io_rod_i_1_bits_privil_ecall & i_rift2Core.cmm_stage.io_rod_i_1_valid & i_rift2Core.cmm_stage.io_rod_i_0_ready );
wire [63:0] x3 = (i_rift2Core.i_regfiles.archit_ptr_3 == 2'd0 ? i_rift2Core.i_regfiles.files_3_0 : i_rift2Core.i_regfiles.archit_ptr_3 == 2'd1 ? i_rift2Core.i_regfiles.files_3_1 : i_rift2Core.i_regfiles.archit_ptr_3 == 2'd2 ? i_rift2Core.i_regfiles.files_3_2 : i_rift2Core.i_regfiles.files_3_3);


reg sim_end = 0;
always @(posedge CLK)begin 
	if (isEcall) begin
		// result <= x3;
		#1 sim_end <= 1;

	end

end


always @(posedge CLK ) begin
	if ( sim_end == 1 ) begin
		if ( x3 == 64'd1 ) begin
			$display("PASS");
			$finish;
		end
		else begin
			$display("Fail");
			$stop;
		end
	end
end


`define ICCM i_tl_mem.ram
`define DCCM d_tl_mem.ram
reg [7:0] mem [0:200000];

localparam DP = 2**14;
integer i, by;
initial begin
	// $readmemh("./ci/rv64mi-p-ma_addr.verilog", mem);
	$readmemh(testName, mem);
	
	for ( i = 0; i < DP; i = i + 1 ) begin
		for ( by = 0; by < 16; by = by + 1 ) begin
			if ( | mem[i*16+by] ) begin
				`ICCM[i][8*by +: 8] = mem[i*16+by];
				`DCCM[i][8*by +: 8] = mem[i*16+by];
			end
			else begin
				`ICCM[i][8*by +: 8] = 8'h0;
				`DCCM[i][8*by +: 8] = 8'h0;
			end
		end


	end

end 



endmodule






