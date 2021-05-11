/*
* @File name: rift2chip_tb
* @Author: Ruige Lee
* @Email: wut.ruigeli@gmail.com
* @Date:   2021-04-21 15:17:49
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-05-11 19:56:16
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




module rift2chip_tb (

	
);

	reg CLK;
	reg RSTn;


	wire         io_mem_chn_ar_ready;
	wire         io_mem_chn_ar_valid;
	wire [31:0]  io_mem_chn_ar_bits_addr;
	wire [7:0]   io_mem_chn_ar_bits_len;
	wire [2:0]   io_mem_chn_ar_bits_size;
	wire [1:0]   io_mem_chn_ar_bits_burst;
	wire         io_mem_chn_r_ready;
	wire         io_mem_chn_r_valid;
	wire         io_mem_chn_r_bits_id;
	wire [127:0] io_mem_chn_r_bits_data;
	wire [1:0]   io_mem_chn_r_bits_rsp;
	wire         io_mem_chn_r_bits_last;
	wire         io_mem_chn_aw_ready;
	wire         io_mem_chn_aw_valid;
	wire [31:0]  io_mem_chn_aw_bits_addr;
	wire [7:0]   io_mem_chn_aw_bits_len;
	wire [2:0]   io_mem_chn_aw_bits_size;
	wire [1:0]   io_mem_chn_aw_bits_burst;
	wire         io_mem_chn_w_ready;
	wire         io_mem_chn_w_valid;
	wire [127:0] io_mem_chn_w_bits_data;
	wire [15:0]  io_mem_chn_w_bits_strb;
	wire         io_mem_chn_w_bits_last;
	wire         io_mem_chn_b_ready;
	wire         io_mem_chn_b_valid;
	wire [1:0]   io_mem_chn_b_bits_rsp;





Rift2Chip s_Rift2Chip(
	.clock(CLK),
	.reset(~RSTn),

	.io_mem_chn_ar_ready     (io_mem_chn_ar_ready),
	.io_mem_chn_ar_valid     (io_mem_chn_ar_valid),
	.io_mem_chn_ar_bits_id   (),
	.io_mem_chn_ar_bits_addr (io_mem_chn_ar_bits_addr),
	.io_mem_chn_ar_bits_len  (io_mem_chn_ar_bits_len),
	.io_mem_chn_ar_bits_size (io_mem_chn_ar_bits_size),
	.io_mem_chn_ar_bits_burst(io_mem_chn_ar_bits_burst),
	.io_mem_chn_ar_bits_lock (),
	.io_mem_chn_ar_bits_cache(),
	.io_mem_chn_ar_bits_port (),
	.io_mem_chn_ar_bits_qos  (),
	.io_mem_chn_ar_bits_user (),
	.io_mem_chn_r_ready      (io_mem_chn_r_ready),
	.io_mem_chn_r_valid      (io_mem_chn_r_valid),
	.io_mem_chn_r_bits_id    (1'b0),
	.io_mem_chn_r_bits_data  (io_mem_chn_r_bits_data),
	.io_mem_chn_r_bits_rsp   (io_mem_chn_r_bits_rsp),
	.io_mem_chn_r_bits_last  (io_mem_chn_r_bits_last),
	.io_mem_chn_r_bits_user  (1'b0),
	.io_mem_chn_aw_ready     (io_mem_chn_aw_ready),
	.io_mem_chn_aw_valid     (io_mem_chn_aw_valid),
	.io_mem_chn_aw_bits_id   (),
	.io_mem_chn_aw_bits_addr (io_mem_chn_aw_bits_addr),
	.io_mem_chn_aw_bits_len  (io_mem_chn_aw_bits_len),
	.io_mem_chn_aw_bits_size (io_mem_chn_aw_bits_size),
	.io_mem_chn_aw_bits_burst(io_mem_chn_aw_bits_burst),
	.io_mem_chn_aw_bits_lock (),
	.io_mem_chn_aw_bits_cache(),
	.io_mem_chn_aw_bits_port (),
	.io_mem_chn_aw_bits_qos  (),
	.io_mem_chn_aw_bits_user (),
	.io_mem_chn_w_ready      (io_mem_chn_w_ready),
	.io_mem_chn_w_valid      (io_mem_chn_w_valid),
	.io_mem_chn_w_bits_data  (io_mem_chn_w_bits_data),
	.io_mem_chn_w_bits_strb  (io_mem_chn_w_bits_strb),
	.io_mem_chn_w_bits_last  (io_mem_chn_w_bits_last),
	.io_mem_chn_w_bits_user  (),
	.io_mem_chn_b_ready      (io_mem_chn_b_ready),
	.io_mem_chn_b_valid      (io_mem_chn_b_valid),
	.io_mem_chn_b_bits_id    (1'b0),
	.io_mem_chn_b_bits_rsp   (io_mem_chn_b_bits_rsp),
	.io_mem_chn_b_bits_user  (1'b0)
);




axi_full_slv_sram # ( .DW(128), .AW(14) ) s_axi_full_slv_sram 
(

	.MEM_AWADDR(io_mem_chn_aw_bits_addr),
	.MEM_AWLEN(io_mem_chn_aw_bits_len),
	.MEM_AWSIZE(io_mem_chn_aw_bits_size),
	.MEM_AWBURST(io_mem_chn_aw_bits_burst),
	.MEM_AWVALID(io_mem_chn_aw_valid),
	.MEM_AWREADY(io_mem_chn_aw_ready),


	.MEM_WDATA(io_mem_chn_w_bits_data),
	.MEM_WSTRB(io_mem_chn_w_bits_strb),
	.MEM_WLAST(io_mem_chn_w_bits_last),
	.MEM_WVALID(io_mem_chn_w_valid),
	.MEM_WREADY(io_mem_chn_w_ready),

	.MEM_BRESP(io_mem_chn_b_bits_rsp),
	.MEM_BVALID(io_mem_chn_b_valid),
	.MEM_BREADY(io_mem_chn_b_ready),

	.MEM_ARADDR(io_mem_chn_ar_bits_addr),
	.MEM_ARLEN(io_mem_chn_ar_bits_len),
	.MEM_ARSIZE(io_mem_chn_ar_bits_size),
	.MEM_ARBURST(io_mem_chn_ar_bits_burst),
	.MEM_ARVALID(io_mem_chn_ar_valid),
	.MEM_ARREADY(io_mem_chn_ar_ready),

	.MEM_RDATA(io_mem_chn_r_bits_data),
	.MEM_RRESP(io_mem_chn_r_bits_rsp),
	.MEM_RLAST(io_mem_chn_r_bits_last),
	.MEM_RVALID(io_mem_chn_r_valid),
	.MEM_RREADY(io_mem_chn_r_ready),

	.CLK        (CLK),
	.RSTn       (RSTn)
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
	$dumpvars(0, rift2chip_tb);//tb模块名称
end

wire isEcall = 
	( s_Rift2Chip.i_rift2Core.cmm_stage.io_rod_i_0_bits_privil_ecall & s_Rift2Chip.i_rift2Core.cmm_stage.io_rod_i_0_valid ) | 
	( s_Rift2Chip.i_rift2Core.cmm_stage.io_rod_i_1_bits_privil_ecall & s_Rift2Chip.i_rift2Core.cmm_stage.io_rod_i_1_valid & s_Rift2Chip.i_rift2Core.cmm_stage.io_rod_i_0_ready );
wire [63:0] x3 = (s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 2'd0 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_3_0 : s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 2'd1 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_3_1 : s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 2'd2 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_3_2 : s_Rift2Chip.i_rift2Core.i_regfiles.files_3_3);


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



`define MEM s_axi_full_slv_sram.i_sram.ram
reg [7:0] mem [0:200000];

localparam DP = 2**14;
integer i, by;
initial begin
	$readmemh("./ci/rv64uc-p-rvc.verilog", mem);
	// $readmemh(testName, mem);
	
	for ( i = 0; i < DP; i = i + 1 ) begin
		for ( by = 0; by < 16; by = by + 1 ) begin
			if ( | mem[i*16+by] ) begin
				`MEM[i][8*by +: 8] = mem[i*16+by];
			end
			else begin
				`MEM[i][8*by +: 8] = 8'h0;
			end
		end


	end

end 



endmodule






