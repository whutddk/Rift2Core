


/*
  Copyright (c) 2020 - 2022 Wuhan University of Technology <295054118@whut.edu.cn>

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
	reg rtc_clock;
	reg RSTn;


	wire         io_mem_chn_ar_ready;
	wire         io_mem_chn_ar_valid;
	wire [31:0]  io_mem_chn_ar_bits_addr;
	wire [7:0]   io_mem_chn_ar_bits_len;
	wire [2:0]   io_mem_chn_ar_bits_size;
	wire [1:0]   io_mem_chn_ar_bits_burst;
	wire         io_mem_chn_r_ready;
	wire         io_mem_chn_r_valid;

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




	wire         io_sys_chn_ar_ready;
	wire         io_sys_chn_ar_valid;
	wire         io_sys_chn_ar_bits_id;
	wire [31:0]  io_sys_chn_ar_bits_addr;
	wire [7:0]   io_sys_chn_ar_bits_len;
	wire [2:0]   io_sys_chn_ar_bits_size;
	wire [1:0]   io_sys_chn_ar_bits_burst;
	wire         io_sys_chn_ar_bits_lock;
	wire [3:0]   io_sys_chn_ar_bits_cache;
	wire [2:0]   io_sys_chn_ar_bits_port;
	wire [3:0]   io_sys_chn_ar_bits_qos;
	wire         io_sys_chn_ar_bits_user;
	wire         io_sys_chn_r_ready;
	wire         io_sys_chn_r_valid;
	wire         io_sys_chn_r_bits_id;
	wire [63:0]  io_sys_chn_r_bits_data;
	wire [1:0]   io_sys_chn_r_bits_rsp;
	wire         io_sys_chn_r_bits_last;
	wire         io_sys_chn_r_bits_user;
	wire         io_sys_chn_aw_ready;
	wire         io_sys_chn_aw_valid;
	wire         io_sys_chn_aw_bits_id;
	wire [31:0]  io_sys_chn_aw_bits_addr;
	wire [7:0]   io_sys_chn_aw_bits_len;
	wire [2:0]   io_sys_chn_aw_bits_size;
	wire [1:0]   io_sys_chn_aw_bits_burst;
	wire         io_sys_chn_aw_bits_lock;
	wire [3:0]   io_sys_chn_aw_bits_cache;
	wire [2:0]   io_sys_chn_aw_bits_port;
	wire [3:0]   io_sys_chn_aw_bits_qos;
	wire         io_sys_chn_aw_bits_user;
	wire         io_sys_chn_w_ready;
	wire         io_sys_chn_w_valid;
	wire [63:0]  io_sys_chn_w_bits_data;
	wire [7:0]   io_sys_chn_w_bits_strb;
	wire         io_sys_chn_w_bits_last;
	wire         io_sys_chn_w_bits_user;
	wire         io_sys_chn_b_ready;
	wire         io_sys_chn_b_valid;
	wire         io_sys_chn_b_bits_id;
	wire [1:0]   io_sys_chn_b_bits_rsp;
	wire         io_sys_chn_b_bits_user;

wire [3:0] io_mem_chn_ar_bits_id;
wire [3:0] io_mem_chn_r_bits_id;
wire [3:0] io_mem_chn_aw_bits_id;
wire [3:0] io_mem_chn_b_bits_id;

Rift2Chip s_Rift2Chip(
	.clock(CLK),
	.reset(~RSTn),

	.io_sys_chn_ar_ready     (io_sys_chn_ar_ready),
	.io_sys_chn_ar_valid     (io_sys_chn_ar_valid),
	.io_sys_chn_ar_bits_id   (),
	.io_sys_chn_ar_bits_addr (io_sys_chn_ar_bits_addr),
	.io_sys_chn_ar_bits_len  (),
	.io_sys_chn_ar_bits_size (),
	.io_sys_chn_ar_bits_burst(),
	.io_sys_chn_ar_bits_lock (),
	.io_sys_chn_ar_bits_cache(),
	.io_sys_chn_ar_bits_port (),
	.io_sys_chn_ar_bits_qos  (),
	.io_sys_chn_ar_bits_user (),
	.io_sys_chn_r_ready      (io_sys_chn_r_ready),
	.io_sys_chn_r_valid      (io_sys_chn_r_valid),
	.io_sys_chn_r_bits_id    (1'b0),
	.io_sys_chn_r_bits_data  (io_sys_chn_r_bits_data),
	.io_sys_chn_r_bits_rsp   (io_sys_chn_r_bits_rsp),
	.io_sys_chn_r_bits_last  (1'b1),
	.io_sys_chn_r_bits_user  (1'b0),
	.io_sys_chn_aw_ready     (io_sys_chn_aw_ready),
	.io_sys_chn_aw_valid     (io_sys_chn_aw_valid),
	.io_sys_chn_aw_bits_id   (),
	.io_sys_chn_aw_bits_addr (io_sys_chn_aw_bits_addr),
	.io_sys_chn_aw_bits_len  (),
	.io_sys_chn_aw_bits_size (),
	.io_sys_chn_aw_bits_burst(),
	.io_sys_chn_aw_bits_lock (),
	.io_sys_chn_aw_bits_cache(),
	.io_sys_chn_aw_bits_port (),
	.io_sys_chn_aw_bits_qos  (),
	.io_sys_chn_aw_bits_user (),
	.io_sys_chn_w_ready      (io_sys_chn_w_ready),
	.io_sys_chn_w_valid      (io_sys_chn_w_valid),
	.io_sys_chn_w_bits_data  (io_sys_chn_w_bits_data),
	.io_sys_chn_w_bits_strb  (io_sys_chn_w_bits_strb),
	.io_sys_chn_w_bits_last  (),
	.io_sys_chn_w_bits_user  (),
	.io_sys_chn_b_ready      (io_sys_chn_b_ready),
	.io_sys_chn_b_valid      (io_sys_chn_b_valid),
	.io_sys_chn_b_bits_id    (1'b0),
	.io_sys_chn_b_bits_rsp   (io_sys_chn_b_bits_rsp),
	.io_sys_chn_b_bits_user  (1'b0),

  .memory_0_aw_ready(io_mem_chn_aw_ready),
  .memory_0_aw_valid(io_mem_chn_aw_valid),
  .memory_0_aw_bits_id(io_mem_chn_aw_bits_id),
  .memory_0_aw_bits_addr(io_mem_chn_aw_bits_addr),
  .memory_0_aw_bits_len(io_mem_chn_aw_bits_len),
  .memory_0_aw_bits_size(io_mem_chn_aw_bits_size),
  .memory_0_aw_bits_burst(io_mem_chn_aw_bits_burst),
  .memory_0_aw_bits_lock(),
  .memory_0_aw_bits_cache(),
  .memory_0_aw_bits_prot(),
  .memory_0_aw_bits_qos(),
  .memory_0_w_ready(io_mem_chn_w_ready),
  .memory_0_w_valid(io_mem_chn_w_valid),
  .memory_0_w_bits_data(io_mem_chn_w_bits_data),
  .memory_0_w_bits_strb(io_mem_chn_w_bits_strb),
  .memory_0_w_bits_last(io_mem_chn_w_bits_last),
  .memory_0_b_ready(io_mem_chn_b_ready),
  .memory_0_b_valid(io_mem_chn_b_valid),
  .memory_0_b_bits_id(io_mem_chn_b_bits_id),
  .memory_0_b_bits_resp(io_mem_chn_b_bits_rsp),

  .memory_0_ar_ready(io_mem_chn_ar_ready),
  .memory_0_ar_valid(io_mem_chn_ar_valid),
  .memory_0_ar_bits_id(io_mem_chn_ar_bits_id),
  .memory_0_ar_bits_addr(io_mem_chn_ar_bits_addr),
  .memory_0_ar_bits_len(io_mem_chn_ar_bits_len),
  .memory_0_ar_bits_size(io_mem_chn_ar_bits_size),
  .memory_0_ar_bits_burst(io_mem_chn_ar_bits_burst),
  .memory_0_ar_bits_lock(),
  .memory_0_ar_bits_cache(),
  .memory_0_ar_bits_prot(),
  .memory_0_ar_bits_qos(),
  .memory_0_r_ready(io_mem_chn_r_ready),
  .memory_0_r_valid(io_mem_chn_r_valid),
  .memory_0_r_bits_id(io_mem_chn_r_bits_id),
  .memory_0_r_bits_data(io_mem_chn_r_bits_data),
  .memory_0_r_bits_resp(io_mem_chn_r_bits_rsp),
  .memory_0_r_bits_last(io_mem_chn_r_bits_last),


	.io_rtc_clock            (rtc_clock)
);




axi_full_slv_sram # ( .DW(128), .AW(14) ) s_axi_full_slv_sram 
(

	.MEM_AWID   ({4'b0,io_mem_chn_aw_bits_id}),
	.MEM_BID    (io_mem_chn_b_bits_id),
	.MEM_ARID   ({4'b0,io_mem_chn_ar_bits_id}),
	.MEM_RID    (io_mem_chn_r_bits_id),

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



debuger i_debuger(

	.DEBUGER_AWADDR(io_sys_chn_aw_bits_addr),
	.DEBUGER_AWVALID(io_sys_chn_aw_valid),
	.DEBUGER_AWREADY(io_sys_chn_aw_ready),

	.DEBUGER_WDATA(io_sys_chn_w_bits_data),   
	.DEBUGER_WSTRB(io_sys_chn_w_bits_strb),
	.DEBUGER_WVALID(io_sys_chn_w_valid),
	.DEBUGER_WREADY(io_sys_chn_w_ready),

	.DEBUGER_BRESP(io_sys_chn_b_bits_rsp),
	.DEBUGER_BVALID(io_sys_chn_b_valid),
	.DEBUGER_BREADY(io_sys_chn_b_ready),

	.DEBUGER_ARADDR(io_sys_chn_ar_bits_addr),
	.DEBUGER_ARVALID(io_sys_chn_ar_valid),
	.DEBUGER_ARREADY(io_sys_chn_ar_ready),

	.DEBUGER_RDATA(io_sys_chn_r_bits_data),
	.DEBUGER_RRESP(io_sys_chn_r_bits_rsp),
	.DEBUGER_RVALID(io_sys_chn_r_valid),
	.DEBUGER_RREADY(io_sys_chn_r_ready),

	.CLK(CLK),
	.RSTn(RSTn)
	
);





reg [255:0] testName;

initial begin
	if ( $value$plusargs("%s",testName[255:0]) ) begin
		$display("%s",testName);
	end
end



initial begin
	CLK = 0;
	rtc_clock = 0;
	RSTn = 0;

	#20

	RSTn <= 1;

	#500000
			$display("Time Out !!!");
	$stop;
end


initial begin
	forever
	begin 
		 #5 CLK <= ~CLK;
	end
end

initial begin
	forever
	begin 
		 #610 rtc_clock <= ~rtc_clock;
	end
end



// initial
// begin
// 	$dumpfile("./build/wave.vcd"); //生成的vcd文件名称
// 	$dumpvars(0, rift2chip_tb);//tb模块名称
// end

wire is_ecall_U = s_Rift2Chip.i_rift2Core.diff.io_commit_is_ecall_U;
wire is_ecall_M = s_Rift2Chip.i_rift2Core.diff.io_commit_is_ecall_M;
wire is_ecall_S = s_Rift2Chip.i_rift2Core.diff.io_commit_is_ecall_S;
wire [63:0] gp  = s_Rift2Chip.i_rift2Core.diff.io_register_gp;
// wire [63:0] t6  = s_Rift2Chip.i_rift2Core.diff.io_register_t6;


// reg sim_end = 0;
// always @(negedge CLK)begin 
// 	if (isEcall) begin
// 		// result <= x3;
// 		#1 sim_end <= 1;

// 	end

// end


always @(negedge CLK ) begin
	if ( is_ecall_U | is_ecall_M | is_ecall_S ) begin
		if ( gp == 64'd1 ) begin
			$display("PASS");
			# 1000 
			$finish;
		end
		else begin
			$display("Fail");
			# 100
			$stop;
		end
	end
end



`define MEM s_axi_full_slv_sram.i_sram.ram
reg [7:0] mem [0:200000];

localparam DP = 2**14;
integer i, by;
initial begin

	$readmemh(testName, mem);
	
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






