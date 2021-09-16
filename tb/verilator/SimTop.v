



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




module SimTop (

	output success,
	output fail,

  output [63:0] trace_abi_zero,
  output [63:0] trace_abi_ra  ,
  output [63:0] trace_abi_sp  ,
  output [63:0] trace_abi_gp  ,
  output [63:0] trace_abi_tp  ,
  output [63:0] trace_abi_t0  ,
  output [63:0] trace_abi_t1  ,
  output [63:0] trace_abi_t2  ,
  output [63:0] trace_abi_s0  ,
  output [63:0] trace_abi_s1  ,
  output [63:0] trace_abi_a0  ,
  output [63:0] trace_abi_a1  ,
  output [63:0] trace_abi_a2  ,
  output [63:0] trace_abi_a3  ,
  output [63:0] trace_abi_a4  ,
  output [63:0] trace_abi_a5  ,
  output [63:0] trace_abi_a6  ,
  output [63:0] trace_abi_a7  ,
  output [63:0] trace_abi_s2  ,
  output [63:0] trace_abi_s3  ,
  output [63:0] trace_abi_s4  ,
  output [63:0] trace_abi_s5  ,
  output [63:0] trace_abi_s6  ,
  output [63:0] trace_abi_s7  ,
  output [63:0] trace_abi_s8  ,
  output [63:0] trace_abi_s9  ,
  output [63:0] trace_abi_s10 ,
  output [63:0] trace_abi_s11 ,
  output [63:0] trace_abi_t3  ,
  output [63:0] trace_abi_t4  ,
  output [63:0] trace_abi_t5  ,
  output [63:0] trace_abi_t6  ,

  output [63:0] trace_pc_0,
  output [63:0] trace_pc_1,
  output trace_comfirm_0,
  output trace_comfirm_1,
  output trace_abort_0,
  output [1:0] trace_priv,

	input CLK,

	input RSTn
	
);

	reg rtc_clock;


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

	.MEM_AWID   (io_mem_chn_aw_bits_id),
	.MEM_BID    (io_mem_chn_b_bits_id),
	.MEM_ARID   (io_mem_chn_ar_bits_id),
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

initial begin
	rtc_clock = 0;
end





wire is_ecall_U = s_Rift2Chip.i_rift2Core.diff.io_commit_is_ecall_U;
wire is_ecall_M = s_Rift2Chip.i_rift2Core.diff.io_commit_is_ecall_M;
wire is_ecall_S = s_Rift2Chip.i_rift2Core.diff.io_commit_is_ecall_S;
wire [63:0] gp  = s_Rift2Chip.i_rift2Core.diff.io_register_gp;

reg success_reg = 1'b0;
reg fail_reg = 1'b0;

assign success = success_reg;
assign fail = fail_reg;

always @(negedge CLK ) begin
	if ( is_ecall_U | is_ecall_M | is_ecall_S ) begin
		if ( gp == 64'd1 ) begin
			// $display("PASS");
			success_reg = 1'b1;
			// $finish;
		end
		else begin
			// $display("Fail");
			fail_reg = 1'b1;
			// $stop;
		end
	end
end


reg [255:0] testName;

`define MEM s_axi_full_slv_sram.i_sram.ram
reg [7:0] mem [0:200000];

localparam DP = 2**14;
integer i, by;
initial begin


	if ( $value$plusargs("%s",testName[255:0]) ) begin
		$display("%s",testName);
	  $readmemh(testName, mem);		

	end
	else begin 
		$error("Failed to read Files!");
	end

	
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




  assign trace_abi_zero = s_Rift2Chip.i_rift2Core.diff.io_register_zero;
  assign trace_abi_ra   = s_Rift2Chip.i_rift2Core.diff.io_register_ra ;
  assign trace_abi_sp   = s_Rift2Chip.i_rift2Core.diff.io_register_sp ;
  assign trace_abi_gp   = s_Rift2Chip.i_rift2Core.diff.io_register_gp ;
  assign trace_abi_tp   = s_Rift2Chip.i_rift2Core.diff.io_register_tp ;
  assign trace_abi_t0   = s_Rift2Chip.i_rift2Core.diff.io_register_t0 ;
  assign trace_abi_t1   = s_Rift2Chip.i_rift2Core.diff.io_register_t1 ;
  assign trace_abi_t2   = s_Rift2Chip.i_rift2Core.diff.io_register_t2 ;
  assign trace_abi_s0   = s_Rift2Chip.i_rift2Core.diff.io_register_s0 ;
  assign trace_abi_s1   = s_Rift2Chip.i_rift2Core.diff.io_register_s1 ;
  assign trace_abi_a0   = s_Rift2Chip.i_rift2Core.diff.io_register_a0 ;
  assign trace_abi_a1   = s_Rift2Chip.i_rift2Core.diff.io_register_a1 ;
  assign trace_abi_a2   = s_Rift2Chip.i_rift2Core.diff.io_register_a2 ;
  assign trace_abi_a3   = s_Rift2Chip.i_rift2Core.diff.io_register_a3 ;
  assign trace_abi_a4   = s_Rift2Chip.i_rift2Core.diff.io_register_a4 ;
  assign trace_abi_a5   = s_Rift2Chip.i_rift2Core.diff.io_register_a5 ;
  assign trace_abi_a6   = s_Rift2Chip.i_rift2Core.diff.io_register_a6 ;
  assign trace_abi_a7   = s_Rift2Chip.i_rift2Core.diff.io_register_a7 ;
  assign trace_abi_s2   = s_Rift2Chip.i_rift2Core.diff.io_register_s2 ;
  assign trace_abi_s3   = s_Rift2Chip.i_rift2Core.diff.io_register_s3 ;
  assign trace_abi_s4   = s_Rift2Chip.i_rift2Core.diff.io_register_s4 ;
  assign trace_abi_s5   = s_Rift2Chip.i_rift2Core.diff.io_register_s5 ;
  assign trace_abi_s6   = s_Rift2Chip.i_rift2Core.diff.io_register_s6 ;
  assign trace_abi_s7   = s_Rift2Chip.i_rift2Core.diff.io_register_s7 ;
  assign trace_abi_s8   = s_Rift2Chip.i_rift2Core.diff.io_register_s8 ;
  assign trace_abi_s9   = s_Rift2Chip.i_rift2Core.diff.io_register_s9 ;
  assign trace_abi_s10  = s_Rift2Chip.i_rift2Core.diff.io_register_s10;
  assign trace_abi_s11  = s_Rift2Chip.i_rift2Core.diff.io_register_s11;
  assign trace_abi_t3   = s_Rift2Chip.i_rift2Core.diff.io_register_t3 ;
  assign trace_abi_t4   = s_Rift2Chip.i_rift2Core.diff.io_register_t4 ;
  assign trace_abi_t5   = s_Rift2Chip.i_rift2Core.diff.io_register_t5 ;
  assign trace_abi_t6   = s_Rift2Chip.i_rift2Core.diff.io_register_t6 ;

  assign trace_pc_0 = s_Rift2Chip.i_rift2Core.diff.io_commit_pc_0;
  assign trace_pc_1 = s_Rift2Chip.i_rift2Core.diff.io_commit_pc_1;
  assign trace_comfirm_0 = s_Rift2Chip.i_rift2Core.diff.io_commit_comfirm_0;
  assign trace_comfirm_1 = s_Rift2Chip.i_rift2Core.diff.io_commit_comfirm_1;
  assign trace_abort_0 = s_Rift2Chip.i_rift2Core.diff.io_commit_abort_0;

  assign trace_priv = s_Rift2Chip.i_rift2Core.diff.io_commit_priv_lvl;







endmodule






