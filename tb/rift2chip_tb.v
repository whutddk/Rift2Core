/*
* @File name: rift2chip_tb
* @Author: Ruige Lee
* @Email: wut.ruigeli@gmail.com
* @Date:   2021-04-21 15:17:49
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-07-09 16:35:35
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

	wire         io_mem_chn_ar_ready_1;
	wire         io_mem_chn_ar_valid_1;
	wire [31:0]  io_mem_chn_ar_bits_addr_1;
	wire [7:0]   io_mem_chn_ar_bits_len_1;
	wire [2:0]   io_mem_chn_ar_bits_size_1;
	wire [1:0]   io_mem_chn_ar_bits_burst_1;
	wire         io_mem_chn_r_ready_1;
	wire         io_mem_chn_r_valid_1;
	wire [3:0]   io_mem_chn_ar_bits_id_1;
	wire [3:0]   io_mem_chn_r_bits_id_1;
	wire [127:0] io_mem_chn_r_bits_data_1;
	wire [1:0]   io_mem_chn_r_bits_rsp_1;
	wire         io_mem_chn_r_bits_last_1;
	wire         io_mem_chn_aw_ready_1;
	wire         io_mem_chn_aw_valid_1;
	wire [31:0]  io_mem_chn_aw_bits_addr_1;
	wire [7:0]   io_mem_chn_aw_bits_len_1;
	wire [2:0]   io_mem_chn_aw_bits_size_1;
	wire [1:0]   io_mem_chn_aw_bits_burst_1;
	wire         io_mem_chn_w_ready_1;
	wire         io_mem_chn_w_valid_1;
	wire [127:0] io_mem_chn_w_bits_data_1;
	wire [15:0]  io_mem_chn_w_bits_strb_1;
	wire         io_mem_chn_w_bits_last_1;
	wire         io_mem_chn_b_ready_1;
	wire         io_mem_chn_b_valid_1;
	wire [1:0]   io_mem_chn_b_bits_rsp_1;


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

wire [3:0] io_mem_chn_aw_bits_id_1;
wire [3:0] io_mem_chn_b_bits_id_1;

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
	.io_mem_chn_b_bits_user  (1'b0),

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

  .memory_0_aw_ready(io_mem_chn_aw_ready_1),
  .memory_0_aw_valid(io_mem_chn_aw_valid_1),
  .memory_0_aw_bits_id(io_mem_chn_aw_bits_id_1),
  .memory_0_aw_bits_addr(io_mem_chn_aw_bits_addr_1),
  .memory_0_aw_bits_len(io_mem_chn_aw_bits_len_1),
  .memory_0_aw_bits_size(io_mem_chn_aw_bits_size_1),
  .memory_0_aw_bits_burst(io_mem_chn_aw_bits_burst_1),
  .memory_0_aw_bits_lock(),
  .memory_0_aw_bits_cache(),
  .memory_0_aw_bits_prot(),
  .memory_0_aw_bits_qos(),
  .memory_0_w_ready(io_mem_chn_w_ready_1),
  .memory_0_w_valid(io_mem_chn_w_valid_1),
  .memory_0_w_bits_data(io_mem_chn_w_bits_data_1),
  .memory_0_w_bits_strb(io_mem_chn_w_bits_strb_1),
  .memory_0_w_bits_last(io_mem_chn_w_bits_last_1),
  .memory_0_b_ready(io_mem_chn_b_ready_1),
  .memory_0_b_valid(io_mem_chn_b_valid_1),
  .memory_0_b_bits_id(io_mem_chn_b_bits_id_1),
  .memory_0_b_bits_resp(io_mem_chn_b_bits_rsp_1),

  .memory_0_ar_ready(io_mem_chn_ar_ready_1),
  .memory_0_ar_valid(io_mem_chn_ar_valid_1),
  .memory_0_ar_bits_id(io_mem_chn_ar_bits_id_1),
  .memory_0_ar_bits_addr(io_mem_chn_ar_bits_addr_1),
  .memory_0_ar_bits_len(io_mem_chn_ar_bits_len_1),
  .memory_0_ar_bits_size(io_mem_chn_ar_bits_size_1),
  .memory_0_ar_bits_burst(io_mem_chn_ar_bits_burst_1),
  .memory_0_ar_bits_lock(),
  .memory_0_ar_bits_cache(),
  .memory_0_ar_bits_prot(),
  .memory_0_ar_bits_qos(),
  .memory_0_r_ready(io_mem_chn_r_ready_1),
  .memory_0_r_valid(io_mem_chn_r_valid_1),
  .memory_0_r_bits_id(io_mem_chn_r_bits_id_1),
  .memory_0_r_bits_data(io_mem_chn_r_bits_data_1),
  .memory_0_r_bits_resp(io_mem_chn_r_bits_rsp_1),
  .memory_0_r_bits_last(io_mem_chn_r_bits_last_1),


	.io_rtc_clock            (rtc_clock)
);




axi_full_slv_sram # ( .DW(128), .AW(14) ) s_axi_full_slv_sram 
(

	.MEM_AWID   ({4'b0,io_mem_chn_aw_bits_id}),
	.MEM_BID    (io_mem_chn_b_bits_id),
	.MEM_ARID   ({4'b0,io_mem_chn_ar_bits_id}),
	.MEM_RID    (io_mem_chn_ar_bits_id),

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


axi_full_slv_sram # ( .DW(128), .AW(14) ) s_axi_full_slv_sram_1
(

	.MEM_BID    (io_mem_chn_b_bits_id_1),
	.MEM_RID    (io_mem_chn_r_bits_id_1),
	.MEM_ARID   ({4'b0,io_mem_chn_ar_bits_id_1}),
	.MEM_AWID   ({4'b0,io_mem_chn_aw_bits_id_1}),

	.MEM_AWADDR(io_mem_chn_aw_bits_addr_1),
	.MEM_AWLEN(io_mem_chn_aw_bits_len_1),
	.MEM_AWSIZE(io_mem_chn_aw_bits_size_1),
	.MEM_AWBURST(io_mem_chn_aw_bits_burst_1),
	.MEM_AWVALID(io_mem_chn_aw_valid_1),
	.MEM_AWREADY(io_mem_chn_aw_ready_1),


	.MEM_WDATA(io_mem_chn_w_bits_data_1),
	.MEM_WSTRB(io_mem_chn_w_bits_strb_1),
	.MEM_WLAST(io_mem_chn_w_bits_last_1),
	.MEM_WVALID(io_mem_chn_w_valid_1),
	.MEM_WREADY(io_mem_chn_w_ready_1),

	.MEM_BRESP(io_mem_chn_b_bits_rsp_1),
	.MEM_BVALID(io_mem_chn_b_valid_1),
	.MEM_BREADY(io_mem_chn_b_ready_1),

	.MEM_ARADDR(io_mem_chn_ar_bits_addr_1),
	.MEM_ARLEN(io_mem_chn_ar_bits_len_1),
	.MEM_ARSIZE(io_mem_chn_ar_bits_size_1),
	.MEM_ARBURST(io_mem_chn_ar_bits_burst_1),
	.MEM_ARVALID(io_mem_chn_ar_valid_1),
	.MEM_ARREADY(io_mem_chn_ar_ready_1),

	.MEM_RDATA(io_mem_chn_r_bits_data_1),
	.MEM_RRESP(io_mem_chn_r_bits_rsp_1),
	.MEM_RLAST(io_mem_chn_r_bits_last_1),
	.MEM_RVALID(io_mem_chn_r_valid_1),
	.MEM_RREADY(io_mem_chn_r_ready_1),

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

initial begin
	forever
	begin 
		 #610 rtc_clock <= ~rtc_clock;
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
wire [63:0] x3 = 
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd0  ? s_Rift2Chip.i_rift2Core.i_regfiles.files_0  :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd1  ? s_Rift2Chip.i_rift2Core.i_regfiles.files_1  :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd2  ? s_Rift2Chip.i_rift2Core.i_regfiles.files_2  :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd3  ? s_Rift2Chip.i_rift2Core.i_regfiles.files_3  :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd4  ? s_Rift2Chip.i_rift2Core.i_regfiles.files_4  :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd5  ? s_Rift2Chip.i_rift2Core.i_regfiles.files_5  :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd6  ? s_Rift2Chip.i_rift2Core.i_regfiles.files_6  :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd7  ? s_Rift2Chip.i_rift2Core.i_regfiles.files_7  :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd8  ? s_Rift2Chip.i_rift2Core.i_regfiles.files_8  :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd9  ? s_Rift2Chip.i_rift2Core.i_regfiles.files_9  :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd10 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_10 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd11 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_11 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd12 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_12 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd13 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_13 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd14 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_14 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd15 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_15 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd16 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_16 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd17 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_17 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd18 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_18 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd19 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_19 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd20 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_20 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd21 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_21 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd22 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_22 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd23 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_23 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd24 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_24 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd25 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_25 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd26 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_26 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd27 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_27 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd28 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_28 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd29 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_29 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd30 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_30 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd31 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_31 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd32 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_32 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd33 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_33 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd34 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_34 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd35 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_35 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd36 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_36 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd37 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_37 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd38 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_38 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd39 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_39 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd40 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_40 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd41 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_41 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd42 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_42 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd43 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_43 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd44 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_44 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd45 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_45 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd46 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_46 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd47 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_47 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd48 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_48 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd49 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_49 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd50 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_50 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd51 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_51 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd52 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_52 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd53 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_53 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd54 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_54 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd55 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_55 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd56 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_56 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd57 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_57 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd58 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_58 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd59 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_59 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd60 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_60 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd61 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_61 :
	s_Rift2Chip.i_rift2Core.i_regfiles.archit_ptr_3 == 6'd62 ? s_Rift2Chip.i_rift2Core.i_regfiles.files_62 : s_Rift2Chip.i_rift2Core.i_regfiles.files_63;


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
			# 1000 
			$finish;
		end
		else begin
			$display("Fail");
			$stop;
		end
	end
end



`define MEM s_axi_full_slv_sram.i_sram.ram
`define MEM1 s_axi_full_slv_sram_1.i_sram.ram
reg [7:0] mem [0:200000];

localparam DP = 2**14;
integer i, by;
initial begin
	$readmemh("./ci/rv64ui-p-sb.verilog", mem);
	// $readmemh(testName, mem);
	
	for ( i = 0; i < DP; i = i + 1 ) begin
		for ( by = 0; by < 16; by = by + 1 ) begin
			if ( | mem[i*16+by] ) begin
				`MEM[i][8*by +: 8] = mem[i*16+by];
				`MEM1[i][8*by +: 8] = mem[i*16+by];
			end
			else begin
				`MEM[i][8*by +: 8] = 8'h0;
				`MEM1[i][8*by +: 8] = 8'h0;
			end
		end


	end

end 



endmodule






