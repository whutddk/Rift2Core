



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




module SimTop (

	output success,
	output fail,

  input          io_interrupt_0,
  input          io_interrupt_1,
  input          io_interrupt_2,
  input          io_interrupt_3,
  input          io_interrupt_4,
  input          io_interrupt_5,
  input          io_interrupt_6,
  input          io_interrupt_7,
  input          io_interrupt_8,
  input          io_interrupt_9,
  input          io_interrupt_10,
  input          io_interrupt_11,
  input          io_interrupt_12,
  input          io_interrupt_13,
  input          io_interrupt_14,
  input          io_interrupt_15,
  input          io_interrupt_16,
  input          io_interrupt_17,
  input          io_interrupt_18,
  input          io_interrupt_19,
  input          io_interrupt_20,
  input          io_interrupt_21,
  input          io_interrupt_22,
  input          io_interrupt_23,
  input          io_interrupt_24,
  input          io_interrupt_25,
  input          io_interrupt_26,
  input          io_interrupt_27,
  input          io_interrupt_28,
  input          io_interrupt_29,
  input          io_interrupt_30,


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

  output [63:0] trace1_abi_ft0,
  output [63:0] trace1_abi_ft1,
  output [63:0] trace1_abi_ft2,
  output [63:0] trace1_abi_ft3,
  output [63:0] trace1_abi_ft4,
  output [63:0] trace1_abi_ft5,
  output [63:0] trace1_abi_ft6,
  output [63:0] trace1_abi_ft7,
  output [63:0] trace1_abi_fs0,
  output [63:0] trace1_abi_fs1,
  output [63:0] trace1_abi_fa0,
  output [63:0] trace1_abi_fa1,
  output [63:0] trace1_abi_fa2,
  output [63:0] trace1_abi_fa3,
  output [63:0] trace1_abi_fa4,
  output [63:0] trace1_abi_fa5,
  output [63:0] trace1_abi_fa6,
  output [63:0] trace1_abi_fa7,
  output [63:0] trace1_abi_fs2,
  output [63:0] trace1_abi_fs3,
  output [63:0] trace1_abi_fs4,
  output [63:0] trace1_abi_fs5,
  output [63:0] trace1_abi_fs6,
  output [63:0] trace1_abi_fs7,
  output [63:0] trace1_abi_fs8,
  output [63:0] trace1_abi_fs9,
  output [63:0] trace1_abi_fs10,
  output [63:0] trace1_abi_fs11,
  output [63:0] trace1_abi_ft8,
  output [63:0] trace1_abi_ft9,
  output [63:0] trace1_abi_ft10,
  output [63:0] trace1_abi_ft11,

  output [63:0] trace2_abi_ft0,
  output [63:0] trace2_abi_ft1,
  output [63:0] trace2_abi_ft2,
  output [63:0] trace2_abi_ft3,
  output [63:0] trace2_abi_ft4,
  output [63:0] trace2_abi_ft5,
  output [63:0] trace2_abi_ft6,
  output [63:0] trace2_abi_ft7,
  output [63:0] trace2_abi_fs0,
  output [63:0] trace2_abi_fs1,
  output [63:0] trace2_abi_fa0,
  output [63:0] trace2_abi_fa1,
  output [63:0] trace2_abi_fa2,
  output [63:0] trace2_abi_fa3,
  output [63:0] trace2_abi_fa4,
  output [63:0] trace2_abi_fa5,
  output [63:0] trace2_abi_fa6,
  output [63:0] trace2_abi_fa7,
  output [63:0] trace2_abi_fs2,
  output [63:0] trace2_abi_fs3,
  output [63:0] trace2_abi_fs4,
  output [63:0] trace2_abi_fs5,
  output [63:0] trace2_abi_fs6,
  output [63:0] trace2_abi_fs7,
  output [63:0] trace2_abi_fs8,
  output [63:0] trace2_abi_fs9,
  output [63:0] trace2_abi_fs10,
  output [63:0] trace2_abi_fs11,
  output [63:0] trace2_abi_ft8,
  output [63:0] trace2_abi_ft9,
  output [63:0] trace2_abi_ft10,
  output [63:0] trace2_abi_ft11,

  output [63:0] trace_pc_0,
  output [63:0] trace_pc_1,
  output trace_comfirm_0,
  output trace_comfirm_1,
  output trace_abort_0,
  output trace_abort_1,
  output [1:0] trace_priv,

	output [63:0] trace_mstatus,
	output [63:0] trace_mtvec,
	output [63:0] trace_mscratch,
	output [63:0] trace_mepc,
	output [63:0] trace_mcause,
	output [63:0] trace_mtval,
  output [63:0] trace_mvendorid,
  output [63:0] trace_marchid,
  output [63:0] trace_mimpid,
  output [63:0] trace_mhartid,
  output [63:0] trace_misa,
  output [63:0] trace_mie,
  output [63:0] trace_mip,
  output [63:0] trace_medeleg,
  output [63:0] trace_mideleg,
  // output [63:0] trace_mcounteren,
  // output [63:0] trace_mcountinhibit,
  // output [63:0] trace_tselect,
  // output [63:0] trace_tdata1,
  // output [63:0] trace_tdata2,
  // output [63:0] trace_tdata3,
  // output [63:0] trace_mhpmevent,
  output [63:0] trace_pmpcfg_0,
  // output [63:0] trace_pmpcfg_1,
  // output [63:0] trace_pmpcfg_2,
  // output [63:0] trace_pmpcfg_3,

	output [63:0] trace_pmpaddr_0,
	output [63:0] trace_pmpaddr_1,
	output [63:0] trace_pmpaddr_2,
	output [63:0] trace_pmpaddr_3,
	output [63:0] trace_pmpaddr_4,
	output [63:0] trace_pmpaddr_5,
	output [63:0] trace_pmpaddr_6,
	output [63:0] trace_pmpaddr_7,
	// output [63:0] trace_pmpaddr_8,
	// output [63:0] trace_pmpaddr_9,
	// output [63:0] trace_pmpaddr_10,
	// output [63:0] trace_pmpaddr_11,
	// output [63:0] trace_pmpaddr_12,
	// output [63:0] trace_pmpaddr_13,
	// output [63:0] trace_pmpaddr_14,
	// output [63:0] trace_pmpaddr_15,

  output [63:0] trace_stvec,
  output [63:0] trace_sscratch,
  output [63:0] trace_sepc,
  output [63:0] trace_scause,
  output [63:0] trace_stval,
  output [63:0] trace_satp,
  // output [63:0] trace_scounteren,
  // output [63:0] trace_dcsr,
  // output [63:0] trace_dpc,
  // output [63:0] trace_dscratch,
  output [31:0] trace_fflags,
  output [7:0] trace_frm,

  output [63:0] trace_mcycle,
  output [63:0] trace_minstret,
  output [63:0] trace_scsBPredict,
  output [63:0] trace_misBPredict,
  output [63:0] trace_scsJPredict,
  output [63:0] trace_misJPredict,

	input CLK,
  input rtc_clock,

	input RSTn
	

);


  wire         trstn = 1'b1;
  wire         tck = 1'b0;
  wire         tms = 1'b0;
  wire         tdi = 1'b0;
  wire         tdo;
  wire         tdo_en;


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
	wire [3:0]   io_sys_chn_ar_bits_id;
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
	wire [3:0]   io_sys_chn_r_bits_id;
	wire [63:0]  io_sys_chn_r_bits_data;
	wire [1:0]   io_sys_chn_r_bits_rsp;
	wire         io_sys_chn_r_bits_last;
	wire         io_sys_chn_r_bits_user;

	wire         io_sys_chn_aw_ready;
	wire         io_sys_chn_aw_valid;
	wire  [3:0]   io_sys_chn_aw_bits_id;
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
	wire [3:0]   io_sys_chn_b_bits_id;
	wire [1:0]   io_sys_chn_b_bits_rsp;
	wire         io_sys_chn_b_bits_user;

wire [3:0] io_mem_chn_ar_bits_id;
wire [3:0] io_mem_chn_r_bits_id;
wire [3:0] io_mem_chn_aw_bits_id;
wire [3:0] io_mem_chn_b_bits_id;



Rift2Chip s_Rift2Chip(
	.clock(CLK),
	.reset(~RSTn),

  .system_0_aw_ready(io_sys_chn_aw_ready),
  .system_0_aw_valid(io_sys_chn_aw_valid),
  .system_0_aw_bits_id(io_sys_chn_aw_bits_id),
  .system_0_aw_bits_addr(io_sys_chn_aw_bits_addr),
  .system_0_aw_bits_len(io_sys_chn_aw_bits_len),
  .system_0_aw_bits_size(io_sys_chn_aw_bits_size),
  .system_0_aw_bits_burst(io_sys_chn_aw_bits_burst),
  .system_0_aw_bits_lock(io_sys_chn_aw_bits_lock),
  .system_0_aw_bits_cache(io_sys_chn_aw_bits_cache),
  .system_0_aw_bits_prot(io_sys_chn_aw_bits_port),
  .system_0_aw_bits_qos(io_sys_chn_aw_bits_qos),
  .system_0_w_ready(io_sys_chn_w_ready),
  .system_0_w_valid(io_sys_chn_w_valid),
  .system_0_w_bits_data(io_sys_chn_w_bits_data),
  .system_0_w_bits_strb(io_sys_chn_w_bits_strb),
  .system_0_w_bits_last(io_sys_chn_w_bits_last),
  .system_0_b_ready(io_sys_chn_b_ready),
  .system_0_b_valid(io_sys_chn_b_valid),
  .system_0_b_bits_id(io_sys_chn_b_bits_id),
  .system_0_b_bits_resp(io_sys_chn_b_bits_rsp),

  .system_0_ar_ready(io_sys_chn_ar_ready),
  .system_0_ar_valid(io_sys_chn_ar_valid),
  .system_0_ar_bits_id(io_sys_chn_ar_bits_id),
  .system_0_ar_bits_addr(io_sys_chn_ar_bits_addr),
  .system_0_ar_bits_len(io_sys_chn_ar_bits_len),
  .system_0_ar_bits_size(io_sys_chn_ar_bits_size),
  .system_0_ar_bits_burst(io_sys_chn_ar_bits_burst),
  .system_0_ar_bits_lock(io_sys_chn_aw_bits_lock),
  .system_0_ar_bits_cache(io_sys_chn_ar_bits_cache),
  .system_0_ar_bits_prot(io_sys_chn_ar_bits_port),
  .system_0_ar_bits_qos(io_sys_chn_ar_bits_qos),
  .system_0_r_ready(io_sys_chn_r_ready),
  .system_0_r_valid(io_sys_chn_r_valid),
  .system_0_r_bits_id(io_sys_chn_r_bits_id),
  .system_0_r_bits_data(io_sys_chn_r_bits_data),
  .system_0_r_bits_resp(io_sys_chn_r_bits_rsp),
  .system_0_r_bits_last(io_sys_chn_r_bits_last),


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

  .io_JtagIO_TRSTn(trstn),
  .io_JtagIO_TCK(tck),
  .io_JtagIO_TMS(tms),
  .io_JtagIO_TDI(tdi),
  .io_JtagIO_TDO(tdo),
  .io_JtagIO_TDO_driven(tdo_en),
  .io_ndreset(),

  .io_interrupt_0(io_interrupt_0),
  .io_interrupt_1(io_interrupt_1),
  .io_interrupt_2(io_interrupt_2),
  .io_interrupt_3(io_interrupt_3),
  .io_interrupt_4(io_interrupt_4),
  .io_interrupt_5(io_interrupt_5),
  .io_interrupt_6(io_interrupt_6),
  .io_interrupt_7(io_interrupt_7),
  .io_interrupt_8(io_interrupt_8),
  .io_interrupt_9(io_interrupt_9),
  .io_interrupt_10(io_interrupt_10),
  .io_interrupt_11(io_interrupt_11),
  .io_interrupt_12(io_interrupt_12),
  .io_interrupt_13(io_interrupt_13),
  .io_interrupt_14(io_interrupt_14),
  .io_interrupt_15(io_interrupt_15),
  .io_interrupt_16(io_interrupt_16),
  .io_interrupt_17(io_interrupt_17),
  .io_interrupt_18(io_interrupt_18),
  .io_interrupt_19(io_interrupt_19),
  .io_interrupt_20(io_interrupt_20),
  .io_interrupt_21(io_interrupt_21),
  .io_interrupt_22(io_interrupt_22),
  .io_interrupt_23(io_interrupt_23),
  .io_interrupt_24(io_interrupt_24),
  .io_interrupt_25(io_interrupt_25),
  .io_interrupt_26(io_interrupt_26),
  .io_interrupt_27(io_interrupt_27),
  .io_interrupt_28(io_interrupt_28),
  .io_interrupt_29(io_interrupt_29),
  .io_interrupt_30(io_interrupt_30),

	.io_rtc_clock(rtc_clock)
);




axi_full_slv_sram # ( .DW(128), .AW(18) ) s_axi_full_slv_sram 
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

wire debugger_success;

debuger i_debuger(
  .success(debugger_success),
	.DEBUGER_AWID   (io_sys_chn_aw_bits_id),
	.DEBUGER_BID    (io_sys_chn_b_bits_id),
	.DEBUGER_ARID   (io_sys_chn_ar_bits_id),
	.DEBUGER_RID    (io_sys_chn_r_bits_id),

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










wire is_ecall_U = s_Rift2Chip.i_rift2Core.diff.io_commit_is_ecall_U;
wire is_ecall_M = s_Rift2Chip.i_rift2Core.diff.io_commit_is_ecall_M;
wire is_ecall_S = s_Rift2Chip.i_rift2Core.diff.io_commit_is_ecall_S;
wire [63:0] gp  = s_Rift2Chip.i_rift2Core.diff.XReg_gp;

reg success_reg;
reg fail_reg;

assign success = success_reg | debugger_success;
assign fail = fail_reg;

reg is_ecall_U_reg;
reg is_ecall_M_reg;
reg is_ecall_S_reg;

always @(posedge CLK or negedge RSTn) begin 
  if ( !RSTn ) begin
    is_ecall_U_reg <= 1'b0;
    is_ecall_M_reg <= 1'b0;
    is_ecall_S_reg <= 1'b0;
  end else begin
    is_ecall_U_reg <= is_ecall_U;
    is_ecall_M_reg <= is_ecall_M;
    is_ecall_S_reg <= is_ecall_S;
  end
end

always @(negedge CLK or negedge RSTn) begin
  if ( !RSTn ) begin
    success_reg <= 1'b0;
    fail_reg <= 1'b0;
  end
	else if ( is_ecall_U_reg | is_ecall_M_reg | is_ecall_S_reg ) begin
		if ( gp == 64'd1 ) begin
			// $display("PASS");
			success_reg <= 1'b1;
			// $finish;
		end
		else begin
			// $display("Fail");
			fail_reg <= 1'b1;
			// $stop;
		end
	end
end

// reg [63:0]  fileName;
// reg [255:0] testName;
string testName;

`define MEM s_axi_full_slv_sram.i_sram.ram
reg [7:0] mem [0:200000];

localparam DP = 2**18;
integer i, by;
initial begin


	// if ( $value$plusargs("%s",fileName[255:0]) ) begin
	if ( $value$plusargs("%s",testName) ) begin
    // testName = $sformatf()
		$display("%s",testName);
	  $readmemh(testName, mem);		

	end
	else begin 
    $display("%s",testName);
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




  assign trace_abi_zero = s_Rift2Chip.i_rift2Core.diff.XReg_zero;
  assign trace_abi_ra   = s_Rift2Chip.i_rift2Core.diff.XReg_ra ;
  assign trace_abi_sp   = s_Rift2Chip.i_rift2Core.diff.XReg_sp ;
  assign trace_abi_gp   = s_Rift2Chip.i_rift2Core.diff.XReg_gp ;
  assign trace_abi_tp   = s_Rift2Chip.i_rift2Core.diff.XReg_tp ;
  assign trace_abi_t0   = s_Rift2Chip.i_rift2Core.diff.XReg_t_0 ;
  assign trace_abi_t1   = s_Rift2Chip.i_rift2Core.diff.XReg_t_1 ;
  assign trace_abi_t2   = s_Rift2Chip.i_rift2Core.diff.XReg_t_2 ;
  assign trace_abi_s0   = s_Rift2Chip.i_rift2Core.diff.XReg_s_0 ;
  assign trace_abi_s1   = s_Rift2Chip.i_rift2Core.diff.XReg_s_1 ;
  assign trace_abi_a0   = s_Rift2Chip.i_rift2Core.diff.XReg_a_0 ;
  assign trace_abi_a1   = s_Rift2Chip.i_rift2Core.diff.XReg_a_1 ;
  assign trace_abi_a2   = s_Rift2Chip.i_rift2Core.diff.XReg_a_2 ;
  assign trace_abi_a3   = s_Rift2Chip.i_rift2Core.diff.XReg_a_3 ;
  assign trace_abi_a4   = s_Rift2Chip.i_rift2Core.diff.XReg_a_4 ;
  assign trace_abi_a5   = s_Rift2Chip.i_rift2Core.diff.XReg_a_5 ;
  assign trace_abi_a6   = s_Rift2Chip.i_rift2Core.diff.XReg_a_6 ;
  assign trace_abi_a7   = s_Rift2Chip.i_rift2Core.diff.XReg_a_7 ;
  assign trace_abi_s2   = s_Rift2Chip.i_rift2Core.diff.XReg_s_2 ;
  assign trace_abi_s3   = s_Rift2Chip.i_rift2Core.diff.XReg_s_3 ;
  assign trace_abi_s4   = s_Rift2Chip.i_rift2Core.diff.XReg_s_4 ;
  assign trace_abi_s5   = s_Rift2Chip.i_rift2Core.diff.XReg_s_5 ;
  assign trace_abi_s6   = s_Rift2Chip.i_rift2Core.diff.XReg_s_6 ;
  assign trace_abi_s7   = s_Rift2Chip.i_rift2Core.diff.XReg_s_7 ;
  assign trace_abi_s8   = s_Rift2Chip.i_rift2Core.diff.XReg_s_8 ;
  assign trace_abi_s9   = s_Rift2Chip.i_rift2Core.diff.XReg_s_9 ;
  assign trace_abi_s10  = s_Rift2Chip.i_rift2Core.diff.XReg_s_10;
  assign trace_abi_s11  = s_Rift2Chip.i_rift2Core.diff.XReg_s_11;
  assign trace_abi_t3   = s_Rift2Chip.i_rift2Core.diff.XReg_t_3 ;
  assign trace_abi_t4   = s_Rift2Chip.i_rift2Core.diff.XReg_t_4 ;
  assign trace_abi_t5   = s_Rift2Chip.i_rift2Core.diff.XReg_t_5 ;
  assign trace_abi_t6   = s_Rift2Chip.i_rift2Core.diff.XReg_t_6 ;

  assign trace1_abi_ft0  = s_Rift2Chip.i_rift2Core.diff.FReg1_ft_0;
  assign trace1_abi_ft1  = s_Rift2Chip.i_rift2Core.diff.FReg1_ft_1;
  assign trace1_abi_ft2  = s_Rift2Chip.i_rift2Core.diff.FReg1_ft_2;
  assign trace1_abi_ft3  = s_Rift2Chip.i_rift2Core.diff.FReg1_ft_3;
  assign trace1_abi_ft4  = s_Rift2Chip.i_rift2Core.diff.FReg1_ft_4;
  assign trace1_abi_ft5  = s_Rift2Chip.i_rift2Core.diff.FReg1_ft_5;
  assign trace1_abi_ft6  = s_Rift2Chip.i_rift2Core.diff.FReg1_ft_6;
  assign trace1_abi_ft7  = s_Rift2Chip.i_rift2Core.diff.FReg1_ft_7;
  assign trace1_abi_fs0  = s_Rift2Chip.i_rift2Core.diff.FReg1_fs_0;
  assign trace1_abi_fs1  = s_Rift2Chip.i_rift2Core.diff.FReg1_fs_1;
  assign trace1_abi_fa0  = s_Rift2Chip.i_rift2Core.diff.FReg1_fa_0;
  assign trace1_abi_fa1  = s_Rift2Chip.i_rift2Core.diff.FReg1_fa_1;
  assign trace1_abi_fa2  = s_Rift2Chip.i_rift2Core.diff.FReg1_fa_2;
  assign trace1_abi_fa3  = s_Rift2Chip.i_rift2Core.diff.FReg1_fa_3;
  assign trace1_abi_fa4  = s_Rift2Chip.i_rift2Core.diff.FReg1_fa_4;
  assign trace1_abi_fa5  = s_Rift2Chip.i_rift2Core.diff.FReg1_fa_5;
  assign trace1_abi_fa6  = s_Rift2Chip.i_rift2Core.diff.FReg1_fa_6;
  assign trace1_abi_fa7  = s_Rift2Chip.i_rift2Core.diff.FReg1_fa_7;
  assign trace1_abi_fs2  = s_Rift2Chip.i_rift2Core.diff.FReg1_fs_2;
  assign trace1_abi_fs3  = s_Rift2Chip.i_rift2Core.diff.FReg1_fs_3;
  assign trace1_abi_fs4  = s_Rift2Chip.i_rift2Core.diff.FReg1_fs_4;
  assign trace1_abi_fs5  = s_Rift2Chip.i_rift2Core.diff.FReg1_fs_5;
  assign trace1_abi_fs6  = s_Rift2Chip.i_rift2Core.diff.FReg1_fs_6;
  assign trace1_abi_fs7  = s_Rift2Chip.i_rift2Core.diff.FReg1_fs_7;
  assign trace1_abi_fs8  = s_Rift2Chip.i_rift2Core.diff.FReg1_fs_8;
  assign trace1_abi_fs9  = s_Rift2Chip.i_rift2Core.diff.FReg1_fs_9;
  assign trace1_abi_fs10 = s_Rift2Chip.i_rift2Core.diff.FReg1_fs_10;
  assign trace1_abi_fs11 = s_Rift2Chip.i_rift2Core.diff.FReg1_fs_11;
  assign trace1_abi_ft8  = s_Rift2Chip.i_rift2Core.diff.FReg1_ft_8;
  assign trace1_abi_ft9  = s_Rift2Chip.i_rift2Core.diff.FReg1_ft_9;
  assign trace1_abi_ft10 = s_Rift2Chip.i_rift2Core.diff.FReg1_ft_10;
  assign trace1_abi_ft11 = s_Rift2Chip.i_rift2Core.diff.FReg1_ft_11;

  assign trace2_abi_ft0  = s_Rift2Chip.i_rift2Core.diff.FReg2_ft_0;
  assign trace2_abi_ft1  = s_Rift2Chip.i_rift2Core.diff.FReg2_ft_1;
  assign trace2_abi_ft2  = s_Rift2Chip.i_rift2Core.diff.FReg2_ft_2;
  assign trace2_abi_ft3  = s_Rift2Chip.i_rift2Core.diff.FReg2_ft_3;
  assign trace2_abi_ft4  = s_Rift2Chip.i_rift2Core.diff.FReg2_ft_4;
  assign trace2_abi_ft5  = s_Rift2Chip.i_rift2Core.diff.FReg2_ft_5;
  assign trace2_abi_ft6  = s_Rift2Chip.i_rift2Core.diff.FReg2_ft_6;
  assign trace2_abi_ft7  = s_Rift2Chip.i_rift2Core.diff.FReg2_ft_7;
  assign trace2_abi_fs0  = s_Rift2Chip.i_rift2Core.diff.FReg2_fs_0;
  assign trace2_abi_fs1  = s_Rift2Chip.i_rift2Core.diff.FReg2_fs_1;
  assign trace2_abi_fa0  = s_Rift2Chip.i_rift2Core.diff.FReg2_fa_0;
  assign trace2_abi_fa1  = s_Rift2Chip.i_rift2Core.diff.FReg2_fa_1;
  assign trace2_abi_fa2  = s_Rift2Chip.i_rift2Core.diff.FReg2_fa_2;
  assign trace2_abi_fa3  = s_Rift2Chip.i_rift2Core.diff.FReg2_fa_3;
  assign trace2_abi_fa4  = s_Rift2Chip.i_rift2Core.diff.FReg2_fa_4;
  assign trace2_abi_fa5  = s_Rift2Chip.i_rift2Core.diff.FReg2_fa_5;
  assign trace2_abi_fa6  = s_Rift2Chip.i_rift2Core.diff.FReg2_fa_6;
  assign trace2_abi_fa7  = s_Rift2Chip.i_rift2Core.diff.FReg2_fa_7;
  assign trace2_abi_fs2  = s_Rift2Chip.i_rift2Core.diff.FReg2_fs_2;
  assign trace2_abi_fs3  = s_Rift2Chip.i_rift2Core.diff.FReg2_fs_3;
  assign trace2_abi_fs4  = s_Rift2Chip.i_rift2Core.diff.FReg2_fs_4;
  assign trace2_abi_fs5  = s_Rift2Chip.i_rift2Core.diff.FReg2_fs_5;
  assign trace2_abi_fs6  = s_Rift2Chip.i_rift2Core.diff.FReg2_fs_6;
  assign trace2_abi_fs7  = s_Rift2Chip.i_rift2Core.diff.FReg2_fs_7;
  assign trace2_abi_fs8  = s_Rift2Chip.i_rift2Core.diff.FReg2_fs_8;
  assign trace2_abi_fs9  = s_Rift2Chip.i_rift2Core.diff.FReg2_fs_9;
  assign trace2_abi_fs10 = s_Rift2Chip.i_rift2Core.diff.FReg2_fs_10;
  assign trace2_abi_fs11 = s_Rift2Chip.i_rift2Core.diff.FReg2_fs_11;
  assign trace2_abi_ft8  = s_Rift2Chip.i_rift2Core.diff.FReg2_ft_8;
  assign trace2_abi_ft9  = s_Rift2Chip.i_rift2Core.diff.FReg2_ft_9;
  assign trace2_abi_ft10 = s_Rift2Chip.i_rift2Core.diff.FReg2_ft_10;
  assign trace2_abi_ft11 = s_Rift2Chip.i_rift2Core.diff.FReg2_ft_11;


  // for ( i <- 0 until 8 )  yield { FReg.ft(i) := io.diffFReg(i) }
  // for ( i <- 0 until 2 )  yield { FReg.fs(i) := io.diffFReg(8+i) }
  // for ( i <- 0 until 8 )  yield { FReg.fa(i) := io.diffFReg(10+i) }
  // for ( i <- 0 until 10 ) yield { FReg.fs(2+i) := io.diffFReg(18+i) }
  // for ( i <- 0 until 4 )  yield { FReg.ft(8+i) := io.diffFReg(28+i) }

  assign trace_pc_0 = s_Rift2Chip.i_rift2Core.diff.io_commit_pc_0;
  assign trace_pc_1 = s_Rift2Chip.i_rift2Core.diff.io_commit_pc_1;
  assign trace_comfirm_0 = s_Rift2Chip.i_rift2Core.diff.io_commit_comfirm_0;
  assign trace_comfirm_1 = s_Rift2Chip.i_rift2Core.diff.io_commit_comfirm_1;
  assign trace_abort_0 = s_Rift2Chip.i_rift2Core.diff.io_commit_abort_0;
  assign trace_abort_1 = s_Rift2Chip.i_rift2Core.diff.io_commit_abort_1;

  assign trace_priv = s_Rift2Chip.i_rift2Core.diff.io_commit_priv_lvl;


	assign trace_mstatus   = s_Rift2Chip.i_rift2Core.diff.io_csr_mstatus;
	assign trace_mtvec     = s_Rift2Chip.i_rift2Core.diff.io_csr_mtvec;
	assign trace_mscratch  = s_Rift2Chip.i_rift2Core.diff.io_csr_mscratch;
	assign trace_mepc      = s_Rift2Chip.i_rift2Core.diff.io_csr_mepc;
	assign trace_mcause    = s_Rift2Chip.i_rift2Core.diff.io_csr_mcause;
	assign trace_mtval     = s_Rift2Chip.i_rift2Core.diff.io_csr_mtval;
	assign trace_mvendorid = s_Rift2Chip.i_rift2Core.diff.io_csr_mvendorid;
	assign trace_marchid   = s_Rift2Chip.i_rift2Core.diff.io_csr_marchid;
	assign trace_mimpid    = s_Rift2Chip.i_rift2Core.diff.io_csr_mimpid;
	assign trace_mhartid   = s_Rift2Chip.i_rift2Core.diff.io_csr_mhartid;
	assign trace_misa      = s_Rift2Chip.i_rift2Core.diff.io_csr_misa;
	assign trace_mie       = s_Rift2Chip.i_rift2Core.diff.io_csr_mie;
	assign trace_mip       = s_Rift2Chip.i_rift2Core.diff.io_csr_mip;
	assign trace_medeleg   = s_Rift2Chip.i_rift2Core.diff.io_csr_medeleg;
	assign trace_mideleg   = s_Rift2Chip.i_rift2Core.diff.io_csr_mideleg;

// assign trace_mcounteren    = s_Rift2Chip.i_rift2Core.diff.io_csr_mcounteren;
// assign trace_mcountinhibit = s_Rift2Chip.i_rift2Core.diff.io_csr_mcountinhibit;
// assign trace_tselect       = s_Rift2Chip.i_rift2Core.diff.io_csr_tselect;
// assign trace_tdata1        = s_Rift2Chip.i_rift2Core.diff.io_csr_tdata1;
// assign trace_tdata2        = s_Rift2Chip.i_rift2Core.diff.io_csr_tdata2;
// assign trace_tdata3        = s_Rift2Chip.i_rift2Core.diff.io_csr_tdata3;
// assign trace_mhpmevent     = s_Rift2Chip.i_rift2Core.diff.io_csr_mhpmevent;

	assign trace_pmpcfg_0 = s_Rift2Chip.i_rift2Core.diff.io_csr_pmpcfg_0;
	// assign trace_pmpcfg_1 = s_Rift2Chip.i_rift2Core.diff.io_csr_pmpcfg_1;
	// assign trace_pmpcfg_2 = s_Rift2Chip.i_rift2Core.diff.io_csr_pmpcfg_2;
	// assign trace_pmpcfg_3 = s_Rift2Chip.i_rift2Core.diff.io_csr_pmpcfg_3;

	assign trace_pmpaddr_0  = s_Rift2Chip.i_rift2Core.diff.io_csr_pmpaddr_0;
	assign trace_pmpaddr_1  = s_Rift2Chip.i_rift2Core.diff.io_csr_pmpaddr_1;
	assign trace_pmpaddr_2  = s_Rift2Chip.i_rift2Core.diff.io_csr_pmpaddr_2;
	assign trace_pmpaddr_3  = s_Rift2Chip.i_rift2Core.diff.io_csr_pmpaddr_3;
	assign trace_pmpaddr_4  = s_Rift2Chip.i_rift2Core.diff.io_csr_pmpaddr_4;
	assign trace_pmpaddr_5  = s_Rift2Chip.i_rift2Core.diff.io_csr_pmpaddr_5;
	assign trace_pmpaddr_6  = s_Rift2Chip.i_rift2Core.diff.io_csr_pmpaddr_6;
	assign trace_pmpaddr_7  = s_Rift2Chip.i_rift2Core.diff.io_csr_pmpaddr_7;
	// assign trace_pmpaddr_8  = s_Rift2Chip.i_rift2Core.diff.io_csr_pmpaddr_8;
	// assign trace_pmpaddr_9  = s_Rift2Chip.i_rift2Core.diff.io_csr_pmpaddr_9;
	// assign trace_pmpaddr_10 = s_Rift2Chip.i_rift2Core.diff.io_csr_pmpaddr_10;
	// assign trace_pmpaddr_11 = s_Rift2Chip.i_rift2Core.diff.io_csr_pmpaddr_11;
	// assign trace_pmpaddr_12 = s_Rift2Chip.i_rift2Core.diff.io_csr_pmpaddr_12;
	// assign trace_pmpaddr_13 = s_Rift2Chip.i_rift2Core.diff.io_csr_pmpaddr_13;
	// assign trace_pmpaddr_14 = s_Rift2Chip.i_rift2Core.diff.io_csr_pmpaddr_14;
	// assign trace_pmpaddr_15 = s_Rift2Chip.i_rift2Core.diff.io_csr_pmpaddr_15;

	assign trace_stvec    = s_Rift2Chip.i_rift2Core.diff.io_csr_stvec;
	assign trace_sscratch = s_Rift2Chip.i_rift2Core.diff.io_csr_sscratch;
	assign trace_sepc     = s_Rift2Chip.i_rift2Core.diff.io_csr_sepc;
	assign trace_scause   = s_Rift2Chip.i_rift2Core.diff.io_csr_scause;
	assign trace_stval    = s_Rift2Chip.i_rift2Core.diff.io_csr_stval;
	assign trace_satp     = s_Rift2Chip.i_rift2Core.diff.io_csr_satp;
	// assign trace_scounteren = s_Rift2Chip.i_rift2Core.diff.io_csr_scounteren;
	// assign trace_dcsr       = s_Rift2Chip.i_rift2Core.diff.io_csr_dcsr;
	// assign trace_dpc        = s_Rift2Chip.i_rift2Core.diff.io_csr_dpc;
	// assign trace_dscratch   = s_Rift2Chip.i_rift2Core.diff.io_csr_dscratch;

	assign trace_fflags    = s_Rift2Chip.i_rift2Core.diff.io_csr_fflags;
	assign trace_frm       = s_Rift2Chip.i_rift2Core.diff.io_csr_frm;

  assign trace_mcycle     = s_Rift2Chip.i_rift2Core.diff.io_csr_mcycle;
  assign trace_minstret   = s_Rift2Chip.i_rift2Core.diff.io_csr_minstret;
  assign trace_scsBPredict = s_Rift2Chip.i_rift2Core.diff.io_csr_mhpmcounter_3;
  assign trace_misBPredict = s_Rift2Chip.i_rift2Core.diff.io_csr_mhpmcounter_4;
  assign trace_scsJPredict = s_Rift2Chip.i_rift2Core.diff.io_csr_mhpmcounter_5;
  assign trace_misJPredict = s_Rift2Chip.i_rift2Core.diff.io_csr_mhpmcounter_6;


endmodule






