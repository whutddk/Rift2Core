/*
* @File name: tl_mem
* @Author: Ruige Lee
* @Email: wut.ruigeli@gmail.com
* @Date:   2021-04-20 19:55:02
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-21 15:51:20
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


module tl_mem (

	input [2:0] tlslv_a_opcode,
	input [2:0] tlslv_a_param,
	input [7:0] tlslv_a_size,
	input [2:0] tlslv_a_source,
	input [31:0] tlslv_a_address,
	input [15:0] tlslv_a_mask,
	input [127:0] tlslv_a_data,
	input tlslv_a_corrupt,

	input tlslv_a_valid,
	output tlslv_a_ready,

	output [2:0] tlslv_d_opcode,
	output [1:0] tlslv_d_param,
	output [7:0] tlslv_d_size,
	output [2:0] tlslv_d_source,
	output [2:0] tlslv_d_sink,
	output tlslv_d_denied,
	output [127:0] tlslv_d_data,
	output tlslv_d_corrupt,

	output tlslv_d_valid,
	input tlslv_d_ready,

	input clk,
	input rst
	
);

	wire is_a_ack = tlslv_a_valid & tlslv_a_ready;
	wire is_d_ack = tlslv_d_valid & tlslv_d_ready;




	reg is_wr_trans;
	reg is_rd_trans;
	reg [7:0] remain;
	reg [31:0] rsp_addr;
	reg [7:0] d_size;


	always @(posedge clk or posedge rst) begin
		if (rst) begin
			is_wr_trans <= 1'd0;
			is_rd_trans <= 1'd0;
			remain <= 0;
			rsp_addr <= 32'd0;
			d_size <= 0;
		end

		else begin
			if ( ~is_wr_trans & ~is_rd_trans & is_a_ack ) begin
				remain <= (1 << tlslv_a_size) - 16;
				rsp_addr <= tlslv_a_address + 32'b10000;
				d_size <= tlslv_a_size;

				if ( tlslv_a_opcode == 0 ) is_wr_trans <= 1'b1;
				if ( tlslv_a_opcode == 4 ) is_rd_trans <= 1'b1;
			end

			if ( (is_rd_trans & is_d_ack) | (is_wr_trans & is_a_ack) ) begin
				remain <= remain - 16;
				rsp_addr <= rsp_addr + 32'b10000;
			end

			if ( remain == 0 & is_wr_trans) begin
				is_wr_trans <= 1'd0;

			end
			if ( remain == 0 & is_rd_trans) begin
				is_rd_trans <= 1'd0;
			end
		end
	end

	wire w_last = remain == 0 & is_wr_trans;



	// // reg [7:0] a_remain;
	// reg [7:0] d_remain;

	// reg [31:0] rsp_addr;
	// reg [2:0] state;
	// reg [7:0] d_size;

	// wire is_d_busy = (d_remain != 0);

	assign tlslv_a_ready = 1'b1;

	// reg d_valid;
	// always @(posedge clk or posedge rst) begin
	// 	if(rst) begin
	// 		d_valid <= 0;
	// 	end else begin
	// 		if ( is_wr_trans & remain == 0) begin
	// 			d_valid <= 1'b1;
	// 		end
	// 		else if ( is_rd_trans & remain != 0 ) begin
	// 			d_valid <= 1'b1;
	// 		end
	// 		else d_valid <= 1'b0;
	// 	end
	// end
	assign tlslv_d_valid = (is_wr_trans & remain == 0) | (is_rd_trans );



	// always @(posedge clk or posedge rst) begin
	// 	if (rst) begin
	// 		d_valid <= 1'd0;

	// 		d_remain <= 8'd0;

	// 		rsp_addr <= 32'd0;
	// 		state <= 3'd0;
	// 		d_size <= 8'd0;

	// 	end else begin
	// 		if ( tlslv_a_valid & tlslv_a_ready ) begin
				
	// 			if ( ~is_d_busy ) begin
	// 				d_remain <= 1 << tlslv_a_size;
	// 				d_size <= tlslv_a_size;
	// 				rsp_addr <= tlslv_a_address;
	// 				state <= tlslv_a_opcode;
	// 			end

	// 		end

	// 		if (tlslv_d_valid & tlslv_d_ready) begin
	// 			if ( is_d_busy ) begin
	// 				d_remain <= d_remain - 16;
	// 				rsp_addr <= rsp_addr + 32'b10000;
	// 			end
	// 		end

	// 		if ( is_d_busy ) begin
	// 			if ( state == 3'd4 ) begin
	// 				d_valid <= 1'd1;					
	// 			end
	// 			if ( state == 3'd0 ) begin

	// 			end


	// 		end
	// 		else begin
	// 			d_valid <= 1'd0;
	// 		end
	// 	end
	// end

localparam DP = 2**14;

	reg [127:0] ram[0:4096-1];
	reg [127:0] ram_out;

	assign tlslv_d_opcode = is_rd_trans ? 0 : 1;
	assign tlslv_d_param = 2'd0;
	assign tlslv_d_size = d_size;
	assign tlslv_d_source = 0;
	assign tlslv_d_sink = 0;
	assign tlslv_d_denied = 1'd0;
	assign tlslv_d_data = ram_out;
	assign tlslv_d_corrupt = 1'd0;

	wire [13:0] addr = ( ~is_rd_trans & ~is_wr_trans ) ? tlslv_a_address[4 +: 12] : rsp_addr[4 +: 12];



	always @(posedge clk or posedge rst) begin
		if ( tlslv_a_valid & tlslv_a_ready & tlslv_a_opcode == 0 ) begin
			ram[addr] <= tlslv_a_data;
		end

		ram_out <= ram[addr];

	end
	








reg [7:0] mem [0:200000];

integer i, by;
initial begin
	$readmemh("./ci/rv64ui-p-simple.verilog", mem);
	for ( i = 0; i < DP; i = i + 1 ) begin
		for ( by = 0; by < 16; by = by + 1 ) begin
			if ( | mem[i*16+by] ) begin
				ram[i][8*by +: 8] = mem[i*16+by];
			end
			else begin
				ram[i][8*by +: 8] = 8'h0;
			end
		end


	end

end 









endmodule



