

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



module tl_mem_tb (

	
);


	reg clk;
	reg rst;

	reg [2:0] tlslv_a_opcode;
	reg [2:0] tlslv_a_param;
	reg [7:0] tlslv_a_size;
	reg [2:0] tlslv_a_source;
	reg [31:0] tlslv_a_address;
	reg [15:0] tlslv_a_mask;
	reg [127:0] tlslv_a_data;
	reg tlslv_a_corrupt;
	reg tlslv_a_valid;
	reg tlslv_d_ready;


	wire tlslv_a_ready;
	wire [2:0] tlslv_d_opcode;
	wire [1:0] tlslv_d_param;
	wire [7:0] tlslv_d_size;
	wire [2:0] tlslv_d_source;
	wire [2:0] tlslv_d_sink;
	wire tlslv_d_denied;
	wire [127:0] tlslv_d_data;
	wire tlslv_d_corrupt;
	wire tlslv_d_valid;












tl_mem s_tl_mem(
	.tlslv_a_opcode (tlslv_a_opcode),
	.tlslv_a_param  (tlslv_a_param),
	.tlslv_a_size   (tlslv_a_size),
	.tlslv_a_source (tlslv_a_source),
	.tlslv_a_address(tlslv_a_address),
	.tlslv_a_mask   (tlslv_a_mask),
	.tlslv_a_data   (tlslv_a_data),
	.tlslv_a_corrupt(tlslv_a_corrupt),
	.tlslv_a_valid  (tlslv_a_valid),
	.tlslv_a_ready  (tlslv_a_ready),
	.tlslv_d_opcode (tlslv_d_opcode),
	.tlslv_d_param  (tlslv_d_param),
	.tlslv_d_size   (tlslv_d_size),
	.tlslv_d_source (tlslv_d_source),
	.tlslv_d_sink   (tlslv_d_sink),
	.tlslv_d_denied (tlslv_d_denied),
	.tlslv_d_data   (tlslv_d_data),
	.tlslv_d_corrupt(tlslv_d_corrupt),
	.tlslv_d_valid  (tlslv_d_valid),
	.tlslv_d_ready  (tlslv_d_ready),
	.clk            (clk),
	.rst            (rst)
);







initial begin
	clk = 0;
	rst = 1;

	#20

	rst <= 0;

	#160000
			$display("Time Out !!!");
	$stop;
end


initial forever #5 clk <= ~clk;

initial
begin
	$dumpfile("../build/wave.vcd"); //生成的vcd文件名称
	$dumpvars(0, tl_mem_tb);//tb模块名称
end

initial begin

	tlslv_a_opcode = 0;
	tlslv_a_param = 0;
	tlslv_a_size = 0;
	tlslv_a_source = 0;
	tlslv_a_address = 32'h0;
	tlslv_a_mask = 0;
	tlslv_a_data = 0;
	tlslv_a_corrupt = 0;
	tlslv_a_valid = 0;
	tlslv_d_ready = 1;

# 42
// 	tlslv_a_opcode = 0;
// 	tlslv_a_address = 32'h0;
// 	tlslv_a_data = 128'd1;
// 	tlslv_a_size = 5;
// 	tlslv_a_valid = 1;

// # 10
// tlslv_a_data = 128'd2;
// 	tlslv_a_valid = 1;

// # 10

// 	tlslv_a_valid = 0;

// # 10
// tlslv_a_data = 128'd4;
// 	tlslv_a_valid = 1;

// # 10
// tlslv_a_data = 128'd8;
// 	tlslv_a_valid = 1;
// # 10

// 	tlslv_a_valid = 0;




# 10
	tlslv_a_size = 5;
	tlslv_a_opcode = 4;
	tlslv_a_address = 32'h20;
	tlslv_a_valid = 1;


# 10

	tlslv_a_valid = 0;

end




endmodule

