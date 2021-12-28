/*
* @File name: random_op_rsl
* @Author: Ruige Lee
* @Email: wut.ruigeli@gmail.com
* @Date:   2021-12-21 11:34:39
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-12-21 12:05:07
*/
module random_op_rsl(
	input clock,
	input reset,

  output         io_deq_0_ready,
  output         io_deq_1_ready,

  output reg        io_is_op_rsl_0_ready,

  output reg       io_is_op_rsl_1_ready,

  output reg        io_is_op_rsl_2_ready,

  output reg        io_is_op_rsl_3_ready

);

wire [15:0] random;

lfsr i_lfsr
(
	.random(random),

	.CLK(clock)
);


assign io_deq_0_ready = 1'b1;
assign io_deq_1_ready = 1'b1;


always @(posedge clock or posedge reset) begin
	if(reset) begin
		io_is_op_rsl_0_ready <= 0;
		io_is_op_rsl_1_ready <= 0;
		io_is_op_rsl_2_ready <= 0;
		io_is_op_rsl_3_ready <= 0;
	end else begin
		io_is_op_rsl_0_ready <= ((random[6:0] < 50) ? 1'b1 : 1'b0);
		io_is_op_rsl_1_ready <= ((random[7:1] < 50) ? 1'b1 : 1'b0);
		io_is_op_rsl_2_ready <= ((random[8:2] < 50) ? 1'b1 : 1'b0);
		io_is_op_rsl_3_ready <= ((random[9:3] < 50) ? 1'b1 : 1'b0);

	end
end


endmodule

