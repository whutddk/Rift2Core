

module random_wb_ack(
	input clock,
	input reset,

  output reg io_is_wb_ack_0,
  output reg io_is_wb_ack_1,
  output reg io_is_wb_ack_2
);


wire [15:0] random;

lfsr i_lfsr
(
	.random(random),

	.CLK(clock)
);

always @(posedge clock or posedge reset) begin
	if(reset) begin
		io_is_wb_ack_0 <= 1'b0;
		io_is_wb_ack_1 <= 1'b0;
		io_is_wb_ack_2 <= 1'b0;
	end else begin
		io_is_wb_ack_0 <= (( random[6:0] < 50) ? 1'b1 : 1'b0);
		io_is_wb_ack_1 <= (( random[7:1] < 50) ? 1'b1 : 1'b0);
		io_is_wb_ack_2 <= (( random[8:2] < 50) ? 1'b1 : 1'b0);
	end
end

endmodule

