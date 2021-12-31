

module random_csr_req(
  input         clock,
  input         reset,

  input        io_enq_0_ready,
  output reg   io_enq_0_valid,
  output reg  io_enq_0_bits_isa_fcsr_rw,


  input           io_enq_1_ready,
  output  reg     io_enq_1_valid,
  output  reg     io_enq_1_bits_isa_fcsr_rs


);

wire [15:0] random;

lfsr i_lfsr
(
	.random(random),

	.CLK(clock)
);

always @(posedge clock or posedge reset) begin
	if(reset) begin
		io_enq_0_valid <= 1'b0;
		io_enq_0_bits_isa_fcsr_rw <= 1'b0;

		io_enq_1_valid <= 1'b0;
		io_enq_1_bits_isa_fcsr_rs <= 1'b0;		
	end else begin
		if ( io_enq_0_valid & ~io_enq_0_ready) begin
		end else begin
			io_enq_0_valid <= 1'b1;
			io_enq_0_bits_isa_fcsr_rw <= ((random[9:0] < 100) ? 1'b1 : 1'b0);
		end

		if ( io_enq_1_valid & ~io_enq_1_ready) begin
		end else begin
			io_enq_1_valid <= ((random[6:0]  < 25) ? 1'b1 : 1'b0);
			io_enq_1_bits_isa_fcsr_rs <= ((random[6:0]  < 25) ? 1'b1 : 1'b0) & ((random[10:1] < 100) ? 1'b1 : 1'b0);
		end
	end
end


endmodule // random_csr_req



