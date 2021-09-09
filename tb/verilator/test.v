/*
* @File name: test
* @Author: Ruige Lee
* @Email: wut.ruigeli@gmail.com
* @Date:   2021-09-07 11:05:57
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-09-07 15:33:52
*/


module test(
	input CLK,
	input RSTn
);


reg [255:0] testName;
reg [255:0] testName1;





reg [7:0] mem [0:200000];

localparam DP = 2**14;

initial begin

if ( $test$plusargs("abcd") ) begin
	$display("find abcd");
end else begin
	$display("no abcd");
end

  $value$plusargs("ab=%s",testName[255:0]);
  $value$plusargs("cd=%s",testName1[255:0]);


  $display("Begin");
   $display("%s",testName);
   $display("%s",testName1);

  $display("End");
end

endmodule

