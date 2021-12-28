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

