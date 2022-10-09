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

package base

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import rift2Core.define._

import rift2Chip._

import chisel3.util.random._
import rift2Core.define._


class DatRAM(dw: Int, cl: Int) extends Module {
  // require( dw == 128 )
  // require( cl <= 256 )
  def line_w   = log2Ceil(cl)
  val io = IO(new Bundle{
    val addrr  = Input(UInt(line_w.W))
    val addrw  = Input(UInt(line_w.W))

    val dataw  = Input( Vec( dw/8, UInt(8.W) ) )
    val datawm = Input( Vec( dw/8, Bool()) )

    val datar = Output( Vec( dw/8, UInt(8.W) ) )
    val enw   = Input(Bool())
    val enr   = Input(Bool())
  })

  val datMem = SyncReadMem( cl, Vec( dw/8, UInt(8.W) ) )
  io.datar := DontCare
  when( io.enr ) {
    io.datar := datMem.read( io.addrr )
  } 
  when( io.enw ) {
    datMem.write( io.addrw, io.dataw, io.datawm )
  }

  // val datMem = Module(new Generate_sram)

  //   datMem.io.data_w  := Cat(io.dataw.reverse)
  //   datMem.io.addr_w  := io.addrw
  //   datMem.io.data_wstrb := Cat(io.datawm.reverse)
  //   datMem.io.en_w   := io.enw

  //   datMem.io.addr_r := io.addrr
  //   for ( i <- 0 until 16 ) io.datar(i) := datMem.io.data_r( 8*i+7, 8*i )
  //   datMem.io.en_r   := io.enr

  //   datMem.io.CLK := clock 

}

class Generate_sram extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle{
    
    val data_w  = Input( UInt(128.W) )
    val addr_w  = Input(UInt(4.W))
    val data_wstrb = Input( UInt(16.W)  )
    val en_w   = Input(Bool())

    val data_r = Output( UInt(128.W) )
    val addr_r  = Input(UInt(4.W))
    val en_r   = Input(Bool())

    val CLK = Input(Clock())
  })

  setInline("Generate_sram.v",
              """
module Generate_sram(
    |  	input [127:0] data_w,
    |  	input [3:0] addr_w,
    |  	input [15:0] data_wstrb,
    |  	input en_w,
    |  
    |  	output [127:0] data_r,
    |  	input [3:0] addr_r,
    |  	input en_r,
    |  
    |  	input CLK
    |  );
    |  
    |  	reg [127:0] ram[0:15];
    |  	reg [127:0] data_r_reg;
    |  
    |  	generate
    |  		for ( genvar i = 0; i < 16; i = i + 1) begin
    |  			always @(posedge CLK) begin
    |  				if (en_w) begin
    |  					if (data_wstrb[i]) begin
    |  						ram[addr_w][i*8+:8] <= #1 data_w[i*8+:8] ;					
    |  					end
    |  				end
    |  
    |  				if (en_r) begin
    |  					data_r_reg[i*8+:8] <= #1 ram[addr_r][i*8+:8];
    |  				end
    |  			end
    |  
    |  
    |  		end
    |  	endgenerate
    |  
    |  	assign data_r = data_r_reg[127:0];
    |  
    |  
    |  integer i;
    |  initial begin
    |  	for ( i = 0; i < 16; i = i + 1 ) begin
    |  		ram[i] = $random;
    |  	end
    |  
    |  	data_r_reg = $random;
    |  end
    |   
    |endmodule
    """.stripMargin)
}




class TagRAM(tag_w: Int,  cl: Int) extends Module {
  def line_w   = log2Ceil(cl)
  val io = IO(new Bundle{
    val addrr  = Input(UInt(line_w.W))
    val addrw  = Input(UInt(line_w.W))

    val dataw = Input( UInt(tag_w.W) )
    val datar = Output( UInt(tag_w.W) )
    val enw   = Input(Bool())
    val enr   = Input(Bool())
  })

  val tagMem = SyncReadMem( cl, UInt(tag_w.W) )


  // io.datar := DontCare
  // when( io.enw ) {
  //   tagMem.write( io.addr, io.dataw )
  // } .otherwise {
  //   io.datar := tagMem.read( io.addr )
  // }


  io.datar := DontCare
  when( io.enr ) {
    io.datar := tagMem.read( io.addrr )
  } 
  when( io.enw ) {
    tagMem.write( io.addrw, io.dataw )
  }




}


