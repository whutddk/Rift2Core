


/*
  Copyright (c) 2020 - 2023 Wuhan University of Technology <295054118@whut.edu.cn>

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

package rift2Core.backend.lsu

import chisel3._
import chisel3.util._

import rift2Core.define._

import org.chipsalliance.cde.config._
import chisel3.experimental.dataview._







object Strb2Mask{
  def apply(strb: UInt): UInt = {
    Cat(strb.asBools.map{ x => Fill(8, x) }.reverse)
  } 
}




object overlap_wr{
  def apply( ori: UInt, ori_wstrb: UInt, wdata: UInt, wstrb: UInt): (UInt, UInt) = {
    require( ori.getWidth == wdata.getWidth )
    require( ori_wstrb.getWidth == wstrb.getWidth )

    val wmask = Strb2Mask(wstrb)

    val new_data = (ori & ~wmask) | (wdata & wmask)
    val new_strb = ori_wstrb | wstrb

    return (new_data, new_strb)
  }
}

object get_loadRes{
  def apply( fun: Lsu_isa, vsew: UInt, paddr: UInt, rdata: UInt ) = {
    require( rdata.getWidth == 64 )
    val res = Wire(UInt(64.W))

    def load_byte(is_usi: Bool, rdata: UInt): UInt = Cat( Fill(56, Mux(is_usi, 0.U, rdata(7)) ),  rdata(7,0)  )
    def load_half(is_usi: Bool, rdata: UInt): UInt = Cat( Fill(48, Mux(is_usi, 0.U, rdata(15)) ), rdata(15,0) )
    def load_word(is_usi: Bool, rdata: UInt): UInt = Cat( Fill(32, Mux(is_usi, 0.U, rdata(31)) ), rdata(31,0) )

    val align = reAlign_data( from = 64, to = 8, rdata, paddr )

    res := Mux1H(Seq(
      fun.isByte    -> load_byte(fun.is_usi, align),
      fun.isHalf    -> load_half(fun.is_usi, align),
      fun.isWord    -> load_word(fun.is_usi, align),
      fun.isDubl    -> align,
      fun.isDynamic -> Mux1H(Seq(
                        (vsew === "b000".U) -> load_byte(fun.is_usi, align),
                        (vsew === "b001".U) -> load_half(fun.is_usi, align),
                        (vsew === "b010".U) -> load_word(fun.is_usi, align),
                        (vsew === "b011".U) -> align,
                      ))

    ))  

    res
  }
}

object reAlign_data{
  def apply( from: Int, to: Int, data: UInt, addr: UInt ): UInt = {
    require( isPow2(from) )
    require( isPow2(to) )
    require( data.getWidth == from )
    val from_lsb = log2Ceil(from/8)
    val to_lsb   = log2Ceil(to/8)

    val align_data = Wire(UInt( (to max 64).W ))
    if ( from > to ) {
      align_data := data >> ( addr( from_lsb-1, 0) >> to_lsb << to_lsb << 3 ) 
    } else if ( from < to ) {
      align_data := data << ( addr( to_lsb-1,0 ) >> from_lsb << from_lsb << 3 )
    } else {
      align_data := data
    }
    return align_data
  }
}

object reAlign_strb{
  def apply( from: Int, to: Int, strb: UInt, addr: UInt ): UInt = {
    require( isPow2(from) )
    require( isPow2(to) )
    require( strb.getWidth == from/8 )
    val from_lsb = log2Ceil(from/8)
    val to_lsb   = log2Ceil(to/8)

    val align_strb = Wire(UInt( (to/8 max 8).W ))
    if ( from > to ) {
      align_strb := strb >> ( addr( from_lsb-1, 0) >> to_lsb << to_lsb) 
    } else if ( from < to ) {
      align_strb := strb << ( addr( to_lsb-1, 0 ) >> from_lsb << from_lsb )
    } else {
      align_strb := strb
    }
    return align_strb
  }
}

