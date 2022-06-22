


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

package rift2Core.backend.memory

import chisel3._
import chisel3.util._
import rift2Core.define._
import rift2Core.backend._

import rift2Core.L1Cache._
import rift2Core.privilege._
import rift._
import base._
import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import chisel3.experimental.dataview._







object Strb2Mask{
  def apply(strb: UInt): UInt = {

    Cat(strb.asBools.map{ x => Fill(8, x) }.reverse)


    // val mask = Wire(UInt(64.W))

    // // for ( i <- 0 until 8 ) yield {
    // //   mask(8*i+7,8*i) := Fill(8, strb(i))
    // // } 
    // mask := Cat(
    //   Fill(8, strb(7)), Fill(8, strb(6)),
    //   Fill(8, strb(5)), Fill(8, strb(4)),
    //   Fill(8, strb(3)), Fill(8, strb(2)),
    //   Fill(8, strb(1)), Fill(8, strb(0))
    // )
    // mask
  } 
}


object pkg_Info_cache_s0s1{
  def apply( ori: Info_miss_rsp )(implicit p: Parameters) = {
    val res = Wire(new Info_cache_s0s1)

    res.paddr := ori.paddr
    res.wstrb := "hFFFFFFFF".U
    res.wdata := ori.wdata

    {
      res.fun := 0.U.asTypeOf(new Cache_op)
      res.fun.grant := true.B      
    }
    res.rd := 0.U.asTypeOf(new Register_dstntn(64))
    res.chk_idx := 0.U
    res
  }
  
  def apply( ori: Info_probe_req )(implicit p: Parameters) = {
    val res = Wire(new Info_cache_s0s1)
    res.paddr := ori.paddr
    res.wstrb := 0.U
    res.wdata := 0.U

    {
      res.fun := 0.U.asTypeOf(new Cache_op)
      res.fun.probe := true.B      
    }
    res.rd := 0.U.asTypeOf(new Register_dstntn(64))
    res.chk_idx := 0.U
    res
  }

  /** package write and amo operation*/
  def apply( ori: Lsu_iss_info, overlapReq: Stq_req_Bundle, overlapResp: Stq_resp_Bundle)(implicit p: Parameters) = {

    val res = Wire(new Info_cache_s0s1)

    res.paddr := ori.paddr
    res.wdata := Mux( ori.fun.is_lu, reAlign_data( from = 64, to = 256, data = overlapResp.wdata, addr = overlapReq.paddr ), ori.wdata_align256)
    res.wstrb := Mux( ori.fun.is_lu, reAlign_strb( from = 64, to = 256, strb = overlapResp.wstrb, addr = overlapReq.paddr ), ori.wstrb_align256)


    {
      res.fun := 0.U.asTypeOf(new Cache_op)
      res.fun.viewAsSupertype(new Lsu_isa) := ori.fun.viewAsSupertype(new Lsu_isa)

    }
    res.rd.rd0 := ori.param.rd0

    res.chk_idx := 0.U
    res
  
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
  def apply( fun: Lsu_isa, paddr: UInt, rdata: UInt ) = {
    require( rdata.getWidth == 64 )
    val res = Wire(UInt(64.W))

    // def reAlign(rdata: UInt, paddr: UInt) = {
    //   val res = Wire(UInt(64.W))
    //   res := rdata >> (paddr(2,0) << 3)
    //   res
    // }

    def load_byte(is_usi: Bool, rdata: UInt): UInt = Cat( Fill(56, Mux(is_usi, 0.U, rdata(7)) ),  rdata(7,0)  )
    def load_half(is_usi: Bool, rdata: UInt): UInt = Cat( Fill(48, Mux(is_usi, 0.U, rdata(15)) ), rdata(15,0) )
    def load_word(is_usi: Bool, rdata: UInt): UInt = Cat( Fill(32, Mux(is_usi, 0.U, rdata(31)) ), rdata(31,0) )


    
    val align = reAlign_data( from = 64, to = 8, rdata, paddr )

    res := Mux1H(Seq(
      fun.is_byte -> load_byte(fun.is_usi, align),
      fun.is_half -> load_half(fun.is_usi, align),
      fun.is_word -> load_word(fun.is_usi, align),
      fun.is_dubl -> align
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

