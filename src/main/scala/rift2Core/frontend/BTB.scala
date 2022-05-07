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

package rift2Core.frontend

import chisel3._
import chisel3.util._







class BTB extends IFetchModule {

  val io = IO(new Bundle{
    val reqFromIF1  = Flipped(Decoupled(new Info_IF1))
    val reqFromIF3  = Flipped(Decoupled())

    val resp = Valid( Vec( fetch_instr, new Info_BTB) )
  })



  val rd_cl_sel  = HashTo0( in1 = io.reqFromIF1.bits.addr >> 3 << 3, len = log2Ceil(btb_cl) )
  val wr_cl_sel  = HashTo0( in1 = io.reqFromIF3.bits.addr >> 3 << 3, len = log2Ceil(btb_cl) )

  val rd_tag_sel = for ( i <- 0 until fetch_instr ) yield {
    HashTo1( in1 = (io.reqFromIF1.bits.addr >> 3 << 3) + (i*2).U, len = btb_tag_w ),
  }


  val wr_tag_sel = HashTo1( in1 = io.reqFromIF3.bits.addr, len = btb_tag_w )

  val btb_valid = RegInit( VecInit( Seq.fill(btb_cl)( VecInit(Seq.fill(btb_cb)(false.B)) ) ) )
  val btb_table = for( j <- 0 until btb_cb ) yield {
    Mem( btb_cl, new Info_BTB )
  }

  val btb_info = for( j <- 0 until btb_cb ) yield {
    btb_table(j).read(rd_cl_sel)
  }

  val is_hit = for ( i <- 0 until fetch_instr ) yield {
    for ( j <- 0 until btb_cb ) yield {
      btb_info(j).tag === rd_tag_sel(i) & btb_valid(btb_cl)(j)
    }
  }

  /** hit should be one hot, or that is a hash collision*/
  val is_hit_invalid = for ( i <- 0 until fetch_instr ) yield {
    when(PopCount( is_hit(i) ) > 1.U ) {
      printf("Warning, a hash collision happened!")
      true.B
    } .otherwise { false.B }
  }


  val hit_sel = for ( i <- 0 until fetch_instr ) yield {
    OHToUInt(is_hit(i).asUInt)
  }


  when( io.reqFromIF1.fire ) {
    ( 0 until fetch_instr ).map{ i => {
      when( is_hit(i).reduce(_|_) & ~is_hit_invalid ) {
        io.resp(i) := btb_info(hit_sel(i))
      }
      
    }}
    
  }


  when( io.reqFromIF3.fire ) {
    val cb_allinUsed = btb_valid(wr_cl_sel).forall( (x:Bool) => (x === true.B))
    val cb_empty_sel = btb_valid(wr_cl_sel).indexWhere( (x:Bool) => (x === false.B))
    val ram_cb = LFSR16()

    val rpl_cb = Mux( cb_allinUsed, ram_cb, cb_empty_sel )

    btb_table(rpl_cb).write(wr_cl_sel, io.reqFromIF3.bits)
    btb_valid(wr_cl_sel)(rpl_cb) := true.B

  } .elsewhen( is_hit_invalid.reduce(_|_) ) {
    for( j <- 0 until btb_cb ) yield {
      btb_table(j).write(rd_cl_sel, 0.U.asTypeOf(new Info_BTB) )
      btb_valid(wr_cl_sel)(j) := false.B
    }
    
  }


}
