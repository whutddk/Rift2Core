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
import base._
import rift2Core.define._
import chisel3.experimental.dataview._
import chipsalliance.rocketchip.config.Parameters

import chisel3.util.random._

abstract class uBTBBase()(implicit p: Parameters) extends IFetchModule {

  val io = IO(new Bundle{
    val req  = Input(new uBTBReq_Bundle)
    val resp = Output( new uBTBResp_Bundle )

    val update = Flipped(Valid(new uBTBUpdate_Bundle))
  })


  val buff  = RegInit(VecInit(Seq.fill(uBTB_entry)(0.U(64.W))))
  val tag   = RegInit(VecInit( for( entry <- 0 until uBTB_entry ) yield { ~(entry.U)(uBTB_tag_w.W) } ) )
}

trait uBTBLookup { this: uBTBBase =>

  val reqTag = Wire(Vec(7, UInt(uBTB_tag_w.W) )) //never predict at 7.U

  for( i <- 0 until 7 ) yield {
    reqTag(i) := io.req.pc(1+uBTB_tag_w-1, 1) + i.U
  }

  for ( i <- 0 until 8 ) {io.resp.isRedirect(i) := false.B; io.resp.target := 0.U} // will be override

  for( i <- 6 to 0 by -1 ) {
    when( (7.U - io.req.pc(3,1)) >= i.U ) {
      for ( entry <- 0 until uBTB_entry ) {
        when( reqTag(i) === tag(entry) ) {
          io.resp.isRedirect(i) := true.B
          io.resp.target := buff(entry)
          for ( j <- i+1 until 8 ) { io.resp.isRedirect(j) := false.B }
        }
      }
    }
  }

  for( i <- 0 until 8 ) {  //never predict at 7.U or next frame
    when( io.req.pc(3,1) >= i.U ) {
      io.resp.isRedirect(7-i) := false.B      
    }
  }

  assert( io.resp.isRedirect(7) === false.B )



  when( io.resp.isRedirect.reduce(_|_) === false.B ) {
    for ( i <- 0 until 8 )
    io.resp.isActive(i) := ((7.U - io.req.pc(3,1)) >= i.U)
  } .otherwise {
    for ( i <- 0 until 8 ) {
      if ( i == 0 ) {
        io.resp.isActive(i) := Mux( io.resp.isRedirect(i), true.B, io.resp.isActive(i+1) )
      } else if ( i > 0 && i < 7 ) {
        io.resp.isActive(i) := Mux( io.resp.isRedirect(i) || (io.resp.isRedirect(i-1) ), true.B, io.resp.isActive(i+1) )
      } else { //i == 7
        io.resp.isActive(i) := io.resp.isRedirect(i-1)
      }
    }
  }

  assert( PopCount( io.resp.isRedirect ) <= 1.U )

}

trait uBTBUpdate { this: uBTBBase =>
  val chkTag = io.update.bits.pc(1+uBTB_tag_w-1,1)
  val isHit = VecInit(tag.map{ x => (x === chkTag) })
  when( io.update.fire ) { assert( PopCount(isHit) <= 1.U ) }
  val hitSel = OHToUInt(isHit)

  val entrySel = Mux( isHit.reduce(_|_), hitSel, LFSR( log2Ceil(uBTB_entry), true.B))

  when( io.update.fire ) {
    when( io.update.bits.pc(3,1) =/= 7.U ) { //never predict at 7.U
      buff(entrySel)  := io.update.bits.target
      tag(entrySel)   := io.update.bits.pc(1+uBTB_tag_w-1,1)    
    }

  }

}

class uBTB()(implicit p: Parameters) extends uBTBBase with uBTBLookup with uBTBUpdate{
}
