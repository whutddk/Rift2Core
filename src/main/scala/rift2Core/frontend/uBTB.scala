/*
  Copyright (c) 2020 - 2024 Wuhan University of Technology <295054118@whut.edu.cn>

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
import rift2Core.define._
import org.chipsalliance.cde.config._

import chisel3.util.random._

abstract class uBTBBase()(implicit p: Parameters) extends IFetchModule {

  class uBTBIO extends Bundle{
    val req  = Input(new uBTBReq_Bundle)
    val resp = Output( new uBTBResp_Bundle )

    val update = Flipped(Valid(new uBTBUpdate_Bundle))
    val if4Redirect = Flipped(Valid(new IF4_Redirect_Bundle))    
  }

  val io: uBTBIO = IO(new uBTBIO)


  val buff    = RegInit(VecInit( Seq.fill(uBTB_entry)(0.U(vlen.W))))
  val tag     = RegInit(VecInit( for( entry <- 0 until uBTB_entry ) yield { ~(entry.U)(uBTB_tag_w.W) } ) )
  val isValid = RegInit(VecInit( Seq.fill(uBTB_entry)(false.B) ) )
}

trait uBTBLookup { this: uBTBBase =>

  val reqTag = Wire(Vec(ftChn-1, UInt(uBTB_tag_w.W) )) //never predict at (ftChn-1).U

  for( i <- 0 until ftChn-1 ) yield {
    reqTag(i) := io.req.pc(1+uBTB_tag_w-1, 1) + i.U
  }

  for ( i <- 0 until ftChn ) {io.resp.isRedirect(i) := false.B; io.resp.target := 0.U} // will be override

  for( i <- (ftChn-2) to 0 by -1 ) {
    when( ((ftChn-1).U - io.req.pc( (log2Ceil(ftChn*16/8)-1) ,1)) >= i.U ) {
      for ( entry <- 0 until uBTB_entry ) {
        when( reqTag(i) === tag(entry) & isValid(entry) ) {
          io.resp.isRedirect(i) := true.B
          io.resp.target := buff(entry)
          for ( j <- i+1 until ftChn ) { io.resp.isRedirect(j) := false.B }
        }
      }
    }
  }

  for( i <- 0 until ftChn ) {  //never predict at (ftChn-1).U or next frame
    when( io.req.pc((log2Ceil(ftChn*16/8)-1),1) >= i.U ) {
      io.resp.isRedirect((ftChn-1)-i) := false.B      
    }
  }

  assert( io.resp.isRedirect((ftChn-1)) === false.B )



  when( io.resp.isRedirect.reduce(_|_) === false.B ) {
    for ( i <- 0 until ftChn )
    io.resp.isActive(i) := (((ftChn-1).U - io.req.pc((log2Ceil(ftChn*16/8)-1),1)) >= i.U)
  } .otherwise {
    for ( i <- 0 until ftChn ) {
      if ( i == 0 ) {
        io.resp.isActive(i) := Mux( io.resp.isRedirect(i), true.B, io.resp.isActive(i+1) )
      } else if ( i > 0 && i < (ftChn-1) ) {
        io.resp.isActive(i) := Mux( io.resp.isRedirect(i) || (io.resp.isRedirect(i-1) ), true.B, io.resp.isActive(i+1) )
      } else { //i == (ftChn-1)
        io.resp.isActive(i) := io.resp.isRedirect(i-1)
      }
    }
  }

  assert( PopCount( io.resp.isRedirect ) <= 1.U )

}



trait uBTBFlush { this: uBTBBase => 
  val flushTag = io.if4Redirect.bits.pc(1+uBTB_tag_w-1,1)
  val isFlushHit = VecInit(( 0 until uBTB_entry ).map{ i => ((tag(i) === flushTag) & (isValid(i) === true.B)) })
  val entryFlushSel = OHToUInt(isFlushHit)
  when( io.if4Redirect.fire & io.if4Redirect.bits.isDisAgree ) {
    assert( PopCount(isFlushHit) <= 1.U )
    when( isFlushHit.reduce(_|_) ) {
      buff(entryFlushSel)    := 0.U
      tag(entryFlushSel)     := 0.U
      isValid(entryFlushSel) := false.B        
    }
  }
}

trait uBTBUpdate { this: uBTBBase =>
  val updateTag = io.update.bits.pc(1+uBTB_tag_w-1,1)
  val isUpdateHit = VecInit(( 0 until uBTB_entry ).map{ i => ((tag(i) === updateTag) & (isValid(i) === true.B)) })
  when( io.update.fire ) { assert( PopCount(isUpdateHit) <= 1.U ) }
  val hitSel = OHToUInt(isUpdateHit)
  val isFull = isValid.reduce(_&_)

  val entryUpdateSel =
    Mux( isUpdateHit.reduce(_|_), hitSel,
      Mux( isFull, LFSR( log2Ceil(uBTB_entry), true.B), isValid.indexWhere((x:Bool) => (x === false.B)) ) )

  when( io.update.fire ) {
    when( io.update.bits.pc((log2Ceil(ftChn*16/8)-1),1) =/= (ftChn-1).U ) { //never predict at (ftChn-1).U
      when( io.update.bits.isTaken ) {
        buff(entryUpdateSel)  := io.update.bits.target
        tag(entryUpdateSel)   := io.update.bits.pc(1+uBTB_tag_w-1,1)
        isValid(entryUpdateSel) := true.B        
      }

    }
  }
}


class uBTB()(implicit p: Parameters) extends uBTBBase with uBTBLookup with uBTBFlush with uBTBUpdate {
}


class FakeuBTB()(implicit p: Parameters) extends uBTBBase {

  io.resp := 0.U.asTypeOf(new uBTBResp_Bundle)
  for ( i <- 0 until ftChn ) {
    io.resp.isActive(i) := (((ftChn-1).U - io.req.pc((log2Ceil(ftChn*16/8)-1),1)) >= i.U)    
  }

  

}
