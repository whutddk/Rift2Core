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
import chisel3.experimental.dataview._
import chipsalliance.rocketchip.config.Parameters

import rift2Core.define._
import base._
/**
  * instract fetch stage 3, instr pre-decode, realign, predict-state 1
  */
abstract class IF3Base()(implicit p: Parameters) extends IFetchModule {
  val io = IO(new Bundle{
    val if3_req = Vec(4, Flipped(new DecoupledIO(new IF2_Bundle) ))
    val if3_resp = Vec(2, Decoupled(new IF3_Bundle))

    val jcmm_update = Flipped(Valid(new Jump_CTarget_Bundle))
    val bcmm_update = Flipped(Valid(new Branch_CTarget_Bundle))

    val if4_update_ghist = Vec(2, Flipped(Valid(new Ghist_reflash_Bundle)))
    val if4Redirect = Flipped(Valid(new IF4_Redirect_Bundle))

    val flush = Input(Bool())
  })

  val ghist_snap = RegInit( 0.U(64.W) )
  val ghist_active = RegInit( 0.U(64.W) )

  val reAlign = Wire(Vec(4, DecoupledIO(new IF3_Bundle)))
  val combPDT = Module(new RePort( new IF3_Bundle, 4))

  val pipeLineLock = RegInit(false.B)

  val btb = Module(new BTB)
  val bim = Module(new BIM)
  val tage = Module(new TAGE)

  val predictor_ready = btb.io.isReady & bim.io.isReady & tage.io.isReady

  val if3_resp_fifo = Module(new MultiPortFifo( new IF3_Bundle, 4, 4, 2 ))

  if3_resp_fifo.io.flush := io.if4Redirect.fire | io.flush
  if3_resp_fifo.io.enq <> combPDT.io.deq
  io.if3_resp <> if3_resp_fifo.io.deq
 
}





trait IF3_PreDecode{ this: IF3Base => 
  val is_instr16 = io.if3_req.map{ _.bits.instr(1,0) =/= "b11".U }
  val is_instr32 = io.if3_req.map{ _.bits.instr(1,0) === "b11".U }

  for ( i <- 0 until 4 ) {
    reAlign(i).valid := false.B
    reAlign(i).bits  := 0.U.asTypeOf(new IF3_Bundle)
    io.if3_req(i).ready := false.B
  }
  for( i <- 0 until 4 ) yield {
    if ( i == 0 ){
      when( is_instr32(i) ) { 
        reAlign(i).bits.pc         := io.if3_req(i).bits.pc
        reAlign(i).bits.instr      := Mux( io.if3_req(i+1).bits.isFault, io.if3_req(i+1).bits.instr,              Cat(io.if3_req(i+1).bits.instr, io.if3_req(i).bits.instr) )
        reAlign(i).bits.preDecode  := Mux( io.if3_req(i+1).bits.isFault, PreDecode16(io.if3_req(i+1).bits.instr), PreDecode32(instr32 = Cat(io.if3_req(i+1).bits.instr, io.if3_req(i).bits.instr)) )
        reAlign(i).bits.predict := 0.U.asTypeOf(new Predict_Bundle) 

        reAlign(i).valid      := io.if3_req(i).fire & io.if3_req(i+1).fire & predictor_ready & ~pipeLineLock
        io.if3_req(i).ready   := reAlign(i).ready & predictor_ready & ~pipeLineLock & io.if3_req(i+1).valid
        io.if3_req(i+1).ready := reAlign(i).ready & predictor_ready & ~pipeLineLock

        assert(io.if3_req(i).fire === io.if3_req(i+1).fire)
      } .otherwise {
        reAlign(i).bits.pc         := io.if3_req(i).bits.pc
        reAlign(i).bits.instr      := io.if3_req(i).bits.instr
        reAlign(i).bits.preDecode  := PreDecode16(instr16 = io.if3_req(i).bits.instr)
        reAlign(i).bits.predict    := 0.U.asTypeOf(new Predict_Bundle) 

        reAlign(i).valid      := io.if3_req(i).fire & predictor_ready & ~pipeLineLock
        io.if3_req(i).ready   := reAlign(i).ready & predictor_ready & ~pipeLineLock
      }        
    } else {
      when( is_instr32( i-1 ) === false.B ) {
        when( is_instr32(i) ) { if ( i != 3 ) {
          reAlign(i).bits.pc         := io.if3_req(i).bits.pc
          reAlign(i).bits.instr      := Mux( io.if3_req(i+1).bits.isFault, io.if3_req(i+1).bits.instr,              Cat(io.if3_req(i+1).bits.instr, io.if3_req(i).bits.instr) )
          reAlign(i).bits.preDecode  := Mux( io.if3_req(i+1).bits.isFault, PreDecode16(io.if3_req(i+1).bits.instr), PreDecode32(instr32 = Cat(io.if3_req(i+1).bits.instr, io.if3_req(i).bits.instr)) )
          reAlign(i).bits.predict    := 0.U.asTypeOf(new Predict_Bundle) 

          reAlign(i).valid      := io.if3_req(i).fire & io.if3_req(i+1).fire & predictor_ready & ~pipeLineLock
          io.if3_req(i).ready   := reAlign(i).ready & predictor_ready & ~pipeLineLock & io.if3_req(i+1).valid
          io.if3_req(i+1).ready := reAlign(i).ready & predictor_ready & ~pipeLineLock

          assert(io.if3_req(i).fire === io.if3_req(i+1).fire)
        }} .otherwise {
          reAlign(i).bits.pc    := io.if3_req(i).bits.pc
          reAlign(i).bits.instr := io.if3_req(i).bits.instr
          reAlign(i).bits.preDecode  := PreDecode16(instr16 = io.if3_req(i).bits.instr)
          reAlign(i).bits.predict := 0.U.asTypeOf(new Predict_Bundle) 

          reAlign(i).valid      := io.if3_req(i).fire & predictor_ready & ~pipeLineLock
          io.if3_req(i).ready   := reAlign(i).ready & predictor_ready & ~pipeLineLock
          
        }        
      }

    }
  }



}


trait IF3_Predict{ this: IF3Base => 


  val is_req_btb   = reAlign.map{ _.bits.preDecode.is_req_btb}
  val is_req_bim   = reAlign.map{ _.bits.preDecode.is_req_bim}
  val is_req_tage  = reAlign.map{ _.bits.preDecode.is_req_tage}
  val is_lock_pipe = reAlign.map{ _.bits.preDecode.is_lock_pipe}

  reAlign <> combPDT.io.enq//waiting for overriding
  
  for ( i <- 0 until 4 ) {

    btb.io.req.pc := 0.U
    when( is_req_btb(i) ) {
      when( if ( i == 0 ) {false.B} else { ( 0 until i ).map{ j => is_req_btb(j) }.reduce(_|_) } ) {
        for ( k <- i until 4 ) {
          combPDT.io.enq(k).valid := false.B
          reAlign(k).ready := false.B          
        }
      } .otherwise{
        btb.io.req.pc := reAlign(i).bits.pc
        combPDT.io.enq(i).bits.predict.btb := btb.io.combResp
      }
    }

    bim.io.req.pc := 0.U
    when( is_req_bim(i) ) {
      when( if ( i == 0 ) { false.B } else { ( 0 until i ).map{ j => is_req_bim(j) }.reduce(_|_) }  ) {
        for ( k <- i until 4 ) {
          combPDT.io.enq(k).valid := false.B
          reAlign(k).ready := false.B          
        }
      } .otherwise{
        bim.io.req.pc := reAlign(i).bits.pc
        combPDT.io.enq(i).bits.predict.bim := bim.io.combResp
      }
    }

    tage.io.req.pc    := 0.U
    tage.io.req.ghist := 0.U
    when( is_req_tage(i) ) {
      when( if ( i == 0 ) { false.B } else { ( 0 until i ).map{ j => is_req_tage(j) }.reduce(_|_) } ) {
        for ( k <- i until 4 ) {
          combPDT.io.enq(k).valid := false.B
          reAlign(k).ready := false.B          
        }
      } .otherwise{
        tage.io.req.pc    := reAlign(i).bits.pc
        tage.io.req.ghist := ghist_active
        combPDT.io.enq(i).bits.predict.tage := tage.io.combResp
      }
    }

    when( is_lock_pipe(i) ) {
      for ( k <- i+1 until 4 ) {
        combPDT.io.enq(k).valid := false.B
        reAlign(k).ready := false.B
        pipeLineLock := true.B
      }
    }
  }


}

trait IF3_Update{ this: IF3Base => 

  btb.io.update.valid           := io.jcmm_update.valid
  btb.io.update.bits.pc         := io.jcmm_update.bits.pc
  btb.io.update.bits.target     := io.jcmm_update.bits.finalTarget

  bim.io.update.valid                := io.bcmm_update.valid
  bim.io.update.bits.viewAsSupertype( new BIMResp_Bundle ) := io.bcmm_update.bits.bimResp
  bim.io.update.bits.pc              := io.bcmm_update.bits.pc
  bim.io.update.bits.isFinalTaken    := io.bcmm_update.bits.isFinalTaken

  tage.io.update.valid              := io.bcmm_update.valid
  tage.io.update.bits.viewAsSupertype( new TageResp_Bundle ) := io.bcmm_update.bits.tageResp
  tage.io.update.bits.pc            := io.bcmm_update.bits.pc
  tage.io.update.bits.ghist         := io.bcmm_update.bits.ghist
  tage.io.update.bits.isFinalTaken  := io.bcmm_update.bits.isFinalTaken


  when( io.flush & ~pipeLineLock ) {
    ghist_active := 0.U
  } .elsewhen( (io.bcmm_update.valid & io.bcmm_update.bits.isMisPredict) | (io.jcmm_update.valid & io.jcmm_update.bits.isMisPredict) | ( io.flush & pipeLineLock ) ) {
    ghist_active := ghist_snap
  } .elsewhen( io.if4_update_ghist(0).valid & io.if4_update_ghist(1).valid ) {
    ghist_active := (ghist_active << 2) | Cat( io.if4_update_ghist(0).bits.isTaken, io.if4_update_ghist(1).bits.isTaken )
  } .elsewhen ( io.if4_update_ghist(0).valid ) {
    ghist_active := (ghist_active << 1) | io.if4_update_ghist(0).bits.isTaken
  } .elsewhen ( io.if4_update_ghist(1).valid ) {
    ghist_active := (ghist_active << 1) | io.if4_update_ghist(1).bits.isTaken
  }



  //ifence & vmaFence can keep the history for pipeline is locking
  when( io.flush & ~pipeLineLock ) {
    ghist_snap := 0.U
  } .elsewhen( io.bcmm_update.valid ) {
    ghist_snap := (ghist_snap << 1) | io.bcmm_update.bits.isFinalTaken
  }

  when( io.flush | io.if4Redirect.fire ) { pipeLineLock := false.B }
}

class IF3()(implicit p: Parameters) extends IF3Base with IF3_PreDecode with IF3_Predict with IF3_Update {
  btb.io.flush  := false.B
  bim.io.flush  := false.B
  tage.io.flush := false.B
}



object PreDecode16{
  def apply( instr16: UInt )(implicit p: Parameters): PreDecode_Bundle = {
    require( instr16.getWidth == 16 )

    val info16 = Wire( new PreDecode_Bundle )

    info16.is_rvc := true.B

    info16.is_jal    := (instr16 === BitPat("b101???????????01"))
    info16.is_jalr   := (instr16 === BitPat("b100??????0000010") & instr16(11,7) =/= 0.U)
    info16.is_branch := (instr16 === BitPat("b11????????????01"))
    info16.is_call   := (instr16 === BitPat("b1001?????0000010") & instr16(11,7) =/= 0.U)
    info16.is_return := (instr16 === BitPat("b100000?010000010"))
    info16.is_fencei := false.B
    info16.is_sfencevma := false.B
    info16.imm       :=
      Mux1H( Seq(
        info16.is_jal    -> Cat( Fill(52, instr16(12)), instr16(12), instr16(8), instr16(10,9), instr16(6), instr16(7), instr16(2), instr16(11), instr16(5,3), 0.U(1.W)),
        info16.is_jalr   -> 0.U,
        info16.is_branch -> Cat( Fill(55, instr16(12)), instr16(12), instr16(6,5), instr16(2), instr16(11,10), instr16(4,3), 0.U(1.W))
      ))
    return info16
  }
}


object PreDecode32{
  def apply(instr32: UInt)(implicit p: Parameters): PreDecode_Bundle = {
    require( instr32.getWidth == 32 )

    val info32 = Wire(new PreDecode_Bundle)

    info32.is_rvc := false.B

    info32.is_jal    := (instr32(6,0) === "b1101111".U)
    info32.is_jalr   := (instr32(6,0) === "b1100111".U)
    info32.is_branch := (instr32(6,0) === "b1100011".U)
    info32.is_call   := ( info32.is_jal | info32.is_jalr ) & ( instr32(11,7) === BitPat("b00?01") ) //is 1 or 5
    info32.is_return := info32.is_jalr & ( instr32(19,15) === BitPat("b00?01") ) & (instr32(19,15) =/= instr32(11,7))
    info32.is_fencei := ( instr32 === BitPat("b?????????????????001?????0001111") )
    info32.is_sfencevma := ( instr32 === BitPat("b0001001??????????000000001110011") )
    info32.imm       :=
      Mux1H( Seq(
        info32.is_jal    -> Cat( Fill(44, instr32(31)), instr32(19,12), instr32(20), instr32(30,21), 0.U(1.W) ),
        info32.is_jalr   -> Cat( Fill(52, instr32(31)), instr32(31,20) ),
        info32.is_branch -> Cat( Fill(52, instr32(31)), instr32(7), instr32(30,25), instr32(11,8), 0.U(1.W) )
      ))
    return info32
  }
}
