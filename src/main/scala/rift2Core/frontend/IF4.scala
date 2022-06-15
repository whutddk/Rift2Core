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
import rift2Core.define._
import chipsalliance.rocketchip.config.Parameters
import rift._
import base._

/**
  * instract fetch stage 4, instr decode,  predict-state 2
  */
abstract class IF4Base()(implicit p: Parameters) extends IFetchModule {
  val io = IO(new Bundle{
    val if4_req = Vec(2, Flipped(Decoupled(new IF3_Bundle)))
    val btbResp  = Vec(2, Flipped(Decoupled(new BTBResp_Bundle)))
    val bimResp  = Vec(2, Flipped(Decoupled(new BIMResp_Bundle)))
    val tageResp = Vec(2, Flipped(Decoupled(Vec(6, new TageTableResp_Bundle ))))

    val if4_resp = Vec(2, Decoupled(new IF4_Bundle))

    val if4_update_ghist = Vec(2, Valid(new Ghist_reflash_Bundle))
    val if4Redirect = Valid(new IF4_Redirect_Bundle)

    val jcmm_update = Flipped(Valid(new Jump_CTarget_Bundle))

    val bftq = Decoupled(new Branch_FTarget_Bundle)
    val jftq = Decoupled(new Jump_FTarget_Bundle)

    val flush = Input(Bool())
  })

  val ras = Module(new RAS)
  val instr_fifo = Module(new MultiPortFifo( new IF4_Bundle, (if(!isMinArea) 4 else 2), 2, 2 ))
  val bftq = Module(new MultiPortFifo( dw = new Branch_FTarget_Bundle, (if(!isMinArea) 4 else 2), 2, 1 ))
  val jftq = Module(new MultiPortFifo( dw = new Jump_FTarget_Bundle,   (if(!isMinArea) 4 else 2), 2, 1 ))


  val bRePort = Module(new RePort( new Branch_FTarget_Bundle, port = 2) )
  val jRePort = Module(new RePort( new Jump_FTarget_Bundle, port = 2) )




  val is_jal       = io.if4_req.map{_.bits.preDecode.is_jal}
  val is_jalr      = io.if4_req.map{_.bits.preDecode.is_jalr}
  val is_branch    = io.if4_req.map{_.bits.preDecode.is_branch}
  val is_call      = io.if4_req.map{_.bits.preDecode.is_call}
  val is_return    = io.if4_req.map{_.bits.preDecode.is_return}
  val is_rvc       = io.if4_req.map{_.bits.preDecode.is_rvc}
  val is_fencei    = io.if4_req.map{_.bits.preDecode.is_fencei}
  val is_sfencevma = io.if4_req.map{_.bits.preDecode.is_sfencevma}
  val pc           = io.if4_req.map{_.bits.pc}
  val ghist        = io.if4_req.map{_.bits.ghist}
  val imm          = io.if4_req.map{_.bits.preDecode.imm}

  val is_req_btb   = io.if4_req.map{ _.bits.preDecode.is_req_btb}
  val is_req_bim   = io.if4_req.map{ _.bits.preDecode.is_req_bim}
  val is_req_tage  = io.if4_req.map{ _.bits.preDecode.is_req_tage}


  val tageRedirect = ReDirect(io.tageResp, VecInit(is_req_tage) )
  val bimRedirect  = ReDirect(io.bimResp,  VecInit(is_req_bim ) )
  val btbRedirect  = ReDirect(io.btbResp,  VecInit(is_req_btb ) )


  val tage_decode = tageRedirect.map{ x => Tage_Decode(x.bits)}
  val bim_decode  = bimRedirect.map{ _.bits}
  val btb_decode  = btbRedirect.map{ _.bits}

}

trait IF4_Decode{ this: IF4Base =>

  for ( i <- 0 until 2 ) yield {
    instr_fifo.io.enq(i).bits :=
      Mux( is_rvc(i),
        Decode16(x = io.if4_req(i).bits.instr(15,0), pc = io.if4_req(i).bits.pc, hasFpu),
        Decode32(x = io.if4_req(i).bits.instr,       pc = io.if4_req(i).bits.pc, hasFpu)
      )
  }

}

trait IF4_Predict{ this: IF4Base =>
  val redirectTarget = Wire(Vec(2, UInt(64.W)))
  val isRedirect = Wire(Vec(2, Bool()))

  val isDisAgreeWithIF1 = Wire(Vec(2, Bool()))
  val isIf4Redirect = Wire(Vec(2, Bool()))

  val is_bTaken = for ( i <- 0 until 2 ) yield {
    // Mux( ~tage_decode(i).isAlloc.reduce(_&_),
    //     tage_decode(i).isPredictTaken,
    //     bim_decode(i).bim_p
    // )
    bim_decode(i).bim_p
  }  

  for( i <- 0 until 2 ) {
    isRedirect(i) := 
    (is_branch(i) & is_bTaken(i)) |
    is_jal(i) |
    is_jalr(i)
  }

  val jalr_pc = for( i <- 0 until 2 ) yield {
    Mux( is_return(i) & ras.io.deq.valid, ras.io.deq.bits.target, btb_decode(i).target  )
  }
  //ignore ras-pop-valid
  ras.io.deq.ready := ( 0 until 2 ).map{ i => (is_return(i) & io.if4_req(i).fire)}.reduce(_|_)

  ras.io.enq.valid := (is_call(0)  & io.if4_req(0).fire) | (is_call(1) & io.if4_req(1).fire)
  ras.io.enq.bits.target :=
    Mux( is_call(0), pc(0) + Mux(is_rvc(0), 2.U, 4.U), 
      Mux( is_call(1), pc(1) + Mux(is_rvc(1), 2.U, 4.U), 0.U ))

  for( i <- 0 until 2 ) yield {
    redirectTarget(i) := 
    Mux1H(Seq(
      (is_branch(i) & is_bTaken(i)) -> (pc(i) + imm(i)),
      is_jal(i)                     -> (pc(i) + imm(i)),
      is_jalr(i)                    -> (jalr_pc(i)),
    ))

  }

  for( i <- 0 until 2 ) yield {
    io.if4_update_ghist(i).valid :=
      io.if4_req(i).fire & is_branch(i)

    io.if4_update_ghist(i).bits.isTaken := is_bTaken(i)
  }

  for( i <- 0 until 2 ) yield {
    isDisAgreeWithIF1(i) :=
      ( isRedirect(i) =/= io.if4_req(i).bits.isRedirect) |
      ((isRedirect(i) === io.if4_req(i).bits.isRedirect) & (io.if4_req(i).bits.target =/= redirectTarget(i)))
  
    isIf4Redirect(i) := io.if4_req(i).fire & isDisAgreeWithIF1(i)

    when( ~isRedirect(i) ) { assert( redirectTarget(i) === 0.U ) }
    when( ~io.if4_req(i).bits.isRedirect ) { assert( io.if4_req(i).bits.target === 0.U ) }
  }

  io.if4Redirect.valid := isIf4Redirect.reduce(_|_)
    
  io.if4Redirect.bits.target := 
    Mux( isIf4Redirect(0), Mux( isRedirect(0), redirectTarget(0), (pc(0) + Mux(is_rvc(0), 2.U, 4.U))),
      Mux( isIf4Redirect(1), Mux( isRedirect(1), redirectTarget(1), (pc(1) + Mux(is_rvc(1), 2.U, 4.U))), 0.U ) )

  io.if4Redirect.bits.pc :=
    Mux( isIf4Redirect(0), pc(0),
      Mux( isIf4Redirect(1), pc(1), 0.U ) )

  io.if4Redirect.bits.isDisAgree :=
    Mux( isIf4Redirect(0), isDisAgreeWithIF1(0) & io.if4_req(0).bits.isRedirect,
      Mux( isIf4Redirect(1), isDisAgreeWithIF1(1) & io.if4_req(1).bits.isRedirect, 0.U ) )

  for ( i <- 0 until 2 ) yield {
    bRePort.io.enq(i).bits.pc             := pc(i)
    bRePort.io.enq(i).bits.ghist          := ghist(i)
    bRePort.io.enq(i).bits.bimResp        := bim_decode(i)
    bRePort.io.enq(i).bits.tageResp       := tage_decode(i)
    bRePort.io.enq(i).bits.revertTarget   := Mux( is_bTaken(i), (pc(i) + Mux(is_rvc(i), 2.U, 4.U)), (pc(i) + imm(i)) )
    bRePort.io.enq(i).bits.predicTarget   := Mux(~is_bTaken(i), (pc(i) + Mux(is_rvc(i), 2.U, 4.U)), (pc(i) + imm(i)) )
    bRePort.io.enq(i).bits.isPredictTaken := is_bTaken(i)

    jRePort.io.enq(i).bits.pc      := pc(i)
    jRePort.io.enq(i).bits.btbResp := btb_decode(i)
    jRePort.io.enq(i).bits.rasResp := ras.io.deq.bits
    jRePort.io.enq(i).bits.isRas   := is_return(i) & ras.io.deq.valid
  }

  //only when ras make a wrong prediction will it flush, Warning: we don't care abort other pipeline flush this time
  // ras.io.flush := io.jcmm_update.valid & io.jcmm_update.bits.isRas & io.jcmm_update.bits.isMisPredict
  
  //ras flush immediately when pipeline flush
  ras.io.flush := io.flush
}

trait IF4SRAM { this: IF4Base =>



  // io.btbResp.ready  := false.B 
  // io.bimResp.ready  := false.B 
  // io.tageResp.ready := false.B 

  // val is_ram_block = 
  //   (PopCount( is_req_btb ) > 1.U ) |
  //   (PopCount( is_req_bim ) > 1.U ) |
  //   (PopCount( is_req_tage ) > 1.U )

  for ( i <- 0 until 2 ) yield {
    btbRedirect(i).ready := is_req_btb(i) & io.if4_req(i).fire
    bimRedirect(i).ready := is_req_bim(i) & io.if4_req(i).fire
    tageRedirect(i).ready := is_req_tage(i) & io.if4_req(i).fire

    assert( ~(btbRedirect(i).ready  & ~btbRedirect(i).valid) )
    assert( ~(bimRedirect(i).ready  & ~bimRedirect(i).valid) )
    assert( ~(tageRedirect(i).ready & ~tageRedirect(i).valid))
  }

      
}


class IF4()(implicit p: Parameters) extends IF4Base with IF4_Decode with IF4_Predict with IF4SRAM{





  io.if4_resp <> instr_fifo.io.deq
  io.bftq <> bftq.io.deq(0)
  io.jftq <> jftq.io.deq(0)

  bftq.io.enq <> bRePort.io.deq
  jftq.io.enq <> jRePort.io.deq


  for ( i <- 0 until 2 ) yield {

    bRePort.io.enq(i).valid := io.if4_req(i).fire & is_branch(i)
    jRePort.io.enq(i).valid := io.if4_req(i).fire & is_jalr(i)

    instr_fifo.io.enq(i).valid := io.if4_req(i).fire
              
  }

  io.if4_req(0).ready := bRePort.io.enq(0).ready & jRePort.io.enq(0).ready & instr_fifo.io.enq(0).ready
  io.if4_req(1).ready := bRePort.io.enq(1).ready & jRePort.io.enq(1).ready & instr_fifo.io.enq(1).ready & ~isIf4Redirect(0)

  instr_fifo.io.flush := io.flush
  bftq.io.flush := io.flush
  jftq.io.flush := io.flush

}


