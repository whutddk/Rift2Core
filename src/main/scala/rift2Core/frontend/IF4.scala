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
    val if4_req = Vec(rn_chn, Flipped(Decoupled(new IF3_Bundle)))
    val btbResp  = Vec(rn_chn, Flipped(Decoupled(new BTBResp_Bundle)))
    val bimResp  = Vec(rn_chn, Flipped(Decoupled(new BIMResp_Bundle)))
    val tageResp = Vec(rn_chn, Flipped(Decoupled(Vec(6, new TageTableResp_Bundle ))))

    val if4_resp = Vec(rn_chn, Decoupled(new IF4_Bundle))

    val if4_update_ghist = Vec(rn_chn, Valid(new Ghist_reflash_Bundle))
    val if4Redirect = Valid(new IF4_Redirect_Bundle)

    val jcmm_update = Flipped(Valid(new Jump_CTarget_Bundle))

    val bftq = Decoupled(new Branch_FTarget_Bundle)
    val jftq = Decoupled(new Jump_FTarget_Bundle)

    val flush = Input(Bool())
  })

  val ras = Module(new RAS)
  val instr_fifo = Module(new MultiPortFifo( new IF4_Bundle, (if(!isMinArea) 4 else 1), rn_chn, rn_chn ))
  val bftq = Module(new MultiPortFifo( dw = new Branch_FTarget_Bundle, (if(!isMinArea) 4 else 1), rn_chn, 1 ))
  val jftq = Module(new MultiPortFifo( dw = new Jump_FTarget_Bundle,   (if(!isMinArea) 4 else 1), rn_chn, 1 ))


  val bRePort = Module(new RePort( new Branch_FTarget_Bundle, port = rn_chn) )
  val jRePort = Module(new RePort( new Jump_FTarget_Bundle, port = rn_chn) )

  val preDecodeAgn = io.if4_req.map{ x => Mux(x.bits.isRVC, PreDecode16(x.bits.instr(15,0)), PreDecode32(x.bits.instr) ) }


  val is_jal       = preDecodeAgn.map{_.is_jal}
  val is_jalr      = preDecodeAgn.map{_.is_jalr}
  val is_branch    = preDecodeAgn.map{_.is_branch}
  val is_call      = preDecodeAgn.map{_.is_call}
  val is_return    = preDecodeAgn.map{_.is_return}
  val is_rvc       = io.if4_req.map{_.bits.isRVC}
  val is_fencei    = preDecodeAgn.map{_.is_fencei}
  val is_sfencevma = preDecodeAgn.map{_.is_sfencevma}
  val pc           = io.if4_req.map{_.bits.pc}
  val ghist        = if (!isMinArea) { io.if4_req.map{_.bits.ghist} } else { io.if4_req.map{_ => 0.U(64.W)} }
  val imm          = preDecodeAgn.map{_.imm}

  val is_req_btb   = (if ( btb_cl != 0 ) {preDecodeAgn.map{ _.is_req_btb}} else {preDecodeAgn.map{_ => false.B}})
  val is_req_bim   = preDecodeAgn.map{ _.is_req_bim}
  val is_req_tage  = if (!isMinArea) { preDecodeAgn.map{ _.is_req_tage} } else {io.if4_req.map{ _ => false.B}}


  val tageRedirect = ReDirect(io.tageResp, VecInit(is_req_tage) )
  val bimRedirect  = ReDirect(io.bimResp,  VecInit(is_req_bim ) )
  val btbRedirect  = ReDirect(io.btbResp,  VecInit(is_req_btb ) )


  val tage_decode = tageRedirect.map{ x => Tage_Decode(x.bits)}
  val bim_decode  = bimRedirect.map{ _.bits}
  val btb_decode  = btbRedirect.map{ _.bits}

}

trait IF4_Decode{ this: IF4Base =>

  for ( i <- 0 until rn_chn ) yield {
    instr_fifo.io.enq(i).bits :=
      Mux( is_rvc(i),
        Decode16(x = io.if4_req(i).bits.instr(15,0), pc = io.if4_req(i).bits.pc, hasFpu),
        Decode32(x = io.if4_req(i).bits.instr,       pc = io.if4_req(i).bits.pc, hasFpu)
      )
  }

}

trait IF4_Predict{ this: IF4Base =>
  val redirectTarget = Wire(Vec(rn_chn, UInt(64.W)))
  val isRedirect = Wire(Vec(rn_chn, Bool()))

  val isDisAgreeWithIF1 = Wire(Vec(rn_chn, Bool()))
  val isIf4Redirect = Wire(Vec(rn_chn, Bool()))

  val is_bTaken = for ( i <- 0 until rn_chn ) yield {
    if (!isMinArea) {
    // Mux( ~tage_decode(i).isAlloc.reduce(_&_),
    //     tage_decode(i).isPredictTaken,
    //     bim_decode(i).bim_p
    // )
      bim_decode(i).bim_p   
    } else {
      bim_decode(i).bim_p      
    }

  }  

  for( i <- 0 until rn_chn ) {
    isRedirect(i) := 
    (is_branch(i) & is_bTaken(i)) |
    is_jal(i) |
    is_jalr(i)
  }

  val jalr_pc = for( i <- 0 until rn_chn ) yield {
    extVaddr( Mux( is_return(i) & ras.io.deq.valid, ras.io.deq.bits.target, btb_decode(i).target), vlen )
  }
  //ignore ras-pop-valid
  ras.io.deq.ready := ( 0 until rn_chn ).map{ i => (is_return(i) & io.if4_req(i).fire)}.reduce(_|_)

  ras.io.enq.valid := ( 0 until rn_chn ).map{ i =>
    is_call(i) & io.if4_req(i).fire
  }.reduce(_|_)

  ras.io.enq.bits.target := MuxCase( 0.U, (0 until rn_chn).map{ i => 
    (is_call(i) -> (pc(i) + Mux(is_rvc(i), 2.U, 4.U)))
  })


  for( i <- 0 until rn_chn ) yield {
    redirectTarget(i) := 
    Mux1H(Seq(
      (is_branch(i) & is_bTaken(i)) -> (pc(i) + imm(i)),
      is_jal(i)                     -> (pc(i) + imm(i)),
      is_jalr(i)                    -> (jalr_pc(i)),
    ))

  }


  for( i <- 0 until rn_chn ) yield {
    if (!isMinArea) { 
      io.if4_update_ghist(i).valid :=
        io.if4_req(i).fire & is_branch(i)
      io.if4_update_ghist(i).bits.isTaken := is_bTaken(i)
    } else {
      io.if4_update_ghist(i).valid := false.B
      io.if4_update_ghist(i).bits.isTaken := DontCare
    }

  }

  for( i <- 0 until rn_chn ) yield {
    isDisAgreeWithIF1(i) :=
      ( isRedirect(i) =/= io.if4_req(i).bits.isRedirect) |
      ((isRedirect(i) === io.if4_req(i).bits.isRedirect) & (io.if4_req(i).bits.target =/= redirectTarget(i)))
  
    isIf4Redirect(i) := io.if4_req(i).fire & isDisAgreeWithIF1(i)

    when( ~isRedirect(i) ) { assert( redirectTarget(i) === 0.U ) }
    when( io.if4_req(i).valid & ~io.if4_req(i).bits.isRedirect ) { assert( io.if4_req(i).bits.target === 0.U ) }
  }

  io.if4Redirect.valid := isIf4Redirect.reduce(_|_)
    
  io.if4Redirect.bits.target := 
    MuxCase(0.U, (0 until rn_chn).map{ i =>
      (isIf4Redirect(i) -> Mux( isRedirect(i), redirectTarget(i), (pc(i) + Mux(is_rvc(i), 2.U, 4.U))))
    })


  io.if4Redirect.bits.pc :=
    MuxCase( 0.U, (0 until rn_chn).map{ i => 
      (isIf4Redirect(i) -> pc(i))
    })

  io.if4Redirect.bits.isDisAgree :=
    MuxCase( 0.U, (0 until rn_chn).map{ i => 
      ( isIf4Redirect(i) -> (isDisAgreeWithIF1(i) & io.if4_req(i).bits.isRedirect) )
    })

  for ( i <- 0 until rn_chn ) yield {
    bRePort.io.enq(i).bits.pc             := pc(i)
    if (!isMinArea) { bRePort.io.enq(i).bits.ghist := ghist(i) } else { bRePort.io.enq(i).bits.ghist := DontCare }
    bRePort.io.enq(i).bits.bimResp        := bim_decode(i)
    if (!isMinArea) { bRePort.io.enq(i).bits.tageResp := tage_decode(i) } else { bRePort.io.enq(i).bits.tageResp := DontCare }
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



  for ( i <- 0 until rn_chn ) yield {
    btbRedirect(i).ready := is_req_btb(i) & io.if4_req(i).fire
    bimRedirect(i).ready := is_req_bim(i) & io.if4_req(i).fire
    if (!isMinArea) { tageRedirect(i).ready := is_req_tage(i) & io.if4_req(i).fire } else { tageRedirect(i).ready := true.B }

    assert( ~(btbRedirect(i).ready  & ~btbRedirect(i).valid) )
    assert( ~(bimRedirect(i).ready  & ~bimRedirect(i).valid) )
    if (!isMinArea) { assert( ~(tageRedirect(i).ready & ~tageRedirect(i).valid)) }
  }

      
}


class IF4()(implicit p: Parameters) extends IF4Base with IF4_Decode with IF4_Predict with IF4SRAM{



  io.if4_resp <> instr_fifo.io.deq
  io.bftq <> bftq.io.deq(0)
  io.jftq <> jftq.io.deq(0)

  bftq.io.enq <> bRePort.io.deq
  jftq.io.enq <> jRePort.io.deq


  for ( i <- 0 until rn_chn ) yield {

    bRePort.io.enq(i).valid := io.if4_req(i).fire & is_branch(i)
    jRePort.io.enq(i).valid := io.if4_req(i).fire & is_jalr(i)

    instr_fifo.io.enq(i).valid := io.if4_req(i).fire
              
  }

  io.if4_req(0).ready := bRePort.io.enq(0).ready & jRePort.io.enq(0).ready & instr_fifo.io.enq(0).ready

  for( i <- 1 until rn_chn ) {
    io.if4_req(i).ready := bRePort.io.enq(i).ready & jRePort.io.enq(i).ready & instr_fifo.io.enq(i).ready
    for( j <- 0 until i ) {
      when( isIf4Redirect(j) ) { io.if4_req(i).ready := false.B }     
    }
  }


  instr_fifo.io.flush := io.flush
  bftq.io.flush := io.flush
  jftq.io.flush := io.flush

}


