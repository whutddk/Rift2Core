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
import chipsalliance.rocketchip.config.Parameters
/**
  * instract fetch stage 4, instr decode,  predict-state 2
  */
abstract class IF4Base()(implicit p: Parameters) extends IFetchModule {
  val io = IO(new Bundle{
    val if4_req = Vec(2, Flipped(Decoupled(new IF3_Bundle)))
    val if4_resp = Vec(2, Decoupled(new IF4_Bundle))

    val if4_update_ghist = Vec(2, Valid(new Ghist_reflash_Bundle))
    val if4Redirect = Valid(new IF4_Redirect_Bundle)

    val jcmm_update = Flipped(Valid(new Jump_FTarget_Bundle))

    val bftq = Decoupled(new Branch_FTarget_Bundle)
    val jftq = Decoupled(new Jump_FTarget_Bundle)

    val flush = Input(Bool())
  })

  val ras = Module(new RAS)
  val instr_fifo = Module(new MultiPortFifo( new IF4_Bundle, 4, 2, 2 ))
  val bftq = Module(new MultiPortFifo( dw = new Branch_FTarget_Bundle, 4, 2, 1 ))
  val jftq = Module(new MultiPortFifo( dw = new Jump_FTarget_Bundle,   4, 2, 1 ))


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

  val tage_decode = io.if4_req.map{ Tage_Decode(_.bits.predict.tage) }
  val bim_decode  = io.if4_req.map{ _.bits.predict.bim }
  val btb_decode  = io.if4_req.map{ _.bits.predict.btb }

}

trait IF4_Decode{ this: IF4Base =>

  for ( i <- 0 until 2 ) yield {
    instr_fifo.io.enq(i).bits :=
      Mux( is_rvc(i),
        Decode16(x = io.if4_req(i).bits.instr(15,0), pc = io.if4_req(i).bits.pc),
        Decode32(x = io.if4_req(i).bits.instr,       pc = io.if4_req(i).bits.pc)
      )
  }

}

trait IF4_Predict{ this: IF4Base =>

  val is_bTaken = for( i <- 0 until 2 ) yield {
    Mux( tage_decode(i).is_alloc.exist( (x:Bool) => (x === false.B)),
        tage_decode(i).is_predictTaken,
        bim_decode(i).bim_p
    )
  }

  val is_redirect = for( i <- 0 until 2 ) yield {
    (is_branch(i) & is_bTaken(i)) |
    is_jal(i) |
    is_jalr(i) |

  }

  val jalr_pc = for( i <- 0 until 2 ) yield {
    Mux( is_return(i) & ras.io.deq.valid, ras.io.deq.bits.target, btb_decode(i).target  )
  }
  ras.io.deq.ready := is_return(i) & io.if4_req(i).fire
  ras.io.enq.valid := (is_call(0)  & io.if4_req(0).fire) | (is_call(1) & io.if4_req(1).fire)
  ras.io.enq.bits.target :=
    Mux( is_call(0), pc(0) + Mux(is_rvc(0), 2.U, 4.U), 
      Mux( is_call(1), pc(1) + Mux(is_rvc(1), 2.U, 4.U), 0.U ))

  val redirect_pc = for( i <- 0 until 2 ) yield {
    Mux1H(Seq(
      (is_branch(i) & is_bTaken(i)) -> (pc(i) + imm(i)),
      is_jal(i)                     -> (pc(i) + imm(i)),
      is_jalr(i)                    -> (jalr_pc),
    ))

  }

  for( i <- 0 until 2 ) yield {
    io.if4_update_ghist(i).valid :=
      io.if4_req(i).fire & is_branch(i)

    io.if4_update_ghist(i).bits.is_taken := is_bTaken(i)
  }

  io.if4Redirect.valid := (io.if4_req(0).fire & is_redirect(0)) | (io.if4_req(1).fire & is_redirect(1))
  io.if4Redirect.bits.pc := 
    Mux( io.if4_req(0).fire & is_redirect(0), redirect_pc(0), 
      Mux( io.if4_req(1).fire & is_redirect(1), redirect_pc(1), 0.U ) )


  for ( i <- 0 until 2 ) yield {
    bRePort.io.enq(i).bits.pc             := pc(i)
    bRePort.io.enq(i).bits.ghist          := ghist(i)
    bRePort.io.enq(i).bits.bimResp        := bim_decode(i)
    bRePort.io.enq(i).bits.tageResp       := tage_decode(i)
    bRePort.io.enq(i).bits.revertTarget   := Mux( is_bTaken(i), (pc(i) + imm(i)), (pc(i) + Mux(is_rvc(i), 2.U, 4.U)) )
    bRePort.io.enq(i).bits.isPredictTaken := is_bTaken(i)

    jRePort.io.enq(i).bits.pc      := pc(i)
    jRePort.io.enq(i).bits.btbResp := btb_decode(i)
    jRePort.io.enq(i).bits.rasResp := ras.io.deq.bits.target
    jRePort.io.enq(i).bits.isRas   := is_return(i) & ras.io.deq.valid
  }

  //only when ras make a wrong prediction will it flush, Warning: we don't care abort other pipeline flush this time
  ras.io.flush := io.jcmm_update.valid & io.jcmm_update.isRas & io.jcmm_update.isMisPredict

}


class IF4()(implicit p: Parameters) extends IF4Base with IF4_Decode with IF4_Predict {
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

  io.if4_req(0).ready := bRePort.io.enq(0).ready & bRePort.io.enq(0).ready & instr_fifo.io.enq(0).ready
  io.if4_req(1).ready := jRePort.io.enq(1).ready & jRePort.io.enq(1).ready & instr_fifo.io.enq(1).ready & ~is_redirect(0)

}


