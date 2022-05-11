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

/**
  * instract fetch stage 4, instr decode,  predict-state 2
  */
abstract class IF4Base extends IFetchModule {
  val io = IO(new Bundle{
    val if4_req = Vec(2, Flipped(Decoupled(new IF3_Bundle)))
    val if4_resp = 

    val if4_update_ghist = Vec(2, Valid(new Ghist_reflash_Bundle))
    val if4_redir = Valid(new IF4_Redirect_Bundle)

    val jcmm_update = Flipped(Valid())

    val flush = Input(Bool())
  })

  val ras = Module(new RAS)
  val instr_fifo = Module(new MultiPortFifo( new IF4_Bundle, 4, 2, 2 ))
  val bftq = Module(new MultiPortFifo( dw = new Branch_Target_Bundle, 4, 2, 1 ))
  val jftq = Module(new MultiPortFifo( dw = new Jump_Target_Bundle,   4, 2, 1 ))


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
    Mux( tage_decode(i).is_alloc.exist( (x:Bool()) => (x === false.B)),
        tage_decode(i).is_predictTaken,
        bim_decode(i).bim_p
    )
  }

  val is_redirect = for( i <- 0 until 2 ) yield {
    instr_fifo.io.enq(i).fire & (
      (is_branch(i) & is_bTaken(i)) |
      is_jal(i) |
      is_jalr(i) |
    )
  }

  val jalr_pc = for( i <- 0 until 2 ) yield {
    Mux( is_return(i) & ras.io.deq.valid, ras.io.deq.bits.target, btb_decode(i).target  )
  }
  ras.io.deq.ready := is_return(i) & instr_fifo.io.enq(i).fire
  ras.io.enq.valid := (is_call(0)  & instr_fifo.io.enq(0).fire) | (is_call(1) & instr_fifo.io.enq(1).fire)
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
      instr_fifo.io.enq(i).fire & is_branch(i)

    io.if4_update_ghist(i).bits.is_taken := is_bTaken(i)
  }

  io.if4_redir.valid := is_redirect.reduce(_|_)
  io.if4_redir.bits.pc := 
    Mux( is_redirect(0), redirect_pc(0), 
      Mux( is_redirect(1), redirect_pc(1), 0.U ) )


  // val bftq = Module(new MultiPortFifo( dw = new Branch_Target_Bundle, 4, 2, 1 ))
  // val jftq = Module(new MultiPortFifo( dw = new Jump_Target_Bundle,   4, 2, 1 ))

  for ( i <- 0 until 2 ) yield {
    bftq.io.enq(i).bits.pc = UInt(64.W)
    bftq.io.enq(i).bits.ghist = UInt(64.W)
    bftq.io.enq(i).bits.bimResp  = new BIMResp_Bundle
    bftq.io.enq(i).bits.tageResp = new TageResp_Bundle
    bftq.io.enq(i).bits.revertTarget = UInt(64.W)
    bftq.io.enq(i).bits.isPredictTaken = Bool()

    jftq.io.enq(i).pc       = UInt(64.W)
    jftq.io.enq(i).btbResp = new BIMResp_Bundle
    jftq.io.enq(i).rasResp = new RASPP_Bundle
    jftq.io.enq(i).isBtb = Bool()
    jftq.io.enq(i).isRas = Bool()    
  }



}



