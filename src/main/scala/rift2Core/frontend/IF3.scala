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
  * instract fetch stage 3, instr pre-decode, realign, predict-state 1
  */
abstract class IF3Base extends IFetchModule {
  val io = IO(new Bundle{
    val if3_req = Vec(4, Flipped(new DecoupledIO(new IF2_Bundle) ))
    val if3_resp = Vec(2, Decoupled(new IF3_Bundle))

    val ftq_reslove = Flipped(Valid())
    val ftq_push    = Flipped(Valid())

    val flush = Input(Bool())
  })

  val ghist_snap = RegInit( 0.U(64.W) )
  val ghist_active = RegInit( 0.U(64.W) )

  val reAlign = Vec(4, DecoupledIO(new IF3_Bundle))


  val btb = Module(new BTB)
  val bim = Module(new BIM)
  val tage = Module(new TAGE)

  val predictor_ready = btb.io.req.ready & bim.io.req.ready & tage.io.req.ready

  val IF3_resp_fifo = Module(new MultiPortFifo( new IF3_Bundle, 4, 4, 2 ))
  val preDicit_fifo = 
}





trait PreDecode{ this: IF3Base => 
  val is_instr16 = io.if3_req.map{ _.bits.instr(1,0) =/= "b11".U }
  val is_instr32 = io.if3_req.map{ _.bits.instr(1,0) === "b11".U }


  val is_req_btb   = preDecode_info.map{ _.is_req_btb}
  val is_req_ras   = preDecode_info.map{ _.is_req_ras}
  val is_req_bim   = preDecode_info.map{ _.is_req_bim}
  val is_req_tage  = preDecode_info.map{ _.is_req_tage}

  val is_predict = preDecode_info.map{ _.is_req_tage | _.is_req_bim | _.is_req_ras | _.is_req_btb }
  val is_lock_pipe = preDecode_info.map{ _.is_lock_pipe}


  val block_sel = WireDefault(7.U(3.W))
  println("Warning, mixing bim, btb, ras, tage together")
    for( i <- 3 to 0 ) yield {
      when(is_predict(i)){
        for( j <- 0 until i ) yield {
          when( is_predict(j) ) {
            block_sel := i.U
          }
        }
      } .elsewhen( is_lock_pipe(i) ) {
        block_sel := i.U + 1.U //may overflow to 4, But it doesn't matter
      }
    }


  for( i <- 0 until 4 ) yield {
    if ( i == 0 ){
      when( is_instr32(i) ) { 
        reAlign(i).bits.pc    := io.if3_req(i).bits.pc
        reAlign(i).bits.instr := Cat(io.if3_req(i+1).bits.instr, io.if3_req(i).bits.instr)
        reAlign(i).bits.info  := PreDecode32(instr32 = Cat(io.if3_req(i+1).bits.instr, io.if3_req(i).bits.instr))

        reAlign(i).valid(i)   := io.if3_req(i).fire & io.if3_req(i+1).fire
        io.if3_req(i).ready   := reAlign(i).ready & (i.U < block_sel) & predictor_ready
        io.if3_req(i+1).ready := reAlign(i).ready & (i.U < block_sel) & predictor_ready
      } .otherwise {
        reAlign(i).bits.pc    := io.if3_req(i).bits.pc
        reAlign(i).bits.instr := io.if3_req(i).bits.instr
        reAlign(i).bits.info  := PreDecode16(instr16 = io.if3_req(i).bits.instr)

        reAlign(i).valid      := io.if3_req(i).fire
        io.if3_req(i).ready   := reAlign(i).ready & (i.U < block_sel) & predictor_ready
      }        
    } else {
      when( is_instr32( i-1 ) === false.B ) {
        when( is_instr32(i) ) { if ( i != 3 ) {
          reAlign(i).bits.pc    := io.if3_req(i).bits.pc
          reAlign(i).bits.instr := Cat(io.if3_req(i+1).bits.instr, io.if3_req(i).bits.instr)
          reAlign(i).bits.info  := PreDecode32(instr32 = Cat(io.if3_req(i+1).bits.instr, io.if3_req(i).bits.instr))
          
          reAlign(i).valid      := io.if3_req(i).fire & io.if3_req(i+1).fire
          io.if3_req(i).ready   := reAlign(i).ready & (i.U < block_sel) & predictor_ready
          io.if3_req(i+1).ready := reAlign(i).ready & (i.U < block_sel) & predictor_ready
        }} .otherwise {
          reAlign(i).bits.pc    := io.if3_req(i).bits.pc
          reAlign(i).bits.instr := io.if3_req(i).bits.instr
          reAlign(i).bits.info  := PreDecode16(instr16 = io.if3_req(i).bits.instr)

          reAlign(i).valid      := io.if3_req(i).fire
          io.if3_req(i).ready   := reAlign(i).ready & (i.U < block_sel) & predictor_ready
          
        }        
      }

    }
  }

  IF3_resp_fifo.io.enq <> RePort( enq = reAlign )

}


trait PreDecode{ this: IF3Base => 
  Tage(param: TageParams = TageParams()) extends IFetchModule {
  val io = IO(new Bundle{
    val req = Flipped(Decouple(new TageReq_Bundle))
    val resp = Valid( new TageResp_Bundle )

    val update_tage = Flipped(Decouple(new TageUpdate_Bundle))
    val flush = Input(Bool())
  })


  RAS[T<:Data]( dw: T, aw: Int ) extends Module {
  val io = IO(new Bundle{
    val enq = Flipped(new DecoupledIO(dw))
    val deq  = new DecoupledIO(dw)
}

