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

import base._
import rift2Core.define._

class BIM()(implicit p: Parameters) extends IFetchModule {
  val io = IO(new Bundle{
    val req  = Flipped(Decoupled(new BIMReq_Bundle))
    val resp = Decoupled(new BIMResp_Bundle)
    val update   = Flipped(Valid(new BIMUpdate_Bundle))

    val isReady = Output(Bool())
    val flush    = Input(Bool())
  })

  /** BIM needs power reset to initialize the ram */
  val por_reset = RegInit(true.B)
  val (reset_cl, reset_end) = Counter( Range(0, bim_cl), por_reset )
  when( reset_end ) { por_reset := false.B }
  io.isReady := ~por_reset

  /** branch history table predict bits
    * @note when successfully predict, keep this bit
    * @note when mis-predict, revert or not this bit considering bim_H
    */
  val bim_P = Mem( bim_cl, Bool() )

  /** branch history table histroy bits 
    * @note when successfully predict, set this bit to true.B
    * @note when mis-predict, revert this bit
    */
  val bim_H = Mem( bim_cl, Bool() )

  /** branch resolve write cache line */
  val wr_cl = HashTo0( in = io.update.bits.pc, len = log2Ceil(bim_cl))
  /** branch predice read cache line */
  val rd_cl = HashTo0( in = io.req.bits.pc,    len = log2Ceil(bim_cl))


  // no overlap mis-predict will happened, for the following instr will be flushed
  when( por_reset ) {
    bim_P.write( reset_cl, true.B )
    bim_H.write( reset_cl, false.B )
  } .elsewhen( io.update.valid ) {
    when( io.update.bits.isMisPredict ) {
      when( io.update.bits.bim_h === false.B ) { bim_P.write(wr_cl, ~io.update.bits.bim_p) }
      bim_H.write(wr_cl, ~io.update.bits.bim_h )
    } .otherwise {
      bim_H.write(wr_cl, true.B)
    }
  }

  io.resp.bits.bim_p := RegNext(bim_P.read(rd_cl))
  io.resp.bits.bim_h := RegNext(bim_H.read(rd_cl))
  io.resp.valid      := RegNext(io.req.fire)
  io.req.ready       := io.resp.ready
}

