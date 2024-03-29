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
import base._
import rift2Core.define._
import chisel3.experimental.dataview._
import org.chipsalliance.cde.config._



/**
  * A branch target buffer (BTB) module for RISC-V processors.
  * @constructor Creates a new instance of the BTB.
  * @param p The implicit Parameters object for the processor.
  */
class BTB()(implicit p: Parameters) extends IFetchModule {

  class BTBIO extends Bundle{
    val req  = Flipped(Decoupled(new BTBReq_Bundle))
    val resp = Decoupled( new BTBResp_Bundle )

    val update = Flipped(Valid(new BTBUpdate_Bundle))

    val isReady = Output(Bool())
    val flush = Input(Bool())    
  }

  val io: BTBIO = IO(new BTBIO)

  
  val bypassFifo = Module( new Queue( new BTBResp_Bundle, entries = 1, pipe = false, flow = true) )

  if ( btb_cl != 0 ) {
    // def cl_w = log2Ceil(btb_cl)
    /** tage_table needs poweron reset to initialize the ram */
    val por_reset = RegInit(true.B)
    val (reset_cl, reset_end) = Counter( Range(0, btb_cl), por_reset )
    when( reset_end ) { por_reset := false.B }
    io.isReady := ~por_reset

    val rd_cl_sel  = HashTo0( in = io.req.bits.pc, len = log2Ceil(btb_cl) )
    val wr_cl_sel  = HashTo0( in = io.update.bits.pc, len = log2Ceil(btb_cl) )

    val btb_table = SyncReadMem( btb_cl, new BTBResp_Bundle )

    when( por_reset | io.update.fire ) {
      if( btb_cl != 0 ) {
        btb_table.write(
          Mux(por_reset, reset_cl, wr_cl_sel),
          Mux(por_reset, "h80000000".U.asTypeOf(new BTBResp_Bundle), io.update.bits.viewAsSupertype( new BTBResp_Bundle ) )
        )      
      }
    }


    bypassFifo.io.enq.bits := btb_table.read(rd_cl_sel)
    io.resp.bits := bypassFifo.io.deq.bits
  } else {
    bypassFifo.io.enq.bits := DontCare
    io.resp.bits := DontCare
    io.isReady := true.B
  }

  bypassFifo.io.enq.valid := RegNext(io.req.fire)
  io.req.ready := bypassFifo.io.enq.ready

  io.resp.valid := bypassFifo.io.deq.valid
  bypassFifo.io.deq.ready := io.resp.ready










}
