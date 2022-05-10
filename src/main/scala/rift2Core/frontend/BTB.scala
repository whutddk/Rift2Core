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







class BTB extends IFetchModule {
  def cl_w = log2Ceil(btb_cl)
  val io = IO(new Bundle{
    val req  = Flipped(Decoupled(new BTBReq_Bundle))
    val resp = Decoupled( new BTBResp_Bundle )

    val update = Valid(new BTBUpdate_Bundle)

    val flush = Input(Bool())
  })

  /** tage_table needs poweron reset to initialize the ram */
  val por_reset = RegInit(true.B)
  val (reset_cl, reset_end) = Counter( range(0, btb_cl), por_reset )
  when( reset_end ) { por_reset := false.B }
  io.req.ready := ~por_reset & io.resp.ready




  val rd_cl_sel  = HashTo0( in1 = io.req.bits.pc, len = log2Ceil(btb_cl) )
  val wr_cl_sel  = HashTo0( in1 = io.update.bits.pc, len = log2Ceil(btb_cl) )


  val btb_table = Mem( btb_cl, new BTBResp_Bundle )

  io.resp.valid := RegNext(io.req.fire, false.B)
  io.resp.bits  := RegEnable(btb_table.read(rd_cl_sel), io.req.fire)


  when( por_reset ) {
    btb_table.write( reset_cl, "b80000000".U.asTypeOf(new BTBResp_Bundle) )
  } .otherwise {
    when( io.update.valid ) {
      btb_table.write(wr_cl_sel, io.update.bits)
    }
  }


}
