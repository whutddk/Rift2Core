
/*
  Copyright (c) 2020 - 2023 Wuhan University of Technology <295054118@whut.edu.cn>

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

package rift2Core.backend

import chisel3._
import chisel3.util._
import rift2Core.define._
import base._

import rift2Core.privilege._
import rift2Chip._
import org.chipsalliance.cde.config._


class LSU_Request_Source extends Bundle{
  val isXPU = Bool()
  val isFPU = Bool()
  val isVPU = Bool()
}

class LSUDptMux()(implicit p: Parameters) extends RiftModule{
  class LSUDptMuxIO extends Bundle{

    val roQueue = Flipped(new DecoupledIO(new LSU_Request_Source))

    val xpu_lsu_req = Flipped(new DecoupledIO(new Lsu_iss_info))
    val fpu_lsu_req = Flipped(new DecoupledIO(new Lsu_iss_info))
    val vpu_lsu_req = Flipped(new DecoupledIO(new Lsu_iss_info))

    val lsu_iss_exe = new DecoupledIO(new Lsu_iss_info)

    val flush = Input(Bool())
  } 

  val io: lsuDptIO = IO(new LSUDptMuxIO)

  /** A bypass fifo to drain upstream request*/
  val dptMuxFifo = Module(new Queue(new Lsu_iss_info, 8, pipe = false, flow = true))
  dptMuxFifo.io.deq <> io.lsu_iss_exe

  dptMuxFifo.io.enq.valid := false.B
  dptMuxFifo.io.enq.bits  := 0.U.asTypeOf(new Lsu_iss_info)
  io.xpu_lsu_req.ready := false.B
  io.fpu_lsu_req.ready := false.B
  io.vpu_lsu_req.ready := false.B

  when( io.xpu_lsu_req.valid & io.roQueue.valid & io.roQueue.bits.isXPU ){
    dptMuxFifo.io.enq <> io.xpu_lsu_req
  }
  .elsewhen( io.fpu_lsu_req.valid & io.roQueue.valid & io.roQueue.bits.isFPU ){
    dptMuxFifo.io.enq <> io.fpu_lsu_req
  }
  .elsewhen( io.vpu_lsu_req.valid & io.roQueue.valid & io.roQueue.bits.isVPU ){
    dptMuxFifo.io.enq <> io.vpu_lsu_req
  }

}
