

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

package rift2Core.backend.mem

import chisel3._
import chisel3.util._
import rift2Core.define._
import rift2Core.backend._


class Cache_Lsu(edge: TLEdgeOut)(implicit p: Parameters) extends RiftModule{
  val io = IO(new Bundle{
    val enq = Flipped(new DecoupledIO(new Lsu_iss_info))
    val deq = new DecoupledIO(new WriteBack_info)

    val missUnit_dcache_acquire = Decoupled(new TLBundleA(edge.bundle))
    val missUnit_dcache_grant = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    val missUnit_dcache_grantAck = Decoupled(new TLBundleE(edge.bundle))
    val probeUnit_dcache_probe = Flipped(DecoupledIO(new TLBundleB(edge.bundle)))
    val writeBackUnit_dcache_release = DecoupledIO(new TLBundleC(edge.bundle))
    val writeBackUnit_dcache_grant   = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))

    val flush = Input(Bool())
  })


  val dcache = Module(new Dcache(edge(i))) val overlap = new Info_overlap

  val su_wb_fifo = Module( new Queue( new WriteBack_info, 1, false, true ) )
  val lu_wb_fifo = Module( new Queue( new WriteBack_info, 1, false, true ) )

  val wb_arb = Module(new Arbiter(new WriteBack_info, 2))


  io.enq <> opMux.io.enq 




  su_wb_fifo.io.enq.valid := opMux.io.st_deq.fire
  su_wb_fifo.io.enq.bits  := opMux.io.st_deq.bits.param.rd

  stQueue.io.enq.valid := opMux.io.st_deq.fire | opMux.io.am_deq.fire
  stQueue.io.enq.bits :=
    Mux1H(Seq(
      opMux.io.st_deq.fire -> opMux.io.st_deq.bits,
      opMux.io.am_deq.fire -> opMux.io.am_deq.bits,
    ))

    val is_commited = Input(Vec(2,Bool()))
    val overlap_paddr = ValidIO(UInt(64.W))
    val overlap_wdata = Flipped(ValidIO(UInt(64.W)))
    val overlap_wstrb = Flipped(ValidIO(UInt(64.W)))


  opMux.io.st_deq.ready := su_wb_fifo.io.enq.ready & stQueue.io.enq.ready
  opMux.io.am_deq.ready := stQueue.io.enq.ready


  


  dcache.io.flush := io.flush

  dcache.io.missUnit_dcache_acquire <> io.missUnit_dcache_acquire
  dcache.io.missUnit_dcache_grant <> io.missUnit_dcache_grant
  dcache.io.missUnit_dcache_grantAck <> io.missUnit_dcache_grantAck
  dcache.io.probeUnit_dcache_probe <> io.probeUnit_dcache_probe
  dcache.io.writeBackUnit_dcache_release <> io.writeBackUnit_dcache_release
  dcache.io.writeBackUnit_dcache_grant <> io.writeBackUnit_dcache_grant


  lu_wb_fifo.io.enq.valid := dcache.io.deq.valid & dcache.io.deq.bits.is_load_amo
  lu_wb_fifo.io.enq.bits  := dcache.io.deq.bits.wb
  su_wb_fifo.io.enq.valid := dcache.io.deq.valid & ~dcache.io.deq.bits.is_load_amo
  su_wb_fifo.io.enq.bits  := dcache.io.deq.bits.wb

  dcache.io.deq.ready := lu_wb_fifo.io.enq.ready & su_wb_fifo.io.enq.ready

  wb_arb.io.in(0) <> su_wb_fifo.io.deq
  wb_arb.io.in(1) <> lu_wb_fifo.io.deq
  wb_arb.io.out   <> io.deq

}


