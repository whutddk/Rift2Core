
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

package rift2Core.backend.lsu

import chisel3._
import chisel3.util._

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.tilelink._

import base._
import rift._

class Info_probe_req(implicit p: Parameters) extends RiftBundle {
  val paddr = UInt(plen.W)
}



/**
  * ProbeUnit will accept probe request from l2cache and forward it to l1cache to resp data
  */
class ProbeUnit(edge: TLEdgeOut, id: Int)(implicit p: Parameters) extends RiftModule {
  val io = IO(new Bundle {
    val cache_probe = Flipped(new DecoupledIO(new TLBundleB(edge.bundle)))
    val req = new DecoupledIO(new Info_probe_req)
  })

  /** a tiny fifo that buffer the probe request from l2cache */
  val probe_fifo = Module(new Queue(UInt(plen.W), 1, false, false))




  probe_fifo.io.enq.valid := io.cache_probe.valid
  probe_fifo.io.enq.bits := io.cache_probe.bits.address
  io.cache_probe.ready := probe_fifo.io.enq.ready

  // assert( ~(io.cache_probe.fire & io.cache_probe.bits.source =/= id.U), "Assert Failed at probe-unit, source id mis-match" )

  io.req.valid := probe_fifo.io.deq.valid
  probe_fifo.io.deq.ready := io.req.ready
  io.req.bits.paddr := probe_fifo.io.deq.bits




  when( ~probe_fifo.io.enq.ready ) {
    assert( ~probe_fifo.io.enq.valid, "Once the Probe is issued, the slave should not issue further Probes on that block until it receives a ProbeAck. Spec-1.8.1 Page-69")
  }
  
}




