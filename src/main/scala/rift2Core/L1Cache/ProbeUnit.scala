package rift2Core.L1Cache


import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import base._
import freechips.rocketchip.tilelink._
import rift._

class Info_probe_req(implicit p: Parameters) extends RiftBundle {
  val paddr = UInt(plen.W)
}



/**
  * ProbeUnit will accept probe request from l2cache and forward it to l1cache to resp data
  */
class ProbeUnit(edge: TLEdgeOut, id: Int)(implicit p: Parameters) extends L1CacheModule {
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




