package rift2Core.L1Cache


import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import base._
import freechips.rocketchip.tilelink._


class Info_probe_req extends Bundle {
  val paddr = UInt(64.W)
}



/**
  * ProbeUnit will accept probe request from l2cache and forward it to l1cache to resp data
  */
class ProbeUnit(edge: TLEdgeOut)(implicit p: Parameters) extends L1CacheModule {
  val io = IO(new Bundle {
    val cache_probe = Flipped(new DecoupledIO(new TLBundleB(edge.bundle)))
    val req = new DecoupledIO(new Info_probe_req)
  })

  /** a tiny fifo that buffer the probe request from l2cache */
  val probe_fifo = Module(new Queue(UInt(32.W), 4, true, false))




  probe_fifo.io.enq.valid := io.cache_probe.valid
  probe_fifo.io.enq.bits := io.cache_probe.bits.address
  io.cache_probe.ready := probe_fifo.io.enq.ready


  io.req.valid := probe_fifo.io.deq.valid
  probe_fifo.io.deq.ready := io.req.ready
  io.req.bits.paddr := probe_fifo.io.deq.bits

}




