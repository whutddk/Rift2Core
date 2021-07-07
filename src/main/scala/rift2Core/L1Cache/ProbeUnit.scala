package rift2Core.L1Cache


import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import base._
import freechips.rocketchip.tilelink._


class ProbeUnit(edge: TLEdgeOut)(implicit p: Parameters) extends L1CacheModule {
  val io = IO(new Bundle {
    val dcache_probe = Flipped(new DecoupledIO(new TLBundleB(edge.bundle)))
    val req = new DecoupledIO(new Info_cache_s0s1)
  })

  val probe_fifo = Module(new Queue(UInt(32.W), 4, true, false))




  probe_fifo.io.enq.valid := io.dcache_probe.valid
  probe_fifo.io.enq.bits := io.dcache_probe.bits.address
  io.dcache_probe.ready := probe_fifo.io.enq.ready


  io.req.valid := probe_fifo.io.deq.valid
  probe_fifo.io.deq.ready := io.req.ready
  io.req.bits.paddr := probe_fifo.io.deq.bits
  io.req.bits.wdata := DontCare
  io.req.bits.wmask := DontCare

  {
    io.req.bits.op := 0.U.asTypeOf(new Cache_op)
    io.req.bits.op.probe := true.B    
  }


  io.req.bits.chk_idx  := DontCare


}




