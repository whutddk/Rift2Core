package rift2Core.L1Cache


import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import base._
import freechips.rocketchip.tilelink._


class ProbeUnit(edge: TLEdgeOut)(implicit p: Parameters) extends L1CacheModule {
  val io = IO(new Bundle {
    val dcache_probe = Flipped(DecoupledIO(new TLBundleB(edge.bundle)))
    val req = DecoupledIO(new Info_cache_s0s1)
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
  io.req.bits.op.grant := false.B
  io.req.bits.op.load  := false.B
  io.req.bits.op.store := false.B
  io.req.bits.op.probe := true.B
  io.req.bits.op.grant := false.B
  io.req.bits.op.lr    := false.B
  io.req.bits.op.sc    := false.B
  io.req.bits.op.swap  := false.B
  io.req.bits.op.add   := false.B
  io.req.bits.op.and   := false.B
  io.req.bits.op.or    := false.B
  io.req.bits.op.xor   := false.B
  io.req.bits.op.max   := false.B
  io.req.bits.op.maxu  := false.B
  io.req.bits.op.min   := false.B
  io.req.bits.op.minu  := false.B
  io.req.bits.rd0_phy  := DontCare


}




