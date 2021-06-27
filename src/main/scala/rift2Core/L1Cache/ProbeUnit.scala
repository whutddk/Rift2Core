package rift2Core.L1Cache


import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import base._
import freechips.rocketchip.tilelink._

class Info_probe_req extends Bundle {
  val addr = UInt(32.W)
}


class ProbeUnit(edge: TLEdgeOut) extends Module {
  val io = IO(new Bundle {
    val dcache_probe = Filpped(DecoupledIO(new TLBundleB(edge.bundle)))
    val req = DecoupledIO(new Info_probe_req)
  })

  val probe_fifo = Module(new Queue(new Info_probe_req, 4, true, false))

  io.req <> probe_fifo.io.deq

  probe_fifo.io.enq.valid := dcache_probe.valid
  dcache_probe.ready := probe_fifo.io.enq.ready
  dcache_probe.bits.addr := probe_fifo.io.enq.bits.address


}




