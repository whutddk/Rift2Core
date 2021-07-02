package rift2Core.L1Cache


import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import base._
import freechips.rocketchip.tilelink._


class Info_writeBack_req extends Bundle {
  val addr = UInt(32.W)
  val data = UInt(256.W)
  val is_releaseData = Bool()
  val is_release = Bool()
  val is_probe = Bool()
  val is_probeData = Bool()

  // assert( PopCount(Cat(is_releaseData, is_release, is_probe, is_probeData)) === 1.U )
}


class WriteBackUnit(edge: TLEdgeOut) extends Module {
  val io = IO(new Bundle {
    val req = Flipped(DecoupledIO(new Info_writeBack_req))
    val dcache_release = DecoupledIO(new TLBundleC(edge.bundle))
    val dcache_grant   = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))


    val release_ban = Input(Bool())
    val miss_ban = Output(Bool())
  })

  val wb_fifo = Module(new Queue(new Info_writeBack_req, 8, false, false))

  io.req <> wb_fifo.io.enq

  val wb_state_dnxt = Wire(UInt(2.W))
  val wb_state_qout = RegNext(wb_state_dnxt, 0.U)

  val is_release_done = edge.count(io.dcache_release)._3

  val dcache_release_valid = RegInit(false.B)
  val dcache_grant_ready   = RegInit(false.B)

  io.dcache_release.valid := dcache_release_valid
  io.dcache_grant.ready   := dcache_grant_ready


  wb_state_dnxt :=
    Mux1H(Seq(
      (wb_state_qout === 0.U) -> Mux(wb_fifo.io.deq.valid & ~io.release_ban, 1.U, 0.U),
      (wb_state_qout === 1.U) ->
        Mux(~is_release_done, 1.U,
          Mux( wb_fifo.io.deq.bits.is_probeData | wb_fifo.io.deq.bits.is_probe, 0.U, 2.U )),
      (wb_state_qout === 2.U) -> Mux( io.dcache_grant.fire, 0.U, 2.U )
    ))

  when( wb_state_qout === 0.U & wb_state_dnxt === 1.U ) {
    when( io.dcache_release.fire ) { dcache_release_valid := false.B }
    .elsewhen( ~io.dcache_release.valid ) { dcache_release_valid := true.B }
  }

  io.dcache_release.bits := {
    val info_probe = edge.ProbeAck(
      fromSource = 66.U,
      toAddress = wb_fifo.io.deq.bits.addr,
      lgSize = log2Ceil(256/8).U,
      reportPermissions = TLPermissions.TtoN
    )

    val info_probeData = edge.ProbeAck(
      fromSource = 66.U,
      toAddress = wb_fifo.io.deq.bits.addr,
      lgSize = log2Ceil(256/8).U,
      reportPermissions = TLPermissions.TtoN,
      data = wb_fifo.io.deq.bits.data
    )

    val info_release = edge.Release(
      fromSource = 66.U,
      toAddress = wb_fifo.io.deq.bits.addr,
      lgSize = log2Ceil(256/8).U,
      shrinkPermissions = TLPermissions.TtoN
    )._2

    val info_releaseData = edge.Release(
      fromSource = 66.U,
      toAddress = wb_fifo.io.deq.bits.addr,
      lgSize = log2Ceil(256/8).U,
      shrinkPermissions = TLPermissions.TtoN,
      data = wb_fifo.io.deq.bits.data
    )._2


    Mux1H(Seq(
      wb_fifo.io.deq.bits.is_probe       -> info_probe,
      wb_fifo.io.deq.bits.is_probeData   -> info_probeData,
      wb_fifo.io.deq.bits.is_release     -> info_release,
      wb_fifo.io.deq.bits.is_releaseData -> info_releaseData,
    ))    
  }

  when( io.dcache_grant.valid ) {
    dcache_grant_ready := true.B
    assert( wb_state_qout === 2.U )
  } .elsewhen( io.dcache_grant.ready ) {
    dcache_grant_ready := false.B
    assert( wb_state_qout === 2.U )   
  }

  wb_fifo.io.deq.ready := 
    wb_state_qout =/= 0.U & wb_state_dnxt === 0.U

  io.miss_ban := wb_state_qout === 1.U | wb_state_qout === 2.U


}



