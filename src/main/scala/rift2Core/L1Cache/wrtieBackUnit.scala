package rift2Core.L1Cache


import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import base._
import freechips.rocketchip.tilelink._
import rift._

/** the operation infomation of writeback Unit */
class Info_writeBack_req(implicit p: Parameters) extends RiftBundle {
  val paddr = UInt(plen.W)
  val data = UInt(256.W)
  val is_releaseData = Bool()
  val is_release = Bool()
  val is_probe = Bool()
  val is_probeData = Bool()

  // assert( PopCount(Cat(is_releaseData, is_release, is_probe, is_probeData)) === 1.U )
}

/**
  * a writeBack Unit accept data and paddr from l1cache and write it back to l2cache
  */
class WriteBackUnit(edge: TLEdgeOut, setting: Int, id: Int)(implicit p: Parameters) extends RiftModule {
  val io = IO(new Bundle {
    val wb_req = Flipped(new ValidIO(new Info_writeBack_req))
    val pb_req = Flipped(new ValidIO(new Info_writeBack_req))
    val cache_release = new DecoupledIO(new TLBundleC(edge.bundle))
    val cache_grant   = Flipped(new DecoupledIO(new TLBundleD(edge.bundle)))


    val release_ban = Input(Bool())
    val miss_ban = Output(Bool())
  })





  /** a tiny fifo that temporarily store the writeback info from l1cache */
  val wb_fifo = Module(new Queue(new Info_writeBack_req, 1, false, true))
  val pb_fifo = Module(new Queue(new Info_writeBack_req, 1, false, true))
  val fun_arb = Module(new Arbiter(new Info_writeBack_req, 2))
  val op_fifo = Module(new Queue(new Info_writeBack_req, 1, false, true))

  wb_fifo.io.enq.valid := io.wb_req.valid
  wb_fifo.io.enq.bits  := io.wb_req.bits

  pb_fifo.io.enq.valid := io.pb_req.valid
  pb_fifo.io.enq.bits  := io.pb_req.bits

  fun_arb.io.in(0) <> pb_fifo.io.deq
  fun_arb.io.in(1) <> wb_fifo.io.deq
  op_fifo.io.enq <> fun_arb.io.out


  val wb_state_dnxt = Wire(UInt(2.W))
  val wb_state_qout = RegNext(wb_state_dnxt, 0.U)

  /** a last flag of release transmision */
  val is_release_done = edge.count(io.cache_release)._3

  /** a register of io.cache_release.valid */
  val cache_release_valid = RegInit(false.B)

  /** a register of io.cache_grant.ready */
  val cache_grant_ready   = RegInit(false.B)

  io.cache_release.valid := cache_release_valid
  io.cache_grant.ready   := cache_grant_ready


  wb_state_dnxt :=
    Mux1H(Seq(
      (wb_state_qout === 0.U) -> Mux(
        op_fifo.io.deq.valid & (
          (~io.release_ban & ( op_fifo.io.deq.bits.is_release | op_fifo.io.deq.bits.is_releaseData )) |
          ( op_fifo.io.deq.bits.is_probe | op_fifo.io.deq.bits.is_probeData )
        )
        , 1.U, 0.U),
      (wb_state_qout === 1.U) ->
        Mux(~is_release_done, 1.U,
          Mux( op_fifo.io.deq.bits.is_probeData | op_fifo.io.deq.bits.is_probe, 0.U, 2.U )),
      (wb_state_qout === 2.U) -> Mux( io.cache_grant.fire, 0.U, 2.U )
    ))

  /** a 2 step counter to select data */
  val beatCnt = RegInit(false.B)
  when( wb_state_qout === 0.U & wb_state_dnxt === 1.U ) { beatCnt := false.B }
  .elsewhen( io.cache_release.fire ) { beatCnt := ~beatCnt }
    
  when( wb_state_qout === 1.U ) {
    when( io.cache_release.fire ) { cache_release_valid := false.B }
    .elsewhen( ~io.cache_release.valid ) { cache_release_valid := true.B }
  }


  io.cache_release.bits := {
    def permit: UInt = {
      var res = 0.U
      if ( setting == 0 ) {
        res = TLPermissions.BtoN
      } else if ( setting == 2 ) {
        res = TLPermissions.TtoN
      }
      res
    }

    val info_probe = edge.ProbeAck(
      fromSource = id.U,
      toAddress = op_fifo.io.deq.bits.paddr & ("hFFFFFFFF".U << log2Ceil(256/8).U),
      lgSize = log2Ceil(256/8).U,
      reportPermissions = permit,    
    )

    val info_probeData = edge.ProbeAck(
      fromSource = id.U,
      toAddress = op_fifo.io.deq.bits.paddr & ("hFFFFFFFF".U << log2Ceil(256/8).U),
      lgSize = log2Ceil(256/8).U,
      reportPermissions = permit,
      data = Mux(beatCnt, op_fifo.io.deq.bits.data(255,128), op_fifo.io.deq.bits.data(127,0))
    )

    val info_release = edge.Release(
      fromSource = id.U,
      toAddress = op_fifo.io.deq.bits.paddr & ("hFFFFFFFF".U << log2Ceil(256/8).U),
      lgSize = log2Ceil(256/8).U,
      shrinkPermissions = permit
    )._2

    val info_releaseData = edge.Release(
      fromSource = id.U,
      toAddress = op_fifo.io.deq.bits.paddr & ("hFFFFFFFF".U << log2Ceil(256/8).U),
      lgSize = log2Ceil(256/8).U,
      shrinkPermissions = permit,
      data = Mux(beatCnt, op_fifo.io.deq.bits.data(255,128), op_fifo.io.deq.bits.data(127,0))
    )._2


    Mux1H(Seq(
      op_fifo.io.deq.bits.is_probe       -> info_probe,
      op_fifo.io.deq.bits.is_probeData   -> info_probeData,
      op_fifo.io.deq.bits.is_release     -> info_release,
      op_fifo.io.deq.bits.is_releaseData -> info_releaseData,
    ))    
  }

  when( io.cache_grant.valid & ~io.cache_grant.ready ) {
    cache_grant_ready := true.B
    assert( wb_state_qout === 2.U )
  } .elsewhen( io.cache_grant.ready ) {
    cache_grant_ready := false.B
    assert( wb_state_qout === 2.U )   
  }
  assert( ~(io.cache_grant.fire & io.cache_grant.bits.source =/= id.U), "Assert Failed at writeBack-Unit, grant-id mis-match" )
  op_fifo.io.deq.ready := 
    wb_state_qout =/= 0.U & wb_state_dnxt === 0.U

  io.miss_ban := wb_state_qout === 1.U | wb_state_qout === 2.U | ~op_fifo.io.enq.ready



  when( ~pb_fifo.io.enq.ready ) {
    assert( ~pb_fifo.io.enq.valid, "Once the Probe is issued, the slave should not issue further Probes on that block until it receives a ProbeAck. Spec-1.8.1 Page-69")
  }

  when( ~wb_fifo.io.enq.ready ) {
    assert(~io.wb_req.valid, "When WriteBack Unit is busy, no new acquire will be emitted, and there is no wb request!")
  }
}



