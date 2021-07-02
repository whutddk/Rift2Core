package rift2Core.L1Cache


import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import base._
import freechips.rocketchip.tilelink._


class Info_mshr_req extends Bundle {
  val paddr = UInt(32.W)
}



class MissUnit(edge: TLEdgeOut, entry: Int = 8)(implicit p: Parameters) extends L1CacheModule {
  val io = IO(new Bundle{
    val req = Flipped(DecoupledIO(new Info_mshr_req))
    val rsp = DecoupledIO(new Info_cache_s1s2)

    val dcache_acquire = Decoupled(new TLBundleA(edge.bundle))
    val dcache_grant   = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    val dcache_grantAck  = Decoupled(new TLBundleE(edge.bundle))

    val miss_ban = Input(Bool())
    val release_ban = Output(Bool())

  })

  val miss_queue = RegInit(VecInit( Seq.fill(entry)( 0.U.asTypeOf(new Info_mshr_req) )))
  val miss_valid = RegInit(VecInit( Seq.fill(entry)( false.B )))

  val miss_rsp = RegInit(VecInit( Seq.fill(2)(0.U(128.W))  ))

  val mshr_state_dnxt = Wire(UInt(3.W))
  val mshr_state_qout = RegNext(mshr_state_dnxt, 0.U)

  val (_, _, is_trans_done, transCnt) = edge.count(io.dcache_grant)

  val acquire_sel = miss_valid.indexWhere( (x: Bool) => (x === true.B) )
  val dcache_acquire_vaild  = RegInit(false.B)
  val dcache_grant_ready    = Wire(Bool())
  val dcache_grantAck_valid = RegInit(false.B)

  val rsp_valid = RegInit(false.B)

  io.dcache_acquire.valid := dcache_acquire_vaild
  io.dcache_grant.ready   := dcache_grant_ready
  io.dcache_grantAck.valid := dcache_grantAck_valid
  io.rsp.valid := rsp_valid

  mshr_state_dnxt := 
    Mux1H(Seq(
      (mshr_state_qout === 0.U) -> Mux(miss_valid.contains(true.B) & ~io.miss_ban, 1.U, 0.U) ,//cfree
      (mshr_state_qout === 1.U) -> Mux(io.dcache_acquire.fire, 2.U, 1.U),//acquire
      (mshr_state_qout === 2.U) -> Mux(is_trans_done, 3.U, 2.U),//grant
      (mshr_state_qout === 3.U) -> Mux(io.dcache_grantAck.fire, 0.U, 3.U)//grantack
    ))

  when( mshr_state_qout === 0.U & mshr_state_dnxt === 1.U ) {
    dcache_acquire_vaild := true.B
  } .elsewhen( io.dcache_acquire.fire ) {
    dcache_acquire_vaild := false.B
    assert(mshr_state_qout === 1.U)
  }

  io.dcache_acquire.bits := {
    // val grow_param = ClientMetadata(ClientStates.Nothing).onAccess(MemoryOpCategories.wr)._2
    edge.AcquireBlock(
      fromSource = 65.U,
      toAddress = miss_queue(acquire_sel).paddr,
      lgSize = log2Ceil(256/8).U,
      growPermissions = TLPermissions.NtoT
    )._2
  }

  dcache_grant_ready := (mshr_state_qout === 2.U)
  when( io.dcache_grant.fire ) {
    when(~is_trans_done) { miss_rsp(0) := io.dcache_grant.bits.data }
    .otherwise { miss_rsp(1) := io.dcache_grant.bits.data }
    assert( mshr_state_qout === 2.U )
  }

  when( mshr_state_dnxt === 3.U & mshr_state_qout === 2.U ) {
    rsp_valid := true.B
  } .elsewhen(io.rsp.fire) {
    rsp_valid := false.B
    dcache_grantAck_valid := true.B
    assert(mshr_state_qout === 3.U)
  } .elsewhen( io.dcache_grantAck.fire ) {
    dcache_grantAck_valid := false.B
    miss_valid( acquire_sel ) := false.B
    assert(mshr_state_qout === 3.U)
  }

  io.rsp.bits.paddr := miss_queue(acquire_sel).paddr


  io.rsp.bits.wdata(0) := miss_rsp(0)(63,0)
  io.rsp.bits.wdata(1) := miss_rsp(0)(127,64)
  io.rsp.bits.wdata(2) := miss_rsp(1)(63,0)
  io.rsp.bits.wdata(3) := miss_rsp(1)(127,64)


  io.rsp.bits.wmask := "hFF".U
  io.rsp.bits.op.grant := true.B
  io.rsp.bits.op.load  := false.B
  io.rsp.bits.op.store := false.B
  io.rsp.bits.op.probe := false.B
  io.rsp.bits.op.grant := false.B
  io.rsp.bits.op.lr    := false.B
  io.rsp.bits.op.sc    := false.B
  io.rsp.bits.op.swap  := false.B
  io.rsp.bits.op.add   := false.B
  io.rsp.bits.op.and   := false.B
  io.rsp.bits.op.or    := false.B
  io.rsp.bits.op.xor   := false.B
  io.rsp.bits.op.max   := false.B
  io.rsp.bits.op.maxu  := false.B
  io.rsp.bits.op.min   := false.B
  io.rsp.bits.op.minu  := false.B
  io.rsp.bits.rd0_phy  := DontCare
  io.rsp.bits.rdata    := DontCare
  io.rsp.bits.tag      := DontCare

  io.release_ban := mshr_state_qout === 1.U | mshr_state_qout === 2.U





  val is_missQueue_full = miss_valid.forall( (x:Bool) => (x === true.B) )
  val load_sel = miss_valid.indexWhere( (x:Bool) => (x === false.B) )
  io.req.ready := ~is_missQueue_full

  val is_merge_addr  = miss_queue.exists((x: Info_mshr_req) => (x.paddr === io.req.bits.paddr) )
  val merge_idx      = miss_queue.indexWhere((x: Info_mshr_req) => (x.paddr === io.req.bits.paddr) )
  val is_merge_valid = miss_valid(merge_idx) === true.B
  when(io.req.fire) {
    when( ~is_merge_addr | ~is_merge_valid ) {
      miss_queue(load_sel) := io.req.bits
      miss_valid(load_sel) := true.B      
    }
  }

}


