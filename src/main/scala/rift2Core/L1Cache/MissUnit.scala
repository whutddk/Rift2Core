package rift2Core.L1Cache


import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import base._
import freechips.rocketchip.tilelink._


class Info_mshr_req extends Bundle {
  val paddr = UInt(32.W)
}


/** The Queue of cache to request acquire and waiting for grant and ack grant */
class MissUnit(edge: TLEdgeOut, entry: Int = 8)(implicit p: Parameters) extends L1CacheModule {
  val io = IO(new Bundle{
    val req = Flipped(DecoupledIO(new Info_mshr_req))
    val rsp = DecoupledIO(new Info_cache_s1s2)

    val dcache_acquire = new DecoupledIO(new TLBundleA(edge.bundle))
    val dcache_grant   = Flipped(new DecoupledIO(new TLBundleD(edge.bundle)))
    val dcache_grantAck  = new DecoupledIO(new TLBundleE(edge.bundle))

    val miss_ban = Input(Bool())
    val release_ban = Output(Bool())

  })

  /** a parallel buff of *paddr* miss request, when a duplicated request comes, it will be acked but dismiss */
  val miss_queue = RegInit(VecInit( Seq.fill(entry)( 0.U.asTypeOf(new Info_mshr_req) )))

  /** a valid flag indicated whether a buff is in-used */
  val miss_valid = RegInit(VecInit( Seq.fill(entry)( false.B )))

  /** a grant will complete in 2 beat, and get 256-bits data */ 
  val miss_rsp = RegInit(VecInit( Seq.fill(2)(0.U(128.W))  ))

  val mshr_state_dnxt = Wire(UInt(3.W))
  val mshr_state_qout = RegNext(mshr_state_dnxt, 0.U)

  /**
    * @param is_trans_done indicated whether the trans is in last beat
    * @param transCnt shows the beats count
    */
  val (_, _, is_trans_done, transCnt) = edge.count(io.dcache_grant)

  /** when the bus is free, a valid paddr will be selected to emit */
  val acquire_sel = miss_valid.indexWhere( (x: Bool) => (x === true.B) )

  /** a register of io.dcache_acquire.valid */
  val dcache_acquire_vaild  = RegInit(false.B)

  /** a wire of io.dcache_grant.ready */
  val dcache_grant_ready    = Wire(Bool())

  /** a register of io.dcache_grantAck.valid */
  val dcache_grantAck_valid = RegInit(false.B)

  /** a register of io.rsp.valid */
  val rsp_valid = RegInit(false.B)

  io.dcache_acquire.valid := dcache_acquire_vaild
  io.dcache_grant.ready   := dcache_grant_ready
  io.dcache_grantAck.valid := dcache_grantAck_valid
  io.dcache_grantAck.bits  := edge.GrantAck(io.dcache_grant.bits)
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

  io.dcache_acquire.bits := 
    edge.AcquireBlock(
      fromSource = 0.U,
      toAddress = miss_queue(acquire_sel).paddr & ("hFFFFFFFF".U << log2Ceil(256/8).U),
      lgSize = log2Ceil(256/8).U,
      growPermissions = TLPermissions.NtoT
    )._2
  

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

  {
    io.rsp.bits.op := 0.U.asTypeOf(new Cache_op)
    io.rsp.bits.op.grant := true.B
  }
  
  io.rsp.bits.chk_idx  := DontCare
  io.rsp.bits.rdata    := DontCare
  io.rsp.bits.tag      := DontCare

  io.release_ban := mshr_state_dnxt === 2.U | mshr_state_qout === 2.U




  /** when all missQueue BUFF is in used, miss req will be bypassed */
  val is_missQueue_full = miss_valid.forall( (x:Bool) => (x === true.B) )

  /** select an empty buff to load paddr, except when *buff full* or *can merge* */
  val load_sel = miss_valid.indexWhere( (x:Bool) => (x === false.B) )

  io.req.ready := true.B

  /** findout if there is no buff is valid and has the same paddr, or merge it! */
  val is_merge = {
    for ( i <- 0 until entry ) yield {
      (miss_queue(i).paddr === io.req.bits.paddr) & miss_valid(i) === true.B
    }
  }.reduce(_|_)


  when(io.req.fire) {
    when( ~is_merge & ~is_missQueue_full ) {
      miss_queue(load_sel) := io.req.bits
      miss_valid(load_sel) := true.B      
    }
  }

}


