/*
  Copyright (c) 2020 - 2023 Wuhan University of Technology <295054118@whut.edu.cn>

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

import base._
import rift2Chip._

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.tilelink._



class Info_miss_req(implicit p: Parameters) extends RiftBundle {
  val paddr = UInt(plen.W)
}

class Info_miss_rsp(implicit p: Parameters) extends RiftBundle with HasDcacheParameters{
  val paddr = UInt(plen.W)
  val wdata = UInt(dw.W)
}


/** The Queue of cache to request acquire and waiting for grant and ack grant */
class MissUnit(edge: TLEdgeOut, setting: Int, id: Int)(implicit p: Parameters) extends RiftModule with HasDcacheParameters{
  val io = IO(new Bundle{
    val req = Flipped(Valid(new Info_miss_req))
    val rsp = DecoupledIO(new Info_miss_rsp)

    val cache_acquire = new DecoupledIO(new TLBundleA(edge.bundle))
    val cache_grant   = Flipped(new DecoupledIO(new TLBundleD(edge.bundle)))
    val cache_grantAck  = new DecoupledIO(new TLBundleE(edge.bundle))

    val miss_ban = Input(Bool())
    val release_ban = Output(Bool())

  })

  /** a parallel buff of *paddr* miss request, when a duplicated request comes, it will be acked but dismiss */
  val miss_queue = RegInit(VecInit( Seq.fill(dcacheParams.sbEntry)( 0.U.asTypeOf(new Info_miss_req) )))

  /** a valid flag indicated whether a buff is in-used */
  val miss_valid = RegInit(VecInit( Seq.fill(dcacheParams.sbEntry)( false.B )))

  /** a grant will complete in 2 beat, and get 256-bits data */ 
  val miss_rsp = RegInit(VecInit( Seq.fill(dw/l1BeatBits)(0.U(l1BeatBits.W))  ))

  val mshr_state_dnxt = Wire(UInt(3.W))
  val mshr_state_qout = RegNext(mshr_state_dnxt, 0.U)

  /**
    * @param is_trans_done indicated whether the trans is in last beat
    * @param transCnt shows the beats count
    */
  val (_, _, is_trans_done, transCnt) = edge.count(io.cache_grant)

  /** when the bus is free, a valid paddr will be selected to emit */
  val acquire_sel = {
    val sel = miss_valid.indexWhere( (x: Bool) => (x === true.B) )
    Mux( mshr_state_qout === 0.U, sel, RegEnable(sel, mshr_state_qout === 0.U & mshr_state_dnxt === 1.U) )
  }


  /** a register of io.cache_acquire.valid */
  val cache_acquire_vaild  = RegInit(false.B)

  /** a wire of io.cache_grant.ready */
  val cache_grant_ready    = Wire(Bool())

  /** a register of io.cache_grantAck.valid */
  val cache_grantAck_valid = RegInit(false.B)

  /** a register of io.rsp.valid */
  val rsp_valid = RegInit(false.B)

  val cache_grant_reg = RegEnable(io.cache_grant.bits, io.cache_grant.fire)
  assert( ~(io.cache_grant.fire & io.cache_grant.bits.source =/= id.U), "Assert Failed at missUnit, source-id of grant mis-match"  )

  io.cache_acquire.valid := cache_acquire_vaild
  io.cache_grant.ready   := cache_grant_ready
  io.cache_grantAck.valid := cache_grantAck_valid
  io.cache_grantAck.bits  := edge.GrantAck(cache_grant_reg)
  io.rsp.valid := rsp_valid

  mshr_state_dnxt := 
    Mux1H(Seq(
      (mshr_state_qout === 0.U) -> Mux(miss_valid.contains(true.B) & ~io.miss_ban, 1.U, 0.U) ,//cfree
      (mshr_state_qout === 1.U) -> Mux(io.cache_acquire.fire, 2.U, 1.U),//acquire
      (mshr_state_qout === 2.U) -> Mux(is_trans_done, 3.U, 2.U),//grant
      (mshr_state_qout === 3.U) -> Mux(io.cache_grantAck.fire, 0.U, 3.U)//grantack
    ))

  when( mshr_state_qout === 0.U & mshr_state_dnxt === 1.U ) {
    cache_acquire_vaild := true.B
  } .elsewhen( io.cache_acquire.fire ) {
    cache_acquire_vaild := false.B
    assert(mshr_state_qout === 1.U)
  }

  io.cache_acquire.bits := {
    def permit: UInt = {
      var res = 0.U
      if ( setting == 0 ) {
        res = TLPermissions.NtoB
      } else if ( setting == 2 ) {
        res = TLPermissions.NtoT
      }
      res
    }


    edge.AcquireBlock(
      fromSource = id.U,
      toAddress = miss_queue(acquire_sel).paddr & ("hFFFFFFFF".U << log2Ceil(dw/8).U),
      lgSize = log2Ceil(dw/8).U,
      growPermissions = permit

    )._2
  }

  

  cache_grant_ready := (mshr_state_qout === 2.U)
  when( io.cache_grant.fire ) {
     miss_rsp(transCnt) := io.cache_grant.bits.data
    assert( mshr_state_qout === 2.U )
  }

  when( mshr_state_dnxt === 3.U & mshr_state_qout === 2.U ) {
    rsp_valid := true.B
  } .elsewhen(io.rsp.fire) {
    rsp_valid := false.B
    cache_grantAck_valid := true.B
    assert(mshr_state_qout === 3.U)
  } .elsewhen( io.cache_grantAck.fire ) {
    cache_grantAck_valid := false.B
    miss_valid( acquire_sel ) := false.B
    assert(mshr_state_qout === 3.U)
  }

  io.rsp.bits.paddr := miss_queue(acquire_sel).paddr
  io.rsp.bits.wdata := Cat( miss_rsp.reverse)


  io.release_ban := mshr_state_dnxt === 2.U | mshr_state_qout === 2.U




  /** when all missQueue BUFF is in used, miss req will be bypassed */
  val is_missQueue_full = miss_valid.forall( (x:Bool) => (x === true.B) )

  /** select an empty buff to load paddr, except when *buff full* or *can merge* */
  val load_sel = miss_valid.indexWhere( (x:Bool) => (x === false.B) )


  

  /** findout if there is no buff is valid and has the same paddr, or merge it! */
  val is_merge = {
    for ( i <- 0 until dcacheParams.sbEntry ) yield {
      (miss_queue(i).paddr === io.req.bits.paddr) & miss_valid(i) === true.B
    }
  }.reduce(_|_)


  when(io.req.fire) {
    when( ~is_merge & ~is_missQueue_full ) {
      miss_queue(load_sel) := io.req.bits
      miss_valid(load_sel) := true.B      
    }
  }


  // io.req.ready := true.B
  when( io.req.valid ) {
    assert(
      miss_valid.exists((x:Bool) => (x === false.B)) ||
      (miss_queue zip miss_valid).map{ case(a,b) => (a.paddr === io.req.bits.paddr) & b === true.B }.reduce(_|_),
      "Assert Failed at MissUnit, the Miss-Entry is equal to SB-Entry, which hints that missUnit never full!" )
  }

  // when( io.rsp.valid ) {
  //   assert( io.rsp.ready === true.B, "Assert Failed at MissUnit, As Highest-priority Arbiter Input, Dcache Pipeline should never stuck!" )
  // }
}






/** The Queue of cache to request acquire and waiting for grant and ack grant Get-Mode*/
class GetUnit(edge: TLEdgeOut, id: Int)(implicit p: Parameters) extends RiftModule with HasDcacheParameters{
  val io = IO(new Bundle{
    val req = Flipped(Valid(new Info_miss_req))
    val rsp = DecoupledIO(new Info_miss_rsp)

    val getPut = new DecoupledIO(new TLBundleA(edge.bundle))
    val access = Flipped(new DecoupledIO(new TLBundleD(edge.bundle)))

    val miss_ban = Input(Bool())
    val release_ban = Output(Bool())

  })

  /** a parallel buff of *paddr* miss request, when a duplicated request comes, it will be acked but dismiss */
  val miss_queue = RegInit(VecInit( Seq.fill(dcacheParams.sbEntry)( 0.U.asTypeOf(new Info_miss_req) )))

  /** a valid flag indicated whether a buff is in-used */
  val miss_valid = RegInit(VecInit( Seq.fill(dcacheParams.sbEntry)( false.B )))

  /** a grant will complete in 2 beat, and get 256-bits data */ 
  val miss_rsp = RegInit(VecInit( Seq.fill(dw/l1BeatBits)(0.U(l1BeatBits.W))  ))

  val mshr_state_dnxt = Wire(UInt(3.W))
  val mshr_state_qout = RegNext(mshr_state_dnxt, 0.U)

  /**
    * @param is_trans_done indicated whether the trans is in last beat
    * @param transCnt shows the beats count
    */
  val (_, _, is_trans_done, transCnt) = edge.count(io.access)

  /** when the bus is free, a valid paddr will be selected to emit */
  val acquire_sel = {
    val sel = miss_valid.indexWhere( (x: Bool) => (x === true.B) )
    Mux( mshr_state_qout === 0.U, sel, RegEnable(sel, mshr_state_qout === 0.U & mshr_state_dnxt === 1.U) )
  }


  /** a register of io.getPut.valid */
  val getPut_vaild  = RegInit(false.B)

  /** a wire of io.access.ready */
  val access_ready    = Wire(Bool())



  /** a register of io.rsp.valid */
  val rsp_valid = RegInit(false.B)

  val access_reg = RegEnable(io.access.bits, io.access.fire)
  assert( ~(io.access.fire & io.access.bits.source =/= id.U), "Assert Failed at missUnit, source-id of grant mis-match"  )

  io.getPut.valid := getPut_vaild
  io.access.ready   := access_ready

  io.rsp.valid := rsp_valid

  mshr_state_dnxt := 
    Mux1H(Seq(
      (mshr_state_qout === 0.U) -> Mux(miss_valid.contains(true.B) & ~io.miss_ban, 1.U, 0.U) ,//cfree
      (mshr_state_qout === 1.U) -> Mux(io.getPut.fire, 2.U, 1.U),//acquire
      (mshr_state_qout === 2.U) -> Mux(is_trans_done, 3.U, 2.U),//grant
      (mshr_state_qout === 3.U) -> Mux(io.rsp.fire, 4.U, 3.U),
      (mshr_state_qout === 4.U) -> 0.U//grantack
    ))

  when( mshr_state_qout === 0.U & mshr_state_dnxt === 1.U ) {
    getPut_vaild := true.B
  } .elsewhen( io.getPut.fire ) {
    getPut_vaild := false.B
    assert(mshr_state_qout === 1.U)
  }


  io.getPut.bits := 
    edge.Get(
      fromSource = id.U,
      toAddress = miss_queue(acquire_sel).paddr & ("hFFFFFFFF".U << log2Ceil(dw/8).U),
      lgSize = log2Ceil(dw/8).U
    )._2


  

  access_ready := (mshr_state_qout === 2.U)
  when( io.access.fire ) {
     miss_rsp(transCnt) := io.access.bits.data
    assert( mshr_state_qout === 2.U )
  }

  when( mshr_state_dnxt === 3.U & mshr_state_qout === 2.U ) {
    rsp_valid := true.B
  } .elsewhen( io.rsp.fire ) {
    rsp_valid := false.B
    assert(mshr_state_qout === 3.U)
  } .elsewhen(mshr_state_qout === 4.U){
    miss_valid( acquire_sel ) := false.B
  }

  io.rsp.bits.paddr := miss_queue(acquire_sel).paddr
  io.rsp.bits.wdata := Cat( miss_rsp.reverse)


  io.release_ban := mshr_state_dnxt === 2.U | mshr_state_qout === 2.U




  /** when all missQueue BUFF is in used, miss req will be bypassed */
  val is_missQueue_full = miss_valid.forall( (x:Bool) => (x === true.B) )

  /** select an empty buff to load paddr, except when *buff full* or *can merge* */
  val load_sel = miss_valid.indexWhere( (x:Bool) => (x === false.B) )


  

  /** findout if there is no buff is valid and has the same paddr, or merge it! */
  val is_merge = {
    for ( i <- 0 until dcacheParams.sbEntry ) yield {
      (miss_queue(i).paddr === io.req.bits.paddr) & miss_valid(i) === true.B
    }
  }.reduce(_|_)


  when(io.req.fire) {
    when( ~is_merge & ~is_missQueue_full ) {
      miss_queue(load_sel) := io.req.bits
      miss_valid(load_sel) := true.B      
    }
  }


  when( io.req.valid ) {
    assert(
      miss_valid.exists((x:Bool) => (x === false.B)) ||
      (miss_queue zip miss_valid).map{ case(a,b) => (a.paddr === io.req.bits.paddr) & b === true.B }.reduce(_|_),
      "Assert Failed at MissUnit, the Miss-Entry is equal to SB-Entry, which hints that missUnit never full!" )
  }

}



