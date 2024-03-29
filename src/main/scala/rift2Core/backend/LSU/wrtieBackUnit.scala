/*
  Copyright (c) 2020 - 2024 Wuhan University of Technology <295054118@whut.edu.cn>

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

import rift2Chip._

import org.chipsalliance.cde.config._
import freechips.rocketchip.tilelink._


/** the operation infomation of writeback Unit */
class Info_writeBack_req(implicit p: Parameters) extends RiftBundle with HasDcacheParameters{
  val paddr = UInt(plen.W)
  val data = UInt(dw.W)
  val is_releaseData = Bool()
  val is_release = Bool()
  val is_probe = Bool()
  val is_probeData = Bool()

  // assert( PopCount(Cat(is_releaseData, is_release, is_probe, is_probeData)) === 1.U )
}

/**
  * a writeBack Unit accept data and paddr from l1cache and write it back to l2cache Probe-Release-Mode
  */
class WriteBackUnit(edge: TLEdgeOut, setting: Int, id: Int)(implicit p: Parameters) extends RiftModule with HasDcacheParameters{

  class WriteBackUnitIO extends Bundle{
    val wb_req = Flipped(new ValidIO(new Info_writeBack_req))

    val cache_release = new DecoupledIO(new TLBundleC(edge.bundle))
    val cache_grant   = Flipped(new DecoupledIO(new TLBundleD(edge.bundle)))


    val release_ban = Input(Bool())
    val miss_ban = Output(Bool())    
  }

  val io: WriteBackUnitIO = IO(new WriteBackUnitIO)





  /** a tiny fifo that temporarily store the writeback info from l1cache */
  val wb_fifo = Module(new Queue(new Info_writeBack_req, 2, false, true))

  wb_fifo.io.enq.valid := io.wb_req.valid
  wb_fifo.io.enq.bits  := io.wb_req.bits




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
        wb_fifo.io.deq.valid & (
          (~io.release_ban & ( wb_fifo.io.deq.bits.is_release | wb_fifo.io.deq.bits.is_releaseData )) |
          ( wb_fifo.io.deq.bits.is_probe | wb_fifo.io.deq.bits.is_probeData )
        )
        , 1.U, 0.U),
      (wb_state_qout === 1.U) ->
        Mux(~is_release_done, 1.U,
          Mux( wb_fifo.io.deq.bits.is_probeData | wb_fifo.io.deq.bits.is_probe, 0.U, 2.U )),
      (wb_state_qout === 2.U) -> Mux( io.cache_grant.fire, 0.U, 2.U )
    ))

  /** a n step counter to select data */
  val beatCnt = RegInit( 0.U((log2Ceil(dw/l1BeatBits)).W) )
  when( wb_state_qout === 0.U & wb_state_dnxt === 1.U ) { beatCnt := 0.U }
  .elsewhen( io.cache_release.fire ) { beatCnt := beatCnt + 1.U }
    
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
      toAddress = wb_fifo.io.deq.bits.paddr & ("hFFFFFFFF".U << log2Ceil(dw/8).U),
      lgSize = log2Ceil(dw/8).U,
      reportPermissions = permit,    
    )

    val info_probeData = edge.ProbeAck(
      fromSource = id.U,
      toAddress = wb_fifo.io.deq.bits.paddr & ("hFFFFFFFF".U << log2Ceil(dw/8).U),
      lgSize = log2Ceil(dw/8).U,
      reportPermissions = permit,
      data = wb_fifo.io.deq.bits.data >> (beatCnt << log2Ceil(l1BeatBits))
    )

    val info_release = edge.Release(
      fromSource = id.U,
      toAddress = wb_fifo.io.deq.bits.paddr & ("hFFFFFFFF".U << log2Ceil(dw/8).U),
      lgSize = log2Ceil(dw/8).U,
      shrinkPermissions = permit
    )._2

    val info_releaseData = edge.Release(
      fromSource = id.U,
      toAddress = wb_fifo.io.deq.bits.paddr & ("hFFFFFFFF".U << log2Ceil(dw/8).U),
      lgSize = log2Ceil(dw/8).U,
      shrinkPermissions = permit,
      data = wb_fifo.io.deq.bits.data >> (beatCnt << log2Ceil(l1BeatBits))
    )._2


    Mux1H(Seq(
      wb_fifo.io.deq.bits.is_probe       -> info_probe,
      wb_fifo.io.deq.bits.is_probeData   -> info_probeData,
      wb_fifo.io.deq.bits.is_release     -> info_release,
      wb_fifo.io.deq.bits.is_releaseData -> info_releaseData,
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
  wb_fifo.io.deq.ready := 
    wb_state_qout =/= 0.U & wb_state_dnxt === 0.U

  io.miss_ban := wb_state_qout === 1.U | wb_state_qout === 2.U | wb_fifo.io.deq.valid



  when( ~wb_fifo.io.enq.ready ) {
    assert(~io.wb_req.valid, "When WriteBack Unit is busy, no new acquire will be emitted, and there is no wb request!")
  }
}




/**
  * a writeBack Unit accept data and paddr from l1cache and write it back to l2cache Put-Mode
  */
class PutUnit(edge: TLEdgeOut, id: Int)(implicit p: Parameters) extends RiftModule with HasDcacheParameters{

  class PutUnitIO extends Bundle{
    val wb_req = Flipped(new ValidIO(new Info_writeBack_req))

    val getPut    = new DecoupledIO(new TLBundleA(edge.bundle))
    val access = Flipped(new DecoupledIO(new TLBundleD(edge.bundle)))

    val release_ban = Input(Bool())
    val miss_ban = Output(Bool())
  }

  val io: PutUnitIO = IO(new PutUnitIO)





  /** a tiny fifo that temporarily store the writeback info from l1cache */
  val wb_fifo = Module(new Queue(new Info_writeBack_req, 2, false, true))

  wb_fifo.io.enq.valid := io.wb_req.valid & io.wb_req.bits.is_releaseData //bypass is_release
  wb_fifo.io.enq.bits  := io.wb_req.bits


  val wb_state_dnxt = Wire(UInt(2.W))
  val wb_state_qout = RegNext(wb_state_dnxt, 0.U)

  /** a last flag of release transmision */
  val is_release_done = edge.count(io.getPut)._3

  /** a register of io.getPut.valid */
  val getPut_valid = RegInit(false.B)

  /** a register of io.access.ready */
  val access_ready   = RegInit(false.B)

  io.getPut.valid := getPut_valid
  io.access.ready := access_ready


  wb_state_dnxt :=
    Mux1H(Seq(
      (wb_state_qout === 0.U) -> Mux( wb_fifo.io.deq.valid & ~io.release_ban , 1.U, 0.U),
      (wb_state_qout === 1.U) -> Mux(~is_release_done, 1.U, 2.U ),
      (wb_state_qout === 2.U) -> Mux( io.access.fire, 0.U, 2.U )
    ))

  /** a n step counter to select data */
  val beatCnt = RegInit( 0.U((log2Ceil(dw/l1BeatBits)).W) )
  when( wb_state_qout === 0.U & wb_state_dnxt === 1.U ) { beatCnt := 0.U }
  .elsewhen( io.getPut.fire ) { beatCnt := beatCnt + 1.U }
    
  when( wb_state_qout === 1.U ) {
    when( io.getPut.fire ) { getPut_valid := false.B }
    .elsewhen( ~io.getPut.valid & ~io.release_ban ) { getPut_valid := true.B }
  }

  io.getPut.bits :=
    edge.Put(
      fromSource = id.U,
      toAddress = wb_fifo.io.deq.bits.paddr & ("hFFFFFFFF".U << log2Ceil(dw/8).U),
      lgSize = log2Ceil(dw/8).U,
      data = wb_fifo.io.deq.bits.data >> (beatCnt << log2Ceil(l1BeatBits)),
      mask = Fill(dw/8, 1.U)
    )._2


  when( io.access.valid & ~io.access.ready ) {
    access_ready := true.B
    assert( wb_state_qout === 2.U )
  } .elsewhen( io.access.ready ) {
    access_ready := false.B
    assert( wb_state_qout === 2.U )   
  }
  assert( ~(io.access.fire & io.access.bits.source =/= id.U), "Assert Failed at writeBack-Unit, grant-id mis-match" )
  wb_fifo.io.deq.ready := 
    wb_state_qout =/= 0.U & wb_state_dnxt === 0.U

  io.miss_ban := wb_state_qout === 1.U | wb_state_qout === 2.U | ~wb_fifo.io.enq.ready

  when( ~wb_fifo.io.enq.ready ) {
    assert(~io.wb_req.valid, "When WriteBack Unit is busy, no new acquire will be emitted, and there is no wb request!")
  }
}
