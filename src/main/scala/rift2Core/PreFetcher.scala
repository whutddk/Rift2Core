/*
  Copyright (c) 2020 - 2022 Wuhan University of Technology <295054118@whut.edu.cn>

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

package rift2Core

import chisel3._
import chisel3.util._
import rift2Core.define._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import rift2Chip._
import chipsalliance.rocketchip.config.Parameters

class PreFetcher(edge: TLEdgeOut)(implicit p: Parameters) extends RiftModule{
  val io = IO(new Bundle{

    val stqReq          = Flipped(Valid( new PreFetch_Req_Bundle))
    val icacheRefillReq = Flipped(Valid( new PreFetch_Req_Bundle))
    // val bruReq          = Flipped(Valid( new PreFetch_Req_Bundle))
    // val invPredictReq   = Flipped(Valid( new PreFetch_Req_Bundle))

    val intent    = new DecoupledIO(new TLBundleA(edge.bundle))
    val hintAck = Flipped(new DecoupledIO(new TLBundleD(edge.bundle))) 
  })

  // val prefetchFifo = Module(new Queue( new PreFetch_Req_Bundle, 16 ))

  val arb = Module(new Arbiter(new PreFetch_Req_Bundle, 2))

  // val intentValid = RegInit(false.B)
  val isBusy = RegInit(false.B)

  arb.io.in(0).valid := io.stqReq.valid
  arb.io.in(0).bits := io.stqReq.bits
  arb.io.in(1).valid := false.B
  arb.io.in(1).bits := io.icacheRefillReq.bits
  // arb.io.in(2) := io.bruReq
  // arb.io.in(3) := io.invPredictReq

  // prefetchFifo.io.enq <> arb.io.out

  arb.io.out.ready := true.B

  // prefetchFifo.io.deq.ready := io.intent.fire

  val (_, _, is_trans_done, _) = edge.count(io.intent)

  if(hasPreFetch) {
    io.intent.valid := arb.io.out.valid & ~isBusy
    io.intent.bits :=
      edge.Get(
        fromSource = 0.U,
        toAddress = arb.io.out.bits.paddr >> 5 << 5,
        lgSize = log2Ceil(256/8).U
      )._2   
  } else {
    io.intent.valid := false.B
    io.intent.bits := 0.U.asTypeOf(new TLBundleA(edge.bundle))
  }

  



    // edge.Hint(
    //   fromSource = 0.U,
    //   toAddress = arb.io.out.bits.paddr >> 5 << 5,
    //   lgSize = log2Ceil(256/8).U,
    //   param = 1.U)._2

  when( io.intent.fire ) {
    isBusy := true.B
  } .elsewhen( is_trans_done ) {
    isBusy := false.B
  }

  // when( ~intentValid & ~isBusy &  prefetchFifo.io.deq.valid ) {
  //   intentValid := true.B
  // } .elsewhen( io.intent.fire ) {
  //   intentValid := false.B
    
  //   assert( (prefetchFifo.io.deq.bits.paddr & "h80000000".U) =/= 0.U )
  // }

  io.hintAck.ready := true.B

}
