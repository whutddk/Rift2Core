

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


package rift2Core.frontend

import chisel3._
import chisel3.util._


import rift2Core.define._
import rift2Core.L1Cache._
import rift2Core.privilege._

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import rift._

abstract class IF2Base(edge: TLEdgeOut)(implicit p: Parameters) extends IcacheModule {
  val iEdge = edge

  val io = IO(new Bundle {

    val if2_req  = Flipped(new DecoupledIO( new IF1_Bundle ))
    val if2_resp = Vec(4, new DecoupledIO(new IF2_Bundle) )

    val if_mmu = DecoupledIO(new Info_mmu_req)
    val mmu_if = Flipped(DecoupledIO(new Info_mmu_rsp))

    val if_cmm = Output(new Info_if_cmm)


    val icache_get    = new DecoupledIO(new TLBundleA(iEdge.bundle))
    val icache_access = Flipped(new DecoupledIO(new TLBundleD(iEdge.bundle)))

    val flush = Input(Bool())

    val ifence = Input(Bool())

    /** prefetch is not guarantee to be accepted by cache*/
    val preFetch = ValidIO( new PreFetch_Req_Bundle )
  })

}

trait IF2_PreFetch { this: IF2Base => 
  if (hasPreFetch) {
    io.preFetch.valid := io.icache_get.fire
    io.preFetch.bits.paddr := io.icache_get.bits.address + "b100000".U    
  } else {
    io.preFetch.valid := false.B
    io.preFetch.bits.paddr := 0.U
  }

}

class IF2(edge: TLEdgeOut)(implicit p: Parameters) extends IF2Base(edge) with ICache with IF2_PreFetch {


}








