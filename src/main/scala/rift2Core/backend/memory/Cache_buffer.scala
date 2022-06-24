


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

package rift2Core.backend.memory

import chisel3._
import chisel3.util._
import rift2Core.define._
import rift2Core.backend._
import rift2Core.L1Cache._
import rift._
import base._
import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

class Cache_buffer()(implicit p: Parameters) extends DcacheModule{
  val io = IO(new Bundle{
    val buf_enq = Flipped(DecoupledIO(new Info_cache_s0s1))
    val enq_idx = Output(UInt((log2Ceil(sbEntry)).W))
    val is_storeBuff_empty = Output(Bool())
    val buf_deq = Flipped(DecoupledIO(new Info_cache_retn))
    
  })


  val buff = RegInit(VecInit(Seq.fill(sbEntry)(0.U.asTypeOf(new Info_cache_s0s1))))
  val valid = RegInit(VecInit(Seq.fill(sbEntry)(false.B)))




  val is_hazard = VecInit(buff.map( x => (x.paddr(plen-1,3) === io.buf_enq.bits.paddr(plen-1,3)) )).reduce(_|_)
  val idx = valid.indexWhere( (x:Bool) => (x === false.B) )
  io.enq_idx := idx

  io.buf_enq.ready := 
    valid.exists( (x:Bool) => (x === false.B) ) &
    ~is_hazard

  when( io.buf_enq.fire ) {
    buff(idx)  := io.buf_enq.bits
    valid(idx) := true.B
  }

  when( io.buf_deq.fire ) {
    buff(io.buf_deq.bits.chk_idx)  := 0.U.asTypeOf( new Info_cache_s0s1 )
    valid(io.buf_deq.bits.chk_idx) := false.B
  }

  io.is_storeBuff_empty := valid.forall((x:Bool) => (x === false.B))
  io.buf_deq.ready := ~io.is_storeBuff_empty



  for ( i <- 0 until sbEntry ) yield {
    when( valid(i) === true.B ) {
      assert( buff.count( (x: Info_cache_s0s1) => (x.paddr === buff(i).paddr) ) === 1.U )
    }
  }
}

