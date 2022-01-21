


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

trait Cache_buffer {


  printf("Cache_Buff depth is 16\n")


  val buf_enq = Decoupled(new Info_cache_s0s1)
  val buf_deq = Decoupled(UInt(log2Ceil(16).W))


  val buff = RegInit(VecInit(Seq.fill(16)(0.U.asTypeOf(new Info_cache_s0s1))))
  val valid = RegInit(VecInit(Seq.fill(16)(false.B)))




  val is_hazard = buff.map( x => (x.paddr === buf_enq.paddr) ).reduce(_|_)
  val idx = valid.indexWhere( (x:Bool) => (x === true.B) )


  buf_enq.ready := 
    valid.exists( (x:Bool) => (x === false.B) ) &
    ~is_hazard

  when( buf_enq.fire ) {
    buff(idx)  := buf_enq.bits
    valid(idx) := true.B
  }

  when( buf_deq.fire ) {
    buff(buf_deq.bits)  := 0.U.asTypeOf( new Info_cache_s0s1 )
    valid(buf_deq.bits) := false.B
  }

  def is_storeBuff_empty = valid.forall((x:Bool) => (x === false.B))
  buf_deq.ready := ~is_storeBuff_empty



  for ( i <- 0 until 16 ) yield {
    when( valid(i) === true.B ) {
      assert( buff.count( (x: Reservation_Info) => (x.paddr === buff(i).paddr) ) === 1.U )
    }
  }
}

