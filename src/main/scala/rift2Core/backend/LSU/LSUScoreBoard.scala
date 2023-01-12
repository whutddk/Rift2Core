// /*
//   Copyright (c) 2020 - 2023 Wuhan University of Technology <295054118@whut.edu.cn>

//    Licensed under the Apache License, Version 2.0 (the "License");
//    you may not use this file except in compliance with the License.
//    You may obtain a copy of the License at

//        http://www.apache.org/licenses/LICENSE-2.0

//    Unless required by applicable law or agreed to in writing, software
//    distributed under the License is distributed on an "AS IS" BASIS,
//    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//    See the License for the specific language governing permissions and
//    limitations under the License.
// */

// package rift2Core.backend.lsu

// import chisel3._
// import chisel3.util._
// import chisel3.util.random._

// import rift2Core.define._

// import rift2Chip._
// import base._

// import chipsalliance.rocketchip.config.Parameters
// import freechips.rocketchip.diplomacy._



// trait DcacheScoreBoard { this: DcacheBase =>

//   val sbBuff = RegInit(VecInit(Seq.fill(sbEntry)(0.U.asTypeOf(new Dcache_Enq_Bundle))))
//   val sbValid = RegInit(VecInit(Seq.fill(sbEntry)(false.B)))

//   val isHazard = VecInit(sbBuff.map( x => (x.paddr(plen-1,3) === io.enq.bits.paddr(plen-1,3)) )).reduce(_|_)
//   val sbIdx = sbValid.indexWhere( (x:Bool) => (x === false.B) )


//   val sbEnqReady =
//     sbValid.exists( (x:Bool) => (x === false.B) ) &
//     ~isHazard

//   when( io.enq.fire ) {
//     sbBuff(sbIdx)  := io.enq.bits
//     sbValid(sbIdx) := true.B
//   }

//   when( io.deq.fire ) {
//     sbBuff (io.deq.bits.chkIdx) := 0.U.asTypeOf( new Dcache_Enq_Bundle )
//     sbValid(io.deq.bits.chkIdx) := false.B
//   }

//   val isSBEmpty = sbValid.forall((x:Bool) => (x === false.B))
//   when( io.deq.fire ) { assert(~isSBEmpty) }


//   for ( i <- 0 until sbEntry ) {
//     when( sbValid(i) === true.B ) {
//       assert( sbBuff.count( (x: Dcache_Enq_Bundle) => (x.paddr === sbBuff(i).paddr) ) === 1.U )
//     }
//   }

// }

