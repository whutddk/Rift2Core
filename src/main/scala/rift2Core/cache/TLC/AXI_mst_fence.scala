/*
  Copyright (c) 2020 - 2021 Ruige Lee <m201772520@hust.edu.cn>

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

package rift2Core.cache.TLC

import chisel3._
import chisel3.util._

import axi._

import base._
import rift2Core.cache._


trait AXI_mst_fence extends TLC_base {
  val mst_chn_aw1 = new DecoupledIO(new AXI_chn_a( 64, 1, 1 ))
  val mst_chn_w1  = new DecoupledIO(new AXI_chn_w( 128, 1 )) 
  val mst_chn_b1 = Flipped(new DecoupledIO( new AXI_chn_b( 1, 1 )))

  val fence = IO( new DecoupledIO(Bool()) )

  assert( cb == 1, "Assert Failed at AXI_fence, invalid configuration of cb" )

  // val cache_inv = RegInit( VecInit( Seq.fill(cl)( VecInit(Seq.fill(cb)( VecInit( Seq.fill(bk)(false.B)))))))
  // val cache_mdf = RegInit( VecInit( Seq.fill(cl)( VecInit(Seq.fill(cb)( VecInit( Seq.fill(bk)(false.B)))))))


  when( fence.valid ) {

  }

}


