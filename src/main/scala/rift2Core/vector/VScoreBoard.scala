
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

package rift2Core.backend

import chisel3._
import chisel3.util._

import rift2Core.define._
import rift2Chip._

import rift2Core.backend._

class VscoreBoard_Enq_Bundle extends RiftBundle{
  val rd   = Vec(16, UInt((log2Ceil(vRegNum)).W))
  val mask = Vec(16, Vec( vParams.vlen/8, Bool() ))
  val eew  = Vec(4, Bool())
}

class VscoreBoard_Deq_Bundle extends RiftBundle{
  
}

abstract class VScoreBoardBase( entry: Int )(implicit p: Parameters) extends RiftModule{
  val io = IO(new Bundle{
    val enq = Flipped(Decoupled())
    val ewb = Flipped(Decoupled())
    val deq = Decoupled()
  })
}


