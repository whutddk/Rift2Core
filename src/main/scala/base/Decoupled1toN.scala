




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

package base

import chisel3._
import chisel3.util._


class Decoupled1toN[T<:Data]( dw: T, out: Int ) extends Module{
  val io = IO(new Bundle{
    val enq = Flipped(new DecoupledIO(dw))
    val deq  = Vec(out, new DecoupledIO(dw) )
  })

  io.enq.ready := io.deq.map(x => x.ready).reduce(_&_)

  for ( i <- 0 until out ) yield {
    io.deq(i).valid := io.enq.fire
    io.deq(i).bits  := io.enq.bits
  }
}

object Decoupled1toN {
  def apply[T <: Data](deq: Seq[DecoupledIO[T]] ): DecoupledIO[T] = {
    val mdl = Module(new Decoupled1toN( chiselTypeOf(deq(0).bits), deq.length ))
    mdl.io.deq <> deq

    mdl.io.enq
  }
}


