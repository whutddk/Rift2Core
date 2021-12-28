


/*
  Copyright (c) 2020 - 2021 Ruige Lee <wut.ruigeli@gmail.com>

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


class RePort[T<:Data]( dw: T, port: Int) extends Module{
  val io = IO( new Bundle{
    val in = Vec(port, Flipped(new DecoupledIO(dw)) )
    val out  = Vec(port, new DecoupledIO(dw) )
    } )


  for ( i <- 0 until port ) yield {
    io.out(i).valid := false.B 
    io.out(i).bits  := DontCare
    io.in(i).ready  := false.B
  }

  val is_end = Wire( Vec( port, Bool()) )
  val sel = Wire( Vec( port, UInt(log2Ceil(port).W)) )
  val in_next = Wire( Vec( port, UInt(port.W) ) )


  {
    val in = VecInit(io.in.map(x => x.valid)).asUInt
    is_end(0) := in === 0.U
    sel(0) := VecInit(in.asBools).indexWhere( (x:Bool) => (x === true.B) )
    in_next(0) := in & ~UIntToOH(sel(0))
  }

  for ( i <- 1 until port ) {
    val in = in_next(i-1)
    is_end(i) := in === 0.U
    sel(i) := VecInit(in.asBools).indexWhere( (x:Bool) => (x === true.B) )
    in_next(i) := in & ~UIntToOH(sel(i))
  }

  for ( out <- 0 until port ) yield {
    when( ~is_end(out) ) {
      io.out(out) <> io.in(sel(out))
    }
  }

}

object RePort{
  def apply[T <: Data]( in: Vec[ReadyValidIO[T]] ): Vec[DecoupledIO[T]] = {
    val mdl = Module(new RePort( chiselTypeOf(in(0).bits), in.length ))
    in <> mdl.io.in
    return mdl.io.out
  }

}
