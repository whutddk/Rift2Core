


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

package base

import chisel3._
import chisel3.util._


class RePort[T<:Data]( dw: T, port: Int) extends Module{
  val io = IO( new Bundle{
    val enq = Vec(port, Flipped(new DecoupledIO(dw)) )
    val deq  = Vec(port, new DecoupledIO(dw) )
    } )


  for ( i <- 0 until port ) yield {
    io.deq(i).valid := false.B 
    io.deq(i).bits  := DontCare
    io.enq(i).ready  := false.B
  }

  val is_end = Wire( Vec( port, Bool()) )
  val sel = Wire( Vec( port, UInt(log2Ceil(port).W)) )
  val in_next = Wire( Vec( port, UInt(port.W) ) )


  {
    val in = VecInit(io.enq.map(x => x.valid)).asUInt
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

  for ( i <- 0 until port ) yield {
    when( ~is_end(i) ) {
      io.deq(i).bits := io.enq(sel(i)).bits
    }
    // for decouple
    io.deq(i).valid := (io.enq.count((x:DecoupledIO[T]) => (x.valid === true.B)) > i.U)
    io.enq(i).ready := (for ( j <- 0 to i ) yield { io.deq(i).ready === true.B }).reduce(_&_)

  }

}

object RePort{
  def apply[T <: Data]( enq: Vec[ReadyValidIO[T]] ): Vec[DecoupledIO[T]] = {
    val mdl = Module(new RePort( chiselTypeOf(enq(0).bits), enq.length ))
    enq <> mdl.io.enq
    return mdl.io.deq
  }
}

class ReDirect[T<:Data]( dw: T, port: Int) extends Module{
  val io = IO( new Bundle{
    val enq = Vec(port, Flipped(new DecoupledIO(dw)) )
    val deq  = Vec(port, new DecoupledIO(dw) )
    val mapper = Input(Vec(port, Bool()))
    } )

  for ( i <- 0 until port ) yield {
    io.deq(i).valid := false.B 
    io.deq(i).bits  := 0.U.asTypeOf(dw)
    io.enq(i).ready  := false.B
  }

  val sel = Wire( Vec( port, Vec( port, UInt(log2Ceil(port).W) ) ))
  for ( i <- 0 until port; j <- 0 until port ) yield sel(i)(j) := j.U
  // for ( i <- 0 until port ) {
  //   for ( j <- port-1 to i  ) {
  //     when( io.mapper(j) ) {
  //       io.deq(j) <> io.enq(i) //override
  //     }
  //   }
  // }
    for ( i <- 0 until port-1 ) yield {
        when( ~io.mapper(i) ) {
          for ( j <- i+1 until port ) { sel(i+1)(j) := sel(i)(j) - 1.U }
        }
    }

    for ( j <- 0 until port ) yield {
      when( io.mapper(j) ) { io.deq(j) <> io.enq(sel(port-1)(j)) }
    }

}

object ReDirect{
  def apply[T <: Data]( enq: Vec[DecoupledIO[T]], mapper: Vec[Bool] ): Vec[DecoupledIO[T]] = {
    require( enq.length == mapper.length )
    val mdl = Module(new ReDirect( chiselTypeOf(enq(0).bits), enq.length ))
    mdl.io.mapper := mapper
    enq <> mdl.io.enq
    return mdl.io.deq
  }
}

