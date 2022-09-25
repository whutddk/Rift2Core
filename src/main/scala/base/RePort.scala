


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

    assert ( PopCount( io.enq.map(_.fire) ) === PopCount( io.deq.map(_.fire) ), "Assert Failed at RePort! enq-fire should equal to deq-fire!"  )


  for ( i <- 0 until port ) yield {
    io.deq(i).valid := false.B 
    io.deq(i).bits  := 0.U.asTypeOf(dw)
    io.enq(i).ready  := false.B
  }

  // val sel = Wire( Vec( port, UInt(log2Ceil(port).W)) )
  // for ( i <- 0 until port ) { sel(i) := 0.U }

  // for ( i <- 0 until port ) {
  //     when( io.enq(i).valid ) {
  //       if ( i == 0 ) { io.enq(i) <> io.deq(0) }
  //       else {
  //         val sel = PopCount( for ( j <- 0 until i ) yield {io.enq(j).valid} )
  //         io.enq(i) <> io.deq(sel)
  //       }
  //     }      
  //   } 


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
    io.deq(i).valid := (io.enq.count((x:DecoupledIO[T]) => (x.fire === true.B)) > i.U)
    io.enq(i).ready := (for ( j <- 0 to i ) yield { io.deq(i).ready === true.B }).reduce(_&_)

  }

  // println("Warning, This is a Temp imp for zip Port");

  // val res_valid = Wire( Vec(port, Vec( port, Bool()) ) )
  // val res_idx   = Wire( Vec(port, Vec( port, UInt( log2Ceil(port+1).W ) )) )

  // //init
  // for( i <- 0 until port ) {
  //   res_valid(0)(i) := io.enq(i).valid
  //   res_idx(0)(i)   := i.U
  //   io.enq(i).ready := false.B
  //   io.deq(i).valid := false.B
  //   io.deq(i).bits  := DontCare
  // }



  // for( j <- 1 until port ) {
  //   val (out_valid, out_idx) = ZipBuff( res_valid(j-1), res_idx(j-1) )
  //   res_valid(j) := out_valid
  //   res_idx(j)   := out_idx
  // }

  // for( i <- 0 until port ) {
  //   val idx = res_idx(port-1)(i)
  //   when( idx =/= port.U ) {
  //     io.deq(i) <> io.enq(idx)
  //   }
  // }

  // //assert check
  // for ( i <- 1 until port ) {
  //   when( io.deq(i).fire ) {
  //     for ( j <- 0 until i ) {
  //       assert( io.deq(j).fire, "Assert Failed at RePort-deq, the deq should following the fire sequence!" )
  //     }
  //   }
  // }

  // def ZipBuff( in_valid: Vec[Bool], in_idx: Vec[UInt] ): (Vec[Bool], Vec[UInt]) = {
  //   val out_valid = WireDefault(in_valid)
  //   val out_idx  = WireDefault(in_idx)
  //   for ( i <- 0 until port-1 ) {
  //     when( in_valid(i) === false.B ) {
  //       out_valid(i)   := in_valid(i+1)
  //       out_valid(i+1) := false.B

  //       out_idx(i)    := in_idx(i+1)
  //       out_idx(i+1)  := port.U
  //     }
  //   }
  //   return (out_valid, out_idx)
  // }





}

object RePort{
  def apply[T <: Data]( enq: Vec[DecoupledIO[T]] ): Vec[DecoupledIO[T]] = {
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


class ZipPort[T<:Data]( dw: T, port: Int) extends Module{
  val io = IO( new Bundle{
    val enq = Vec(port, Flipped(new DecoupledIO(dw)) )
    val deq  = Vec(port, new DecoupledIO(dw) )
    })

  val outValid = Wire( Vec(port, Vec(port, Bool())))
  val inReady  = Wire( Vec(port, Vec(port, Bool())))
  val outBits  = Wire( Vec(port, Vec(port, dw)))

  ( 0 until port ).map{ i =>
    outBits(0)(i)  := io.enq(i).bits
    outValid(0)(i) := io.enq(i).valid
    io.enq(i).ready := inReady(0)(i)
  }


  for ( z <- 1 until port ) {
    val (valid_res, bits_res, ready_res) = 
      ZipPort( outValid(z-1), outBits(z-1), inReady(z) )

    outBits(z)  := bits_res
    outValid(z) := valid_res
    inReady(z-1) := ready_res
  }

  ( 0 until port ).map{ i =>
    io.deq(i).valid := outValid(port-1)(i)
    io.deq(i).bits  := outBits(port-1)(i)
    inReady(port-1)(i) := io.deq(i).ready
  }

  def ZipPort( inValid: Vec[Bool], inBits: Vec[T], outReady: Vec[Bool] ): (Vec[Bool], Vec[T], Vec[Bool]) = {

    val outValid = WireDefault(inValid)
    val outBits  = WireDefault(inBits)
    val inReady = WireDefault(outReady)

    for ( i <- 0 until port-1 ) {
      when( inValid(i) === false.B & inValid(i+1) === true.B ) {
        outValid(i)   := true.B
        outValid(i+1) := false.B

        outBits(i)    := inBits(i+1)
        outBits(i+1)  := 0.U.asTypeOf(dw)

        inReady(i)    := false.B
        inReady(i+1)  := outReady(i)
      }
    }
    return (outValid, outBits, inReady)
  }

}
