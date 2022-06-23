
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


class ZipQueue[T<:Data]( dw: T, aw: Int, in: Int, out: Int, zip: Int ) extends Module{
  def dp: Int = { var res = 1; for ( i <- 0 until aw ) { res = res * 2 }; return res }
  val io = IO(new Bundle{
    val enq = Vec(in, Flipped(new DecoupledIO(dw)) )
    val deq  = Vec(out, new DecoupledIO(dw) )
  })




  val buff  = RegInit(VecInit(Seq.fill(dp)(0.U.asTypeOf(dw))))
  val valid = RegInit(VecInit(Seq.fill(dp)(false.B)))

  val buff_next  = Wire(Vec( zip, Vec(dp, dw    )))
  val valid_next = Wire(Vec( zip, Vec(dp, Bool())))

  for ( z <- 0 until zip ) {
    val (valid_res, buff_res) = 
      if ( z == 0 ) {
        val (deq_valid, deq_buf) = PopBuff(valid, buff, io.deq)
        ZipBuff( deq_valid, deq_buf )
      } else {
        ZipBuff( valid_next(z-1), buff_next(z-1) )
      }
    buff_next(z)  := buff_res
    valid_next(z) := valid_res
  }

  buff  := buff_next(zip-1)  //enq will override this
  valid := valid_next(zip-1) //enq will override this

  val isAvailable = Wire(Vec(dp, Bool()))
  val enqIdx   = Wire(Vec(in, UInt(aw.W)))


  for ( i <- 0 until dp ) {
    isAvailable(i) := ( i until dp ).map{ j => valid_next(zip-1)(j) === false.B }.reduce(_&_)
  }

  for ( i <- 0 until in ) {
    io.enq(i).ready := (PopCount( isAvailable ) > i.U )
    if ( i == 0 ) {enqIdx(i) := isAvailable.indexWhere( (x:Bool) => (x === true.B) )}
    else { enqIdx(i) := enqIdx(i-1) + 1.U }
  }

  for ( i <- 0 until in ) { //override buff
    when( io.enq(i).fire ) {
      buff( enqIdx(i) )  := io.enq(i).bits
      valid( enqIdx(i) ) := true.B

      assert( valid_next(zip-1)( enqIdx(i) ) === false.B )
    }
  }

  for ( i <- 0 until out ) {
    io.deq(i).valid := valid(i)
    io.deq(i).bits  := buff(i)
  }



  def PopBuff( in_valid: Vec[Bool], in_buff: Vec[T], deq: Vec[DecoupledIO[T]] ): (Vec[Bool], Vec[T]) = {
    val out_valid = WireDefault(in_valid)
    val out_buff  = WireDefault(in_buff)

    for( i <- 0 until out ) {
      when( deq(i).fire ) {
        out_valid(i) := false.B
        out_buff(i)  := 0.U.asTypeOf(dw)
      }
    }

    return (out_valid, out_buff)
  }

  def ZipBuff( in_valid: Vec[Bool], in_buff: Vec[T] ): (Vec[Bool], Vec[T]) = {
    val out_valid = WireDefault(in_valid)
    val out_buff  = WireDefault(in_buff)
    for ( i <- 0 until dp -1 ) {
      when( in_valid(i) === false.B & in_valid(i+1) === true.B ) {
        out_valid(i)   := true.B
        out_valid(i+1) := false.B

        out_buff(i)    := in_buff(i+1)
        out_buff(i+1)  := 0.U.asTypeOf(dw)
      }
    }
    return (out_valid, out_buff)
  }



















  // val is_zipped  = Wire(Vec(dp,Vec(3,  Bool())))
  // val zipping = Wire(Vec(dp, UInt(2.W)))

  // val is_full = valid(dp-1) === true.B
  // wr_ptr := 
  //   Mux(
  //     is_full, 0.U,
  //     valid.lastIndexWhere((x:Bool) => (x === false.B))
  // )



  // for ( i <- 0 until dp ) yield { zipping(i) := 0.U; is_zipped(i)(0) := false.B; is_zipped(i)(1) := false.B; is_zipped(i)(2) := false.B }


  // for ( i <- 0 until dp ) {
  //   when( valid(i) === false.B ) {
  //     if ( i < dp - 3 ) {
  //       when( valid(i+1) === true.B && is_zipped(i+1)(1) === false.B && is_zipped(i+1)(2) === false.B) {
  //         is_zipped(i+1)(0) := true.B
  //         zipping(i) := 1.U
  //       } .elsewhen( valid(i+2) === true.B && is_zipped(i+2)(2) === false.B) {
  //         is_zipped(i+2)(1) := true.B
  //         zipping(i) := 2.U
  //       } .elsewhen( valid(i+3) === true.B ) {
  //         is_zipped(i+3)(2) := true.B
  //         zipping(i) := 3.U
  //       }
  //     }
  //   }
  // }



  // for ( i <- 0 until dp ) {
  //   when( is_zipped(i).exists((x:Bool) => (x === true.B))) {
  //     buff(i) := 0.U.asTypeOf(dw)
  //     valid(i) := false.B
  //     assert( valid(i) === true.B )
  //     assert( zipping(i) === 0.U )
  //   }

  //   for ( j <- 0 until in ) yield {  
  //     when( io.enq(j).fire && (i.U === wr_ptr+j.U) ) {
  //       buff(i) := io.enq(j).bits
  //       valid(i) := true.B

  //       assert( is_zipped(i).forall((x:Bool) => (x === false.B)) )
  //     }
  //   }

  //     if ( i < dp - 3 ) {
  //   when( zipping(i) === 1.U ) {

  //     buff(i) := buff(i+1)
  //     valid(i) := valid(i+1)
  //   } .elsewhen( zipping(i) === 2.U ) {
  //     buff(i) := buff(i+2)
  //     valid(i) := valid(i+2)
  //   } .elsewhen( zipping(i) === 3.U ) {
  //     buff(i) := buff(i+3)
  //     valid(i) := valid(i+3)
  //   }
  // }

  //   for ( j <- 0 until out ) yield {
  //     when( io.deq(j).fire && (i.U === j.U) ) {
  //       buff(i) := 0.U.asTypeOf(dw)
  //       valid(i) := false.B
  //     }    
  //   }

  // }


  // for ( i <- 0 until in ) yield {  
  //   io.enq(i).ready := (valid(wr_ptr+i.U) === false.B) & (wr_ptr < (dp-i).U) & ~is_full
  // }

  // for ( i <- 0 until out ) yield {
  //   io.deq(i).valid := valid(i) & is_zipped(i).forall((x:Bool) => (x === false.B))
  //   io.deq(i).bits  := Mux(io.deq(i).valid, buff(i), DontCare)
  // }


  // for ( i <- 0 until in; j <- 0 until in ) yield 
  //   assert( !(io.enq(i).valid === true.B && io.enq(j).valid === false.B && i.U >= j.U), "Assert Fail! enq port illegal")


}


