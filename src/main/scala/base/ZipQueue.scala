
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

import rift._
import chipsalliance.rocketchip.config.Parameters

class ZipQueue[T<:Data]( dw: T, aw: Int, in: Int, out: Int, zip: Int )(implicit p: Parameters) extends RiftModule{
  def dp: Int = { var res = 1; for ( i <- 0 until aw ) { res = res * 2 }; return res }
  val io = IO(new Bundle{
    val enq = Vec(in, Flipped(new DecoupledIO(dw)) )
    val deq  = Vec(out, new DecoupledIO(dw) )
  })

  if( out != 1 ) {
    val buff  = Reg(Vec(dp, dw))
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
  } else { //out == 1
    val mdl = Module(new MultiPortFifo( dw, aw, in, out ) with chisel3.util.experimental.InlineInstance)
    io.enq <> mdl.io.enq
    io.deq <> mdl.io.deq
    mdl.io.flush := false.B
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


}


