
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


class ZipQueue[T<:Data]( dw: T, aw: Int, in: Int, out: Int ) extends Module{
  def dp: Int = { var res = 1; for ( i <- 0 until aw ) { res = res * 2 }; return res }
  val io = IO(new Bundle{
    val enq = Vec(in, Flipped(new DecoupledIO(dw)) )
    val deq  = Vec(out, new DecoupledIO(dw) )
  })

  val wr_ptr = Wire(UInt(aw.W))


  val buff  = RegInit(VecInit(Seq.fill(dp)(0.U.asTypeOf(dw))))
  val valid = RegInit(VecInit(Seq.fill(dp)(false.B)))
  val zip_cnt = Wire(Vec(dp, UInt(aw.W)))
  val is_zip = Wire(Vec(dp,Vec(3,  false.B)))


  wr_ptr := 
    Mux(
      valid.forall((x:Bool) => (x === false.B)), 0.U,
      valid.lastIndexWhere((x:Bool) => (x === true.B)) + 1.U
  )



  for ( i <- 0 until dp ) yield { is_zip(i)(0) := false.B; is_zip(i)(1) := false.B; is_zip(i)(2) := false.B }


  for ( i <- 0 until dp ) {
    when( valid(i) === true.B ) {
      when( is_zip(i).exists((x:Bool) => (x === true.B))){
        buff(i) := 0.U.asTypeOf(dw)
        valid(i) := false.B
      } .otherwise {  } //is_zip(i) === false.B
    } .otherwise { //valid(i) === false.B
      if ( i < dp - 2 ) {
        when( valid(i+1) === true.B && is_zip(i+1)(1) === false.B && is_zip(i+1)(2) === false.B) {
          buff(i) := buff(i+1)
          valid(i) := valid(i+1)
          is_zip(i+1)(0) := true.B
        } .elsewhen( valid(i+2) === true.B && is_zip(i+2)(2) === false.B) {
          buff(i) := buff(i+2)
          valid(i) := valid(i+2)
          is_zip(i+2)(1) := true.B
        } .elsewhen( valid(i+3) === true.B ) {
          buff(i) := buff(i+3)
          valid(i) := valid(i+3)
          is_zip(i+3)(2) := true.B
        }        
      }

      for ( j <- 0 until in ) yield {  
        when( io.enq(j).fire && (i.U === wr_ptr+j.U) ) {
          buff(i) := io.enq(j).bits
          valid(i) := true.B

          assert( is_zip(i).forall((x:Bool) => (x === false.B)) )
        }
      }
    }

  }

  for ( i <- 0 until in ) yield {  
    io.enq(i).ready := valid(wr_ptr+i.U) === false.B
  }

  for ( i <- 0 until out ) yield {
    io.deq(i).valid := valid(i) & is_zip(i).forall((x:Bool) => (x === false.B))
    io.deq(i).bits  := Mux(io.deq(i).valid, buff(i), DontCare)
  }


  for ( i <- 0 until in; j <- 0 until in ) yield 
    assert( !(io.enq(i).valid === true.B && io.enq(j).valid === false.B && i.U >= j.U), "Assert Fail! enq port illegal")

  for ( i <- 0 until out; j <- 0 until out ) yield 
    assert( !(io.deq(i).valid === true.B && io.deq(j).valid === false.B && i.U >= j.U), "Assert Fail! deq port illegal")

}
