

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




//dw:data type aw:address width, in: input port num, out: output port num 

class MultiPortFifo[T<:Data]( dw: T, aw: Int, in: Int, out: Int ) extends Module{
  val io = IO(new Bundle{
    val enq = Vec(in, Flipped(new DecoupledIO(dw)) )
    val deq  = Vec(out, new DecoupledIO(dw) )

    val flush = Input(Bool())
  })

  override def desiredName = "MultiPortFifo_in"+in+"_out"+out

  def dp: Int = { var res = 1; for ( i <- 0 until aw ) { res = res * 2 }; return res }

  
  
  require( (in < dp && out < dp) , "dp = "+dp+", in = "+in+", out = "+ out )

  val buf = RegInit(VecInit(Seq.fill(dp)(0.U.asTypeOf(dw))))
  val buf_valid = RegInit(VecInit(Seq.fill(dp)(false.B)))

  val rd_ptr = RegInit(0.U(aw.W))
  val wr_ptr = RegInit(0.U(aw.W))


  for ( i <- 0 until in )  yield io.enq(i).ready := (buf_valid((wr_ptr + i.U)(aw-1,0)) === false.B)
  for ( i <- 0 until out ) yield io.deq(i).valid := (buf_valid((rd_ptr + i.U)(aw-1,0)) === true.B)

  def is_enq_ack(i: Int) = io.enq(i).valid & io.enq(i).ready
  def is_deq_ack(i: Int) = io.deq(i).valid & io.deq(i).ready

  def enq_cnt: UInt = {
    val port = for ( i <- 0 until in ) yield { is_enq_ack(in-1-i) === true.B }  //in-1 ~ 0
    val cnt  = for ( i <- 0 until in ) yield { (in-i).U } //in ~ 1

    return MuxCase( 0.U, port zip cnt )
  }
  
  def deq_cnt: UInt = {
    val port = for ( i <- 0 until out ) yield { is_deq_ack(out-1-i) === true.B }  //out-1 ~ 0
    val cnt  = for ( i <- 0 until out ) yield { (out-i).U } //out ~ 1

    return MuxCase( 0.U, port zip cnt )
  }






  when (io.flush) {
    for ( i <- 0 until dp ) yield 	buf_valid(i) := false.B
    rd_ptr := 0.U
    wr_ptr := 0.U
  }
  .otherwise{
    for ( i <- 0 until in; j <- 0 until out ) yield {
      val fifo_ptr_w = (wr_ptr + i.U)(aw-1,0)
      val fifo_ptr_r = (rd_ptr + j.U)(aw-1,0)

      when( is_enq_ack(i) ) {buf_valid(fifo_ptr_w) := true.B}
      when( is_deq_ack(j) ) {buf_valid(fifo_ptr_r) := false.B}


      buf(fifo_ptr_w) := Mux(is_enq_ack(i), io.enq(i).bits, buf(fifo_ptr_w))
    }



    rd_ptr := rd_ptr + deq_cnt
    wr_ptr := wr_ptr + enq_cnt
  }


  for ( i <- 0 until out ) yield {
    val fifo_ptr_r = (rd_ptr + i.U)(aw-1,0)
    io.deq(i).bits := Mux(buf_valid(fifo_ptr_r), buf(fifo_ptr_r), 0.U.asTypeOf(dw))
  }



  for ( i <- 0 until in; j <- 0 until in )
    assert( !(io.enq(i).valid === true.B && io.enq(j).valid === false.B && i.U >= j.U), "Assert Fail! in port illegal")

  for ( i <- 0 until out; j <- 0 until out )
    assert( !(io.deq(i).valid === true.B && io.deq(j).valid === false.B && i.U >= j.U), "Assert Fail! out port illegal")

  for ( i <- 0 until in ) {
    when( io.enq(i).fire ) {
      for ( j <- 0 until i ) {
        assert( io.enq(j).fire, "Assert Failed, illegal multi-port-fifo behavior" )
      }
    }
  }

  for ( i <- 0 until out ) {
    when( io.deq(i).fire ) {
      for ( j <- 0 until i ) {
        assert( io.deq(j).fire, "Assert Failed, illegal multi-port-fifo behavior" )
      }
    }
  }
}

