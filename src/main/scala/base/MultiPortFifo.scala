

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

class MultiPortFifo[T<:Data]( dw: T, aw: Int, in: Int, out: Int, flow: Boolean = false ) extends Module{
  val io = IO(new Bundle{
    val enq = Vec(in, Flipped(new DecoupledIO(dw)) )
    val deq  = Vec(out, new DecoupledIO(dw) )

    val flush = Input(Bool())
  })

  

  def dp: Int = { var res = 1; for ( i <- 0 until aw ) { res = res * 2 }; return res }

  require( (in < dp && out < dp) , "dp = "+dp+", in = "+in+", out = "+ out )
  override def desiredName = "MultiPortFifo_in"+in+"_out"+out

  if ( flow == false ) {



    val buf = RegInit(VecInit(Seq.fill(dp)(0.U.asTypeOf(dw))))
    val buf_valid = RegInit(VecInit(Seq.fill(dp)(false.B)))

    val rd_ptr = RegInit(0.U(aw.W))
    val wr_ptr = RegInit(0.U(aw.W))


    for ( i <- 0 until in )  yield { io.enq(i).ready := (buf_valid((wr_ptr + i.U)(aw-1,0)) === false.B)}
    for ( i <- 0 until out ) yield { io.deq(i).valid := (buf_valid((rd_ptr + i.U)(aw-1,0)) === true.B)}


    def enq_cnt: UInt = {
      val port = for ( i <- 0 until in ) yield { io.enq(in-1-i).fire === true.B }  //in-1 ~ 0
      val cnt  = for ( i <- 0 until in ) yield { (in-i).U } //in ~ 1

      return MuxCase( 0.U, port zip cnt )
    }
    
    def deq_cnt: UInt = {
      val port = for ( i <- 0 until out ) yield { io.deq(out-1-i).fire === true.B }  //out-1 ~ 0
      val cnt  = for ( i <- 0 until out ) yield { (out-i).U } //out ~ 1

      return MuxCase( 0.U, port zip cnt )
    }






    when (io.flush) {
      for ( i <- 0 until dp ) yield { buf_valid(i) := false.B}
      rd_ptr := 0.U
      wr_ptr := 0.U
    }
    .otherwise{
      for ( i <- 0 until in; j <- 0 until out ) yield {
        val fifo_ptr_w = (wr_ptr + i.U)(aw-1,0)
        val fifo_ptr_r = (rd_ptr + j.U)(aw-1,0)

        when( io.enq(i).fire ) {buf_valid(fifo_ptr_w) := true.B}
        when( io.deq(j).fire ) {buf_valid(fifo_ptr_r) := false.B}


        buf(fifo_ptr_w) := Mux(io.enq(i).fire, io.enq(i).bits, buf(fifo_ptr_w))
      }



      rd_ptr := rd_ptr + deq_cnt
      wr_ptr := wr_ptr + enq_cnt
    }

    for ( i <- 0 until out ) yield {
      val fifo_ptr_r = (rd_ptr + i.U)(aw-1,0)
      io.deq(i).bits := Mux(buf_valid(fifo_ptr_r), buf(fifo_ptr_r), 0.U.asTypeOf(dw))
    }


  } else { //flow == true

    val fifo = Module(new MultiPortFifo( dw, aw, in, out, flow = false ))
    val rePortIn = Module(new RePort( dw, port = in))
    val zipPort = Module(new ZipPort( dw, port = in + out))

    rePortIn.io.enq <> io.enq
    rePortIn.io.deq <> fifo.io.enq

    for ( i <- 0 until out  ) {
      zipPort.io.enq(i) <> fifo.io.deq(i)
    }

    for ( i <- out until out+in ) {
      zipPort.io.enq(i).valid := io.enq(i-out).valid
      zipPort.io.enq(i).bits  := io.enq(i-out).bits
    }


    for ( i <- 0 until out ) {
      zipPort.io.deq(i) <> io.deq(i)
    }
    for ( i <- out until out+in  ) {
      zipPort.io.deq(i).ready := false.B
    }

    fifo.io.flush := io.flush

    for ( i <- 0 until in ) {
      when( zipPort.io.enq(out+i).fire ) {
        rePortIn.io.enq(i).valid := false.B
        rePortIn.io.enq(i).bits  := 0.U.asTypeOf(dw)
      }
    }










    // for ( i <- 0 until out ) {

    //   io.deq(i).valid := false.B
    //   io.deq(i).bits  := 0.U.asTypeOf(dw)
    //   mdl.io.deq(i).ready := false.B

    // }

    // for ( i <- 0 until in ) {
    //   mdl.io.enq(i).valid := false.B
    //   mdl.io.enq(i).bits  := 0.U.asTypeOf(dw)
    //   io.enq(i).ready     := mdl.io.enq(i).ready
    // }

    // for ( i <- out-1 to 0 by -1 ) {
    //   when(mdl.io.deq(i).valid) {
    //     mdl.io.deq(i) <> io.deq(i)
    //   } .otherwise{
    //     for ( j <- (in-1 min out-1) to i by -1   ) { //may be override many times
    //       io.deq(j).valid := io.enq(j-i).valid
    //       io.deq(j).bits  := io.enq(j-i).bits

    //       when( io.enq(j-i).fire ) {
    //         when( io.deq(j).fire ){}
    //         .otherwise{
    //           for ( k <- (j-i) until in ) {
    //             mdl.io.enq(k-j+i).valid := io.enq(j-i).valid
    //             mdl.io.enq(k-j+i).bits  := io.enq(j-i).bits
    //           }
    //         }
    //       }
    //     }
    //   }
    // }

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


