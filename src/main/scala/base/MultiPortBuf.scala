

// /*
//   Copyright (c) 2020 - 2024 Wuhan University of Technology <295054118@whut.edu.cn>

//    Licensed under the Apache License, Version 2.0 (the "License");
//    you may not use this file except in compliance with the License.
//    You may obtain a copy of the License at

//        http://www.apache.org/licenses/LICENSE-2.0

//    Unless required by applicable law or agreed to in writing, software
//    distributed under the License is distributed on an "AS IS" BASIS,
//    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//    See the License for the specific language governing permissions and
//    limitations under the License.
// */

// package base

// import chisel3._
// import chisel3.util._

// import rift2Chip._
// import chipsalliance.rocketchip.config.Parameters


// //dw:data type aw:address width, in: input port num, out: output port num 

// class MultiPortBuf[T<:Data]( dw: T, aw: Int, in: Int, out: Int)(implicit p: Parameters) extends RiftModule{
//   val io = IO(new Bundle{
//     val enq = Vec(in, Flipped(new DecoupledIO(dw)) )
//     val deq  = Vec(out, new DecoupledIO(dw) )

//     val flush = Input(Bool())
//   })

  

//   def dp: Int = { var res = 1; for ( i <- 0 until aw ) { res = res * 2 }; return res }

//   require( (in <= dp && out <= dp) , "dp = "+dp+", in = "+in+", out = "+ out )
//   override def desiredName = "MultiPortBuf_in"+in+"_out"+out



//   val buf = Reg(Vec(dp,dw))
//   val buf_valid = RegInit(VecInit(Seq.fill(dp)(false.B)))

//   // val rd_ptr = RegInit(0.U(aw.W))
//   // val wr_ptr = RegInit(0.U(aw.W))


//   for ( i <- 0 until in )  yield { io.enq(i).ready := (buf_valid((wr_ptr + i.U)(aw-1,0)) === false.B)}
//   for ( i <- 0 until out ) yield { io.deq(i).valid := (buf_valid((rd_ptr + i.U)(aw-1,0)) === true.B)}


//   def enq_cnt: UInt = {
//     val port = for ( i <- 0 until in ) yield { io.enq(in-1-i).fire === true.B }  //in-1 ~ 0
//     val cnt  = for ( i <- 0 until in ) yield { (in-i).U } //in ~ 1

//     return MuxCase( 0.U, port zip cnt )
//   }
  
//   def deq_cnt: UInt = {
//     val port = for ( i <- 0 until out ) yield { io.deq(out-1-i).fire === true.B }  //out-1 ~ 0
//     val cnt  = for ( i <- 0 until out ) yield { (out-i).U } //out ~ 1

//     return MuxCase( 0.U, port zip cnt )
//   }






//   when (io.flush) {
//     for ( i <- 0 until dp ) yield { buf_valid(i) := false.B}
//     rd_ptr := 0.U
//     wr_ptr := 0.U
//   }
//   .otherwise{
//     for ( i <- 0 until in; j <- 0 until out ) yield {
//       val fifo_ptr_w = (wr_ptr + i.U)(aw-1,0)
//       val fifo_ptr_r = (rd_ptr + j.U)(aw-1,0)

//       when( io.enq(i).fire ) {buf_valid(fifo_ptr_w) := true.B}
//       when( io.deq(j).fire ) {buf_valid(fifo_ptr_r) := false.B}


//       buf(fifo_ptr_w) := Mux(io.enq(i).fire, io.enq(i).bits, buf(fifo_ptr_w))
//     }



//     rd_ptr := rd_ptr + deq_cnt
//     wr_ptr := wr_ptr + enq_cnt
//   }

//   for ( i <- 0 until out ) yield {
//     val fifo_ptr_r = (rd_ptr + i.U)(aw-1,0)

//     io.deq(i).bits := 
//       Mux( if ( isLowPower ) {buf_valid(fifo_ptr_r)} else {true.B},
//         buf(fifo_ptr_r),
//         0.U.asTypeOf(dw))


//   }


    

// }


