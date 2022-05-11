
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





class RAS extends IFModule {
  val io = IO(new Bundle{
    val enq = Flipped(Valid(new RASPP_Bundle))
    val deq  = new DecoupledIO(new RASPP_Bundle)

    // flush when RAS mis-predict
    val flush = Input(Bool())

  })

  def aw = log2Ceil(ras_dp)

  // val buf = RegInit(VecInit(Seq.fill(ras_dp)(0.U.asTypeOf(dw))))
  val buf = Mem( ras_dp, new RASPP_Bundle )
  val btm_ptr = RegInit(0.U((aw+1).W))
  val top_ptr = RegInit(0.U((aw+1).W))

  val rd_idx = top_ptr(aw-1, 0) - 1.U
  val wr_idx = top_ptr(aw-1, 0)

  val is_empty = (btm_ptr === top_ptr)
  val is_full  = ((btm_ptr(aw-1, 1) === top_ptr(aw-1, 1)) & (btm_ptr(aw) =/= top_ptr(aw)))


  when( io.enq.fire ) {
    // buf(wr_idx) := io.enq.bits
    buf.write(wr_idx, io.enq.bits)
  }

  when(io.flush) {
    btm_ptr := 0.U
    top_ptr := 0.U
  }
  .otherwise{
    when( io.enq.fire ) {
      when(is_full) {
        btm_ptr := btm_ptr + 1.U			
      }
      top_ptr := top_ptr + 1.U
    }
    when( io.deq.fire ) {
      top_ptr := top_ptr - 1.U
    }
  }

  io.enq.ready := true.B
  io.deq.valid  := ~is_empty
  io.deq.bits   := RegNext(buf.read(rd_idx))



  assert ( ~(io.enq.fire & io.deq.fire), "Assert Fail at RSA, RSA will never pop and push at the same times" )


}

