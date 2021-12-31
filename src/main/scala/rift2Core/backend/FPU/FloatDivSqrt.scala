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

package rift2Core.backend.fpu

import chisel3._
import chisel3.util._
import rift2Core.define._
import rift2Core.backend._




object FloatDivSqrt32 {
  def apply(
    is_sqrt: Bool,
    is_div: Bool,

    op1: UInt,
    op2: UInt,
    rm: UInt
  ): DecoupledIO[FPU_out] = {

    def recFN_op1 = hardfloat.recFNFromFN(8, 24, op1)
    def recFN_op2 = hardfloat.recFNFromFN(8, 24, op2)

    def inValid = is_sqrt | is_div
    assert( PopCount( Seq(is_sqrt,is_div)) <= 1.U)

    val divSqrt_fifo = Module( new Queue(new FPU_out,1, false, true))

    val divSqrt32 = Module(new hardfloat.DivSqrtRecFN_small(8, 24, 0) {
      io.inValid := inValid & divSqrt_fifo.io.enq.ready
      io.sqrtOp := is_sqrt
      io.a := recFN_op1
      io.b := recFN_op2
      io.roundingMode := rm
      io.detectTininess := hardfloat.consts.tininess_afterRounding
    })

      divSqrt_fifo.io.enq.valid               := divSqrt32.io.outValid_div | divSqrt32.io.outValid_sqrt
      divSqrt_fifo.io.enq.bits.res            := divSqrt32.io.out
      divSqrt_fifo.io.enq.bits.exceptionFlags := divSqrt32.io.exceptionFlags          
    
    return divSqrt_fifo.io.deq

  }
}


object FloatDivSqrt64 {
  def apply(
    is_sqrt: Bool,
    is_div: Bool,
    
    op1: UInt,
    op2: UInt,
    rm: UInt
  ): DecoupledIO[FPU_out] = {

    def recFN_op1 = hardfloat.recFNFromFN(11, 53, op1)
    def recFN_op2 = hardfloat.recFNFromFN(11, 53, op2)

    def inValid = is_sqrt | is_div
    assert( PopCount( Seq(is_sqrt,is_div)) <= 1.U )

    val divSqrt_fifo = Module( new Queue(new FPU_out,1, false, true) )

    val divSqrt64 = Module(new hardfloat.DivSqrtRecFN_small(11, 53, 0) {
      io.inValid := inValid & divSqrt_fifo.io.enq.ready
      io.sqrtOp := is_sqrt
      io.a := recFN_op1
      io.b := recFN_op2
      io.roundingMode := rm
      io.detectTininess := hardfloat.consts.tininess_afterRounding
    })
  


    divSqrt_fifo.io.enq.valid               := divSqrt64.io.outValid_div | divSqrt64.io.outValid_sqrt
    divSqrt_fifo.io.enq.bits.res            := divSqrt64.io.out
    divSqrt_fifo.io.enq.bits.exceptionFlags := divSqrt64.io.exceptionFlags    

    return divSqrt_fifo.io.deq
  }
}



