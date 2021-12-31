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




object Fclass32 {
  def apply(
    op1: UInt,
    lp: Bool
    ): FPU_out = {

    def recFN_op1 = hardfloat.recFNFromFN(8, 24, op1)
      
    val out = Wire(new FPU_out {
      res := Mux( ~lp, hardfloat.classifyRecFN(8 , 24, recFN_op1), 0.U )
      exceptionFlags := 0.U
    })
    return out
  }
}

object Fclass64 {
  def apply(
    op1: UInt,
    lp: Bool
    ): FPU_out = {

    def recFN_op1 = hardfloat.recFNFromFN(11, 53, op1)

    val out = Wire(new FPU_out {
      res := Mux( ~lp, hardfloat.classifyRecFN(11, 53, recFN_op1), 0.U )
      exceptionFlags := 0.U
    })
    return out
  }
}

object Fmv_x_w {
  def apply(
    op1: UInt,
    lp: Bool
    ): FPU_out = {

    val out = Wire(new FPU_out {
      res := Mux( ~lp, Cat( Fill(32, op1(31)), op1(31,0)), 0.U )
      exceptionFlags := 0.U
    })
    return out

  }
}

object Fmv_w_x {
  def apply(
    op1: UInt,
    lp: Bool
    ): FPU_out = {

    val out = Wire(new FPU_out {
      res := Mux( ~lp, op1(31,0), 0.U )
      exceptionFlags := 0.U
    })
    return out

  }
}

object Fmv_x_d {
  def apply(
    op1: UInt,
    lp: Bool
    ): FPU_out = {

    val out = Wire(new FPU_out {
      res := Mux( ~lp, op1(63,0), 0.U )
      exceptionFlags := 0.U
    })
    return out

  }
}

object Fmv_d_x {
  def apply(
    op1: UInt,
    lp: Bool
    ): FPU_out = {

    val out = Wire(new FPU_out {
      res := Mux( ~lp, op1(63,0), 0.U )
      exceptionFlags := 0.U
    })
    return out

  }
}
