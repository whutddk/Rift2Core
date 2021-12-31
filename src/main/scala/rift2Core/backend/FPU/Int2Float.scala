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

object Int322FloatS {
  def apply(
    op1: UInt,
    rm: UInt,
    is_usi: Bool,
    lp: Bool
    ): FPU_out = {
    
    val i32Tf = Module(new hardfloat.INToRecFN(32, 8 , 24) {
      io.signedIn       := Mux( ~lp, ~is_usi, 0.U )
      io.in             := Mux( ~lp, op1(31,0), 0.U )
      io.roundingMode   := Mux( ~lp, rm, 0.U )
      io.detectTininess := Mux( ~lp, hardfloat.consts.tininess_afterRounding, 0.U )
    })

    val out = Wire( new FPU_out  {
      res := i32Tf.io.out
      exceptionFlags := i32Tf.io.exceptionFlags
    })

    return out

  }
}

object Int642FloatS {
  def apply(
    op1: UInt,
    rm: UInt,
    is_usi: Bool,
    lp: Bool
    ): FPU_out = {

    val i64Tf = Module(new hardfloat.INToRecFN(64, 8 , 24) {
      io.signedIn       := Mux( !lp, ~is_usi, 0.U )
      io.in             := Mux( !lp, op1, 0.U )
      io.roundingMode   := Mux( !lp, rm, 0.U )
      io.detectTininess := Mux( !lp, hardfloat.consts.tininess_afterRounding, 0.U )
    })

    val out = Wire( new FPU_out {
      res := i64Tf.io.out
      exceptionFlags := i64Tf.io.exceptionFlags 
    })
    return out
  }
}
    
object Int322FloatD {
  def apply(
    op1: UInt,
    rm: UInt,
    is_usi: Bool,
    lp: Bool
    ): FPU_out = {

    val i32Td = Module(new hardfloat.INToRecFN(32, 11, 53) {
      io.signedIn       := Mux( ~lp, ~is_usi, 0.U )
      io.in             := Mux( ~lp, op1(31,0), 0.U )
      io.roundingMode   := Mux( ~lp, rm, 0.U )
      io.detectTininess := Mux( ~lp, hardfloat.consts.tininess_afterRounding, 0.U )
    })

    val out = Wire( new FPU_out  {
      res := i32Td.io.out
      exceptionFlags := i32Td.io.exceptionFlags
    })
    return out
  }
}


object Int642FloatD {
  def apply(
    op1: UInt,
    rm: UInt,
    is_usi: Bool,
    lp: Bool
    ): FPU_out = {

    val i64Td = Module(new hardfloat.INToRecFN(64, 11, 53) {
      io.signedIn       := Mux( ~lp, ~is_usi, 0.U )
      io.in             := Mux( ~lp, op1, 0.U )
      io.roundingMode   := Mux( ~lp, rm, 0.U )
      io.detectTininess := Mux( ~lp, hardfloat.consts.tininess_afterRounding, 0.U )
    })

    val out = Wire( new FPU_out  {
      res := i64Td.io.out
      exceptionFlags := i64Td.io.exceptionFlags
    })
    return out

  }
}







