/*
  Copyright (c) 2020 - 2021 Ruige Lee <wut.ruigeli@gmail.com>

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


class Cmp_out extends Bundle {
    val lt = Bool()
    val eq = Bool()
    val gt = Bool()
    val exceptionFlags = Wire(UInt(5.W))
}

    


object FloatSCmp {
  def apply(
    recFN_op1: UInt,
    recFN_op2: UInt,
    rm: UInt,
    lp: Bool
    ): Cmp_out = {

    val fcmp = Module(new hardfloat.CompareRecFN(8, 24) {
      io.a         := Mux( ~lp, recFN_op1, 0.U )
      io.b         := Mux( ~lp, recFN_op2, 0.U )
      io.signaling := Mux( ~lp, !rm(1),    0.U )
    })

    val out = Wire(new Cmp_out {
      lt             := fcmp.io.lt
      eq             := fcmp.io.eq
      gt             := fcmp.io.gt
      exceptionFlags := fcmp.io.exceptionFlags
    })
    return out
  }
}

object FloatDCmp {
  def apply(
    recFN_op1: UInt,
    recFN_op2: UInt,
    rm: UInt,
    lp: Bool
    ): Cmp_out = {

    val dcmp = Module(new hardfloat.CompareRecFN(11, 53) {
      io.a         := Mux( ~lp, recFN_op1, 0.U )
      io.b         := Mux( ~lp, recFN_op2, 0.U )
      io.signaling := Mux( ~lp, !rm(1),    0.U )
    })

    val out = Wire(new Cmp_out {
      lt             := dcmp.io.lt
      eq             := dcmp.io.eq
      gt             := dcmp.io.gt
      exceptionFlags := dcmp.io.exceptionFlags
    })
    return out
  }
}

object FloatSCmp_Res extends FPU_util{
  def apply(
    eq: Bool,
    lt: Bool,
    gt: Bool,
    min: Bool,
    max: Bool,
    op1: UInt,
    op2: UInt,
    rm: UInt,
    lp: Bool
  ): FPU_out = {

    assert( PopCount(Seq(eq,lt,gt,min,max)) < 1.U )

    def recFN_op1 = hardfloat.recFNFromFN(8, 24, op1)
    def recFN_op2 = hardfloat.recFNFromFN(8, 24, op2)

    val cmp_out = FloatSCmp( recFN_op1, recFN_op2, rm, lp )


    val isnan1 = isNaN32(recFN_op1)
    val isnan2 = isNaN32(recFN_op2)
    val isInvalid = isSNaN32(recFN_op1) | isSNaN32(recFN_op2)
    val isNaNOut = isnan1 & isnan2

    val out = Wire( new FPU_out )
      out.res := 
        Mux1H(Seq(
          eq -> Mux( cmp_out.eq, 1.U(64.W), 0.U(64.W) ),
          lt -> Mux( cmp_out.lt, 1.U(64.W), 0.U(64.W) ),
          gt -> Mux( cmp_out.gt, 1.U(64.W), 0.U(64.W) ),
          min -> Mux( isNaNOut, qNaN32, Mux( isnan2 | (cmp_out.lt & ~isnan1), recFN_op2, recFN_op1 ) ),
          max -> Mux( isNaNOut, qNaN32, Mux( isnan2 | (cmp_out.lt & ~isnan1), recFN_op1, recFN_op2 ) ),
        ))
      out.exceptionFlags := cmp_out.exceptionFlags | (isInvalid << 4.U)

    return out
  }
}

object FloatDCmp_Res extends FPU_util{
  def apply(
    eq: Bool,
    lt: Bool,
    gt: Bool,
    min: Bool,
    max: Bool,
    op1: UInt,
    op2: UInt,
    rm: UInt,
    lp: Bool
  ): FPU_out = {

    assert( PopCount(Seq(eq,lt,gt,min,max)) < 1.U )

    def recFN_op1 = hardfloat.recFNFromFN(11, 53, op1)
    def recFN_op2 = hardfloat.recFNFromFN(11, 53, op2)

    val cmp_out = FloatDCmp( recFN_op1, recFN_op2, rm, lp )

    val isnan1 = isNaN64(recFN_op1)
    val isnan2 = isNaN64(recFN_op2)
    val isInvalid = isSNaN64(recFN_op1) | isSNaN64(recFN_op2)
    val isNaNOut = isnan1 & isnan2

    val out = Wire( new FPU_out)
      out.res := 
        Mux1H(Seq(
          eq -> Mux( cmp_out.eq, 1.U(64.W), 0.U(64.W) ),
          lt -> Mux( cmp_out.lt, 1.U(64.W), 0.U(64.W) ),
          gt -> Mux( cmp_out.gt, 1.U(64.W), 0.U(64.W) ),
          min -> Mux( isNaNOut, qNaN32, Mux( isnan2 | (cmp_out.lt & ~isnan1), recFN_op2, recFN_op1 ) ),
          max -> Mux( isNaNOut, qNaN32, Mux( isnan2 | (cmp_out.lt & ~isnan1), recFN_op1, recFN_op2 ) ),
        ))
      out.exceptionFlags := cmp_out.exceptionFlags | (isInvalid << 4.U)

    return out
  }
}




