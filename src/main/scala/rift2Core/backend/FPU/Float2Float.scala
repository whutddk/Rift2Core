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



object FloatS2FloatD extends FPU_util{
  def apply(
    op1: UInt,
    rm: UInt,
    lp: Bool
    ): FPU_out = {

    def recFN_op1 = hardfloat.recFNFromFN(8, 24, op1)


    val widened = Mux( ~lp, Mux(isNaN32(recFN_op1), qNaN32, recFN_op1), 0.U)
    val fsgnj =   Mux( ~lp, widened(32), 0.U)

    val extends64 = Module(new hardfloat.RecFNToRecFN(11, 53, 8, 24) {
      io.in             := Mux( ~lp, recFN_op1, 0.U )
      io.roundingMode   := Mux( ~lp, rm, 0.U )
      io.detectTininess := Mux( ~lp, hardfloat.consts.tininess_afterRounding, 0.U )
    })

    val out = Wire( new FPU_out  {
      res := Cat(fsgnj, extends64.io.out)
      exceptionFlags := extends64.io.exceptionFlags
    })
    return out
  }
}

object FloatD2FloatS extends FPU_util{
  def apply(
    op1: UInt,
    rm: UInt,
    lp: Bool
    ): FPU_out = {

    def recFN_op1 = hardfloat.recFNFromFN(11, 53, op1)

    val widened = Mux( ~lp, Mux(isNaN64(recFN_op1), qNaN64, recFN_op1), 0.U )
    val fsgnj   = Mux( ~lp, widened(64), 0.U )

    val narrower32 = Module(new hardfloat.RecFNToRecFN(8, 24, 11, 53) {
      io.in             := Mux( ~lp, recFN_op1, 0.U )
      io.roundingMode   := Mux( ~lp, rm, 0.U )
      io.detectTininess := Mux( ~lp, hardfloat.consts.tininess_afterRounding, 0.U )
    })

    val out = Wire( new FPU_out {
      res := Cat(fsgnj, narrower32.io.out)
      exceptionFlags := narrower32.io.exceptionFlags
    } )
    return out
      
  }
}


object FloatSign32 {
  def apply(

    op1: UInt,
    op2: UInt,
    rm: UInt,
    lp: Bool
  ): FPU_out = {

    def recFN_op1 = hardfloat.recFNFromFN(8, 24, op1)
    def recFN_op2 = hardfloat.recFNFromFN(8, 24, op2)

    val signNum =
        Mux1H(Seq(
          (rm === "b000".U & ~lp) -> (recFN_op2),
          (rm === "b001".U & ~lp) -> (~recFN_op2),
          (rm === "b010".U & ~lp) -> (recFN_op1 ^ recFN_op2),
        ))

    val fsgnj = Mux( ~lp, Cat(signNum(32), recFN_op1(31, 0)), 0.U )

    val out = Wire( new FPU_out {
      res := fsgnj
      exceptionFlags := 0.U
    } )
    return out
  }
}


object FloatSign64 {
  def apply(

    op1: UInt,
    op2: UInt,
    rm: UInt,
    lp: Bool

  ): FPU_out = {

    def recFN_op1 = hardfloat.recFNFromFN(11, 53, op1)
    def recFN_op2 = hardfloat.recFNFromFN(11, 53, op2)

    val signNum =
        Mux1H(Seq(
          (rm === "b000".U & ~lp) -> (recFN_op2),
          (rm === "b001".U & ~lp) -> (~recFN_op2),
          (rm === "b010".U & ~lp) -> (recFN_op1 ^ recFN_op2),
        ))

    val fsgnj = Mux( ~lp, Cat(signNum(64), recFN_op1(63, 0)), 0.U)

    val out = Wire( new FPU_out  {
      res := fsgnj
      exceptionFlags := 0.U
    })
    return out
  }
}




