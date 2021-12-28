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

object FloatS2Int32 {
  def apply(
    op1: UInt,
    rm: UInt,
    is_usi: Bool,
    lp: Bool
    ): FPU_out = {

    def recFN_op1 = hardfloat.recFNFromFN(8, 24, op1)

    val f2i32 = Module(new hardfloat.RecFNToIN(8, 24, 32) {
      io.in           := Mux( ~lp, recFN_op1, 0.U )
      io.roundingMode := Mux( ~lp, rm, 0.U )
      io.signedOut    := Mux( ~lp, ~is_usi, 0.U )
    })

    val out = Wire(new FPU_out {
      res := Cat( Mux( is_usi, 0.U(32.W), Fill(32, f2i32.io.out(31))), f2i32.io.out )
      exceptionFlags := f2i32.io.intExceptionFlags        
    })

    return out
  }
}

object FloatS2Int64 {
  def apply(
    op1: UInt,
    rm: UInt,
    is_usi: Bool,
    lp: Bool
    ): FPU_out = {

    def recFN_op1 = hardfloat.recFNFromFN(8, 24, op1)

    val f2i64 = Module(new hardfloat.RecFNToIN(8, 24, 64) {
      io.in           := Mux( ~lp, recFN_op1, 0.U )
      io.roundingMode := Mux( ~lp, rm, 0.U )
      io.signedOut    := Mux( ~lp, ~is_usi, 0.U )
    })

    val out = Wire(new FPU_out {
      res := f2i64.io.out
      exceptionFlags := f2i64.io.intExceptionFlags      
    })
      
    return out
  }
}

object FloatD2Int32 {
  def apply(
    op1: UInt,
    rm: UInt,
    is_usi: Bool,
    lp: Bool
    ): FPU_out = {

    def recFN_op1 = hardfloat.recFNFromFN(11, 53, op1)

    val d2i32 = Module(new hardfloat.RecFNToIN(11, 53, 32) {
      io.in           := Mux( ~lp, recFN_op1, 0.U )
      io.roundingMode := Mux( ~lp, rm, 0.U )
      io.signedOut    := Mux( ~lp, ~is_usi, 0.U )
    })

    val out = Wire(new FPU_out {
      res := Cat( Mux( is_usi, 0.U(32.W), Fill(32, d2i32.io.out(31))), d2i32.io.out )
      exceptionFlags := d2i32.io.intExceptionFlags      
    })
    return out
  }
}

object FloatD2Int64 {
  def apply(
    op1: UInt,
    rm: UInt,
    is_usi: Bool,
    lp: Bool
    ): FPU_out = {

    def recFN_op1 = hardfloat.recFNFromFN(11, 53, op1)

    val d2i64 = Module(new hardfloat.RecFNToIN(11, 53, 64) {
      io.in           := Mux( ~lp, recFN_op1, 0.U )
      io.roundingMode := Mux( ~lp, rm,        0.U )
      io.signedOut    := Mux( ~lp, ~is_usi,   0.U )      
    })

    val out = Wire(new FPU_out {
      res := d2i64.io.out
      exceptionFlags := d2i64.io.intExceptionFlags
    })

    return out

  }
}


