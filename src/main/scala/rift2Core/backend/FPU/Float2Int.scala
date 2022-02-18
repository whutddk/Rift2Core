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

// object FloatS2Int32 {
//   def apply(
//     op1: UInt,
//     rm: UInt,
//     is_usi: Bool,
//     lp: Bool
//     ): FPU_out = {

//     def recFN_op1 = hardfloat.recFNFromFN(8, 24, op1)

//     val f2i32 = Module(new hardfloat.RecFNToIN(8, 24, 32) {
//       io.in           := Mux( ~lp, recFN_op1, 0.U )
//       io.roundingMode := Mux( ~lp, rm, 0.U )
//       io.signedOut    := Mux( ~lp, ~is_usi, 0.U )
//     })

//     val out = Wire(new FPU_out {
//       res := Cat( Mux( is_usi, 0.U(32.W), Fill(32, f2i32.io.out(31))), f2i32.io.out )
//       exceptionFlags := f2i32.io.intExceptionFlags        
//     })

//     return out
//   }
// }

// object FloatS2Int64 {
//   def apply(
//     op1: UInt,
//     rm: UInt,
//     is_usi: Bool,
//     lp: Bool
//     ): FPU_out = {

//     def recFN_op1 = hardfloat.recFNFromFN(8, 24, op1)

//     val f2i64 = Module(new hardfloat.RecFNToIN(8, 24, 64) {
//       io.in           := Mux( ~lp, recFN_op1, 0.U )
//       io.roundingMode := Mux( ~lp, rm, 0.U )
//       io.signedOut    := Mux( ~lp, ~is_usi, 0.U )
//     })

//     val out = Wire(new FPU_out {
//       res := f2i64.io.out
//       exceptionFlags := f2i64.io.intExceptionFlags      
//     })
      
//     return out
//   }
// }

// object FloatD2Int32 {
//   def apply(
//     op1: UInt,
//     rm: UInt,
//     is_usi: Bool,
//     lp: Bool
//     ): FPU_out = {

//     def recFN_op1 = hardfloat.recFNFromFN(11, 53, op1)

//     val d2i32 = Module(new hardfloat.RecFNToIN(11, 53, 32) {
//       io.in           := Mux( ~lp, recFN_op1, 0.U )
//       io.roundingMode := Mux( ~lp, rm, 0.U )
//       io.signedOut    := Mux( ~lp, ~is_usi, 0.U )
//     })

//     val out = Wire(new FPU_out {
//       res := Cat( Mux( is_usi, 0.U(32.W), Fill(32, d2i32.io.out(31))), d2i32.io.out )
//       exceptionFlags := d2i32.io.intExceptionFlags      
//     })
//     return out
//   }
// }

// object FloatD2Int64 {
//   def apply(
//     op1: UInt,
//     rm: UInt,
//     is_usi: Bool,
//     lp: Bool
//     ): FPU_out = {

//     def recFN_op1 = hardfloat.recFNFromFN(11, 53, op1)

//     val d2i64 = Module(new hardfloat.RecFNToIN(11, 53, 64) {
//       io.in           := Mux( ~lp, recFN_op1, 0.U )
//       io.roundingMode := Mux( ~lp, rm,        0.U )
//       io.signedOut    := Mux( ~lp, ~is_usi,   0.U )      
//     })

//     val out = Wire(new FPU_out {
//       res := d2i64.io.out
//       exceptionFlags := d2i64.io.intExceptionFlags
//     })

//     return out

//   }
// }



class FPToInt() extends Module{
  val io = new Bundle {
    val in = Input(new Fpu_iss_info)
    val out = Output(new Bundle{
      val lt = Bool()
      val toint = UInt(64.W)
      val exc = UInt(5.W)
      })
    }


  val in = io.in

  val dcmp = Module(new hardfloat.CompareRecFN(maxExpWidth = 11, maxSigWidth = 53)) {
    io.a := in.param.op1
    io.b := in.param.op1
    io.signaling := !in.param.rm(1)
  }

  val tag = in.fun.FtypeTagOut
  val store = 
    Mux1H(Seq(
      (in.fun.FtypeTagOut === 0.U) -> Fill(2, ieee(in.param.op1)(31, 0) ),
      (in.fun.FtypeTagOut === 1.U) -> Fill(1, ieee(in.param.op1)(63, 0) ),
    ))
  val toint = Wire(UInt(64.W))
  toint := store
  // io.out.bits.store := store


  io.out.exc := 0.U

  when ( in.fun.is_fun_class ) {
    val classify_out =
      Mux1H( Seq(
        (in.fun.FtypeTagOut === 0.U) -> FType.S.classify(FType.D.unsafeConvert(in.param.op1, FType.S)),
        (in.fun.FtypeTagOut === 1.U) -> FType.D.classify( in.param.op1 ),
      ))

    toint := classify_out | (store >> 32 << 32)

  }

  when ( in.fun.is_fun_cmp | in.fun.is_fun_maxMin ) { // feq/flt/fle, fcvt
    toint := (~in.param.rm & Cat(dcmp.io.lt, dcmp.io.eq)).orR | (store >> 32 << 32)
    io.out.bits.exc := dcmp.io.exceptionFlags
  }

  when ( in.fun.is_fun_fcvtX ) { // fcvt
    val conv = Module(new hardfloat.RecFNToIN( 11, 53, 64)) {
      io.in := in.param.op1
      io.roundingMode := in.param.rm
      io.signedOut := ~in.fun.is_usi      
    }

    toint := conv.io.out
    io.out.bits.exc := Cat(conv.io.intExceptionFlags(2, 1).orR, 0.U(3.W), conv.io.intExceptionFlags(0))

    when (in.fun.XtypeTagOut === 0) {
      val narrow = Module(new hardfloat.RecFNToIN( 11, 53, 32)) {
        io.in := in.param.op1
        io.roundingMode := in.param.rm
        io.signedOut := ~in.fun.is_usi
      }


      val excSign = in.param.op1(64) && !FType.D.isNaN(in.param.op1)
      val excOut = Cat(conv.io.signedOut === excSign, Fill(31, !excSign))
      val invalid = conv.io.intExceptionFlags(2) || narrow.io.intExceptionFlags(1)
      when (invalid) { toint := Cat(conv.io.out >> 32, excOut) }
      io.out.bits.exc := Cat(invalid, 0.U(3.W), !invalid && conv.io.intExceptionFlags(0))
    } 
    .otherwise {
      assert(in.fun.XtypeTagOut === 1)
      val narrow = Module(new hardfloat.RecFNToIN( 11, 53, 64)) {
        io.in := in.param.op1
        io.roundingMode := in.param.rm
        io.signedOut := ~in.fun.is_usi        
      }


      val excSign = in.param.op1(64) && !FType.D.isNaN(in.param.op1)
      val excOut = Cat(conv.io.signedOut === excSign, Fill(63, !excSign))
      val invalid = conv.io.intExceptionFlags(2) || narrow.io.intExceptionFlags(1)
      when (invalid) { toint := Cat(conv.io.out >> 64, excOut) }
      io.out.bits.exc := Cat(invalid, 0.U(3.W), !invalid && conv.io.intExceptionFlags(0))
    }

  }

  io.out.bits.lt := dcmp.io.lt || (dcmp.io.a.asSInt < 0.S && dcmp.io.b.asSInt >= 0.S)
  io.out.bits.toint := 
    Mux1H( Seq(
      (in.fun.XtypeTagOut === 0.U) -> sextXTo(toint(31,0),64),
      (in.fun.XtypeTagOut === 1.U) -> toint(63,0),
    ))
}
