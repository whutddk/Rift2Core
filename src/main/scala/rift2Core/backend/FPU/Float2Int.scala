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
import base._


class FPToInt() extends Module with HasFPUParameters{
  val io = IO(new Bundle {
    val in = Input(new Fpu_iss_info)
    val out = Output(new Bundle{
      val lt = Bool()
      val eq = Bool()
      val toint = UInt(64.W)
      val exc = UInt(5.W)
      })
  })



  val op1 = unbox(io.in.param.dat.op1, io.in.fun.FtypeTagIn, None)
  val op2 = unbox(io.in.param.dat.op2, io.in.fun.FtypeTagIn, None)
 

  val dcmp =  {
    val mdl = Module(new hardfloat.CompareRecFN(expWidth = 11, sigWidth = 53))
    mdl.io.a := op1
    mdl.io.b := op2
    mdl.io.signaling := ~io.in.param.rm(1)
    mdl
  }

  val store = 
    Mux1H(Seq(
      (io.in.fun.FtypeTagIn === 0.U) -> Fill(2, FType.S.ieee(op1)(31, 0) ),
      (io.in.fun.FtypeTagIn === 1.U) -> Fill(1, FType.D.ieee(op1)(63, 0) ),
    ))
  val toint = Wire(UInt(64.W))
  toint := store

  io.out.exc := 0.U

  when ( io.in.fun.is_fun_class ) {
    val classify_out =
      Mux1H( Seq(
        (io.in.fun.FtypeTagIn === 0.U) -> FType.S.classify(FType.D.unsafeConvert(op1, FType.S)),
        (io.in.fun.FtypeTagIn === 1.U) -> FType.D.classify( op1 ),
      ))

    toint := classify_out | (store >> 32 << 32)

  }

  when ( io.in.fun.is_fun_cmp | io.in.fun.is_fun_maxMin ) { // feq/flt/fle, fcvt
    toint := (~io.in.param.rm & Cat(dcmp.io.lt, dcmp.io.eq)).orR | (store >> 32 << 32)
    io.out.exc := dcmp.io.exceptionFlags
  }

  when ( io.in.fun.is_fun_fcvtX ) { // fcvt
    val conv =  {
      val mdl = Module(new hardfloat.RecFNToIN( 11, 53, 64))
      mdl.io.in := op1
      mdl.io.roundingMode := io.in.param.rm
      mdl.io.signedOut := ~io.in.fun.is_usi
      mdl
    }

    toint := conv.io.out
    io.out.exc := Cat(conv.io.intExceptionFlags(2, 1).orR, 0.U(3.W), conv.io.intExceptionFlags(0))

    when (io.in.fun.XtypeTagOut === 0.U) {
      val narrow = {
        val mdl = Module(new hardfloat.RecFNToIN( 11, 53, 32)) 
        mdl.io.in := op1
        mdl.io.roundingMode := io.in.param.rm
        mdl.io.signedOut := ~io.in.fun.is_usi
        mdl
      }


      val excSign = op1(64) && !FType.D.isNaN(op1)
      val excOut = Cat(conv.io.signedOut === excSign, Fill(31, !excSign))
      val invalid = conv.io.intExceptionFlags(2) || narrow.io.intExceptionFlags(1)
      when (invalid) { toint := Cat(conv.io.out >> 32, excOut) }
      io.out.exc := Cat(invalid, 0.U(3.W), !invalid && conv.io.intExceptionFlags(0))
    } 
    .otherwise {
      assert(io.in.fun.XtypeTagOut === 1.U)
      val narrow = {
        val mdl = Module(new hardfloat.RecFNToIN( 11, 53, 64)) 
        mdl.io.in := op1
        mdl.io.roundingMode := io.in.param.rm
        mdl.io.signedOut := ~io.in.fun.is_usi
        mdl
      }


      val excSign = op1(64) && !FType.D.isNaN(op1)
      val excOut = Cat(conv.io.signedOut === excSign, Fill(63, !excSign))
      val invalid = conv.io.intExceptionFlags(2) || narrow.io.intExceptionFlags(1)
      when (invalid) { toint := Cat(conv.io.out >> 64, excOut) }
      io.out.exc := Cat(invalid, 0.U(3.W), !invalid && conv.io.intExceptionFlags(0))
    }

  }

  when ( io.in.fun.is_fun_fmvX ) {
    toint := ieee(io.in.param.dat.op1, t = FType.D)
    io.out.exc := 0.U
  }

  io.out.eq := dcmp.io.eq
  io.out.lt := dcmp.io.lt || (dcmp.io.a.asSInt < 0.S && dcmp.io.b.asSInt >= 0.S)
  io.out.toint := 
    Mux1H( Seq(
      (io.in.fun.XtypeTagOut === 0.U) -> sextXTo(toint(31,0),64),
      (io.in.fun.XtypeTagOut === 1.U) -> toint(63,0),
    ))
}
