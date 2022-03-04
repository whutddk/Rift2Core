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
import chisel3.experimental.dataview._


class FPToInt(latency: Int) extends Module with HasFPUParameters{
  val io = IO(new Bundle {
    val in = Flipped(ValidIO(new Fpu_iss_info))
    val frm = Input(UInt(3.W))
    val out = ValidIO(new Xres_Info)

  })


  
  val out = Wire(new Xres_Info)
  out.viewAsSupertype(new Fpu_iss_info) := io.in.bits

  val op1 = unbox(io.in.bits.param.dat.op1, io.in.bits.fun.FtypeTagIn, None)
  val op2 = unbox(io.in.bits.param.dat.op2, io.in.bits.fun.FtypeTagIn, None)
 

  val store = 
    Mux1H(Seq(
      (io.in.bits.fun.FtypeTagIn === 0.U) -> Fill(2, FType.S.ieee(op1)(31, 0) ),
      (io.in.bits.fun.FtypeTagIn === 1.U) -> Fill(1, FType.D.ieee(op1)(63, 0) ),
    ))
  val toint = Wire(UInt(64.W))
  toint := store

  out.exc := 0.U

  when ( io.in.bits.fun.is_fun_class ) {
    val classify_out =
      Mux1H( Seq(
        (io.in.bits.fun.FtypeTagIn === 0.U) -> FType.S.classify(FType.D.unsafeConvert(op1, FType.S)),
        (io.in.bits.fun.FtypeTagIn === 1.U) -> FType.D.classify( op1 ),
      ))
    toint := classify_out | (store >> 32 << 32)
  }

  when ( io.in.bits.fun.is_fun_fcmp ) { // feq/flt/fle,

    val dcmp =  {
      val mdl = Module(new hardfloat.CompareRecFN(expWidth = 11, sigWidth = 53))
      mdl.io.a := op1
      mdl.io.b := op2
      mdl.io.signaling := ~io.in.bits.param.rm(1)
      mdl
    }

    toint := (~io.in.bits.param.rm & Cat(dcmp.io.lt, dcmp.io.eq)).orR
    out.exc := dcmp.io.exceptionFlags
  }

  when ( io.in.bits.fun.is_fun_fcvtX ) { // fcvt
    val conv =  {
      val mdl = Module(new hardfloat.RecFNToIN( 11, 53, 64))
      mdl.io.in := op1
      mdl.io.roundingMode := Mux(io.in.bits.param.rm === "b111".U, io.frm, io.in.bits.param.rm)
      mdl.io.signedOut := ~io.in.bits.fun.is_usi
      mdl
    }

    toint := conv.io.out
    out.exc := Cat(conv.io.intExceptionFlags(2, 1).orR, 0.U(3.W), conv.io.intExceptionFlags(0))
    when (io.in.bits.fun.XtypeTagOut === 0.U) {
      val narrow = {
        val mdl = Module(new hardfloat.RecFNToIN( 11, 53, 32)) 
        mdl.io.in := op1
        mdl.io.roundingMode := Mux(io.in.bits.param.rm === "b111".U, io.frm, io.in.bits.param.rm)
        mdl.io.signedOut := ~io.in.bits.fun.is_usi
        mdl
      }
      val excSign = op1(64) && !FType.D.isNaN(op1)
      val excOut = Cat(conv.io.signedOut === excSign, Fill(31, !excSign))
      val invalid = conv.io.intExceptionFlags(2) || narrow.io.intExceptionFlags(1)
      when (invalid) { toint := Cat(conv.io.out >> 32, excOut) }
      out.exc := Cat(invalid, 0.U(3.W), !invalid && conv.io.intExceptionFlags(0))
    } 
  }

  when ( io.in.bits.fun.is_fun_fmvX ) {
    toint := ieee(op1, t = FType.D)
    out.exc := 0.U
  }

  out.toInt := 
    Mux1H( Seq(
      (io.in.bits.fun.XtypeTagOut === 0.U) -> sextXTo(toint(31,0),64),
      (io.in.bits.fun.XtypeTagOut === 1.U) -> toint(63,0),
    ))

  io.out :=
    Pipe.apply(
      enqValid =
        io.in.valid & (
          io.in.bits.fun.is_fun_class |
          io.in.bits.fun.is_fun_fcmp |
          io.in.bits.fun.is_fun_fcvtX |
          io.in.bits.fun.is_fun_fmvX
        ),
      enqBits = out,
      latency = latency
    )

}
