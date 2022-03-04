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
import chisel3.experimental.dataview._

class FDivSqrt() extends Module with HasFPUParameters {
  val io = IO(new Bundle {
    val in = Flipped(ValidIO(new Fpu_iss_info))
    val frm = Input(UInt(3.W))
    val out = ValidIO(new Fres_Info)
    val pending = Output(Bool())
  })

  val op1 = unbox(io.in.bits.param.dat.op1, io.in.bits.fun.FtypeTagIn, None)
  val op2 = unbox(io.in.bits.param.dat.op2, io.in.bits.fun.FtypeTagIn, None)

  io.pending := false.B
  io.out.valid := false.B
  io.out.bits.toFloat := 0.U
  io.out.bits.exc := 0.U
  io.out.bits.viewAsSupertype(new Fpu_iss_info) := RegEnable(io.in.bits, io.in.fire & io.in.bits.fun.is_fun_divSqrt)

  for (t <- floatTypes) {
    val divSqrt = {
      val mdl = Module(new hardfloat.DivSqrtRecFN_small(t.exp, t.sig, 0))

      mdl.io.inValid := io.in.fire & (io.in.bits.fun.FtypeTagIn === typeTag(t).U) & io.in.bits.fun.is_fun_divSqrt
      mdl.io.sqrtOp := io.in.bits.fun.fsqrt_s | io.in.bits.fun.fsqrt_d
      mdl.io.a := maxType.unsafeConvert(op1, t)
      mdl.io.b := maxType.unsafeConvert(op2, t)
      mdl.io.roundingMode :=  Mux(io.in.bits.param.rm === "b111".U, io.frm, io.in.bits.param.rm)
      mdl.io.detectTininess := hardfloat.consts.tininess_afterRounding

      when (mdl.io.outValid_div | mdl.io.outValid_sqrt) {
        io.out.bits.toFloat := box(sanitizeNaN(mdl.io.out, t), t)
        io.out.bits.exc := mdl.io.exceptionFlags
        io.out.valid := true.B
      }
      when( ~mdl.io.inReady ) { io.pending := true.B }
      mdl
    }
  }

}




