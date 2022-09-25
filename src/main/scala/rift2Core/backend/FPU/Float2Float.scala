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

import rift2Chip._
import chipsalliance.rocketchip.config._

class FPToFP(latency: Int)(implicit p: Parameters) extends RiftModule with HasFPUParameters{
  val io = IO(new Bundle {
    val in = Flipped(ValidIO(new Fpu_iss_info))
    val frm = Input(UInt(3.W))
    val out = ValidIO(new Fres_Info)
  })

  val out = Wire(new Fres_Info)
  out.viewAsSupertype(new Fpu_iss_info) := io.in.bits




  val op1 = unbox(io.in.bits.param.dat.op1, io.in.bits.fun.FtypeTagIn, None)
  val op2 = unbox(io.in.bits.param.dat.op2, io.in.bits.fun.FtypeTagIn, None)

  val fsgnjMux_exc     = WireDefault(0.U(5.W))
  val fsgnjMux_toFloat = WireDefault(0.U(65.W))

  when( io.in.bits.fun.is_fun_fsgn ) {
    val signNum = Mux(io.in.bits.param.rm(1), op1 ^ op2, Mux(io.in.bits.param.rm(0), ~op2, op2))
    val fsgnj = Cat(signNum(64), op1(63, 0))

    fsgnjMux_exc := 0.U
    fsgnjMux_toFloat := fsgnj
  }

  out.toFloat := box( fsgnjMux_toFloat, io.in.bits.fun.FtypeTagOut)
  out.exc := fsgnjMux_exc


  when ( io.in.bits.fun.is_fun_maxMin ) { // fmin/fmax

    val dcmp =  {
      val mdl = Module(new hardfloat.CompareRecFN(expWidth = 11, sigWidth = 53))
      mdl.io.a := op1
      mdl.io.b := op2
      mdl.io.signaling := ~io.in.bits.param.rm(1)
      mdl
    }
    val is_lt = dcmp.io.lt | (dcmp.io.a.asSInt < 0.S && dcmp.io.b.asSInt >= 0.S)

    val isnan1 = FType.D.isNaN(op1)
    val isnan2 = FType.D.isNaN(op2)
    val isInvalid = FType.D.isSNaN(op1) || FType.D.isSNaN(op2)
    val isNaNOut = isnan1 && isnan2
    val isLHS = isnan2 || io.in.bits.param.rm(0) =/= is_lt && !isnan1
    fsgnjMux_exc := (isInvalid << 4)
    fsgnjMux_toFloat := Mux(isNaNOut, FType.D.qNaN, Mux(isLHS, op1, op2))
  }


  when ( io.in.bits.fun.FtypeTagOut === 0.U) {
    out.toFloat := box( Cat(fsgnjMux_toFloat >> 33, FType.D.unsafeConvert(fsgnjMux_toFloat, FType.S)), 0.U)
  }


  when ( io.in.bits.fun.is_fun_fcvtF ) { // fcvt

    val widened = Mux(FType.D.isNaN(op1), FType.D.qNaN, op1)
    fsgnjMux_toFloat := widened
    fsgnjMux_exc := FType.D.isSNaN(op1) << 4.U


    when ( (io.in.bits.fun.FtypeTagOut === 0.U) &&  io.in.bits.fun.FtypeTagIn === 1.U) {
      val narrower = Module(new hardfloat.RecFNToRecFN(FType.D.exp, FType.D.sig, FType.S.exp, FType.S.sig))
      narrower.io.in := op1
      narrower.io.roundingMode := Mux(io.in.bits.param.rm === "b111".U, io.frm, io.in.bits.param.rm)
      narrower.io.detectTininess := hardfloat.consts.tininess_afterRounding
      // val narrowed = sanitizeNaN(narrower.io.out, FType.S)
      out.toFloat := box(Cat(widened >> 33, narrower.io.out), 0.U )
      out.exc := narrower.io.exceptionFlags
    }




  }
    io.out :=
      Pipe.apply(
        enqValid =
          io.in.valid & (
            io.in.bits.fun.is_fun_fsgn |
            io.in.bits.fun.is_fun_maxMin |
            io.in.bits.fun.is_fun_fcvtF
          ),
        enqBits = out,
        latency = latency
      )
}

