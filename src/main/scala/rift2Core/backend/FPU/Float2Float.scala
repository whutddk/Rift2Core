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


class FPToFP() extends Module with HasFPUParameters{
  val io = IO(new Bundle {
    val in = Input(new Fpu_iss_info)
    val is_lt = Input(Bool())
    val frm = Input(UInt(3.W))
    val out = Output(new Bundle{
      val toFloat = UInt(65.W)
      val exc = UInt(5.W)
      })
  })




  val op1 = unbox(io.in.param.dat.op1, io.in.fun.FtypeTagIn, None)
  val op2 = unbox(io.in.param.dat.op2, io.in.fun.FtypeTagIn, None)



  val signNum = Mux(io.in.param.rm(1), op1 ^ op2, Mux(io.in.param.rm(0), ~op2, op2))
  val fsgnj = Cat(signNum(64), op1(63, 0))

  val fsgnjMux_exc     = Wire(UInt(5.W))
  val fsgnjMux_toFloat = Wire(UInt(65.W))
  fsgnjMux_exc := 0.U
  fsgnjMux_toFloat := fsgnj

  io.out.toFloat := fsgnjMux_toFloat
  io.out.exc := fsgnjMux_exc

  when ( io.in.fun.is_fun_maxMin) { // fmin/fmax
    val isnan1 = FType.D.isNaN(op1)
    val isnan2 = FType.D.isNaN(op2)
    val isInvalid = FType.D.isSNaN(op1) || FType.D.isSNaN(op2)
    val isNaNOut = isnan1 && isnan2
    val isLHS = isnan2 || io.in.param.rm(0) =/= io.is_lt && !isnan1
    fsgnjMux_exc := isInvalid << 4
    fsgnjMux_toFloat := Mux(isNaNOut, FType.D.qNaN, Mux(isLHS, op1, op2))
  }


  when ( io.in.fun.FtypeTagOut === 0.U) {
    io.out.toFloat := Cat(fsgnjMux_toFloat >> 33, FType.D.unsafeConvert(fsgnjMux_toFloat, FType.S))
  }


  when ( io.in.fun.is_fun_fcvtF ) { // fcvt

    val widened = Mux(FType.D.isNaN(op1), FType.D.qNaN, op1)
    fsgnjMux_toFloat := widened
    fsgnjMux_exc := FType.D.isSNaN(op1) << 4.U


    when ( (io.in.fun.FtypeTagOut === 0.U) &&  io.in.fun.FtypeTagIn === 1.U) {
      val narrower = Module(new hardfloat.RecFNToRecFN(FType.D.exp, FType.D.sig, FType.S.exp, FType.S.sig))
      narrower.io.in := op1
      narrower.io.roundingMode := Mux(io.in.param.rm === "b111".U, io.frm, io.in.param.rm)
      narrower.io.detectTininess := hardfloat.consts.tininess_afterRounding
      // val narrowed = sanitizeNaN(narrower.io.out, FType.S)
      io.out.toFloat := Cat(widened >> 33, narrower.io.out)
      io.out.exc := narrower.io.exceptionFlags
    }

  }



















  // io.out.toFloat := 0.U
  // io.out.exc := 0.U

  // when( io.in.fun.is_fun_fsgn ) {
  //   val signNum = Mux(io.in.param.rm(1), op1 ^ op2, Mux(io.in.param.rm(0), ~op2, op2))
  //   val fsgnj = Cat(signNum(64), op1(63, 0))

  //   when (io.in.fun.FtypeTagOut === 0.U) { //D->S
  //     io.out.toFloat := Cat(fsgnj >> 33.U, FType.D.unsafeConvert(fsgnj, FType.S))
  //   } .otherwise {
  //     io.out.toFloat := fsgnj
  //   }
  // }


  // when ( io.in.fun.is_fun_maxMin ) { // fmin/fmax
  //   val isnan1 = FType.D.isNaN(op1)
  //   val isnan2 = FType.D.isNaN(op2)
  //   val isInvalid = FType.D.isSNaN(op1) || FType.D.isSNaN(op2)
  //   val isNaNOut = isnan1 & isnan2
  //   val isLHS = (isnan2 | (io.in.param.rm(0) =/= io.is_lt)) & ~isnan1
  //   io.out.exc := isInvalid << 4.U

  //   val maxMin_res_temp = Mux(isNaNOut, FType.D.qNaN, Mux(isLHS, op1, op2))

  //   when (io.in.fun.FtypeTagOut === 0.U) { //D->S
  //     io.out.toFloat := Cat(maxMin_res_temp >> 33.U, FType.D.unsafeConvert(maxMin_res_temp, FType.S))
  //   } .otherwise {
  //     io.out.toFloat := maxMin_res_temp
  //   }
  // }

  // when ( io.in.fun.is_fun_fcvtF ) { // fcvt

  //   val s2d_res = Mux(FType.D.isNaN(op1), FType.D.qNaN, op1)
  //   io.out.toFloat := s2d_res
  //   io.out.exc := FType.D.isSNaN(op1) << 4.U

  //   when (io.in.fun.FtypeTagOut === 0.U & io.in.fun.FtypeTagIn === 1.U ) {//D->S
  //     val narrower = {
  //       val mdl = Module(new hardfloat.RecFNToRecFN(11, 53, 8, 24))
  //       mdl.io.in := op1
  //       mdl.io.roundingMode := Mux(io.in.param.rm === "b111".U, io.frm, io.in.param.rm)
  //       mdl.io.detectTininess := hardfloat.consts.tininess_afterRounding
  //       mdl
  //     }
  //     val narrowed = sanitizeNaN(narrower.io.out, FType.S)
  //     io.out.toFloat := Cat(s2d_res >> narrowed.getWidth, narrowed)
  //     io.out.exc := narrower.io.exceptionFlags
  //   }

  // }

}

