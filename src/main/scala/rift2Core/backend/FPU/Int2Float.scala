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


class IntToFP() extends Module with HasFPUParameters {
  val io = new Bundle {
    val in = Input(new Fpu_iss_info) 
    val out = Valid(new FPResult){
      val toFloat = UInt(65.W)
      val exc = UInt(5.W)
    }
  }


  val op1 = io.in.param.dat.op1
  val tag = io.in.fun.FtypeTagOut


  io.out.exc     := 0.U
  io.out.toFloat := recode(op1, io.in.fun.FtypeTagOut)

  val intValue = {
    val res = WireDefault(op1.asSInt)
    val smallInt = op1(31, 0)
    when (io.in.fun.XtypeTagIn === 0) {
      res := Mux(io.in.fun.is_usi, smallInt.zext, smallInt.asSInt)
    }
    res.asUInt
  }

  when ( in.fun.is_fun_xcvtF ) { // fcvt
    val i2fResults = for (t <- floatTypes) yield {
      val i2f = {
        mdl = Module(new hardfloat.INToRecFN(64, t.exp, t.sig))
        mdl.io.signedIn := ~io.in.fun.is_usi
        mdl.io.in := intValue
        mdl.io.roundingMode := io.in.param.rm
        mdl.io.detectTininess := hardfloat.consts.tininess_afterRounding
        mdl
      }
      (sanitizeNaN(i2f.io.out, t), i2f.io.exceptionFlags)
    }

    val (data, exc) = i2fResults.unzip
    val dataPadded = data.init.map(d => Cat(data.last >> d.getWidth, d)) :+ data.last
    io.out.toFloat := dataPadded(io.in.fun.FtypeTagOut)
    io.out.exc     := exc(io.in.fun.FtypeTagOut)
  }

  when ( io.in.fun.is_fun_xmvF ) {
    io.out.toFloat := recode(intValue, io.in.fun.FtypeTagOut)
    io.out.exc     := 0.U
  }


}








