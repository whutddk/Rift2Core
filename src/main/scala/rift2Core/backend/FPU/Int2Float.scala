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
  val io = IO(new Bundle {
    val in = Input(new Fpu_iss_info)
    val frm = Input(UInt(3.W))
    val out = Output(new Bundle{
      val toFloat = UInt(65.W)
      val exc = UInt(5.W)
    })
  })


  val op1 = io.in.param.dat.op1


  io.out.toFloat := 
    Mux1H(Seq(
      (io.in.fun.FtypeTagOut === 0.U) -> recode(op1, 0),
      (io.in.fun.FtypeTagOut === 1.U) -> recode(op1, 1),      
    ))
  io.out.exc     := 0.U

  val intValue = {
    val res = WireDefault(op1.asSInt)
    val smallInt = op1(31, 0)
    when (io.in.fun.XtypeTagIn === 0.U) {
      res := Mux(io.in.fun.is_usi, smallInt.zext, smallInt.asSInt)
    }
    res.asUInt
  }

  when ( io.in.fun.is_fun_xcvtF ) { // fcvt
    val i2fResults = for (t <- floatTypes) yield {
      val i2f = {
        val mdl = Module(new hardfloat.INToRecFN(64, t.exp, t.sig))
        mdl.io.signedIn := ~io.in.fun.is_usi
        mdl.io.in := intValue
        mdl.io.roundingMode := Mux(io.in.param.rm === "b111".U, io.frm, io.in.param.rm)
        mdl.io.detectTininess := hardfloat.consts.tininess_afterRounding
        mdl
      }
      (sanitizeNaN(i2f.io.out, t), i2f.io.exceptionFlags)
    }

    val (data, exc) = i2fResults.unzip
    val dataPadded = data.init.map(d => Cat(data.last >> d.getWidth, d)) :+ data.last

    io.out.toFloat :=
      Mux1H(Seq(
        (io.in.fun.FtypeTagOut === 0.U ) -> dataPadded(0),
        (io.in.fun.FtypeTagOut === 1.U ) -> dataPadded(1),
      ))

    io.out.exc :=
      Mux1H(Seq(
        (io.in.fun.FtypeTagOut === 0.U ) -> exc(0),
        (io.in.fun.FtypeTagOut === 1.U ) -> exc(1),
      ))
  }

  when ( io.in.fun.is_fun_xmvF ) {
    io.out.toFloat :=
      Mux1H(Seq(
        (io.in.fun.FtypeTagOut === 0.U ) -> recode(intValue, 0),
        (io.in.fun.FtypeTagOut === 1.U ) -> recode(intValue, 1),
      ))
    io.out.exc     := 0.U
  }


}








