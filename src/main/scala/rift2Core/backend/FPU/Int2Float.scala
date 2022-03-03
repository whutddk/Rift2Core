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

class IntToFP(latency: Int) extends Module with HasFPUParameters {
  val io = IO(new Bundle {
    val in = Flipped(ValidIO(new Fpu_iss_info))
    val frm = Input(UInt(3.W))
    val out = ValidIO(new Fres_Info)
    val is_empty = Output(Bool)
  })

  val cnt = RegInit(0.U(3.W))
  when( io.in.valid & io.out.valid ) {
    cnt := cnt
  } .elsewhen( io.in.valid ) {
    cnt := cnt + 1.U
    assert(cnt =/= (latency-1).U)
  } .elsewhen( io.out.valid ) {
    cnt := cnt - 1.U
    assert(cnt =/= 0.U)
  }
  io.is_empty := (cnt === 0.U)

  val out = Wire(new Fres_Info)
  out.viewAsSupertype(new Fpu_iss_info) := io.fpu_iss_exe.bits

  val op1 = io.in.param.dat.op1


  out.toFloat := 0.U
  out.exc     := 0.U

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

    out.toFloat :=
      Mux1H(Seq(
        (io.in.fun.FtypeTagOut === 0.U ) -> box(dataPadded(0), 0.U),
        (io.in.fun.FtypeTagOut === 1.U ) -> box(dataPadded(1), 1.U),
      ))

    out.exc :=
      Mux1H(Seq(
        (io.in.fun.FtypeTagOut === 0.U ) -> exc(0),
        (io.in.fun.FtypeTagOut === 1.U ) -> exc(1),
      ))
  }

  when ( io.in.fun.is_fun_xmvF ) {
    out.toFloat :=
      Mux1H(Seq(
        (io.in.fun.XtypeTagIn === 0.U ) -> box(recode(op1, 0), 1.U),
        (io.in.fun.XtypeTagIn === 1.U ) -> box(recode(op1, 1), 1.U),
      ))
    out.exc     := 0.U
  }

  io.out := Pipe.apply(enqValid = io.in.valid, enqBits = out, latency = latency)
}








