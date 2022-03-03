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
      // res.fun.viewAsSupertype(new Lsu_isa) := ori.fun.viewAsSupertype(new Lsu_isa)

// class MulAddRecFNPipe(expWidth: Int, sigWidth: Int) extends Module
// {

//   val io = IO(new Bundle {
//     val validin = Input(Bool())
//     val op = Input( UInt(2.W) )
//     val a = Input( UInt((expWidth + sigWidth + 1).W) )
//     val b = Input( UInt((expWidth + sigWidth + 1).W) )
//     val c = Input( UInt((expWidth + sigWidth + 1).W) )
//     val roundingMode   = Input(UInt(3.W))
//     val detectTininess = Input(UInt(1.W))
//     val out = Output( UInt((expWidth + sigWidth + 1).W) )
//     val exceptionFlags = Output( UInt(5.W) )
//     val validout = Output(Bool())
//   })

//   class Pipe_info extends Bundle {
//     val rm = UInt(3.W)

//   }

//   class PreMul_Info extends Pipe_info {
//     val mulAddA = UInt(sigWidth.W)
//     val mulAddB = UInt(sigWidth.W)
//     val mulAddC = UInt((sigWidth * 2).W)
//     val toPostMul = new hardfloat.MulAddRecFN_interIo(expWidth, sigWidth) 
//   } 


//   val preMul_fifo = Module(new Queue(new PreMul_Info, 2))
//   val mulAddRecFNToRaw_preMul = {
//     val mdl = Module(new hardfloat.MulAddRecFNToRaw_preMul(expWidth, sigWidth))
//     mdl.io.op := io.op
//     mdl.io.a  := io.a
//     mdl.io.b  := io.b
//     mdl.io.c  := io.c
//     preMul_fifo.io.enq.bits.mulAddA   := mulAddRecFNToRaw_preMul.io.mulAddA
//     preMul_fifo.io.enq.bits.mulAddB   := mulAddRecFNToRaw_preMul.io.mulAddB
//     preMul_fifo.io.enq.bits.mulAddC   := mulAddRecFNToRaw_preMul.io.mulAddC
//     preMul_fifo.io.enq.bits.toPostMul := mulAddRecFNToRaw_preMul.io.toPostMul
//     preMul_fifo.io.enq.bits.rm        := 
//     mdl
//   }

//   val mulAddResult =
//       (preMul_fifo.io.deq.mulAddA *
//             preMul_fifo.io.deq.mulAddB) +&
//           preMul_fifo.io.deq.mulAddC

//   class PostMul_Info extends Pipe_Info {
//     val invalidExc  = Bool()
//     val rawOut = new RawFloat(expWidth, sigWidth + 2)
//   }

//   val postMul_fifo = Module(new Queue(new PostMul_Info, 2))
//   val mulAddRecFNToRaw_postMul = {
//     val mdl = Module(new hardfloat.MulAddRecFNToRaw_postMul(expWidth, sigWidth))

//     mdl.io.fromPreMul   := preMul_fifo.io.deq.toPostMul
//     mdl.io.mulAddResult := mulAddResult
//     mdl.io.roundingMode := preMul_fifo.io.deq.rm

//     postMul_fifo.io.enq.bits.viewAsSupertype(new Pipe_Info) := preMul_fifo.io.deq.bits.viewAsSupertype(new Pipe_Info)
//     postMul_fifo.io.enq.bits.invalidExc := mdl.io.invalidExc
//     postMul_fifo.io.enq.bits.rawOut := mdl.io.rawOut
//     mdl
//   }


//   class Res_Info extends Bundle {
//     val out = UInt((expWidth + sigWidth + 1).W)
//     val exceptionFlags = UInt(5.W)
//   }

//   val res_fifo = Module(new Queue(new Res_Info, 2))
//   val roundRawFNToRecFN = {
//     val mdl = Module(new hardfloat.RoundRawFNToRecFN(expWidth, sigWidth, 0))

//     mdl.io.invalidExc         := postMul_fifo.io.deq.bits.invalidExc
//     mdl.io.in                 := postMul_fifo.io.deq.bits.rawOut
//     mdl.io.roundingMode       := postMul_fifo.io.deq.bits.rm
//     mdl.io.detectTininess     := hardfloat.consts.tininess_afterRounding
//     mdl.io.infiniteExc := false.B

//     res_fifo.enq.bits.viewAsSupertype(new Pipe_Info) := postMul_fifo.io.deq.bits.viewAsSupertype(new Pipe_Info)
//     res_fifo.enq.bits.out := mdl.io.out
//     res_fifo.enq.bits.exceptionFlags := mdl.io.exceptionFlags
//     mdl
//   }






//   io.out            := roundRawFNToRecFN.io.out
//   io.exceptionFlags := roundRawFNToRecFN.io.exceptionFlags
// }

// class FPUFMAPipe(val latency: Int, val t: FType)
//                 (implicit p: Parameters) extends FPUModule()(p) with ShouldBeRetimed {
//   require(latency>0)

//   val io = new Bundle {
//     val in = Valid(new FPInput).flip
//     val out = Valid(new FPResult)
//   }

//   val valid = Reg(next=io.in.valid)
//   val in = Reg(new FPInput)
//   when (io.in.valid) {
//     val one = UInt(1) << (t.sig + t.exp - 1)
//     val zero = (io.in.bits.in1 ^ io.in.bits.in2) & (UInt(1) << (t.sig + t.exp))
//     val cmd_fma = io.in.bits.ren3
//     val cmd_addsub = io.in.bits.swap23
//     in := io.in.bits
//     when (cmd_addsub) { in.in2 := one }
//     when (!(cmd_fma || cmd_addsub)) { in.in3 := zero }
//   }

//   val fma = Module(new MulAddRecFNPipe((latency-1) min 2, t.exp, t.sig))
//   fma.io.validin := valid
//   fma.io.op := in.fmaCmd
//   fma.io.roundingMode := in.rm
//   fma.io.detectTininess := hardfloat.consts.tininess_afterRounding
//   fma.io.a := in.in1
//   fma.io.b := in.in2
//   fma.io.c := in.in3

//   val res = Wire(new FPResult)
//   res.data := sanitizeNaN(fma.io.out, t)
//   res.exc := fma.io.exceptionFlags

//   io.out := Pipe(fma.io.validout, res, (latency-3) max 0)
// }

