/*
  Copyright (c) 2020 - 2023 Wuhan University of Technology <295054118@whut.edu.cn>

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

class FPUFMAPipe(latency: Int, val t: FType)(implicit p: Parameters) extends RiftModule with HasFPUParameters {
  val io = IO(new Bundle {
    val in = Flipped(ValidIO(new Fpu_iss_info))
    val frm = Input(UInt(3.W))
    val out = ValidIO(new Fres_Info)
  })

  val out = Wire(new Fres_Info)
  out.viewAsSupertype(new Fpu_iss_info) := io.in.bits

  val op1 = unbox(io.in.bits.param.dat.op1, 0.U, Some(t))
  val op2 = unbox(io.in.bits.param.dat.op2, 0.U, Some(t))
  val op3 = unbox(io.in.bits.param.dat.op3, 0.U, Some(t))



  val one = 1.U << (t.sig + t.exp - 1)
  val zero = (op1 ^ op2) & (1.U << (t.sig + t.exp))

  def fmadd_s  = io.in.bits.fun.fmadd_s 
  def fmsub_s  = io.in.bits.fun.fmsub_s 
  def fnmsub_s = io.in.bits.fun.fnmsub_s
  def fnmadd_s = io.in.bits.fun.fnmadd_s
  def fadd_s   = io.in.bits.fun.fadd_s  
  def fsub_s   = io.in.bits.fun.fsub_s  
  def fmul_s   = io.in.bits.fun.fmul_s  
  def fmadd_d  = io.in.bits.fun.fmadd_d 
  def fmsub_d  = io.in.bits.fun.fmsub_d 
  def fnmsub_d = io.in.bits.fun.fnmsub_d
  def fnmadd_d = io.in.bits.fun.fnmadd_d
  def fadd_d   = io.in.bits.fun.fadd_d  
  def fsub_d   = io.in.bits.fun.fsub_d  
  def fmul_d   = io.in.bits.fun.fmul_d  

    val op = 
      Mux1H(Seq(
        ( fmadd_s | fmadd_d | fadd_s | fadd_d | fmul_s | fmul_d ) -> "b00".U,
        ( fmsub_s | fmsub_d | fsub_s | fsub_d                   ) -> "b01".U,
        ( fnmsub_s | fnmsub_d                                   ) -> "b10".U,
        ( fnmadd_s | fnmadd_d                                   ) -> "b11".U,
      ))

    val in1 = op1
    val in2 = 
      Mux( (fadd_s | fadd_d | fsub_s | fsub_d), one, op2 )
    val in3 = 
      Mux(
        (fmul_s | fmul_d), zero,
        Mux( fadd_s | fadd_d | fsub_s | fsub_d, op2, op3 )
       )



  val fma = {
    val mdl = Module(new hardfloat.MulAddRecFN(t.exp, t.sig))
    mdl.io.op := op
    mdl.io.roundingMode := Mux(io.in.bits.param.rm === "b111".U, io.frm, io.in.bits.param.rm)
    mdl.io.detectTininess := hardfloat.consts.tininess_afterRounding
    mdl.io.a := in1
    mdl.io.b := in2
    mdl.io.c := in3
    mdl
  }


  out.toFloat := box(sanitizeNaN(fma.io.out, t), t)
  out.exc := fma.io.exceptionFlags


  io.out :=
    Pipe.apply(
      enqValid =
        io.in.valid & (
          io.in.bits.fun.is_fun_fma
        ),
      enqBits = out,
      latency = latency
    )
}

