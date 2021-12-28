/*
  Copyright (c) 2020 - 2021 Ruige Lee <wut.ruigeli@gmail.com>

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

class MulAddRecFNPipe(expWidth: Int, sigWidth: Int) extends Module
{

    val io = new Bundle {
        val validin = Input(Bool())
        val op = Input( UInt(2.W) )
        val a = Input( UInt((expWidth + sigWidth + 1).W) )
        val b = Input( UInt((expWidth + sigWidth + 1).W) )
        val c = Input( UInt((expWidth + sigWidth + 1).W) )
        val roundingMode   = Input(UInt(3.W))
        val detectTininess = Input(UInt(1.W))
        val out = Output( UInt((expWidth + sigWidth + 1).W) )
        val exceptionFlags = Output( UInt(5.W) )
        val validout = Output(Bool())
    }

    //------------------------------------------------------------------------
    //------------------------------------------------------------------------

    val mulAddRecFNToRaw_preMul = Module(new hardfloat.MulAddRecFNToRaw_preMul(expWidth, sigWidth))
    val mulAddRecFNToRaw_postMul = Module(new hardfloat.MulAddRecFNToRaw_postMul(expWidth, sigWidth))

    mulAddRecFNToRaw_preMul.io.op := io.op
    mulAddRecFNToRaw_preMul.io.a  := io.a
    mulAddRecFNToRaw_preMul.io.b  := io.b
    mulAddRecFNToRaw_preMul.io.c  := io.c

    val mulAddResult =
        (mulAddRecFNToRaw_preMul.io.mulAddA *
             mulAddRecFNToRaw_preMul.io.mulAddB) +&
            mulAddRecFNToRaw_preMul.io.mulAddC

    val valid_stage0 = Wire(Bool())
    val roundingMode_stage0 = Wire(UInt(3.W))
    val detectTininess_stage0 = Wire(UInt(1.W))

    val postmul_regs = 1
    mulAddRecFNToRaw_postMul.io.fromPreMul   := Pipe(io.validin, mulAddRecFNToRaw_preMul.io.toPostMul, postmul_regs).bits
    mulAddRecFNToRaw_postMul.io.mulAddResult := Pipe(io.validin, mulAddResult, postmul_regs).bits
    mulAddRecFNToRaw_postMul.io.roundingMode := Pipe(io.validin, io.roundingMode, postmul_regs).bits
    roundingMode_stage0                      := Pipe(io.validin, io.roundingMode, postmul_regs).bits
    detectTininess_stage0                    := Pipe(io.validin, io.detectTininess, postmul_regs).bits
    valid_stage0                             := Pipe(io.validin, false.B, postmul_regs).valid

    //------------------------------------------------------------------------
    //------------------------------------------------------------------------

    val roundRawFNToRecFN = Module(new hardfloat.RoundRawFNToRecFN(expWidth, sigWidth, 0))

    val round_regs = 1
    roundRawFNToRecFN.io.invalidExc         := Pipe(valid_stage0, mulAddRecFNToRaw_postMul.io.invalidExc, round_regs).bits
    roundRawFNToRecFN.io.in                 := Pipe(valid_stage0, mulAddRecFNToRaw_postMul.io.rawOut, round_regs).bits
    roundRawFNToRecFN.io.roundingMode       := Pipe(valid_stage0, roundingMode_stage0, round_regs).bits
    roundRawFNToRecFN.io.detectTininess     := Pipe(valid_stage0, detectTininess_stage0, round_regs).bits
    io.validout                             := Pipe(valid_stage0, false.B, round_regs).valid

    roundRawFNToRecFN.io.infiniteExc := false.B

    io.out            := roundRawFNToRecFN.io.out
    io.exceptionFlags := roundRawFNToRecFN.io.exceptionFlags
}



object fma32 {
  def apply(
    validin: Bool,
    op_sel: UInt,
    rm: UInt,
    op1: UInt,
    op2: UInt,
    op3: UInt,
    phy: UInt

  ): (ValidIO[FPU_out], UInt) = {

    def recFN_op1 = hardfloat.recFNFromFN(8, 24, op1)
    def recFN_op2 = hardfloat.recFNFromFN(8, 24, op2)
    def recFN_op3 = hardfloat.recFNFromFN(8, 24, op3)

    val op_a = Wire(UInt(33.W))
    val op_b = Wire(UInt(33.W))
    val op_c = Wire(UInt(33.W))

    def is_addsub = op_sel(3)
    def is_mul = op_sel(2)
    def one = 1.U << 31.U
    def zero = (recFN_op1 ^ recFN_op2) & (1.U << 32.U)
    op_a := recFN_op1
    op_b := Mux( is_addsub, one, recFN_op2 )
    op_c := Mux( is_mul, zero, recFN_op3 )


    val fma32 = Module(new MulAddRecFNPipe(8, 24) {
      io.validin        := validin
      io.op             := Mux( validin, op_sel(1,0),                             0.U )
      io.roundingMode   := Mux( validin, rm,                                      0.U )
      io.detectTininess := Mux( validin, hardfloat.consts.tininess_afterRounding, 0.U )
      io.a              := Mux( validin, op_a,                                    0.U )
      io.b              := Mux( validin, op_b,                                    0.U )      
      io.c              := Mux( validin, op_c,                                    0.U )    
    })

    val out = new ValidIO(new FPU_out) {
      bits.res := fma32.io.out
      bits.exceptionFlags := fma32.io.exceptionFlags
      valid := fma32.io.validout
    }

    val pipe_phy = Pipe(validin, phy, 3)
    assert (pipe_phy.valid === out.valid)
    return (out, pipe_phy.bits)
  }

}

object fma64 {
  def apply(
    validin: Bool,
    op_sel: UInt,
    rm: UInt,
    op1: UInt,
    op2: UInt,
    op3: UInt,
    phy: UInt

  ): (ValidIO[FPU_out], UInt) = {

    def recFN_op1 = hardfloat.recFNFromFN(11, 53, op1)
    def recFN_op2 = hardfloat.recFNFromFN(11, 53, op2)
    def recFN_op3 = hardfloat.recFNFromFN(11, 53, op3)

    val op_a = Wire(UInt(65.W))
    val op_b = Wire(UInt(65.W))
    val op_c = Wire(UInt(65.W))


    def is_addsub = op_sel(3)
    def is_mul = op_sel(2)
    def one = 1.U << 63.U
    def zero = (recFN_op1 ^ recFN_op2) & (1.U << 64.U)

    op_a := recFN_op1
    op_b := Mux( is_addsub, one, recFN_op2 )
    op_c := Mux( is_mul, zero, recFN_op3 )

    val fma64 = Module(new MulAddRecFNPipe(11, 53) {
      io.validin        := validin
      io.op             := Mux( validin, op_sel(1,0),                             0.U )
      io.roundingMode   := Mux( validin, rm,                                      0.U )
      io.detectTininess := Mux( validin, hardfloat.consts.tininess_afterRounding, 0.U )
      io.a              := Mux( validin, op_a,                                    0.U )
      io.b              := Mux( validin, op_b,                                    0.U )      
      io.c              := Mux( validin, op_c,                                    0.U )
    })

    val out = Wire(new Valid(new FPU_out) {
      bits.res := fma64.io.out
      bits.exceptionFlags := fma64.io.exceptionFlags
      valid := fma64.io.validout
    })

    val pipe_phy = Pipe(validin, phy, 3)
    assert (pipe_phy.valid === out.valid)

    return ( out, pipe_phy.bits )
  }

}

