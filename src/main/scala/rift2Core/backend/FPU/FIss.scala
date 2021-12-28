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


class fpu_iss_fwb_info extends Bundle {
  val rs = Output(Vec(3,UInt(6.W)))
  val op = Input(Vec(3,UInt(64.W)))
}

class fpu_iss_iwb_info extends Bundle {
  val rs = Output(UInt(6.W))
  val op = Input(UInt(64.W))
}


















class FIss extends Module {
  val io = IO( new Bundle{

    val fpu_dpt_iss = Vec(2, Flipped(new DecoupledIO(new Fpu_dpt_info)))

    val int2float_iss_exe   = new DecoupledIO(new Fpu_int2float_iss_info)
    val float2float_iss_exe = new DecoupledIO(new Fpu_float2float_iss_info)
    val floatCmp_iss_exe = new DecoupledIO(new Fpu_floatCmp_iss_info)
    val float2int_iss_exe = new DecoupledIO(new Fpu_float2int_iss_info)
    val floatFma_iss_exe = new DecoupledIO(new Fpu_floatFma_iss_info)
    val floatDivSqrt_iss_exe = new DecoupledIO(new Fpu_floatDivSqrt_iss_info)
    val floatCsr_iss_exe = new DecoupledIO(new Fpu_floatCsr_iss_info)

    val fpu_iss_fwb = Vec(2, new fpu_iss_fwb_info)
    val fpu_iss_iwb = Vec(2, new fpu_iss_iwb_info)
  })

  {
    io.fpu_iss_fwb(0).rs(0) := 0.U
    io.fpu_iss_fwb(0).rs(1) := 0.U
    io.fpu_iss_fwb(0).rs(2) := 0.U
    io.fpu_iss_fwb(1).rs(0) := 0.U
    io.fpu_iss_fwb(1).rs(1) := 0.U
    io.fpu_iss_fwb(1).rs(2) := 0.U
    io.fpu_iss_iwb(0).rs    := 0.U
    io.fpu_iss_iwb(1).rs    := 0.U

    when( io.fpu_dpt_iss(0).valid ) {
      when( io.fpu_dpt_iss(0).bits.isa.is_int_iss ) { io.fpu_iss_iwb(0).rs    := io.fpu_dpt_iss(0).bits.phy.rs1 }
      when( io.fpu_dpt_iss(0).bits.isa.is_fot_iss ) { io.fpu_iss_iwb(0).rs(0) := io.fpu_dpt_iss(0).bits.phy.rs1 }
      when( io.fpu_dpt_iss(0).bits.isa.is_fot_iss ) { io.fpu_iss_iwb(0).rs(1) := io.fpu_dpt_iss(0).bits.phy.rs2 }
      when( io.fpu_dpt_iss(0).bits.isa.is_fot_iss ) { io.fpu_iss_iwb(0).rs(2) := io.fpu_dpt_iss(0).bits.phy.rs3 }
    }

    when( io.fpu_dpt_iss(1).valid ) {
      assert( io.fpu_dpt_iss(1).valid )
      when( io.fpu_dpt_iss(1).bits.isa.is_int_iss ) { io.fpu_iss_iwb(1).rs    := io.fpu_dpt_iss(1).bits.phy.rs1 }
      when( io.fpu_dpt_iss(1).bits.isa.is_fot_iss ) { io.fpu_iss_iwb(1).rs(0) := io.fpu_dpt_iss(1).bits.phy.rs1 }
      when( io.fpu_dpt_iss(1).bits.isa.is_fot_iss ) { io.fpu_iss_iwb(1).rs(1) := io.fpu_dpt_iss(1).bits.phy.rs2 }
      when( io.fpu_dpt_iss(1).bits.isa.is_fot_iss ) { io.fpu_iss_iwb(1).rs(2) := io.fpu_dpt_iss(1).bits.phy.rs3 }
    }
  }

  def dpt_w = 2




  val int2float_iss_exe_fifo    = Module( new MultiPortFifo( new Fpu_int2float_iss_info, aw = 2, in = dpt_w, out = 1 ) )
  io.int2float_iss_exe <> int2float_iss_exe_fifo.io.deq
  val float2float_iss_exe_fifo  = Module( new MultiPortFifo( new Fpu_float2float_iss_info, aw = 2, in = dpt_w, out = 1 ) )
  io.float2float_iss_exe <> float2float_iss_exe_fifo.io.deq
  val floatCmp_iss_exe_fifo     = Module( new MultiPortFifo( new Fpu_floatCmp_iss_info, aw = 2, in = dpt_w, out = 1 ) )
  io.floatCmp_iss_exe <> floatCmp_iss_exe_fifo.io.deq
  val float2int_iss_exe_fifo    = Module( new MultiPortFifo( new Fpu_float2int_iss_info, aw = 2, in = dpt_w, out = 1 ) )
  io.float2int_iss_exe <> float2int_iss_exe_fifo.io.deq
  val floatFma_iss_exe_fifo     = Module( new MultiPortFifo( new Fpu_floatFma_iss_info, aw = 4, in = dpt_w, out = 1 ) )
  io.floatFma_iss_exe <> floatFma_iss_exe_fifo.io.deq
  val floatDivSqrt_iss_exe_fifo = Module( new MultiPortFifo( new Fpu_floatDivSqrt_iss_info, aw = 4, in = dpt_w, out = 1 ) )
  io.floatDivSqrt_iss_exe <> floatDivSqrt_iss_exe_fifo.io.deq
  val floatCsr_iss_exe_fifo     = Module( new MultiPortFifo( new Fpu_floatCsr_iss_info, aw = 2, in = dpt_w, out = 1 ) )
  io.floatCsr_iss_exe <> floatCsr_iss_exe_fifo.io.deq


  int2float_iss_exe_fifo.io.enq    <> RePort(int2float( io.fpu_dpt_iss ))
  float2float_iss_exe_fifo.io.enq  <> RePort(float2float( io.fpu_dpt_iss ))
  floatCmp_iss_exe_fifo.io.enq     <> RePort(floatCmp( io.fpu_dpt_iss ) )
  float2int_iss_exe_fifo.io.enq    <> RePort(float2int(io.fpu_dpt_iss ) )
  floatFma_iss_exe_fifo.io.enq     <> RePort(floatFma(io.fpu_dpt_iss ) )
  floatDivSqrt_iss_exe_fifo.io.enq <> RePort(floatDivSqrt(io.fpu_dpt_iss ))
  floatCsr_iss_exe_fifo.io.enq     <> RePort(floatCsr(io.fpu_dpt_iss ))



  def int2float( dpt: Vec[ReadyValidIO[Fpu_dpt_info]] ): Vec[DecoupledIO[Fpu_int2float_iss_info]] = {

    val iss = Wire(Vec( dpt_w, new DecoupledIO(new Fpu_int2float_iss_info)))

    for ( i <- 0 until dpt_w ) {
      iss(i).valid := false.B
      dpt(i).ready := false.B
      iss(i).bits := 0.U.asTypeof( new Fpu_int2float_iss_info )

      when( dpt.valid & dpt.bits.isa.is_fun_int2float ) {
        dpt(i).ready := iss(i).ready
        iss(i).valid := true.B
        iss(i).bits.is_i32tof32 := dpt.bits.isa.fcvt_w_s | dpt.bits.isa.fcvt_wu_s
        iss(i).bits.is_i64tof32 := dpt.bits.isa.fcvt_l_s | dpt.bits.isa.fcvt_lu_s
        iss(i).bits.is_i32tof64 := dpt.bits.isa.fcvt_w_d | dpt.bits.isa.fcvt_wu_d
        iss(i).bits.is_i64tof64 := dpt.bits.isa.fcvt_l_d | dpt.bits.isa.fcvt_lu_d
        iss(i).bits.is_fmv32tof := dpt.bits.isa.fmv_w_x
        iss(i).bits.is_fmv64tof := dpt.bits.isa.fmv_d_x
        iss(i).bits.is_usi = dpt.bits.isa.is_usi
        iss(i).bits.op1 = io.fpu_iss_iwb(i).op1
        iss(i).bits.rm = dpt.bits.param.rm
        iss(i).bits.rd0_phy = dpt.bits.phy.rd0
      }
    }
    retrun iss
  }

  def float2float( dpt: Vec[ReadyValidIO[Fpu_dpt_info]] ): Vec[DecoupledIO[Fpu_float2float_iss_info]] = {
    val iss = Wire(Vec( dpt_w, new DecoupledIO(new Fpu_float2float_iss_info)))

    for ( i <- 0 until dpt_w ) {
      iss(i).valid := false.B
      dpt(i).ready := false.B
      iss(i).bits := 0.U.asTypeof( new Fpu_float2float_iss_info )

      when( dpt.valid & dpt.bits.isa.is_fun_float2float ) {
        dpt(i).ready := iss(i).ready
        iss(i).valid := true.B

        iss(i).bits.is_f32tof64 := dpt(i).bits.isa.fcvt_s_d
        iss(i).bits.is_f64tof32 := dpt(i).bits.isa.fcvt_d_s
        iss(i).bits.is_fSign32 :=
          dpt(i).bits.isa.fsgnj_s | dpt(i).bits.isa.fsgnjn_s | dpt(i).bits.isa.fsgnjx_s
        iss(i).bits.is_fSign64 := 
          dpt(i)x.bits.isa.fsgnj_d | dpt(i).bits.isa.fsgnjn_d | dpt(i).bits.isa.fsgnjx_d

        iss(i).bits.op1 := io.fpu_iss_fwb(i).op1
        iss(i).bits.op2 := io.fpu_iss_fwb(i).op2

        iss(i).bits.rm      := dpt(i).bits.param.rm
        iss(i).bits.rd0_phy := dpt(i).bits.phy.rd0
      }
    }

    retrun iss 
  }


  def floatCmp( dpt: Vec[ReadyValidIO[Fpu_dpt_info]] ): Vec[DecoupledIO[Fpu_floatCmp_iss_info]] = {
    val iss = Wire(Vec( dpt_w, new DecoupledIO(new Fpu_floatCmp_iss_info)))

    for ( i <- 0 until dpt_w ) {
      iss(i).valid := false.B
      dpt(i).ready := false.B
      iss(i).bits := 0.U.asTypeof( new Fpu_floatCmp_iss_info )

      when( dpt.valid & dpt.bits.isa.is_fun_floatCmp ) {
        dpt(i).ready := iss(i).ready
        iss(i).valid := true.B

        iss(i).bits.is_eq := 
          dpt(i).bits.isa.feq_s | dpt(i).bits.isa.feq_d | dpt(i).bits.isa.fle_s | dpt(i).bits.isa.fle_d
        iss(i).bits.is_lt := dpt(i).bits.isa.flt_s | dpt(i).bits.isa.flt_d
        iss(i).bits.is_gt := dpt(i).bits.isa.fle_s | dpt(i).bits.isa.fle_d
        iss(i).bits.is_min := dpt(i).bits.isa.fmin_s | dpt(i).bits.isa.fmin_d
        iss(i).bits.is_max := dpt(i).bits.isa.fmax_s | dpt(i).bits.isa.fmax_d ) )
        iss(i).bits.is_in_64_32n := dpt(i).is_fun_cmp_d
        iss(i).bits.op1 := io.fpu_iss_fwb(i).op1
        iss(i).bits.op2 := io.fpu_iss_fwb(i).op2
        iss(i).bits.rm  := dpt(i).bits.param.rm
        iss(i).bits.rd0_phy := dpt(i).bits.phy.rd0
      }
    }
    retrun iss 
  }

  def float2int( dpt: Vec[ReadyValidIO[Fpu_dpt_info]] ): Vec[DecoupledIO[Fpu_float2int_iss_info]] = {
    val iss = Wire(Vec( dpt_w, new DecoupledIO(new Fpu_float2int_iss_info)))

    for ( i <- 0 until dpt_w ) {
      iss(i).valid := false.B
      dpt(i).ready := false.B
      iss(i).bits := 0.U.asTypeof( new Fpu_float2int_iss_info )

      when( dpt.valid & dpt.bits.isa.is_fun_float2int ) {
        dpt(i).ready := iss(i).ready
        iss(i).valid := true.B

        iss(i).bits.is_f32toi32 := dpt(i).bits.isa.fcvt_s_w | dpt(i).bits.isa.fcvt_s_wu
        iss(i).bits.is_f32toi64 := dpt(i).bits.isa.fcvt_s_l | dpt(i).bits.isa.fcvt_s_lu
        iss(i).bits.is_f64toi32 := dpt(i).bits.isa.fcvt_d_w | dpt(i).bits.isa.fcvt_d_wu
        iss(i).bits.is_f64toi64 := dpt(i).bits.isa.fcvt_d_l | dpt(i).bits.isa.fcvt_d_lu
        iss(i).bits.is_fmvfto32 := dpt(i).bits.isa.fmv_x_w
        iss(i).bits.is_fmvfto64 := dpt(i).bits.isa.fmv_x_d
        iss(i).bits.is_fclass32 := dpt(i).bits.isa.fclass_s
        iss(i).bits.is_fclass64 := dpt(i).bits.isa.fclass_d
        iss(i).bits.is_usi      := dpt(i).bits.isa.is_usi) )
        iss(i).bits.op1 := io.fpu_iss_fwb(i).op1
        iss(i).bits.rm := dpt(i).bits.param.rm
        iss(i).bits.rd0_phy := dpt(i).bits.phy.rd0

      }
    }
    retrun iss 
  }

  def floatFma( dpt: Vec[ReadyValidIO[Fpu_dpt_info]] ): Vec[DecoupledIO[Fpu_floatFma_iss_info]] = {
    val iss = Wire(Vec( dpt_w, new DecoupledIO(new Fpu_floatFma_iss_info)))

    def op_sel(dptBit: Fpu_dpt_info): UInt = {
      val is_addsub = dptBit.isa.fsub_s | dptBit.isa.fsub_d | dptBit.isa.fadd_s | dptBit.isa.fadd_d
      val is_mul = dptBit.isa.fmul_s | dptBit.isa.fmul_d
      val op = 
        Mux1H(Seq(
          ( dptBit.isa.fmadd_s  | dptBit.isa.fmadd_d | dptBit.isa.fadd_s | dptBit.isa.fadd_d | dptBit.isa.fmul_s | dptBit.isa.fmul_d ) -> 0.U(2.W),          
          ( dptBit.isa.fmsub_s  | dptBit.isa.fmsub_d | dptBit.isa.fsub_s | dptBit.isa.fsub_d                   ) -> 1.U(2.W),
          ( dptBit.isa.fnmsub_s | dptBit.isa.fnmsub_d                                    ) -> 2.U(2.W),
          ( dptBit.isa.fnmadd_s | dptBit.isa.fnmadd_d                                    ) -> 3.U(2.W),
        ))
      Cat( is_addsub, is_mul, op )
    }

    for ( i <- 0 until dpt_w ) {
      iss(i).valid := false.B
      dpt(i).ready := false.B
      iss(i).bits := 0.U.asTypeof( new Fpu_floatFma_iss_info )

      when( dpt.valid & dpt.bits.isa.is_fun_floatFma ) {
        dpt(i).ready := iss(i).ready
        iss(i).valid := true.B

        iss(i).bits.is_64_32n := dpt(i).is_fun_floatFma_d
        iss(i).bits.op_sel := op_sel(dpt(i).bits)
        iss(i).bits.rm := dpt(i).bits.param.rm
        iss(i).bits.op1 := io.fpu_iss_fwb(i).op1
        iss(i).bits.op2 := io.fpu_iss_fwb(i).op2
        iss(i).bits.op3 := io.fpu_iss_fwb(i).op3
        iss(i).bits.rd0_phy := dpt(i).bits.phy.rd0

      }
    }
    retrun iss 
  }

  def floatDivSqrt( dpt: Vec[ReadyValidIO[Fpu_dpt_info]] ): Vec[DecoupledIO[Fpu_floatDivSqrt_iss_info]] = {
    val iss = Wire(Vec( dpt_w, new DecoupledIO(new Fpu_floatDivSqrt_iss_info)))

    for ( i <- 0 until dpt_w ) {
      iss(i).valid := false.B
      dpt(i).ready := false.B
      iss(i).bits := 0.U.asTypeof( new Fpu_floatDivSqrt_iss_info )

      when( dpt.valid & dpt.bits.isa.is_fun_floatDivSqrt ) {
        dpt(i).ready := iss(i).ready
        iss(i).valid := true.B

        iss(i).bits.is_div32 :=  dpt(i).bits.isa.fdiv_s
        iss(i).bits.is_div64 :=  dpt(i).bits.isa.fdiv_d
        iss(i).bits.is_sqrt32 := dpt(i).bits.isa.fsqrt_s
        iss(i).bits.is_sqrt64 := dpt(i).bits.isa.fsqrt_d
        iss(i).bits.op1 := io.fpu_iss_fwb(i).op1
        iss(i).bits.op2 := io.fpu_iss_fwb(i).op2
        iss(i).bits.rm := dpt(i).bits.param.rm
        iss(i).bits.rd0_phy := dpt(i).bits.phy.rd0

      }
    }
    retrun iss 
  }

  def floatCsr( dpt: Vec[ReadyValidIO[Fpu_dpt_info]] ): Vec[DecoupledIO[Fpu_floatCsr_iss_info]] = {
    val iss = Wire(Vec( dpt_w, new DecoupledIO(new Fpu_floatCsr_iss_info)))

    for ( i <- 0 until dpt_w ) {
      iss(i).valid := false.B
      dpt(i).ready := false.B
      iss(i).bits := 0.U.asTypeof( new Fpu_floatCsr_iss_info )

      when( dpt.valid & dpt.bits.isa.is_fun_floatCsr ) {
        dpt(i).ready := iss(i).ready
        iss(i).valid := true.B

        iss(i).bits.is_frw := dpt(i).bits.isa.fcsr_rw
        iss(i).bits.is_frs := dpt(i).bits.isa.fcsr_rs
        iss(i).bits.is_frc := dpt(i).bits.isa.fcsr_rc
        iss(i).bits.data := io.fpu_iss_iwb(i).op1
        iss(i).bits.addr := dpt(i).bits.param.imm
        iss(i).bits.rd0_phy := dpt(i).bits.phy.rd0

      }
    }
    retrun iss 
  }


}