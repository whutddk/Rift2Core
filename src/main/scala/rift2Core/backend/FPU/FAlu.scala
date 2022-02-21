
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

class FAlu() extends Module with HasFPUParameters{
  val io = IO(new Bundle{
    val fpu_iss_exe = Flipped(DecoupledIO(new Fpu_iss_info))
    val fpu_exe_iwb = DecoupledIO(new WriteBack_info(64))
    val fpu_exe_fwb = DecoupledIO(new WriteBack_info(64))

    val flush = Input(Bool())
  })

  val fpu_exe_iwb_fifo = {
    val mdl = Module( new Queue( new WriteBack_info(64), 1, true, false ) )
    mdl.io.deq <> io.fpu_exe_iwb
    mdl
  }
  val fpu_exe_fwb_fifo = {
    val mdl = Module( new Queue( new WriteBack_info(64), 1, true, false ) )
    mdl.io.deq <> io.fpu_exe_fwb
    mdl
  }

  val frm    = RegInit(0.U(3.W))
  val fflags = RegInit(0.U(5.W))
  val fcsr   = Cat( 0.U(24.W),frm, fflags )


  val f2i = {
    val mdl = Module(new FPToInt)
    mdl.io.in := io.fpu_iss_exe.bits

    mdl
  }








  val fcsr_res = WireDefault(0.U(64.W))
  when( fpu_exe_iwb_fifo.io.enq.fire | fpu_exe_fwb_fifo.io.enq.fire ) {
    when( io.fpu_iss_exe.bits.fun.is_fun_fcsr ) {
      fcsr_res := 
        Mux1H(Seq(
          (io.fpu_iss_exe.bits.param.dat.op2 === 1.U) -> fflags,
          (io.fpu_iss_exe.bits.param.dat.op2 === 2.U) -> frm,
          (io.fpu_iss_exe.bits.param.dat.op2 === 3.U) -> fcsr,
        ))

      when( io.fpu_iss_exe.bits.param.dat.op2 === 1.U ) {
        fflags := Mux1H(Seq(
          io.fpu_iss_exe.bits.fun.is_fun_frw -> (          io.fpu_iss_exe.bits.param.dat.op1),
          io.fpu_iss_exe.bits.fun.is_fun_frs -> (fflags |  io.fpu_iss_exe.bits.param.dat.op1),
          io.fpu_iss_exe.bits.fun.is_fun_frc -> (fflags & ~io.fpu_iss_exe.bits.param.dat.op1),
        ))
      } .elsewhen( io.fpu_iss_exe.bits.param.dat.op2 === 2.U ) {
        frm := Mux1H(Seq(
          io.fpu_iss_exe.bits.fun.is_fun_frw -> (       io.fpu_iss_exe.bits.param.dat.op1),
          io.fpu_iss_exe.bits.fun.is_fun_frs -> (frm |  io.fpu_iss_exe.bits.param.dat.op1),
          io.fpu_iss_exe.bits.fun.is_fun_frc -> (frm & ~io.fpu_iss_exe.bits.param.dat.op1),
        ))
      } .elsewhen( io.fpu_iss_exe.bits.param.dat.op2 === 3.U ) {
        fflags := Mux1H(Seq(
          io.fpu_iss_exe.bits.fun.is_fun_frw -> (          io.fpu_iss_exe.bits.param.dat.op1(4,0)),
          io.fpu_iss_exe.bits.fun.is_fun_frs -> (fflags |  io.fpu_iss_exe.bits.param.dat.op1(4,0)),
          io.fpu_iss_exe.bits.fun.is_fun_frc -> (fflags & ~io.fpu_iss_exe.bits.param.dat.op1(4,0)),
        ))

        frm := Mux1H(Seq(
          io.fpu_iss_exe.bits.fun.is_fun_frw -> (       io.fpu_iss_exe.bits.param.dat.op1(7,5)),
          io.fpu_iss_exe.bits.fun.is_fun_frs -> (frm |  io.fpu_iss_exe.bits.param.dat.op1(7,5)),
          io.fpu_iss_exe.bits.fun.is_fun_frc -> (frm & ~io.fpu_iss_exe.bits.param.dat.op1(7,5)),
        ))
      } .otherwise {
        assert( false.B, "Assert Failed at fpu exe, invalid csr address req" )
      }
    } .otherwise {
      val fflags_dnxt =
        0.U(5.W) |
        f2i.io.out.exc
      fflags := fflags | fflags_dnxt(4,0)
    }
  }


  fpu_exe_iwb_fifo.io.enq.valid := io.fpu_iss_exe.valid & io.fpu_iss_exe.bits.fun.is_iwb
  fpu_exe_iwb_fifo.io.enq.bits.res :=
    Mux1H(Seq(
      io.fpu_iss_exe.bits.fun.is_fun_fcsr  -> fcsr_res,
      io.fpu_iss_exe.bits.fun.is_fun_class -> f2i.io.out.toint,
      io.fpu_iss_exe.bits.fun.is_fun_fcvtX -> f2i.io.out.toint,
      // io.fpu_iss_exe.bits.fun.is_fun_cmp   -> f2i.io.out.lt,
    ))
  fpu_exe_iwb_fifo.io.enq.bits.viewAsSupertype(new Rd_Param(64)) := io.fpu_iss_exe.bits.param.viewAsSupertype(new Rd_Param(64))
  // fpu_exe_iwb_fifo.io.enq.bits.is_iwb := true.B
  // fpu_exe_iwb_fifo.io.enq.bits.is_fwb := false.B
  // fpu_exe_iwb_fifo.io.enq.bits.rd0 := io.fpu_iss_exe.bits.


  fpu_exe_fwb_fifo.io.enq.valid := false.B//io.fpu_iss_exe.valid & io.fpu_iss_exe.fun.is_fwb
  fpu_exe_fwb_fifo.io.enq.bits.res := 0.U
  fpu_exe_fwb_fifo.io.enq.bits.viewAsSupertype(new Rd_Param(64)) := io.fpu_iss_exe.bits.param.viewAsSupertype(new Rd_Param(64))


}


// class FAlu() extends Module  with FPU_util {//with ShouldBeRetimed
//   val io = new Bundle {

//     val int2float_iss_exe   = Flipped(new DecoupledIO(new Fpu_int2float_iss_info))
//     val float2float_iss_exe = Flipped(new DecoupledIO(new Fpu_float2float_iss_info))
//     val floatCmp_iss_exe = Flipped(new DecoupledIO(new Fpu_floatCmp_iss_info))
//     val float2int_iss_exe = Flipped(new DecoupledIO(new Fpu_float2int_iss_info))
//     val floatFma_iss_exe = Flipped(new DecoupledIO(new Fpu_floatFma_iss_info))
//     val floatDivSqrt_iss_exe = Flipped(new DecoupledIO(new Fpu_floatDivSqrt_iss_info))

//     val floatCsr_iss_exe = Flipped(new DecoupledIO(new Fpu_floatCsr_iss_info))
    


//     val fpu_exe_iwb = new DecoupledIO(new Exe_iwb_info)
//     val fpu_exe_fwb = Vec(2, new ValidIO(new Exe_fwb_info))

//     val flush = Input(Bool())

//   }

//   val iwb_arb = Module(new Arbiter(new Exe_iwb_info, 3))
//   val fwb_arb = for ( i <- 0 until 1 ) yield { val mdl = Module(new Arbiter(new Exe_fwb_info, 3)); mdl }

//   val exceptionFlags_out = Wire( Vec(3, UInt(5.W)) )




//   for ( i <- 0 until 1 ) yield {
//     io.fpu_exe_fwb(i).bits  := fwb_arb(i).io.out.bits
//     io.fpu_exe_fwb(i).valid := fwb_arb(i).io.out.valid
//     fwb_arb(i).io.out.ready := true.B
//   }

//   io.fpu_exe_iwb.bits <> iwb_arb.io.out



//   val frm    = RegInit(0.U(3.W))
//   val fflags = RegInit(0.U(5.W))
//   val fcsr   = Cat( 0.U(24.W),frm, fflags )
  
//   iwb_arb.io.in(0).valid := false.B
//   iwb_arb.io.in(0).bits.res := 0.U
//   iwb_arb.io.in(0).bits.rd0_phy := 0.U
//   io.floatCsr_iss_exe.ready := iwb_arb.io.in(0).ready
//   when( io.floatCsr_iss_exe.valid ) {

//     assert(~io.int2float_iss_exe.valid)
//     assert(~io.float2float_iss_exe.valid)
//     assert(~io.floatCmp_iss_exe.valid)
//     assert(~io.float2int_iss_exe.valid)
//     assert(~io.floatFma_iss_exe.valid)
//     assert(~io.floatDivSqrt_iss_exe.valid)

//     iwb_arb.io.in(0).valid := true.B
//     iwb_arb.io.in(0).bits.rd0_phy := io.floatCsr_iss_exe.bits.rd0_phy

//     when( io.floatCsr_iss_exe.bits.addr === 1.U ) {
//       iwb_arb.io.in(0).bits.res := fflags
//       fflags := Mux1H(Seq(
//         io.floatCsr_iss_exe.bits.is_frw -> (          io.floatCsr_iss_exe.bits.data),
//         io.floatCsr_iss_exe.bits.is_frs -> (fflags |  io.floatCsr_iss_exe.bits.data),
//         io.floatCsr_iss_exe.bits.is_frc -> (fflags & ~io.floatCsr_iss_exe.bits.data),
//       ))

//     }
//     .elsewhen( io.floatCsr_iss_exe.bits.addr === 2.U ) {
//       iwb_arb.io.in(0).bits.res := frm
//       frm := Mux1H(Seq(
//         io.floatCsr_iss_exe.bits.is_frw -> (       io.floatCsr_iss_exe.bits.data),
//         io.floatCsr_iss_exe.bits.is_frs -> (frm |  io.floatCsr_iss_exe.bits.data),
//         io.floatCsr_iss_exe.bits.is_frc -> (frm & ~io.floatCsr_iss_exe.bits.data),
//       ))
//     }
//     .elsewhen( io.floatCsr_iss_exe.bits.addr === 3.U ) {
//       iwb_arb.io.in(0).bits.res := fcsr
//       fcsr := Mux1H(Seq(
//         io.floatCsr_iss_exe.bits.is_frw -> (        io.floatCsr_iss_exe.bits.data),
//         io.floatCsr_iss_exe.bits.is_frs -> (fcsr |  io.floatCsr_iss_exe.bits.data),
//         io.floatCsr_iss_exe.bits.is_frc -> (fcsr & ~io.floatCsr_iss_exe.bits.data),
//       ))
//     }
//     .otherwise {
//       assert( false, "Assert Failed at fpu exe, invalid csr address req" )
//     }
//   }
//   .otherwise {
//     val fcsr_dnxt =
//       0.U(8.W) |
//       Mux(iwb_arb.io.out.fire,    exceptionFlags_out(2), 0.U ) | 
//       Mux(fwb_arb(0).io.out.fire, exceptionFlags_out(0), 0.U ) | 
//       Mux(fwb_arb(1).io.out.fire, exceptionFlags_out(1), 0.U )
//     frm    := frm | fcsr_dnxt(7,5)
//     fflags := fflags | fcsr_dnxt(4,0)
//   }


//   def is_f32toi32_lp = ~( io.float2int_iss_exe.valid & io.float2int_iss_exe.bits.is_f32toi32 )
//   def is_f32toi64_lp = ~( io.float2int_iss_exe.valid & io.float2int_iss_exe.bits.is_f32toi64 )
//   def is_f64toi32_lp = ~( io.float2int_iss_exe.valid & io.float2int_iss_exe.bits.is_f64toi32 )
//   def is_f64toi64_lp = ~( io.float2int_iss_exe.valid & io.float2int_iss_exe.bits.is_f64toi64 )
//   def is_fmvfto32_lp = ~( io.float2int_iss_exe.valid & io.float2int_iss_exe.bits.is_fmvfto32 )
//   def is_fmvfto64_lp = ~( io.float2int_iss_exe.valid & io.float2int_iss_exe.bits.is_fmvfto64 )
//   def is_fclass32_lp = ~( io.float2int_iss_exe.valid & io.float2int_iss_exe.bits.is_fclass32 )
//   def is_fclass64_lp = ~( io.float2int_iss_exe.valid & io.float2int_iss_exe.bits.is_fclass64 )

//   val f32toi32 = FloatS2Int32( io.float2int_iss_exe.bits.op1, io.float2int_iss_exe.bits.rm, io.float2int_iss_exe.bits.is_usi, is_f32toi32_lp )
//   val f32toi64 = FloatS2Int64( io.float2int_iss_exe.bits.op1, io.float2int_iss_exe.bits.rm, io.float2int_iss_exe.bits.is_usi, is_f32toi64_lp )
//   val f64toi32 = FloatD2Int32( io.float2int_iss_exe.bits.op1, io.float2int_iss_exe.bits.rm, io.float2int_iss_exe.bits.is_usi, is_f64toi32_lp )
//   val f64toi64 = FloatD2Int64( io.float2int_iss_exe.bits.op1, io.float2int_iss_exe.bits.rm, io.float2int_iss_exe.bits.is_usi, is_f64toi64_lp )
//   val fmvfto32 = Fmv_x_w     ( io.float2int_iss_exe.bits.op1, is_fmvfto32_lp )
//   val fmvfto64 = Fmv_x_d     ( io.float2int_iss_exe.bits.op1, is_fmvfto64_lp )
//   val fclass32 = Fclass32    ( io.float2int_iss_exe.bits.op1, is_fclass32_lp )
//   val fclass64 = Fclass64    ( io.float2int_iss_exe.bits.op1, is_fclass64_lp )

//   iwb_arb.io.in(1).valid := io.float2int_iss_exe.valid
//   iwb_arb.io.in(1).bits.res :=
//     Mux1H(Seq(
//       ~is_f32toi32_lp -> f32toi32.res,
//       ~is_f32toi64_lp -> f32toi64.res,
//       ~is_f64toi32_lp -> f64toi32.res,
//       ~is_f64toi64_lp -> f64toi64.res,
//       ~is_fmvfto32_lp -> fmvfto32.res,
//       ~is_fmvfto64_lp -> fmvfto64.res,
//       ~is_fclass32_lp -> fclass32.res,
//       ~is_fclass64_lp -> fclass64.res,
//     ))

//   iwb_arb.io.in(1).bits.rd0_phy := io.float2int_iss_exe.bits.rd0_phy
//   io.float2int_iss_exe.ready := iwb_arb.io.in(1).ready


//   def is_i32tof32_lp = ~(io.int2float_iss_exe.valid & io.int2float_iss_exe.bits.is_i32tof32)
//   def is_i64tof32_lp = ~(io.int2float_iss_exe.valid & io.int2float_iss_exe.bits.is_i64tof32)
//   def is_i32tof64_lp = ~(io.int2float_iss_exe.valid & io.int2float_iss_exe.bits.is_i32tof64)
//   def is_i64tof64_lp = ~(io.int2float_iss_exe.valid & io.int2float_iss_exe.bits.is_i64tof64)
//   def is_fmv32tof_lp = ~(io.int2float_iss_exe.valid & io.int2float_iss_exe.bits.is_fmv32tof)
//   def is_fmv64tof_lp = ~(io.int2float_iss_exe.valid & io.int2float_iss_exe.bits.is_fmv64tof)

//   val i32tof32 = Int322FloatS( io.int2float_iss_exe.bits.op1, io.int2float_iss_exe.bits.rm, io.int2float_iss_exe.bits.is_usi, is_i32tof32_lp )
//   val i64tof32 = Int642FloatS( io.int2float_iss_exe.bits.op1, io.int2float_iss_exe.bits.rm, io.int2float_iss_exe.bits.is_usi, is_i64tof32_lp )
//   val i32tof64 = Int322FloatD( io.int2float_iss_exe.bits.op1, io.int2float_iss_exe.bits.rm, io.int2float_iss_exe.bits.is_usi, is_i32tof64_lp )
//   val i64tof64 = Int642FloatD( io.int2float_iss_exe.bits.op1, io.int2float_iss_exe.bits.rm, io.int2float_iss_exe.bits.is_usi, is_i64tof64_lp )
//   val fmv32tof = Fmv_w_x     ( io.int2float_iss_exe.bits.op1, is_fmv32tof_lp )
//   val fmv64tof = Fmv_d_x     ( io.int2float_iss_exe.bits.op1, is_fmv64tof_lp )
//     fwb_arb(1).io.in(1).valid                   := io.int2float_iss_exe.valid
//     fwb_arb(1).io.in(1).bits.res            := 
//       Mux1H(Seq(
//         is_i32tof32_lp -> ieee32(i32tof32.res),
//         is_i64tof32_lp -> ieee32(i64tof32.res),
//         is_i32tof64_lp -> ieee64(i32tof64.res),
//         is_i64tof64_lp -> ieee64(i64tof64.res),
//         is_fmv32tof_lp -> fmv32tof.res,
//         is_fmv64tof_lp -> fmv64tof.res,
//       ))



//     fwb_arb(1).io.in(1).bits.rd0_phy := io.int2float_iss_exe.bits.rd0_phy
//     io.int2float_iss_exe.ready := fwb_arb(1).io.in(1).ready


//     def is_f32tof64_lp = ~( io.float2float_iss_exe.valid & io.float2float_iss_exe.bits.is_f32tof64 )
//     def is_f64tof32_lp = ~( io.float2float_iss_exe.valid & io.float2float_iss_exe.bits.is_f64tof32 )
//     def is_fSign32_lp  = ~( io.float2float_iss_exe.valid & io.float2float_iss_exe.bits.is_fSign32  )
//     def is_fSign64_lp  = ~( io.float2float_iss_exe.valid & io.float2float_iss_exe.bits.is_fSign64  )

//     val f32tof64 = FloatS2FloatD( io.float2float_iss_exe.bits.op1, io.float2float_iss_exe.bits.rm, is_f32tof64_lp )
//     val f64tof32 = FloatD2FloatS( io.float2float_iss_exe.bits.op1, io.float2float_iss_exe.bits.rm, is_f64tof32_lp )
//     val fSign32  = FloatSign32  ( io.float2float_iss_exe.bits.op1, io.float2float_iss_exe.bits.op2, io.float2float_iss_exe.bits.rm, is_fSign32_lp )
//     val fSign64  = FloatSign64  ( io.float2float_iss_exe.bits.op1, io.float2float_iss_exe.bits.op2, io.float2float_iss_exe.bits.rm, is_fSign64_lp )
//       fwb_arb(0).io.in(2).valid                   := io.float2float_iss_exe.valid
//       fwb_arb(0).io.in(2).bits.res            := 
//         Mux1H(Seq(
//           ~is_f32tof64_lp -> ieee64(f32tof64.res),
//           ~is_f64tof32_lp -> ieee32(f64tof32.res),
//           ~is_fSign32_lp  -> ieee32(fSign32.res),
//           ~is_fSign64_lp  -> ieee64(fSign64.res),        
//         ))

//       fwb_arb(0).io.in(2).bits.rd0_phy := io.float2float_iss_exe.bits.rd0_phy
//       io.float2float_iss_exe.ready := fwb_arb(0).io.in(2).ready




//   def is_fCmp32_lp = ~(io.floatCmp_iss_exe.valid & ~io.floatCmp_iss_exe.bits.is_in_64_32n)
//   def is_fCmp64_lp = ~(io.floatCmp_iss_exe.valid &  io.floatCmp_iss_exe.bits.is_in_64_32n)
//   def is_int_rtn   = io.floatCmp_iss_exe.bits.is_eq | io.floatCmp_iss_exe.bits.is_lt | io.floatCmp_iss_exe.bits.is_gt
//   def is_float_rtn = io.floatCmp_iss_exe.bits.is_min | io.floatCmp_iss_exe.bits.is_max

//   val fCmp32 = FloatSCmp_Res( io.floatCmp_iss_exe.bits.is_eq, io.floatCmp_iss_exe.bits.is_lt, io.floatCmp_iss_exe.bits.is_gt, io.floatCmp_iss_exe.bits.is_min, io.floatCmp_iss_exe.bits.is_max, io.floatCmp_iss_exe.bits.op1, io.floatCmp_iss_exe.bits.op2, io.floatCmp_iss_exe.bits.rm, is_fCmp32_lp )
//   val fCmp64 = FloatDCmp_Res( io.floatCmp_iss_exe.bits.is_eq, io.floatCmp_iss_exe.bits.is_lt, io.floatCmp_iss_exe.bits.is_gt, io.floatCmp_iss_exe.bits.is_min, io.floatCmp_iss_exe.bits.is_max, io.floatCmp_iss_exe.bits.op1, io.floatCmp_iss_exe.bits.op2, io.floatCmp_iss_exe.bits.rm, is_fCmp64_lp )
//     fwb_arb(1).io.in(2).valid                   := io.floatCmp_iss_exe.valid & is_float_rtn
//     fwb_arb(1).io.in(2).bits.res            := 
//       Mux1H(Seq(
//         ( ~is_fCmp32_lp & is_float_rtn ) -> ieee32(fCmp32.res),
//         ( ~is_fCmp64_lp & is_float_rtn ) -> ieee64(fCmp64.res),
//       ))

//     fwb_arb(1).io.in(2).bits.rd0_phy := Mux( is_float_rtn, io.floatCmp_iss_exe.bits.rd0_phy, 0.U )

//     iwb_arb.io.in(2).valid := io.floatCmp_iss_exe.valid & is_int_rtn
//     iwb_arb.io.in(2).bits.res := 
//       Mux1H(Seq(
//         ( ~is_fCmp32_lp & is_int_rtn ) -> fCmp32.res(63,0),
//         ( ~is_fCmp64_lp & is_int_rtn ) -> fCmp64.res(63,0),
//       ))
//     iwb_arb.io.in(2).bits.rd0_phy := Mux( is_int_rtn, io.floatCmp_iss_exe.bits.rd0_phy, 0.U )

//     io.floatCmp_iss_exe.ready := 
//       io.floatCmp_iss_exe.valid & 
//       Mux1H(Seq(
//         is_float_rtn -> fwb_arb(1).io.in(2).ready,
//         is_int_rtn   -> iwb_arb.io.in(2).ready,
//       ))

//   def is_fma32 = io.floatFma_iss_exe.valid & ~io.floatFma_iss_exe.bits.is_64_32n
//   def is_fma64 = io.floatFma_iss_exe.valid &  io.floatFma_iss_exe.bits.is_64_32n
//   io.floatFma_iss_exe.ready := true.B

//   val fma32_res = fma32( is_fma32, io.floatFma_iss_exe.bits.op_sel, io.floatFma_iss_exe.bits.rm, io.floatFma_iss_exe.bits.op1, io.floatFma_iss_exe.bits.op2, io.floatFma_iss_exe.bits.op3, io.floatFma_iss_exe.bits.rd0_phy)
//     fwb_arb(0).io.in(0).valid               := fma32_res._1.valid
//     fwb_arb(0).io.in(0).bits.res            := ieee32(fma32_res._1.bits.res)

//     fwb_arb(0).io.in(0).bits.rd0_phy        := fma32_res._2
//     DontCare := fwb_arb(0).io.in(0).ready

//   val fma64_res = fma64( is_fma64, io.floatFma_iss_exe.bits.op_sel, io.floatFma_iss_exe.bits.rm, io.floatFma_iss_exe.bits.op1, io.floatFma_iss_exe.bits.op2, io.floatFma_iss_exe.bits.op3, io.floatFma_iss_exe.bits.rd0_phy)
//     fwb_arb(1).io.in(0).valid               := fma64_res._1.valid
//     fwb_arb(1).io.in(0).bits.res            := ieee64(fma64_res._1.bits.res)

//     fwb_arb(1).io.in(0).bits.rd0_phy        := fma64_res._2
//     DontCare := fwb_arb(1).io.in(0).ready

  



//   def is_div32  = io.floatDivSqrt_iss_exe.valid & io.floatDivSqrt_iss_exe.bits.is_div32
//   def is_div64  = io.floatDivSqrt_iss_exe.valid & io.floatDivSqrt_iss_exe.bits.is_div64
//   def is_sqrt32 = io.floatDivSqrt_iss_exe.valid & io.floatDivSqrt_iss_exe.bits.is_sqrt32
//   def is_sqrt64 = io.floatDivSqrt_iss_exe.valid & io.floatDivSqrt_iss_exe.bits.is_sqrt64

//   val divSqrt32 = FloatDivSqrt32( is_sqrt32, is_div32, io.floatDivSqrt_iss_exe.bits.op1, io.floatDivSqrt_iss_exe.bits.op2, io.floatDivSqrt_iss_exe.bits.rm )
//   val divSqrt64 = FloatDivSqrt64( is_sqrt64, is_div64, io.floatDivSqrt_iss_exe.bits.op1, io.floatDivSqrt_iss_exe.bits.op2, io.floatDivSqrt_iss_exe.bits.rm )
//     fwb_arb(0).io.in(1).valid :=
//       Mux1H(Seq(
//         (is_div32|is_sqrt32) -> divSqrt32.valid,
//         (is_div64|is_sqrt64) -> divSqrt64.valid,
//       ))

//     fwb_arb(0).io.in(1).bits.res :=
//       Mux1H(Seq(
//         (is_div32|is_sqrt32) -> ieee32(divSqrt32.bits.res),
//         (is_div64|is_sqrt64) -> ieee64(divSqrt64.bits.res),
//       ))



//     fwb_arb(0).io.in(1).bits.rd0_phy := io.floatDivSqrt_iss_exe.bits.rd0_phy
//     io.floatDivSqrt_iss_exe.ready := fwb_arb(0).io.in(1).ready


//   exceptionFlags_out(0) :=
//     Mux1H(Seq(
//       fwb_arb(0).io.in(0).fire -> fma32_res._1.bits.exceptionFlags,
//       fwb_arb(0).io.in(1).fire -> 
//         Mux1H(Seq(
//           (is_div32|is_sqrt32) -> divSqrt32.bits.exceptionFlags,
//           (is_div64|is_sqrt64) -> divSqrt32.bits.exceptionFlags,
//         )),
//       fwb_arb(1).io.in(2).fire -> 
//         Mux1H(Seq(
//           ( ~is_fCmp32_lp & is_float_rtn ) -> fCmp32.exceptionFlags,
//           ( ~is_fCmp64_lp & is_float_rtn ) -> fCmp64.exceptionFlags,
//         )),
//     ))

//   exceptionFlags_out(1) :=
//     Mux1H(Seq(
//       fwb_arb(1).io.in(0).fire -> fma64_res._1.bits.exceptionFlags,
//       fwb_arb(1).io.in(1).fire -> 
//         Mux1H(Seq(
//           is_i32tof32_lp -> i32tof32.exceptionFlags,
//           is_i64tof32_lp -> i64tof32.exceptionFlags,
//           is_i32tof64_lp -> i32tof64.exceptionFlags,
//           is_i64tof64_lp -> i64tof64.exceptionFlags,
//           is_fmv32tof_lp -> fmv32tof.exceptionFlags,
//           is_fmv64tof_lp -> fmv64tof.exceptionFlags,
//         )),
//       fwb_arb(0).io.in(2).fire -> 
//         Mux1H(Seq(
//           ~is_f32tof64_lp -> f32tof64.exceptionFlags,
//           ~is_f64tof32_lp -> f64tof32.exceptionFlags,
//           ~is_fSign32_lp  -> fSign32.exceptionFlags,
//           ~is_fSign64_lp  -> fSign64.exceptionFlags,
//         )),
//     ))

//   exceptionFlags_out(2) :=
//     Mux1H(Seq(
//       ~is_f32toi32_lp -> f32toi32.exceptionFlags,
//       ~is_f32toi64_lp -> f32toi64.exceptionFlags,
//       ~is_f64toi32_lp -> f64toi32.exceptionFlags,
//       ~is_f64toi64_lp -> f64toi64.exceptionFlags,
//       ~is_fmvfto32_lp -> fmvfto32.exceptionFlags,
//       ~is_fmvfto64_lp -> fmvfto64.exceptionFlags,
//       ~is_fclass32_lp -> fclass32.exceptionFlags,
//       ~is_fclass64_lp -> fclass64.exceptionFlags,
//     ))





// }





