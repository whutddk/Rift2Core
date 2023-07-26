

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

package rift2Core.backend


import chisel3._
import chisel3.util._
import rift2Core.define._
import rift2Core.privilege._

import rift2Chip._
import org.chipsalliance.cde.config._


abstract class CSRBase(implicit p: Parameters) extends RiftModule {

  class CsrIO extends Bundle {

    val csr_iss_exe = Flipped(new DecoupledIO(new Csr_iss_info))
    val csr_exe_iwb = new DecoupledIO(new WriteBack_info(dw=64))
    val xpuCsrWriteBack = Valid(new Exe_Port)

    val flush = Input(Bool())    
  }

  val io: CsrIO = IO(new CsrIO)

  val csr_exe_iwb_fifo = Module( new Queue( new WriteBack_info(dw=64), 1, true, false ) )
  io.csr_exe_iwb <> csr_exe_iwb_fifo.io.deq
  csr_exe_iwb_fifo.reset := reset.asBool | io.flush


  val rw = io.csr_iss_exe.bits.fun.rw
  val rs = io.csr_iss_exe.bits.fun.rs
  val rc = io.csr_iss_exe.bits.fun.rc
  
  val dat = io.csr_iss_exe.bits.param.dat.op1
  val addr = io.csr_iss_exe.bits.param.dat.op3(11,0)

  val dontWrite = (dat === 0.U) & ( rs | rc )


  io.csr_iss_exe.ready     := csr_exe_iwb_fifo.io.enq.fire



}

trait VConfig{ this: CSRBase =>
  // when( addr === "hC20".U ){ //vl read-only from csr
  //   csr_exe_iwb_fifo.io.enq.bits.res := io.csr_iss_exe.bits.param.dat.op2(62,8) //VConfig(62,8) 
  //   require( (log2Ceil(vParams.vlmax) + 8 + 1) <= 64 )

  //   assert( io.xpuCsrWriteBack.bits.op_rw === false.B & io.xpuCsrWriteBack.bits.op_rs === false.B & io.xpuCsrWriteBack.bits.op_rc === false.B )
  // }

  // when( addr === "hC21".U ){ //vtype
  //   csr_exe_iwb_fifo.io.enq.bits.res := Cat( io.csr_iss_exe.bits.param.dat.op2.extract(63), 0.U(55.W), io.csr_iss_exe.bits.param.dat.op2(7,0))
  //   require( (log2Ceil(vParams.vlmax) + 8 + 1) <= 64 )

  //   assert( io.xpuCsrWriteBack.bits.op_rw === false.B & io.xpuCsrWriteBack.bits.op_rs === false.B & io.xpuCsrWriteBack.bits.op_rc === false.B )
  // }
  val avl = Wire( UInt((log2Ceil(vParams.vlmax)+1).W) )
  val nvl = Wire( UInt((log2Ceil(vParams.vlmax)+1).W) )
  val vill = Wire(Bool())
  val vtype = Wire(UInt(8.W))

  val vsew  = io.csr_iss_exe.bits.param.dat.op2(5,3)
  val vlmul = io.csr_iss_exe.bits.param.dat.op2(2,0)

  val realVlmax =
    Mux1H( Seq(
      (vsew === "b000".U & vlmul === "b101".U) -> ((vParams.vlen / 8 / 8) max 0).U,
      (vsew === "b000".U & vlmul === "b110".U) -> ((vParams.vlen / 8 / 4) max 0).U,
      (vsew === "b000".U & vlmul === "b111".U) -> ((vParams.vlen / 8 / 2) max 0).U,
      (vsew === "b000".U & vlmul === "b000".U) -> ((vParams.vlen / 8 / 1) max 0).U,
      (vsew === "b000".U & vlmul === "b001".U) -> ((vParams.vlen / 8 * 2) max 0).U,
      (vsew === "b000".U & vlmul === "b010".U) -> ((vParams.vlen / 8 * 4) max 0).U,
      (vsew === "b000".U & vlmul === "b011".U) -> ((vParams.vlen / 8 * 8) max 0).U,

      (vsew === "b001".U & vlmul === "b101".U) -> ((vParams.vlen /16 / 8) max 0).U,
      (vsew === "b001".U & vlmul === "b110".U) -> ((vParams.vlen /16 / 4) max 0).U,
      (vsew === "b001".U & vlmul === "b111".U) -> ((vParams.vlen /16 / 2) max 0).U,
      (vsew === "b001".U & vlmul === "b000".U) -> ((vParams.vlen /16 / 1) max 0).U,
      (vsew === "b001".U & vlmul === "b001".U) -> ((vParams.vlen /16 * 2) max 0).U,
      (vsew === "b001".U & vlmul === "b010".U) -> ((vParams.vlen /16 * 4) max 0).U,
      (vsew === "b001".U & vlmul === "b011".U) -> ((vParams.vlen /16 * 8) max 0).U,

      (vsew === "b010".U & vlmul === "b101".U) -> ((vParams.vlen /32 / 8) max 0 ).U,
      (vsew === "b010".U & vlmul === "b110".U) -> ((vParams.vlen /32 / 4) max 0 ).U,
      (vsew === "b010".U & vlmul === "b111".U) -> ((vParams.vlen /32 / 2) max 0 ).U,
      (vsew === "b010".U & vlmul === "b000".U) -> ((vParams.vlen /32 / 1) max 0 ).U,
      (vsew === "b010".U & vlmul === "b001".U) -> ((vParams.vlen /32 * 2) max 0 ).U,
      (vsew === "b010".U & vlmul === "b010".U) -> ((vParams.vlen /32 * 4) max 0 ).U,
      (vsew === "b010".U & vlmul === "b011".U) -> ((vParams.vlen /32 * 8) max 0 ).U,

      (vsew === "b011".U & vlmul === "b101".U) -> ((vParams.vlen /64 / 8) max 0 ).U,
      (vsew === "b011".U & vlmul === "b110".U) -> ((vParams.vlen /64 / 4) max 0 ).U,
      (vsew === "b011".U & vlmul === "b111".U) -> ((vParams.vlen /64 / 2) max 0 ).U,
      (vsew === "b011".U & vlmul === "b000".U) -> ((vParams.vlen /64 / 1) max 0 ).U,
      (vsew === "b011".U & vlmul === "b001".U) -> ((vParams.vlen /64 * 2) max 0 ).U,
      (vsew === "b011".U & vlmul === "b010".U) -> ((vParams.vlen /64 * 4) max 0 ).U,
      (vsew === "b011".U & vlmul === "b011".U) -> ((vParams.vlen /64 * 8) max 0 ).U,
    ) )
    // (vParams.vlen / minEEW * maxMUL) -1
    // Mux( vlmul.extract(2) === 0.U, )
    // Mux1H(Seq(

    // )) 

  avl := io.csr_iss_exe.bits.param.dat.op1 //reFactor in Issue
    // Mux1H(Seq(
    //   io.csr_iss_exe.bits.fun.vsetvli  -> Mux( io.csr_iss_exe.bits.param.dat.op1 =/= 0.U, io.csr_iss_exe.bits.param.dat.op1, Mux(io.csr_iss_exe.bits.param.rd0 =/= 0.U, Fill(vParams.vlmax,1.U), io.csr_iss_exe.bits.param.dat.op3(12+log2Ceil(vParams.vlmax)-1, 12)) ),
    //   io.csr_iss_exe.bits.fun.vsetivli -> io.csr_iss_exe.bits.param.dat.op1,
    //   io.csr_iss_exe.bits.fun.vsetvl   -> Mux( io.csr_iss_exe.bits.param.dat.op1 =/= 0.U, io.csr_iss_exe.bits.param.dat.op1, Mux(io.csr_iss_exe.bits.param.rd0 =/= 0.U, Fill(vParams.vlmax,1.U), io.csr_iss_exe.bits.param.dat.op3(12+log2Ceil(vParams.vlmax)-1, 12)) ),
    // ))



  vill :=
    ( io.csr_iss_exe.bits.param.dat.op2.extract(63) === 1.U ) |
    (vsew.extract(2) === true.B) |
    (vlmul === "b100".U) |
    (vlmul === "b101".U & (
        (if( vParams.elen/8 < 64 ) { vsew === "b011".U } else {false.B}) |
        (if( vParams.elen/8 < 32 ) { vsew === "b010".U } else {false.B}) |
        (if( vParams.elen/8 < 16 ) { vsew === "b001".U } else {false.B}) |
        (if( vParams.elen/8 < 8  ) { vsew === "b000".U } else {false.B})
      )
    ) |
    (vlmul === "b110".U & (
        (if( vParams.elen/4 < 64 ) { vsew === "b011".U } else {false.B}) |
        (if( vParams.elen/4 < 32 ) { vsew === "b010".U } else {false.B}) |
        (if( vParams.elen/4 < 16 ) { vsew === "b001".U } else {false.B}) |
        (if( vParams.elen/4 < 8  ) { vsew === "b000".U } else {false.B})
      )
    ) |
    (vlmul === "b111".U & (
        (if( vParams.elen/2 < 64 ) { vsew === "b011".U } else {false.B}) |
        (if( vParams.elen/2 < 32 ) { vsew === "b010".U } else {false.B}) |
        (if( vParams.elen/2 < 16 ) { vsew === "b001".U } else {false.B}) |
        (if( vParams.elen/2 < 8  ) { vsew === "b000".U } else {false.B})
      )
    )

  vtype := Mux( vill, 0.U, io.csr_iss_exe.bits.param.dat.op2 )
  nvl   := Mux( vill, 0.U, Mux( avl < realVlmax, avl, realVlmax ) )


} 


class CSR(implicit p: Parameters) extends CSRBase 
with VConfig{
  csr_exe_iwb_fifo.io.enq.valid := io.csr_iss_exe.valid

  csr_exe_iwb_fifo.io.enq.bits.res :=
    Mux1H(Seq(
      (io.csr_iss_exe.bits.fun.rw      | io.csr_iss_exe.bits.fun.rs       | io.csr_iss_exe.bits.fun.rc)     -> io.csr_iss_exe.bits.param.dat.op2,
      (io.csr_iss_exe.bits.fun.vsetvli | io.csr_iss_exe.bits.fun.vsetivli | io.csr_iss_exe.bits.fun.vsetvl) -> nvl,
    ))
    
  csr_exe_iwb_fifo.io.enq.bits.rd0 := io.csr_iss_exe.bits.param.rd0

  io.xpuCsrWriteBack.valid := csr_exe_iwb_fifo.io.enq.fire
  io.xpuCsrWriteBack.bits.addr  :=
    Mux1H(Seq(
      (io.csr_iss_exe.bits.fun.rw      | io.csr_iss_exe.bits.fun.rs       | io.csr_iss_exe.bits.fun.rc)     -> addr,
      (io.csr_iss_exe.bits.fun.vsetvli | io.csr_iss_exe.bits.fun.vsetivli | io.csr_iss_exe.bits.fun.vsetvl) -> "hFFE".U,
    ))
    
  io.xpuCsrWriteBack.bits.dat_i  := 
    Mux1H(Seq(
      (io.csr_iss_exe.bits.fun.rw      | io.csr_iss_exe.bits.fun.rs       | io.csr_iss_exe.bits.fun.rc)     -> dat,
      (io.csr_iss_exe.bits.fun.vsetvli | io.csr_iss_exe.bits.fun.vsetivli | io.csr_iss_exe.bits.fun.vsetvl) -> Cat( vill, nvl | 0.U((64-1-8).W), vtype ),
    ))
  io.xpuCsrWriteBack.bits.op_rw := 
    Mux1H(Seq(
      (io.csr_iss_exe.bits.fun.rw      | io.csr_iss_exe.bits.fun.rs       | io.csr_iss_exe.bits.fun.rc)     -> (rw & ~dontWrite),
      (io.csr_iss_exe.bits.fun.vsetvli | io.csr_iss_exe.bits.fun.vsetivli | io.csr_iss_exe.bits.fun.vsetvl) -> true.B,
    ))
  io.xpuCsrWriteBack.bits.op_rs := rs & ~dontWrite
  io.xpuCsrWriteBack.bits.op_rc := rc & ~dontWrite
}

