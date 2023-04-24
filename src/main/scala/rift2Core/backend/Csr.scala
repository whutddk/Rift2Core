

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
            // val csr_cWriteBack = Valid(new SeqReg_WriteBack_Bundle(64, cRegNum))
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

  csr_exe_iwb_fifo.io.enq.valid := io.csr_iss_exe.valid
  csr_exe_iwb_fifo.io.enq.bits.res := io.csr_iss_exe.bits.param.dat.op2
  csr_exe_iwb_fifo.io.enq.bits.rd0 := io.csr_iss_exe.bits.param.rd0

  io.xpuCsrWriteBack.valid := csr_exe_iwb_fifo.io.enq.fire
  io.xpuCsrWriteBack.bits.addr  := addr
  io.xpuCsrWriteBack.bits.dat_i  := dat
  io.xpuCsrWriteBack.bits.op_rw := rw & ~dontWrite
  io.xpuCsrWriteBack.bits.op_rs := rs & ~dontWrite
  io.xpuCsrWriteBack.bits.op_rc := rc & ~dontWrite
  // io.xpuCsrWriteBack.bits.idx   := io.csr_iss_exe.bits.param.csrw(log2Ceil(cRegNum)-1, 0 )

}

// trait FCSR{ this: CSRBase =>
//   when( addr === "h001".U ){ //fflag
//     csr_exe_iwb_fifo.io.enq.bits.res := io.csr_iss_exe.bits.param.dat.op2(4,0) //fcsr(4,0)
//     io.csr_cWriteBack.bits.addr  := "h003".U
//     when( rw ){
//       io.csr_cWriteBack.bits.dati := Cat(io.csr_iss_exe.bits.param.dat.op2(7,5), dat(4,0))
//     }.elsewhen( rs | rc ){
//       io.csr_cWriteBack.bits.dati := Cat( 0.U(3.W), dat(4,0) )
//     }
//   }

//   when( addr === "h002".U ){ //frm
//     csr_exe_iwb_fifo.io.enq.bits.res := io.csr_iss_exe.bits.param.dat.op2(7,5) //fcsr(7,5)
//     io.csr_cWriteBack.bits.addr  := "h003".U
//     when( rw ){
//       io.csr_cWriteBack.bits.dati := Cat( dat(2,0), io.csr_iss_exe.bits.param.dat.op2(4,0) )
//     }.elsewhen( rs | rc ){
//       io.csr_cWriteBack.bits.dati := Cat( dat(2,0), 0.U(5.W) )
//     }
//   }
// } 

// trait VCSR{ this: CSRBase =>
//   when( addr === "h009".U ){ //vxsat
//     csr_exe_iwb_fifo.io.enq.bits.res := io.csr_iss_exe.bits.param.dat.op2(2,1) //Vcsr(2,1)
//     io.csr_cWriteBack.bits.addr  := "h00F".U
//     when( rw ){
//       io.csr_cWriteBack.bits.dati := Cat( dat(1,0), io.csr_iss_exe.bits.param.dat.op2.extract(0) )
//     }.elsewhen( rs | rc ){
//       io.csr_cWriteBack.bits.dati := Cat( dat(1,0), 0.U(1.W) )
//     }
//   }

//   when( addr === "h00A".U ){ //vxrm
//     csr_exe_iwb_fifo.io.enq.bits.res := io.csr_iss_exe.bits.param.dat.op2.extract(0) //Vcsr(0)
//     io.csr_cWriteBack.bits.addr  := "h00F".U
//     when( rw ){
//       io.csr_cWriteBack.bits.dati := Cat( io.csr_iss_exe.bits.param.dat.op2(2,1), dat.extract(0) )
//     }.elsewhen( rs | rc ){
//       io.csr_cWriteBack.bits.dati := Cat( 0.U(2.W), dat.extract(0) )
//     }
//   }
// } 

trait VConfig{ this: CSRBase =>
  when( addr === "hC20".U ){ //vl read-only from csr
    csr_exe_iwb_fifo.io.enq.bits.res := io.csr_iss_exe.bits.param.dat.op2(62,8) //VConfig(62,8) 
    require( (log2Ceil(vParams.vlmax) + 8 + 1) <= 64 )

    assert( io.xpuCsrWriteBack.bits.op_rw === false.B & io.xpuCsrWriteBack.bits.op_rs === false.B & io.xpuCsrWriteBack.bits.op_rc === false.B )
  }

  when( addr === "hC21".U ){ //vtype
    csr_exe_iwb_fifo.io.enq.bits.res := Cat( io.csr_iss_exe.bits.param.dat.op2.extract(63), 0.U(55.W), io.csr_iss_exe.bits.param.dat.op2(7,0))
    require( (log2Ceil(vParams.vlmax) + 8 + 1) <= 64 )

    assert( io.xpuCsrWriteBack.bits.op_rw === false.B & io.xpuCsrWriteBack.bits.op_rs === false.B & io.xpuCsrWriteBack.bits.op_rc === false.B )
  }
  
  when( addr === "hFFE".U ){ //vconfig

    val avl = Wire( UInt((log2Ceil(vParams.vlmax)).W) )
    val nvl = Wire( UInt((log2Ceil(vParams.vlmax)).W) )

    avl := io.csr_iss_exe.bits.param.dat.op1
    nvl := Mux( avl <= (vParams.vlmax).U, avl, Mux( avl < ((vParams.vlmax).U << 1), (avl + 1.U) >> 1, (vParams.vlmax).U ) )

    val vill = Wire(Bool())
    val vtype = Wire(UInt(8.W))

    val vsew  = io.csr_iss_exe.bits.param.dat.op2(5,3)
    val vlmul = io.csr_iss_exe.bits.param.dat.op2(2,0)

    vill :=
      (io.csr_iss_exe.bits.param.dat.op2.extract(63) === true.B) |
      (vsew.extract(2) === true.B) |
      (vlmul === "b100".U) |
      (vlmul === "b101".U & (
          (if( vParams.vlen/8 < 64 ) { vsew === "b011".U } else {false.B}) |
          (if( vParams.vlen/8 < 32 ) { vsew === "b010".U } else {false.B}) |
          (if( vParams.vlen/8 < 16 ) { vsew === "b001".U } else {false.B}) |
          (if( vParams.vlen/8 < 8  ) { vsew === "b000".U } else {false.B})
        )
      ) |
      (vlmul === "b110".U & (
          (if( vParams.vlen/4 < 64 ) { vsew === "b011".U } else {false.B}) |
          (if( vParams.vlen/4 < 32 ) { vsew === "b010".U } else {false.B}) |
          (if( vParams.vlen/4 < 16 ) { vsew === "b001".U } else {false.B}) |
          (if( vParams.vlen/4 < 8  ) { vsew === "b000".U } else {false.B})
        )
      ) |
      (vlmul === "b111".U & (
          (if( vParams.vlen/2 < 64 ) { vsew === "b011".U } else {false.B}) |
          (if( vParams.vlen/2 < 32 ) { vsew === "b010".U } else {false.B}) |
          (if( vParams.vlen/2 < 16 ) { vsew === "b001".U } else {false.B}) |
          (if( vParams.vlen/2 < 8  ) { vsew === "b000".U } else {false.B})
        )
      )

    vtype := Mux( vill, 0.U, io.csr_iss_exe.bits.param.dat.op2(7,0) )


    csr_exe_iwb_fifo.io.enq.bits.res := nvl
    io.xpuCsrWriteBack.bits.addr  := "hFFE".U

    assert( ~rw & ~rs & ~rs )

    io.xpuCsrWriteBack.bits.op_rw := true.B
    io.xpuCsrWriteBack.bits.dat_i := Cat( vill, nvl | 0.U((64-log2Ceil(vParams.vlmax)-8).W), vtype )

  }
} 


class CSR(implicit p: Parameters) extends CSRBase 
// with FCSR
// with VCSR
with VConfig{
  
}

