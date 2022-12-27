

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

package rift2Core.backend


import chisel3._
import chisel3.util._
import rift2Core.define._
import rift2Core.privilege._

import rift2Chip._
import chipsalliance.rocketchip.config.Parameters

class Csr(implicit p: Parameters) extends RiftModule {
  val io = IO(new Bundle{
    val csr_iss_exe = Flipped(new DecoupledIO(new Csr_iss_info))
    val csr_exe_iwb = new DecoupledIO(new WriteBack_info(dw=64))

    val csr_cWriteBack = Valid(new SeqReg_WriteBack_Bundle(64, 4))


    val csr_addr = ValidIO(UInt(12.W))
    val csr_data = Flipped(ValidIO(UInt(64.W)))

    val csr_cmm_op = DecoupledIO( new Exe_Port ) 

    val flush = Input(Bool())
  })

  val csr_exe_iwb_fifo = Module( new Queue( new WriteBack_info(dw=64), 1, true, false ) )
  io.csr_exe_iwb <> csr_exe_iwb_fifo.io.deq
  csr_exe_iwb_fifo.reset := reset.asBool | io.flush

  val csr_op_fifo = Module(new Queue( new Exe_Port, 1, false, false ) )
  io.csr_cmm_op <> csr_op_fifo.io.deq
  csr_op_fifo.reset := reset.asBool | io.flush



  val rw = io.csr_iss_exe.bits.fun.rw
  val rs = io.csr_iss_exe.bits.fun.rs
  val rc = io.csr_iss_exe.bits.fun.rc
  
  val dat = io.csr_iss_exe.bits.param.dat.op1
  val addr = io.csr_iss_exe.bits.csrw( log2Ceil(4)+11, log2Ceil(4)+0 )

  val dontWrite = (dat === 0.U) & ( rs | rc )

  csr_op_fifo.io.enq.bits.addr := addr
  csr_op_fifo.io.enq.bits.dat_i := dat
  csr_op_fifo.io.enq.bits.op_rw := rw & ~dontWrite
  csr_op_fifo.io.enq.bits.op_rs := rs & ~dontWrite
  csr_op_fifo.io.enq.bits.op_rc := rc & ~dontWrite


  when( addr =/= "b300".U ) {
    csr_op_fifo.io.enq.bits.addr := addr
    csr_op_fifo.io.enq.bits.dat_i := dat
    csr_op_fifo.io.enq.bits.op_rw := rw & ~dontWrite
    csr_op_fifo.io.enq.bits.op_rs := rs & ~dontWrite
    csr_op_fifo.io.enq.bits.op_rc := rc & ~dontWrite

    io.csr_addr.bits  := addr
    io.csr_addr.valid := io.csr_iss_exe.valid

    io.csr_iss_exe.ready     := csr_exe_iwb_fifo.io.enq.fire
    csr_op_fifo.io.enq.valid := csr_exe_iwb_fifo.io.enq.fire

    csr_exe_iwb_fifo.io.enq.valid := io.csr_iss_exe.valid & csr_op_fifo.io.enq.ready & io.csr_data.valid
    csr_exe_iwb_fifo.io.enq.bits.res := io.csr_data.bits
    csr_exe_iwb_fifo.io.enq.bits.rd0 := io.csr_iss_exe.bits.param.rd0

    io.csr_cWriteBack.valid := false.B

    io.csr_cWriteBack.bits.addr  := DontCare
    io.csr_cWriteBack.bits.dati  := DontCare
    io.csr_cWriteBack.bits.op_rw := DontCare
    io.csr_cWriteBack.bits.op_rs := DontCare
    io.csr_cWriteBack.bits.op_rc := DontCare
    io.csr_cWriteBack.bits.idx   := DontCare
  } .otherwise {
    csr_op_fifo.io.enq.bits.addr  := DontCare
    csr_op_fifo.io.enq.bits.dat_i := DontCare
    csr_op_fifo.io.enq.bits.op_rw := DontCare
    csr_op_fifo.io.enq.bits.op_rs := DontCare
    csr_op_fifo.io.enq.bits.op_rc := DontCare

    io.csr_addr.bits  := DontCare
    io.csr_addr.valid := false.B

    io.csr_iss_exe.ready     := csr_exe_iwb_fifo.io.enq.fire
    csr_op_fifo.io.enq.valid := false.B


    csr_exe_iwb_fifo.io.enq.valid := io.csr_iss_exe.valid & csr_op_fifo.io.enq.ready
    csr_exe_iwb_fifo.io.enq.bits.res := io.csr_iss_exe.bits.param.dat.op2
    csr_exe_iwb_fifo.io.enq.bits.rd0 := io.csr_iss_exe.bits.param.rd0


    io.csr_cWriteBack.valid := csr_exe_iwb_fifo.io.enq.fire

    io.csr_cWriteBack.bits.addr  := addr
    io.csr_cWriteBack.bits.dati  := dat
    io.csr_cWriteBack.bits.op_rw := rw & ~dontWrite
    io.csr_cWriteBack.bits.op_rs := rs & ~dontWrite
    io.csr_cWriteBack.bits.op_rc := rc & ~dontWrite
    io.csr_cWriteBack.bits.idx   := io.csr_iss_exe.bits.csrw(log2Ceil(4)-1, 0 )
  }



}

