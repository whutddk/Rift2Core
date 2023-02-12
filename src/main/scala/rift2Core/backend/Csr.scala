

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
import chipsalliance.rocketchip.config.Parameters

class Csr(implicit p: Parameters) extends RiftModule {
  val io = IO(new Bundle{
    val csr_iss_exe = Flipped(new DecoupledIO(new Csr_iss_info))
    val csr_exe_iwb = new DecoupledIO(new WriteBack_info(dw=64))

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
  val addr = io.csr_iss_exe.bits.param.dat.op2

  val dontWrite = (dat === 0.U) & ( rs | rc )

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


}

