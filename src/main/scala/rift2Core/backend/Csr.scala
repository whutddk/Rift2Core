/*
* @Author: Ruige Lee
* @Date:   2021-03-29 14:37:39
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-15 11:42:22
*/


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

package rift2Core.backend


import chisel3._
import chisel3.util._
import rift2Core.basic._
import chisel3.experimental.chiselName



@chiselName
class Csr extends Module {
	val io = IO(new Bundle{
		val csr_iss_exe = Flipped(new DecoupledIO(new Csr_iss_info))
		val csr_exe_iwb = new ValidIO(new Exe_iwb_info)

		val csr_addr = Output(UInt(12.W))
		val csr_data = Input(UInt(64.W))

		val csr_cmm_op = DecoupledIO( new Csr_Port ) 

		val flush = Input(Bool())
	})

	val csr_op_fifo = Module(new Queue( new Csr_Port, 1, true, false ) )
	io.csr_cmm_op <> csr_op_fifo.io.deq

	def iss_ack = io.csr_iss_exe.valid & io.csr_iss_exe.ready
	def iwb_ack = io.csr_exe_iwb.valid


	def rw = io.csr_iss_exe.bits.fun.rw
	def rs = io.csr_iss_exe.bits.fun.rs
	def rc = io.csr_iss_exe.bits.fun.rc
	
	def dat = io.csr_iss_exe.bits.param.op1
	def addr = io.csr_iss_exe.bits.param.op2

	def dontRead = (io.csr_iss_exe.bits.param.rd0_raw === 0.U) & rw
	def dontWrite = (dat === 0.U) & ( rs | rc )


	val iwb_valid = Reg(Bool())
	val iwb_res = Reg(UInt(64.W))
	val iwb_rd0 = Reg(UInt(7.W))

	csr_op_fifo.io.enq.bits.addr := addr
	csr_op_fifo.io.enq.bits.dat_i := dat
	csr_op_fifo.io.enq.bits.op_rw := rw
	csr_op_fifo.io.enq.bits.op_rs := rs
	csr_op_fifo.io.enq.bits.op_rc := rc

	io.csr_addr := addr

	when( reset.asBool | io.flush ) {
		iwb_valid := false.B
		iwb_res := 0.U
		iwb_rd0 := 0.U
	}
	.elsewhen( ~io.csr_exe_iwb.valid & io.csr_iss_exe.valid & csr_op_fifo.io.enq.ready ) {
		iwb_valid := true.B
		iwb_res := io.csr_data
		iwb_rd0 := Cat( io.csr_iss_exe.bits.param.rd0_idx, io.csr_iss_exe.bits.param.rd0_raw ) 
	}
	.elsewhen( iwb_ack ) {
		iwb_valid := false.B
	}

	io.csr_iss_exe.ready := iwb_ack
	io.csr_exe_iwb.valid := iwb_valid
	csr_op_fifo.io.enq.valid := iwb_valid & ~dontWrite
	io.csr_exe_iwb.bits.res := iwb_res
	io.csr_exe_iwb.bits.rd0_raw := iwb_rd0(4,0)
	io.csr_exe_iwb.bits.rd0_idx := iwb_rd0(6,5)

}

