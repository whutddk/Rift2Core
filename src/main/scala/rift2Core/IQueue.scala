/*
* @Author: Ruige Lee
* @Date:   2021-04-12 16:52:15
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-15 16:36:05
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


package rift2Core

import chisel3._
import chisel3.util._
import chisel3.util.random._

import chisel3.experimental.ChiselEnum
import rift2Core.basic._
import rift2Core.frontend._







class Iqueue extends Module {
	val io = IO(new Bundle {

		val if_iq = Vec(4, Flipped(new DecoupledIO(UInt(16.W)) ))
		val iq_ib = Vec(2, new DecoupledIO(new Info_iq_ib))

		val pc_iq = Flipped(new ValidIO( UInt(64.W) ))

	})

	val iq_ib_fifo = Module(new MultiPortFifo( new Info_iq_ib, 4, 2, 2 ))
	val pd16 = for ( i <- 0 until 2 ) yield { val mdl = Module(new PreDecode16()); mdl }
	val pd32 = for ( i <- 0 until 2 ) yield { val mdl = Module(new PreDecode32()); mdl }

	val pc_qout = RegInit("h80000000".U)

	val is_if_iq_ack = Wire(Vec(4, Bool()))
	val is_iq_ib_fifo_ack = Wire(Vec(2, Bool()))

	io.iq_ib <> iq_ib_fifo.io.pop

	for ( i <- 0 until 4 ) yield is_if_iq_ack(i) := io.if_iq(i).valid & io.if_iq(i).ready
	for ( i <- 0 until 2 ) yield is_iq_ib_fifo_ack(i) := iq_ib_fifo.push_ack(i)


	io.if_iq(0).ready := Mux1H(Seq(
				is_00p00 -> false.B,
				is_00p16 -> is_iq_ib_fifo_ack(0),
				is_16p16 -> is_iq_ib_fifo_ack(0),
				is_32p16 -> is_iq_ib_fifo_ack(0),
				is_00p32 -> is_iq_ib_fifo_ack(0),
				is_16p32 -> is_iq_ib_fifo_ack(0),
				is_32p32 -> is_iq_ib_fifo_ack(0)
			))


	io.if_iq(1).ready := Mux1H(Seq(
				is_00p00 -> false.B,
				is_00p16 -> false.B,
				is_16p16 -> is_iq_ib_fifo_ack(1),
				is_32p16 -> is_iq_ib_fifo_ack(1),
				is_00p32 -> is_iq_ib_fifo_ack(0),
				is_16p32 -> is_iq_ib_fifo_ack(0),
				is_32p32 -> is_iq_ib_fifo_ack(0)
	))

	io.if_iq(2).ready := Mux1H(Seq(
				is_00p00 -> false.B,
				is_00p16 -> false.B,
				is_16p16 -> false.B,
				is_32p16 -> is_iq_ib_fifo_ack(1),
				is_00p32 -> false.B,
				is_16p32 -> is_iq_ib_fifo_ack(1),
				is_32p32 -> is_iq_ib_fifo_ack(1)
	))

	io.if_iq(3).ready := Mux1H(Seq(
				is_00p00 -> false.B,
				is_00p16 -> false.B,
				is_16p16 -> false.B,
				is_32p16 -> false.B,
				is_00p32 -> false.B,
				is_16p32 -> false.B,
				is_32p32 -> is_iq_ib_fifo_ack(1)
	))

	when( io.pc_iq.valid ) {
		pc_qout := io.pc_iq.bits
	}
	.otherwise {
		pc_qout := MuxCase(pc_qout, Array(
			io.if_iq(3).ready -> ( pc_qout + 8.U ),
			io.if_iq(2).ready -> ( pc_qout + 6.U ),
			io.if_iq(1).ready -> ( pc_qout + 4.U ),
			io.if_iq(0).ready -> ( pc_qout + 2.U )
		))
	}



	iq_ib_fifo.io.flush := io.pc_iq.valid

	iq_ib_fifo.io.push(0).valid := ~is_1st00
	iq_ib_fifo.io.push(1).valid := ~is_2nd00

	iq_ib_fifo.io.push(0).bits.info := Mux( is_1st16, pd16(0).io.info, pd32(0).io.info ) //1st00 will not be care
	iq_ib_fifo.io.push(1).bits.info := Mux( is_2nd16, pd16(1).io.info, pd32(1).io.info ) //2nd00 will not be care

	iq_ib_fifo.io.push(0).bits.instr := Mux( is_1st16, io.if_iq(0).bits,     Cat( io.if_iq(1).bits, io.if_iq(0).bits) )
	iq_ib_fifo.io.push(1).bits.instr := Mux( is_2nd16, io.if_iq(idx_2nd).bits, Cat( io.if_iq(idx_2nd+1.U).bits, io.if_iq(idx_2nd).bits) )

	iq_ib_fifo.io.push(0).bits.pc    := pc_qout
	iq_ib_fifo.io.push(1).bits.pc    := Mux( is_1st16, pc_qout + 2.U, pc_qout + 4.U)







	def is_1st00 =	~is_1st16 & ~is_1st32	
	def is_1st16 = 	(io.if_iq(0).valid === true.B) & (io.if_iq(0).bits(1,0) =/= "b11".U)
	def is_1st32 = 	(io.if_iq(0).valid === true.B) &
					(io.if_iq(1).valid === true.B) &
					(io.if_iq(0).bits(1,0) === "b11".U)


	def idx_2nd = Mux( is_1st16, 1.U, 2.U )

	def is_2nd00 = 	(is_1st00) |
					(~is_2nd16 & ~is_2nd32)

	def is_2nd16 = 	(io.if_iq(idx_2nd).valid === true.B) & io.if_iq(idx_2nd).bits(1,0) =/= "b11".U
	def is_2nd32 = 	(io.if_iq(idx_2nd).valid === true.B) &
					(io.if_iq(idx_2nd+1.U).valid === true.B) &
					(io.if_iq(idx_2nd).bits(1,0) === "b11".U)


	def is_00p00 = is_2nd00 & is_1st00
	def is_00p16 = is_2nd00 & is_1st16
	def is_00p32 = is_2nd00 & is_1st32
	def is_16p16 = is_2nd16 & is_1st16
	def is_16p32 = is_2nd16 & is_1st32
	def is_32p16 = is_2nd32 & is_1st16
	def is_32p32 = is_2nd32 & is_1st32


	pd16(0).io.instr16 := io.if_iq(0).bits
	pd32(0).io.instr32 := Cat( io.if_iq(1).bits, io.if_iq(0).bits)

	pd16(1).io.instr16 := io.if_iq(idx_2nd).bits
	pd32(1).io.instr32 := Cat( io.if_iq(idx_2nd+1.U).bits, io.if_iq(idx_2nd).bits)



}




