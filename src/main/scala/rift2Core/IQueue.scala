/*
* @Author: Ruige Lee
* @Date:   2021-04-12 16:52:15
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-12 16:52:17
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

class IQueue extends BranchPredict {
	val io = IO(new Bundle {
		val bru_iq_b = new DecoupledIO( Bool() )
		val bru_iq_j = new DecoupledIO( new Info_JTB )

		val iq_pc_info = new DecoupledIO(UInt(64.W))

		val if_iq = Vec(4, Flipped(new DecoupledIO(UInt(16.W)) ))
		val iq_id = Vec(2, new DecoupledIO(new Info_iq_id))

		val flush = Input(Bool())
	})

	def if_iq_ack(i: Int) = io.if_iq(i).valid & io.if_iq(i).ready

	val pc_qout = RegInit("h80000000".U)

	io.bru_iq_b <>bru_iq_b
	io.bru_iq_j <>bru_iq_j

	override def pc1: UInt = return pc_qout
	override def pc2: UInt = return Mux( is_1st16, pc_qout + 2.U, pc_qout + 4.U )


	override def is_1st16: Bool = return (io.if_iq(0).valid === true.B) & io.if_iq(0).bits(1,0) =/= "b11".U
	override def is_1st32: Bool = {
										return  (io.if_iq(0).valid === true.B) &
												(io.if_iq(1).valid === true.B) &
												(io.if_iq(0).bits(1,0) === "b11".U)
									}
	override def idx_2nd: UInt = 
						return Mux1H(Seq(
								is_1st00 -> 1.U,	//dontcare
								is_1st16 -> 1.U,
								is_1st32 -> 2.U
							))

	override def is_2nd16: Bool = return (io.if_iq(idx_2nd).valid === true.B) & io.if_iq(idx_2nd).bits(1,0) =/= "b11".U
	override def is_2nd32: Bool = {
									return  (io.if_iq(idx_2nd).valid === true.B) &
											(io.if_iq(idx_2nd+1.U).valid === true.B) &
											(io.if_iq(idx_2nd).bits(1,0) === "b11".U)
								}


	override def is_1stCut: Bool = return preDecode_info(0).is_pineline_cut & ~is_1st00
	override def is_2ndCut: Bool = return preDecode_info(1).is_pineline_cut & ~is_2nd00






	override def ibuf_pop(i: UInt): UInt = return io.if_iq(i).bits


	




	def is_1st_push = {
		~is_1st00 & Mux( is_1stCut, is_branch_predict_nready, true.B ) 
	}

	def is_2nd_push = {
		~is_2nd00 & Mux( is_2ndCut, is_branch_predict_nready, true.B ) 
	}


	val iq_id_fifo = Module(new MultiPortFifo( new Info_iq_id, 4, 2, 2 )) //1 input, 1 output 	

	def iq_id_ack(i : Int) = iq_id_fifo.io.push(i).valid & iq_id_fifo.io.push(i).ready


	iq_id_fifo.io.push(0).bits.instr  := Mux( is_1st16, pd16(0).io.instr16, pd32(0).io.instr32 )
	iq_id_fifo.io.push(0).bits.is_rvc := is_1st16
	iq_id_fifo.io.push(0).bits.pc     := pc1
	iq_id_fifo.io.push(1).bits.instr  := Mux( is_2nd16, pd16(1).io.instr16, pd32(1).io.instr32 )
	iq_id_fifo.io.push(1).bits.is_rvc := is_2nd16
	iq_id_fifo.io.push(1).bits.pc     := pc2

	iq_id_fifo.io.push(0).valid := is_1st_push
	iq_id_fifo.io.push(1).valid := is_2nd_push


	io.if_iq(0).ready := Mux1H(Seq(
				is_00p00 -> false.B,
				is_00p16 -> iq_id_ack(0),
				is_16p16 -> iq_id_ack(0),
				is_32p16 -> iq_id_ack(0),
				is_00p32 -> iq_id_ack(0),
				is_16p32 -> iq_id_ack(0),
				is_32p32 -> iq_id_ack(0)
	))

	io.if_iq(1).ready := Mux1H(Seq(
				is_00p00 -> false.B,
				is_00p16 -> false.B,
				is_16p16 -> iq_id_ack(1),
				is_32p16 -> iq_id_ack(1),
				is_00p32 -> iq_id_ack(0),
				is_16p32 -> iq_id_ack(0),
				is_32p32 -> iq_id_ack(0)
	))

	io.if_iq(2).ready := Mux1H(Seq(
				is_00p00 -> false.B,
				is_00p16 -> false.B,
				is_16p16 -> false.B,
				is_32p16 -> iq_id_ack(1),
				is_00p32 -> false.B,
				is_16p32 -> iq_id_ack(1),
				is_32p32 -> iq_id_ack(1)
	))

	io.if_iq(3).ready := Mux1H(Seq(
				is_00p00 -> false.B,
				is_00p16 -> false.B,
				is_16p16 -> false.B,
				is_32p16 -> false.B,
				is_00p32 -> false.B,
				is_16p32 -> false.B,
				is_32p32 -> iq_id_ack(1)
	))


}


