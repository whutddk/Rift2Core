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







class IQueue extends Module {
	val io = IO(new Bundle {

		val if_iq = Vec(4, Flipped(new DecoupledIO(UInt(16.W)) ))
		val iq_ib = Vec(2, new DecoupledIO(new Info_iq_ib))

		val flush = Input(Bool())
	})

	val iq_ib_fifo = Module(new MultiPortFifo( new Info_iq_ib, 4, 2, 2 ))
	val pd16 = for ( i <- 0 until 2 ) yield { val mdl = Module(new PreDecode16()); mdl }
	val pd32 = for ( i <- 0 until 2 ) yield { val mdl = Module(new PreDecode32()); mdl }

	val pc_qout = RegInit("h80000000".U)

	val is_if_iq_ack = Wire(Vec(4, Bool()))
	val is_iq_ib_ack = Wire(Vec(2, Bool()))

	io.iq_ib <> iq_ib_fifo.io.pop

	for ( i <- 0 until 4 ) yield is_if_iq_ack(i) := io.if_iq(i).valid & io.if_iq(i).ready
	for ( i <- 0 until 2 ) yield is_iq_ib_ack(i) := io.iq_ib(i).valid & io.iq_ib(i).ready

	io.if_iq(0).ready := Mux1H(Seq(
				is_00p00 -> false.B,
				is_00p16 -> is_iq_ib_ack(0),
				is_16p16 -> is_iq_ib_ack(0),
				is_32p16 -> is_iq_ib_ack(0),
				is_00p32 -> is_iq_ib_ack(0),
				is_16p32 -> is_iq_ib_ack(0),
				is_32p32 -> is_iq_ib_ack(0)
			))


	io.if_iq(1).ready := Mux1H(Seq(
				is_00p00 -> false.B,
				is_00p16 -> false.B,
				is_16p16 -> is_iq_ib_ack(1),
				is_32p16 -> is_iq_ib_ack(1),
				is_00p32 -> is_iq_ib_ack(0),
				is_16p32 -> is_iq_ib_ack(0),
				is_32p32 -> is_iq_ib_ack(0)
	))

	io.if_iq(2).ready := Mux1H(Seq(
				is_00p00 -> false.B,
				is_00p16 -> false.B,
				is_16p16 -> false.B,
				is_32p16 -> is_iq_ib_ack(1),
				is_00p32 -> false.B,
				is_16p32 -> is_iq_ib_ack(1),
				is_32p32 -> is_iq_ib_ack(1)
	))

	io.if_iq(3).ready := Mux1H(Seq(
				is_00p00 -> false.B,
				is_00p16 -> false.B,
				is_16p16 -> false.B,
				is_32p16 -> false.B,
				is_00p32 -> false.B,
				is_16p32 -> false.B,
				is_32p32 -> is_iq_ib_ack(1)
	))


	pc_qout := Mux1H(Seq(
				is_00p00 -> pc_qout,
				is_00p16 -> (pc_qout + 2.U),
				is_16p16 -> (pc_qout + 4.U),
				is_32p16 -> (pc_qout + 6.U),
				is_00p32 -> (pc_qout + 4.U),
				is_16p32 -> (pc_qout + 6.U),
				is_32p32 -> (pc_qout + 8.U)
	))




	iq_ib_fifo.io.flush := io.flush

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






// class IQueue extends BranchPredict {
// 	val io = IO(new Bundle {
// 		val bru_iq_b = Flipped(new DecoupledIO( Bool() ))
// 		val bru_iq_j = Flipped(new DecoupledIO( new Info_JTB ))

// 		val iq_pc_info = new DecoupledIO(UInt(64.W))

// 		val if_iq = Vec(4, Flipped(new DecoupledIO(UInt(16.W)) ))
// 		val iq_id = Vec(2, new DecoupledIO(new Info_iq_id))

// 		val flush = Input(Bool())
// 	})

// 	val iq_id_fifo = Module(new MultiPortFifo( new Info_iq_id, 4, 2, 2 )) //1 input, 1 output 	

// 	def if_iq_ack(i: Int) = io.if_iq(i).valid & io.if_iq(i).ready

// 	is_bru_iq_b_ack := io.bru_iq_b.valid & io.bru_iq_b.ready
// 	io.bru_iq_b.ready := true.B

// 	is_bru_iq_j_ack := io.bru_iq_j.valid & io.bru_iq_j.ready
// 	io.bru_iq_j.ready := true.B

// 	io.iq_pc_info.valid := is_jal | is_jalr | is_predict_taken | is_misPredict_taken_b | is_misPredict_taken_j
// 	iq_pc_info_bits := io.iq_pc_info.bits

// 	bhq.io.enq.valid  := is_branch & ((is_1stCut & iq_id_fifo.push_ack(0)) | (is_2ndCut & iq_id_fifo.push_ack(1)))
// 	bhq.io.deq.ready  := io.bru_iq_b.valid & io.bru_iq_b.ready
// 	bhq.reset := reset.asBool | io.flush

// 	jhq.io.enq.valid  := is_jalr & ~is_noCut & ((is_1stCut & iq_id_fifo.push_ack(0)) | (is_2ndCut & iq_id_fifo.push_ack(1)))
// 	jhq.io.deq.ready  := io.bru_iq_j.valid & io.bru_iq_j.ready
// 	jhq.reset := reset.asBool | io.flush

// 	ras.io.push.valid := is_call & ~is_noCut & ((is_1stCut & iq_id_fifo.push_ack(0)) | (is_2ndCut & iq_id_fifo.push_ack(1)))
// 	ras.io.pop.ready := is_ras_taken & ((is_1stCut & iq_id_fifo.push_ack(0)) | (is_2ndCut & iq_id_fifo.push_ack(1)))
// 	ras.io.flush := io.flush



// 	val pc_qout = RegInit("h80000000".U)

// 	bru_iq_b_bits := io.bru_iq_b.bits
// 	bru_iq_j_bits := io.bru_iq_j.bits

// 	pc1 := pc_qout
// 	pc2 := Mux( is_1st16, pc_qout + 2.U, pc_qout + 4.U )


// 	is_1st16 := (io.if_iq(0).valid === true.B) & (io.if_iq(0).bits(1,0) =/= "b11".U)
// 	is_1st32 := (io.if_iq(0).valid === true.B) &
// 				(io.if_iq(1).valid === true.B) &
// 				(io.if_iq(0).bits(1,0) === "b11".U)

// 	override def idx_2nd: UInt = 
// 						return Mux1H(Seq(
// 								is_1st00 -> 1.U,	//dontcare
// 								is_1st16 -> 1.U,
// 								is_1st32 -> 2.U
// 							))

// 	is_2nd16 := (io.if_iq(idx_2nd).valid === true.B) & io.if_iq(idx_2nd).bits(1,0) =/= "b11".U
// 	is_2nd32 := 	(io.if_iq(idx_2nd).valid === true.B) &
// 					(io.if_iq(idx_2nd+1.U).valid === true.B) &
// 					(io.if_iq(idx_2nd).bits(1,0) === "b11".U)



// 	override def is_1stCut: Bool = return preDecode_info(0).is_pineline_cut & ~is_1st00
// 	override def is_2ndCut: Bool = return preDecode_info(1).is_pineline_cut & ~is_2nd00





// 	for ( i <- 0 until 4) yield { ibuf_pop(i) := io.if_iq(i).bits }



// 	pc_qout := Mux(
// 					iq_id_fifo.push_ack(1),
// 					Mux1H(Seq(
// 						is_00p00 -> pc_qout,
// 						is_00p16 -> (pc_qout + 2.U),
// 						is_16p16 -> (pc_qout + 4.U),
// 						is_32p16 -> (pc_qout + 6.U),
// 						is_00p32 -> (pc_qout + 4.U),
// 						is_16p32 -> (pc_qout + 6.U),
// 						is_32p32 -> (pc_qout + 8.U)
// 					)),
// 					Mux( iq_id_fifo.push_ack(0),
// 						Mux1H(Seq(
// 							is_00p00 -> pc_qout,
// 							is_00p16 -> (pc_qout + 2.U),
// 							is_16p16 -> (pc_qout + 4.U),
// 							is_32p16 -> (pc_qout + 2.U),
// 							is_00p32 -> (pc_qout + 4.U),
// 							is_16p32 -> (pc_qout + 4.U),
// 							is_32p32 -> (pc_qout + 4.U)
// 						)),
// 						pc_qout
// 					)
// 				)




// 	def is_1st_push = {
// 		~is_1st00 & Mux( is_1stCut, is_branch_predict_nready, true.B ) 
// 	}

// 	def is_2nd_push = {
// 		~is_2nd00 & Mux( is_2ndCut, is_branch_predict_nready, true.B ) 
// 	}




// 	// def iq_id_ack(i : Int) = iq_id_fifo.push_ack(i)


// 	iq_id_fifo.io.push(0).bits.instr  := Mux( is_1st16, pd16(0).io.instr16, pd32(0).io.instr32 )
// 	iq_id_fifo.io.push(0).bits.is_rvc := is_1st16
// 	iq_id_fifo.io.push(0).bits.pc     := pc1
// 	iq_id_fifo.io.push(1).bits.instr  := Mux( is_2nd16, pd16(1).io.instr16, pd32(1).io.instr32 )
// 	iq_id_fifo.io.push(1).bits.is_rvc := is_2nd16
// 	iq_id_fifo.io.push(1).bits.pc     := pc2

// 	iq_id_fifo.io.push(0).valid := is_1st_push
// 	iq_id_fifo.io.push(1).valid := is_2nd_push


// 	io.if_iq(0).ready := Mux1H(Seq(
// 				is_00p00 -> false.B,
// 				is_00p16 -> iq_id_fifo.push_ack(0),
// 				is_16p16 -> iq_id_fifo.push_ack(0),
// 				is_32p16 -> iq_id_fifo.push_ack(0),
// 				is_00p32 -> iq_id_fifo.push_ack(0),
// 				is_16p32 -> iq_id_fifo.push_ack(0),
// 				is_32p32 -> iq_id_fifo.push_ack(0)
// 	))

// 	io.if_iq(1).ready := Mux1H(Seq(
// 				is_00p00 -> false.B,
// 				is_00p16 -> false.B,
// 				is_16p16 -> iq_id_fifo.push_ack(1),
// 				is_32p16 -> iq_id_fifo.push_ack(1),
// 				is_00p32 -> iq_id_fifo.push_ack(0),
// 				is_16p32 -> iq_id_fifo.push_ack(0),
// 				is_32p32 -> iq_id_fifo.push_ack(0)
// 	))

// 	io.if_iq(2).ready := Mux1H(Seq(
// 				is_00p00 -> false.B,
// 				is_00p16 -> false.B,
// 				is_16p16 -> false.B,
// 				is_32p16 -> iq_id_fifo.push_ack(1),
// 				is_00p32 -> false.B,
// 				is_16p32 -> iq_id_fifo.push_ack(1),
// 				is_32p32 -> iq_id_fifo.push_ack(1)
// 	))

// 	io.if_iq(3).ready := Mux1H(Seq(
// 				is_00p00 -> false.B,
// 				is_00p16 -> false.B,
// 				is_16p16 -> false.B,
// 				is_32p16 -> false.B,
// 				is_00p32 -> false.B,
// 				is_16p32 -> false.B,
// 				is_32p32 -> iq_id_fifo.push_ack(1)
// 	))


// }


