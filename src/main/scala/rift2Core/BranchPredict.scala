




/*
* @Author: Ruige Lee
* @Date:   2021-04-09 10:34:13
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-09 10:34:13
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
import base._
import rift2Core.define._




/*
	update when a branch return 
	lookup when there is a branch predict
*/

trait BHT {

	def bru_iq_b_bits: Bool

	val bht_sel = Wire(UInt(32.W))



	val bht_buf = RegInit(VecInit(Seq.fill(4096)("b01".U(2.W) ) ))  //4096 history table

	def is_bru_iq_b_ack: Bool


	val bht_idx = bht_sel(12,1)

	when( is_bru_iq_b_ack ) {
		bht_buf(bht_idx) := Mux1H(Seq(
			(bht_buf(bht_idx) === "b00".U) -> Mux( bru_iq_b_bits, "b00".U, "b01".U ),
			(bht_buf(bht_idx) === "b01".U) -> Mux( bru_iq_b_bits, "b00".U, "b10".U ),
			(bht_buf(bht_idx) === "b10".U) -> Mux( bru_iq_b_bits, "b01".U, "b11".U ),
			(bht_buf(bht_idx) === "b11".U) -> Mux( bru_iq_b_bits, "b10".U, "b11".U )
		))
	}

	def bht_predict(pc: UInt): Bool = {
		lazy val idx = pc(12,1)
		return Mux1H( Seq(
			(bht_buf(idx) === "b00".U) -> true.B,
			(bht_buf(idx) === "b01".U) -> true.B,
			(bht_buf(idx) === "b10".U) -> false.B,
			(bht_buf(idx) === "b11".U) -> false.B
		)) 
	}
}

class Info_BHQ extends Bundle {
	val dir = Bool()
	val ori_pc = UInt(32.W)
	val opp_pc = UInt(32.W)
}


class Info_JTB extends Bundle {
	val ori_pc = UInt(32.W)
	val tgt_pc = UInt(32.W)
}



class BranchPredict_ss extends Module with BHT with Superscalar{
	lazy val io = IO(new Bundle {
		val bru_iq_b = Flipped(new ValidIO( Bool() ))


		val ib_pc = new ValidIO(new Info_ib_pc)

		val iq_ib = Vec(2, Flipped(new DecoupledIO(new Info_iq_ib) ))
		val ib_id = Vec(2, new DecoupledIO(new Info_ib_id))

		val is_misPredict_taken = Output(Bool())

		//include privilege, mispredict, not-include jalr
		val flush = Input(Bool())
	})

	val ib_lock = RegInit(false.B)
	val ib_id_fifo = Module(new MultiPortFifo( new Info_ib_id, 4, 2, 2 ))
	io.ib_id <> ib_id_fifo.io.deq
	ib_id_fifo.io.flush := io.flush

	def is_ib_id_fifo_enq_ack(i: Int ) = ib_id_fifo.is_enq_ack(i)

	ib_id_fifo.io.enq(0).bits := jfilter(io.iq_ib(0).bits, is_ras_taken & is_1st_solo) 
	ib_id_fifo.io.enq(1).bits := jfilter(io.iq_ib(1).bits, is_ras_taken & is_2nd_solo) 	

/*	
	branch history queue, for bru result checking 
	write when one branch instr decode
	read and pop when a bru return 
*/
	lazy val bhq = Module(new Queue( new Info_BHQ, 16 )) //1 input, 1 output 
	lazy val ras = Module(new Gen_ringStack( UInt(64.W), 4 ))

	override def is_bru_iq_b_ack = io.bru_iq_b.valid
	override def bru_iq_b_bits   = io.bru_iq_b.bits


	override def is_1st_solo = io.iq_ib(0).bits.info.is_pineline_cut & io.iq_ib(0).valid
	override def is_2nd_solo = io.iq_ib(1).bits.info.is_pineline_cut & io.iq_ib(1).valid & ~is_1st_solo
	// def is_noCut  = ~is_1stCut & ~is_2ndCut



	lazy val is_jal    = MuxCase( false.B, Array( is_1st_solo -> io.iq_ib(0).bits.info.is_jal,    is_2nd_solo -> io.iq_ib(1).bits.info.is_jal ))
	lazy val is_jalr   = MuxCase( false.B, Array( is_1st_solo -> io.iq_ib(0).bits.info.is_jalr,   is_2nd_solo -> io.iq_ib(1).bits.info.is_jalr ))
	lazy val is_branch = MuxCase( false.B, Array( is_1st_solo -> io.iq_ib(0).bits.info.is_branch, is_2nd_solo -> io.iq_ib(1).bits.info.is_branch ))
	lazy val is_call   = MuxCase( false.B, Array( is_1st_solo -> io.iq_ib(0).bits.info.is_call,   is_2nd_solo -> io.iq_ib(1).bits.info.is_call ))
	lazy val is_return = MuxCase( false.B, Array( is_1st_solo -> io.iq_ib(0).bits.info.is_return, is_2nd_solo -> io.iq_ib(1).bits.info.is_return ))
	lazy val is_rvc    = MuxCase( false.B, Array( is_1st_solo -> io.iq_ib(0).bits.info.is_rvc,    is_2nd_solo -> io.iq_ib(1).bits.info.is_rvc ))
	lazy val is_fencei = MuxCase( false.B, Array( is_1st_solo -> io.iq_ib(0).bits.info.is_fencei, is_2nd_solo -> io.iq_ib(1).bits.info.is_fencei ))
	lazy val imm       = MuxCase( 0.U,     Array( is_1st_solo -> io.iq_ib(0).bits.info.imm,       is_2nd_solo -> io.iq_ib(1).bits.info.imm ))


	lazy val ori_pc    = (Mux( is_1st_solo, io.iq_ib(0).bits.pc, io.iq_ib(1).bits.pc))
	lazy val next_pc   = (Mux( is_rvc, ori_pc + 2.U, ori_pc + 4.U ))
	lazy val jal_pc    = (ori_pc + imm)
	lazy val branch_pc = (ori_pc + imm)
	lazy val ras_pc    = (ras.io.deq.bits)
	lazy val jalr_pc   = (Mux( is_ras_taken, ras_pc, DontCare))


	lazy val is_ras_taken          = is_return & ras.io.deq.valid
	lazy val is_predict_taken      = is_branch & bht_predict(ori_pc)

	lazy val is_misPredict_taken = io.bru_iq_b.valid & (bru_iq_b_bits =/= bhq.io.deq.bits.dir) 
	io.is_misPredict_taken := is_misPredict_taken





	ib_id_fifo.io.enq(0).valid := ~ib_lock & ~io.flush & io.iq_ib(0).valid & Mux(is_1st_solo, bhq.io.enq.ready, true.B) & ib_id_fifo.io.enq(0).ready
	ib_id_fifo.io.enq(1).valid := ~ib_lock & ~io.flush & io.iq_ib(1).valid & Mux(is_2nd_solo, bhq.io.enq.ready, true.B) & ib_id_fifo.io.enq(1).ready & ~is_1st_solo

	io.iq_ib(0).ready := ~ib_lock &  ~io.flush & ib_id_fifo.is_enq_ack(0)
	io.iq_ib(1).ready := ~ib_lock &  ~io.flush & ib_id_fifo.is_enq_ack(0) & ib_id_fifo.is_enq_ack(1)


	bhq.io.enq.valid := ~ib_lock & ~io.flush & is_branch & bhq.io.enq.ready & Mux(is_1st_solo, ib_id_fifo.io.enq(0).ready, ib_id_fifo.io.enq(1).ready)
	bhq.io.deq.ready := io.bru_iq_b.valid
	bhq.reset := reset.asBool | io.flush

	ras.io.enq.valid := ~ib_lock & ~io.flush & is_call & ((is_1st_solo & ib_id_fifo.is_enq_ack(0)) | (is_2nd_solo & ib_id_fifo.is_enq_ack(1)))
	ras.io.deq.ready := is_ras_taken & ((is_1st_solo & ib_id_fifo.is_enq_ack(0)) | (is_2nd_solo & ib_id_fifo.is_enq_ack(1)))
	ras.io.flush := io.flush

	io.ib_pc.valid :=
						(~ib_lock & (is_jal | is_ras_taken | is_predict_taken)) |
						is_misPredict_taken 
	io.ib_pc.bits.addr  := MuxCase(DontCare, Array(
		is_misPredict_taken   -> bhq.io.deq.bits.opp_pc,
		is_jal                -> jal_pc,
		is_ras_taken          -> jalr_pc,
		is_predict_taken      -> branch_pc

	))

	when( io.flush ) {
		ib_lock := false.B
	}
	.elsewhen( (is_jalr & ~is_ras_taken) | is_fencei ) {
		ib_lock := true.B
	}

	//bht update
	bht_sel := bhq.io.deq.bits.ori_pc




	def jfilter(ori_info: Info_iq_ib, is_ras_pop: Bool) = {

		def f_jrToj: UInt = { return "b1010000000000001".U(32.W) }
		def f_jalrTojal(x: UInt): UInt = { return (x | "b1000".U) }

		val filt_info = Wire(new Info_ib_id)

		filt_info.pc := ori_info.pc
		filt_info.is_rvc := ori_info.info.is_rvc
		filt_info.instr := Mux( is_ras_pop,
								Mux( ori_info.info.is_rvc,
									f_jrToj,
									f_jalrTojal(ori_info.instr)
								),
								ori_info.instr								
		 )
		 
		filt_info
	}


	//bhq update
	bhq.io.enq.bits.dir    := is_predict_taken
	bhq.io.enq.bits.ori_pc := ori_pc
	bhq.io.enq.bits.opp_pc := Mux( is_predict_taken, next_pc, branch_pc )

	//ras
	ras.io.enq.bits := Mux( is_call, next_pc, DontCare)
	// def bhq_nack = bhq.io.enq.valid & ~bhq.io.enq.ready
	// def is_next_ready = bhq_nack


	// assert( ~(bhq.io.enq.valid & ~bhq.io.enq.ready), "Assert Fail at BHQ.push(0)" )
	assert( ~(bhq.io.deq.ready & ~bhq.io.deq.valid), "Assert Fail at BHQ.pop" )




}


