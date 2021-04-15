




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
import rift2Core.basic._


/*
	update when a branch return 
	lookup when there is a branch predict
*/

trait BHT {

	val bru_iq_b_bits = Wire(Bool())

	val bht_sel = Wire(UInt(32.W))



	val bht_buf = RegInit(VecInit(Fill(4096, "b01".U(2.W) ) ))  //4096 history table

	val is_bru_iq_b_ack = Wire(Bool())


	val bht_idx = bht_sel(12,1)

	when( is_bru_iq_b_ack ) {
		bht_buf(bht_idx) := Mux1H(Seq(
			(bht_buf(bht_idx) === "b00".U) -> Mux( bru_iq_b_bits, "b01".U, "b00".U ),
			(bht_buf(bht_idx) === "b01".U) -> Mux( bru_iq_b_bits, "b10".U, "b00".U ),
			(bht_buf(bht_idx) === "b10".U) -> Mux( bru_iq_b_bits, "b11".U, "b01".U ),
			(bht_buf(bht_idx) === "b11".U) -> Mux( bru_iq_b_bits, "b11".U, "b10".U )
		))
	}

	def bht_predict(pc: UInt): Bool = {
		def idx = pc(12,1)
		return Mux1H( Seq(
			(bht_buf(bht_idx) === "b00".U) -> true.B,
			(bht_buf(bht_idx) === "b01".U) -> true.B,
			(bht_buf(bht_idx) === "b10".U) -> false.B,
			(bht_buf(bht_idx) === "b11".U) -> false.B
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


// /*
// 	jalr target buf
// 	read only when ras is down
// 	write when any jalr return
// 	the pc and target pc will both be recorded
// */
// trait JTB {
// 	val bru_iq_j_bits = Wire( new Info_JTB )

// 	val jtb_update_ptr = RegInit(0.U(3.W))
// 	val jtb_buf = RegInit(VecInit(Seq.fill(8)( 0.U.asTypeOf(new Info_JTB) ) )) // 2 pc is init as 8kw

// 	val is_bru_iq_j_ack = Wire(Bool())

// 	def is_pc_hit(ori_pc: UInt) = jtb_buf.exists((x: Info_JTB) => x.ori_pc === ori_pc)
// 	def is_pc_idx(ori_pc: UInt): UInt = jtb_buf.indexWhere((x: Info_JTB) => x.ori_pc === ori_pc)

// 	when( is_bru_iq_j_ack ) {
// 		def updata_idx = Mux( is_pc_hit(bru_iq_j_bits.ori_pc), is_pc_idx(bru_iq_j_bits.ori_pc), jtb_update_ptr )
// 		jtb_update_ptr := Mux( is_pc_hit(bru_iq_j_bits.ori_pc), jtb_update_ptr, jtb_update_ptr + 1.U )

// 		jtb_buf(jtb_update_ptr).ori_pc := bru_iq_j_bits.ori_pc
// 		jtb_buf(jtb_update_ptr).tgt_pc := bru_iq_j_bits.tgt_pc
// 	}

// 	def jtb_read(ori_pc: UInt): UInt = { 
// 		Mux( is_pc_hit(ori_pc), jtb_buf(is_pc_idx(ori_pc)).tgt_pc, "h80000000".U )
// 	} 
// }



class BranchPredict() extends Module with BHT {
	val io = IO(new Bundle {
		val bru_iq_b = Flipped(new ValidIO( Bool() ))
		val bru_iq_j = Flipped(new ValidIO( UInt(64.W) ))

		val ib_pc = new DecoupledIO(new Info_ib_pc)

		val iq_ib = Vec(2, Flipped(new DecoupledIO(new Info_iq_ib) ))
		val ib_id = Vec(2, new DecoupledIO(new Info_ib_id))

		//include privilege, mispredict, not-include jalr
		val flush = Input(Bool())
	})


	val iq_id_fifo = Module(new MultiPortFifo( new Info_ib_id, 4, 2, 2 ))
	iq_id_fifo.io.flush := io.flush

	val is_ib_id_ack = Wire(Vec(2, Bool()))

	val ib_lock = RegInit(false.B)

/*	
	branch history queue, for bru result checking 
	write when one branch instr decode
	read and pop when a bru return 
*/
	val bhq = Module(new Queue( new Info_BHQ, 16 )) //1 input, 1 output 

	val ras = Module(new Gen_ringStack( UInt(32.W), 4 ))

	for ( i <- 0 until 2 ) yield is_ib_id_ack(i) := io.ib_id(i).valid & io.ib_id(i).ready



	is_bru_iq_b_ack := io.bru_iq_b.valid
	bru_iq_b_bits     := io.bru_iq_b.bits


	def is_bru_iq_j_ack = io.bru_iq_j.valid


	io.iq_ib(0).ready := iq_id_fifo.push_ack(0) & ~ib_lock
	io.iq_ib(1).ready := iq_id_fifo.push_ack(1) & ~ib_lock

	iq_id_fifo.io.push(0).bits := jfilter(io.iq_ib(0).bits, is_ras_taken & is_1stCut) 
	iq_id_fifo.io.push(1).bits := jfilter(io.iq_ib(1).bits, is_ras_taken & is_2ndCut) 


	iq_id_fifo.io.push(0).valid := ~ib_lock & io.iq_ib(0).valid & Mux(is_1stCut, is_branch_predict_nready, true.B)
	iq_id_fifo.io.push(1).valid := ~ib_lock & io.iq_ib(1).valid & ~is_1stCut & Mux(is_2ndCut, is_branch_predict_nready, true.B)

	io.ib_id <> iq_id_fifo.io.pop


	bhq.io.enq.valid := is_branch & (is_1stCut | is_2ndCut)
	bhq.io.deq.ready := io.bru_iq_b.valid
	bhq.reset := reset.asBool | io.flush

	ras.io.push.valid := is_call & ((is_1stCut & iq_id_fifo.push_ack(0)) | (is_2ndCut & iq_id_fifo.push_ack(1)))
	ras.io.pop.ready := is_ras_taken & ((is_1stCut & iq_id_fifo.push_ack(0)) | (is_2ndCut & iq_id_fifo.push_ack(1)))
	ras.io.flush := io.flush


	def is_1stCut = io.iq_ib(0).bits.info.is_pineline_cut & io.iq_ib(0).valid
	def is_2ndCut = io.iq_ib(1).bits.info.is_pineline_cut & io.iq_ib(1).valid & ~is_1stCut
	def is_noCut  = ~is_1stCut & ~is_2ndCut



	def is_jal    = MuxCase( false.B, Array( is_1stCut -> io.iq_ib(0).bits.info.is_jal,    is_2ndCut -> io.iq_ib(1).bits.info.is_jal ))
	def is_jalr   = MuxCase( false.B, Array( is_1stCut -> io.iq_ib(0).bits.info.is_jalr,   is_2ndCut -> io.iq_ib(1).bits.info.is_jalr ))
	def is_branch = MuxCase( false.B, Array( is_1stCut -> io.iq_ib(0).bits.info.is_branch, is_2ndCut -> io.iq_ib(1).bits.info.is_branch ))
	def is_call   = MuxCase( false.B, Array( is_1stCut -> io.iq_ib(0).bits.info.is_call,   is_2ndCut -> io.iq_ib(1).bits.info.is_call ))
	def is_return = MuxCase( false.B, Array( is_1stCut -> io.iq_ib(0).bits.info.is_return, is_2ndCut -> io.iq_ib(1).bits.info.is_return ))
	def is_rvc    = MuxCase( false.B, Array( is_1stCut -> io.iq_ib(0).bits.info.is_rvc,    is_2ndCut -> io.iq_ib(1).bits.info.is_rvc ))
	def is_fencei = MuxCase( false.B, Array( is_1stCut -> io.iq_ib(0).bits.info.is_fencei, is_2ndCut -> io.iq_ib(1).bits.info.is_fencei ))
	def imm       = MuxCase( 0.U,     Array( is_1stCut -> io.iq_ib(0).bits.info.imm,       is_2ndCut -> io.iq_ib(1).bits.info.imm ))


	def ori_pc = Mux( is_1stCut, io.iq_ib(0).bits.pc, io.iq_ib(1).bits.pc)


	def next_pc = Mux( is_rvc, ori_pc + 2.U, ori_pc + 4.U )
	def jal_pc = ori_pc + imm
	def branch_pc = ori_pc + imm
	def misPredict_pc_b = bhq.io.deq.bits.opp_pc
	def misPredict_pc_j = io.bru_iq_j.bits
	def ras_pc = ras.io.pop.bits
	def jalr_pc = Mux( is_ras_taken, ras_pc, DontCare)


	def is_ras_taken          = is_return & ras.io.pop.valid
	def is_predict_taken      = bht_predict(ori_pc)

	def is_misPredict_taken = (bru_iq_b_bits =/= bhq.io.deq.bits.dir)



	io.ib_pc.valid := is_jal | is_jalr | is_predict_taken | is_misPredict_taken | is_bru_iq_j_ack
	io.ib_pc.bits.addr  := MuxCase(DontCare, Array(
		is_jal                -> jal_pc,
		is_jalr               -> jalr_pc,
		is_predict_taken      -> branch_pc,
		is_misPredict_taken -> bhq.io.deq.bits.opp_pc,
		is_bru_iq_j_ack       -> io.bru_iq_j.bits
	))

	when( io.flush ) {
		ib_lock := false.B
	}
	.elsewhen( (is_jalr & ~is_ras_taken) | is_fencei ) {
		ib_lock := true.B
	}
	.elsewhen(is_bru_iq_j_ack) {
		ib_lock := false.B
	}


	//bht update
	bht_sel := bhq.io.deq.bits.ori_pc




	def jfilter(ori_info: Info_iq_ib, is_ras_pop: Bool) = {

		def f_jrToj: UInt = { println("JR TO J happened"); return "b1010000000000001".U(32.W) }
		def f_jalrTojal(x: UInt): UInt = { println("Jalr TO Jal happened"); return (x | "b1000".U) }

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
	ras.io.push.bits := Mux( is_call, next_pc, DontCare)


	def bhq_nack = bhq.io.enq.valid & ~bhq.io.enq.ready

	// def ras_nack = ras.io.push.valid & ~ras.io.push.ready
	def is_branch_predict_nready = bhq_nack


	// assert( ~(bhq.io.enq.valid & ~bhq.io.enq.ready), "Assert Fail at BHQ.push(0)" )
	assert( ~(bhq.io.deq.ready & ~bhq.io.deq.valid), "Assert Fail at BHQ.pop" )




}


