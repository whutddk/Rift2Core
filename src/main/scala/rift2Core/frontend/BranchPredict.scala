




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


package rift2Core.frontend

import chisel3._
import chisel3.util._
import rift2Core.basic._


/*
	update when a branch return 
	lookup when there is a branch predict
*/

trait BHT {

	val bru_iq_b = new DecoupledIO(Bool())

	def bht_sel: UInt

	bru_iq_b.ready := true.B

	val bht_buf = RegInit(VecInit(Fill(4096, "b01".U(2.W) ) ))  //4096 history table


	def is_bru_iq_b_ack = bru_iq_b.valid & bru_iq_b.ready

	val bht_idx = bht_sel(12,1)

	when( is_bru_iq_b_ack ) {
		bht_buf(bht_idx) := Mux1H(Seq(
			(bht_buf(bht_idx) === "b00".U) -> Mux( bru_iq_b.bits, "b01".U, "b00".U ),
			(bht_buf(bht_idx) === "b01".U) -> Mux( bru_iq_b.bits, "b10".U, "b00".U ),
			(bht_buf(bht_idx) === "b10".U) -> Mux( bru_iq_b.bits, "b11".U, "b01".U ),
			(bht_buf(bht_idx) === "b11".U) -> Mux( bru_iq_b.bits, "b11".U, "b10".U )
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
	val dir = Wire(Bool())
	val ori_pc = Wire(UInt(64.W))
	val opp_pc = Wire(UInt(64.W))
}


class Info_JTB extends Bundle {
	val ori_pc = UInt(64.W)
	val tgt_pc = UInt(64.W)
}


/*
	jalr target buf
	read only when ras is down
	write when any jalr return
	the pc and target pc will both be recorded
*/
trait JTB {
	val bru_iq_j = new DecoupledIO( new Info_JTB )

	val jtb_update_ptr = RegInit(0.U(3.W))
	val jtb_buf = RegInit(VecInit(Seq.fill(8)( "h8000000080000000".U.asTypeOf(new Info_JTB) ) )) // 2 pc is init as 8kw

	def is_pc_hit(ori_pc: UInt) = jtb_buf.exists((x: Info_JTB) => x.ori_pc === ori_pc)
	def is_pc_idx(ori_pc: UInt): UInt = jtb_buf.indexWhere((x: Info_JTB) => x.ori_pc === ori_pc)

	when( bru_iq_j.valid ) {
		def updata_idx = Mux( is_pc_hit(bru_iq_j.bits.ori_pc), is_pc_idx(bru_iq_j.bits.ori_pc), jtb_update_ptr )
		jtb_update_ptr := Mux( is_pc_hit(bru_iq_j.bits.ori_pc), jtb_update_ptr, jtb_update_ptr + 1.U )

		jtb_buf(jtb_update_ptr).ori_pc := bru_iq_j.bits.ori_pc
		jtb_buf(jtb_update_ptr).tgt_pc := bru_iq_j.bits.tgt_pc
	}

	def jtb_read(ori_pc: UInt): UInt = { 
		Mux( is_pc_hit(ori_pc), jtb_buf(is_pc_idx(ori_pc)).tgt_pc, "h80000000".U )
	} 
}




abstract class BranchPredict() extends Module with PreDecode with BHT with JTB{


	// val bru_iq_b = new DecoupledIO( Bool() )
	// val bru_iq_j = new DecoupledIO( new Info_JTB )

	val iq_pc_info = new DecoupledIO(UInt(64.W))

	// val flush = Input(Bool())


/*	
	branch history queue, for bru result checking 
	write when one branch instr decode
	read and pop when a bru return 
*/
	val bhq = Module(new Queue( new Info_BHQ, 16 )) //1 input, 1 output 


/* 
	jalr history queue, 
	for jalr return check
	check for any jalr return
	push when a jalr decode
	pop and check when a bru jalr return
*/
	val jhq = Module(new Queue( UInt(64.W), 16 )) //1 input, 1 output I dont want to waste resource in 2 jalr instr situationb
	
	val ras = Module(new Gen_ringStack( UInt(32.W), 4 ))




	// def bhq_read(): Info_BHQ = return bhq.io.pop(0).bits
	// def jhq_read(): Info_JTB = return jhq.io.pop(0).bits



	def is_1stCut: Bool
	def is_2ndCut: Bool
	def is_noCut  = ~is_1stCut & ~is_2ndCut
	override def is_2nd00: Bool = { 
		return 	(is_1st00) |
				(~is_2nd16 & ~is_2nd32) |
				is_1stCut
	}

	def is_jal    = MuxCase( false.B, Array( is_1stCut -> preDecode_info(0).is_jal,    is_2ndCut -> preDecode_info(1).is_jal ))
	def is_jalr   = MuxCase( false.B, Array( is_1stCut -> preDecode_info(0).is_jalr,   is_2ndCut -> preDecode_info(1).is_jalr ))
	def is_branch = MuxCase( false.B, Array( is_1stCut -> preDecode_info(0).is_branch, is_2ndCut -> preDecode_info(1).is_branch ))
	def is_call   = MuxCase( false.B, Array( is_1stCut -> preDecode_info(0).is_call,   is_2ndCut -> preDecode_info(1).is_call ))
	def is_return = MuxCase( false.B, Array( is_1stCut -> preDecode_info(0).is_return, is_2ndCut -> preDecode_info(1).is_return ))
	def is_rvc    = MuxCase( false.B, Array( is_1stCut -> preDecode_info(0).is_rvc,    is_2ndCut -> preDecode_info(1).is_rvc ))
	def is_fencei = MuxCase( false.B, Array( is_1stCut -> preDecode_info(0).is_fencei, is_2ndCut -> preDecode_info(1).is_fencei ))
	def imm       = MuxCase( false.B, Array( is_1stCut -> preDecode_info(0).imm,       is_2ndCut -> preDecode_info(1).imm ))

	def pc1: UInt
	def pc2: UInt

	def ori_pc = Mux( is_1stCut, pc1, pc2)






	def next_pc = Mux( is_rvc, ori_pc + 2.U, ori_pc + 4.U )
	def jal_pc = ori_pc + imm
	def branch_pc = ori_pc + imm
	def misPredict_pc_b = bhq.io.deq.bits.opp_pc
	def misPredict_pc_j = bru_iq_j.bits
	def ras_pc = ras.io.pop.bits
	def jtb_pc = jtb_read(ori_pc)
	def jalr_pc = Mux( is_ras_taken, ras_pc, jtb_pc)


	def is_ras_taken          = is_return & ras.io.pop.valid
	def is_predict_taken      = bht_predict(ori_pc)

	def is_misPredict_taken_b = (bru_iq_b.bits =/= bhq.io.deq.bits.dir)
	def is_misPredict_taken_j = (bru_iq_j.bits.tgt_pc =/= jhq.io.deq.bits)



	iq_pc_info.valid := is_jal | is_jalr | is_predict_taken | is_misPredict_taken_b | is_misPredict_taken_j
	iq_pc_info.bits  := MuxCase(DontCare, Array(
		is_jal                -> jal_pc,
		is_jalr               -> jalr_pc,
		is_predict_taken      -> branch_pc,
		is_misPredict_taken_b -> bhq.io.deq.bits.opp_pc,
		is_misPredict_taken_j -> bru_iq_j.bits
	))


	//bht update
	override def bht_sel = bhq.io.deq.bits.ori_pc
	// bru_iq_b <> bru_iq_b

	//bhq update
	bhq.io.enq.bits.dir    := is_predict_taken
	bhq.io.enq.bits.ori_pc := ori_pc
	bhq.io.enq.bits.opp_pc := Mux( is_predict_taken, next_pc, branch_pc )
	bhq.io.enq.valid       := is_branch & ~is_noCut
	//jhq update
	jhq.io.enq.bits  := jalr_pc
	jhq.io.enq.valid := is_jalr & ~is_noCut

	//jtb
	// bru_iq_j <> bru_iq_j

	//ras
	ras.io.push.bits := Mux( is_call, next_pc, DontCare)


	def bhq_nack = bhq.io.enq.valid & ~bhq.io.enq.ready
	def jhq_nack = jhq.io.enq.valid & ~jhq.io.enq.ready
	def is_branch_predict_nready = bhq_nack | jhq_nack
	assert( ~(bhq_nack & jhq_nack) )


	// assert( ~(bhq.io.enq.valid & ~bhq.io.enq.ready), "Assert Fail at BHQ.push(0)" )
	assert( ~(bhq.io.deq.ready & ~bhq.io.deq.valid), "Assert Fail at BHQ.pop" )

	// assert( ~(jhq.io.enq.valid & ~jhq.io.enq.ready), "Assert Fail at JHQ.push" )
	assert( ~(jhq.io.deq.ready & ~jhq.io.deq.valid), "Assert Fail at JHQ.pop" )



}


