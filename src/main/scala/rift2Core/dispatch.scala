/*
* @Author: Ruige Lee
* @Date:   2021-03-23 09:31:02
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-03-24 12:02:49
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
import chisel3.experimental.chiselName


class Rename(ptr: Vec[UInt], log: Vec[Vec[UInt]] ) {

	
	def is_free_1st (rd0: UInt): Bool = return log(rd0).exists( (x:UInt) => (x === 0.U) )
	def is_free_2nd (rd0: UInt, rd1: UInt): Bool = {
		return Mux(
				(rd0 === rd1),
				(log(rd1).count((x: UInt) => (x === 0.U)) >= 2.U),
				is_free_1st (rd1)
			)
	}

	def malloc_1st(rd0: UInt): UInt = return log(rd0).indexWhere( (x:UInt) => (x === 0.U) )
	def malloc_2nd(rd0: UInt): UInt = return log(rd0).lastIndexWhere( (x:UInt) => (x === 0.U) )

	def lookup( rs: UInt ): UInt = return ptr(rs)

}

trait ReOrder {

	def rod_mux_i(id_dpt_info: Info_id_dpt, rd0_idx: UInt): Info_reorder_i = {
		val reorder_i_info = Wire(new Info_reorder_i)
		reorder_i_info.pc             := id_dpt_info.info.param.pc
		reorder_i_info.rd0_raw        := id_dpt_info.info.param.rd0_raw
		reorder_i_info.rd0_idx        := rd0_idx
		reorder_i_info.is_branch      := id_dpt_info.info.bru_isa.is_branch
		reorder_i_info.is_lu          := id_dpt_info.info.lsu_isa.is_lu
		reorder_i_info.is_su          := id_dpt_info.info.lsu_isa.is_su
		reorder_i_info.is_fence       := id_dpt_info.info.lsu_isa.fence
		reorder_i_info.is_csr         := id_dpt_info.info.csr_isa.is_csr
		reorder_i_info.privil         := id_dpt_info.info.privil_isa
		reorder_i_info.is_accessFault := id_dpt_info.is_iFAccessFault
		reorder_i_info.is_illeage     := id_dpt_info.is_illeage

		return reorder_i_info
	}


	def rod_mux_f(id_dpt_info: Info_id_dpt, rd0_idx: UInt): Info_reorder_f = {
		val reorder_f_info = Wire(new Info_reorder_f)
		reorder_f_info.pc             := id_dpt_info.info.param.pc
		reorder_f_info.rd0_raw        := id_dpt_info.info.param.rd0_raw 
		reorder_f_info.rd0_idx        := rd0_idx
		reorder_f_info.is_lu          := id_dpt_info.info.lsu_isa.is_lu
		reorder_f_info.is_su          := id_dpt_info.info.lsu_isa.is_su
		return reorder_f_info
	}

}

trait Dpt{

	def dpt_mux_alu( id_dpt_info: Info_id_dpt, rd0_idx: UInt, rs1_idx: UInt, rs2_idx: UInt ): Alu_dpt_info = {
		val alu_dpt_info = Wire(new Alu_dpt_info)
		alu_dpt_info.isa        := id_dpt_info.info.alu_isa
		alu_dpt_info.param      := id_dpt_info.info.param
		alu_dpt_info.rn.rd0_idx := rd0_idx
		alu_dpt_info.rn.rs1_idx := rs1_idx
		alu_dpt_info.rn.rs2_idx := rs2_idx
		alu_dpt_info.rn.rs3_idx := DontCare
		return alu_dpt_info
	}

	def dpt_mux_bru( id_dpt_info: Info_id_dpt, rd0_idx: UInt, rs1_idx: UInt, rs2_idx: UInt ): Bru_dpt_info = {
		val bru_dpt_info = Wire(new Bru_dpt_info)
		bru_dpt_info.isa        := id_dpt_info.info.bru_isa
		bru_dpt_info.param      := id_dpt_info.info.param
		bru_dpt_info.rn.rd0_idx := rd0_idx
		bru_dpt_info.rn.rs1_idx := rs1_idx
		bru_dpt_info.rn.rs2_idx := rs2_idx
		bru_dpt_info.rn.rs3_idx := DontCare
		return bru_dpt_info
	}

	def dpt_mux_lsu( id_dpt_info: Info_id_dpt, rd0_idx: UInt, rs1_idx: UInt, rs2_idx: UInt ): Lsu_dpt_info = {
		val lsu_dpt_info = Wire(new Lsu_dpt_info)
		lsu_dpt_info.isa        := id_dpt_info.info.lsu_isa
		lsu_dpt_info.param      := id_dpt_info.info.param
		lsu_dpt_info.rn.rd0_idx := rd0_idx
		lsu_dpt_info.rn.rs1_idx := rs1_idx
		lsu_dpt_info.rn.rs2_idx := rs2_idx
		lsu_dpt_info.rn.rs3_idx := DontCare
		return lsu_dpt_info
	}

	def dpt_mux_csr( id_dpt_info: Info_id_dpt, rd0_idx: UInt, rs1_idx: UInt ): Csr_dpt_info = {
		val csr_dpt_info = Wire(new Csr_dpt_info)
		csr_dpt_info.isa        := id_dpt_info.info.csr_isa
		csr_dpt_info.param      := id_dpt_info.info.param
		csr_dpt_info.rn.rd0_idx := rd0_idx
		csr_dpt_info.rn.rs1_idx := rs1_idx
		csr_dpt_info.rn.rs2_idx := DontCare
		csr_dpt_info.rn.rs3_idx := DontCare
		return csr_dpt_info
	}

	def dpt_mux_mul( id_dpt_info: Info_id_dpt, rd0_idx: UInt, rs1_idx: UInt, rs2_idx: UInt ): Mul_dpt_info = {
		val mul_dpt_info = Wire(new Mul_dpt_info)
		mul_dpt_info.isa        := id_dpt_info.info.mul_isa
		mul_dpt_info.param      := id_dpt_info.info.param
		mul_dpt_info.rn.rd0_idx := rd0_idx
		mul_dpt_info.rn.rs1_idx := rs1_idx
		mul_dpt_info.rn.rs2_idx := rs2_idx
		mul_dpt_info.rn.rs3_idx := DontCare
		return mul_dpt_info
	}

	def dpt_mux_fpu( id_dpt_info: Info_id_dpt, rd0_idx: UInt, rs1_idx: UInt, rs2_idx: UInt, rs3_idx: UInt ): Fpu_dpt_info = {
		val fpu_dpt_info = Wire(new Fpu_dpt_info)
		fpu_dpt_info.isa        := id_dpt_info.info.fpu_isa
		fpu_dpt_info.param      := id_dpt_info.info.param
		fpu_dpt_info.rn.rd0_idx := rd0_idx
		fpu_dpt_info.rn.rs1_idx := rs1_idx
		fpu_dpt_info.rn.rs2_idx := rs2_idx
		fpu_dpt_info.rn.rs3_idx := rs3_idx
		return fpu_dpt_info
	}


}




@chiselName
class Dispatch_ss extends Module with Superscalar with ReOrder with Dpt{
	val io = IO(new Bundle{
		val id_dpt = Vec(2, Flipped(new DecoupledIO(new Info_id_dpt())))

		val alu_dpt_iss = new DecoupledIO(new Alu_dpt_info())
		val bru_dpt_iss = new DecoupledIO(new Bru_dpt_info())
		val lsu_dpt_iss = new DecoupledIO(new Lsu_dpt_info())
		val csr_dpt_iss = new DecoupledIO(new Csr_dpt_info())
		val mul_dpt_iss = new DecoupledIO(new Mul_dpt_info())

		val rod_i = Vec(2,new DecoupledIO(new Info_reorder_i))

		val rn_ptr_i = Vec(32, Input(UInt(2.W))) 
		val log_i = Vec(32,Vec(4, Input(UInt(2.W))) )
		val rn_op_i = Vec(32, Vec(4, Output(Bool())))


		val flush = Input(Bool())

	})

	val rename_i = new Rename(io.rn_ptr_i, io.log_i)

	val alu_dpt_iss_fifo = Module(new Queue(new Alu_dpt_info, 4))
	val bru_dpt_iss_fifo = Module(new Queue(new Bru_dpt_info, 4))
	val lsu_dpt_iss_fifo = Module(new Queue(new Lsu_dpt_info, 4))
	val csr_dpt_iss_fifo = Module(new Queue(new Csr_dpt_info, 4))
	val mul_dpt_iss_fifo = Module(new Queue(new Mul_dpt_info, 4))

	val reOrder_fifo_i = Module(new MultiPortFifo(new Info_reorder_i, 4, 2, 2))

	val is_alu_dpt_1st = Wire(Bool())
	val is_bru_dpt_1st = Wire(Bool())
	val is_lsu_dpt_1st = Wire(Bool())
	val is_csr_dpt_1st = Wire(Bool())
	val is_mul_dpt_1st = Wire(Bool())
	val is_privil_dpt_1st = Wire(Bool())
	val is_dpt_1st = Wire(Bool())

	val is_alu_dpt_2nd    = Wire(Bool())
	val is_bru_dpt_2nd    = Wire(Bool())
	val is_lsu_dpt_2nd    = Wire(Bool())
	val is_csr_dpt_2nd    = Wire(Bool())
	val is_mul_dpt_2nd    = Wire(Bool())
	val is_privil_dpt_2nd = Wire(Bool())
	val is_dpt_2nd = Wire(Bool())




	alu_dpt_iss_fifo.reset := io.flush | reset.asBool
	bru_dpt_iss_fifo.reset := io.flush | reset.asBool
	lsu_dpt_iss_fifo.reset := io.flush | reset.asBool
	csr_dpt_iss_fifo.reset := io.flush | reset.asBool
	mul_dpt_iss_fifo.reset := io.flush | reset.asBool

	reOrder_fifo_i.io.flush := io.flush


	io.alu_dpt_iss <> alu_dpt_iss_fifo.io.deq
	io.bru_dpt_iss <> bru_dpt_iss_fifo.io.deq
	io.lsu_dpt_iss <> lsu_dpt_iss_fifo.io.deq
	io.csr_dpt_iss <> csr_dpt_iss_fifo.io.deq
	io.mul_dpt_iss <> mul_dpt_iss_fifo.io.deq

	io.rod_i <> reOrder_fifo_i.io.deq

	def is_iwb(i: Int) = true.B

	def id_dpt_info(i: Int) = io.id_dpt(i).bits
	def instruction_info(i: Int) = id_dpt_info(i).info


	def rd0_raw(i: Int) = instruction_info(i).param.rd0_raw
	def rs1_raw(i: Int) = instruction_info(i).param.rs1_raw	
	def rs2_raw(i: Int) = instruction_info(i).param.rs2_raw
	def rs3_raw(i: Int) = instruction_info(i).param.rs3_raw



	val rd0_idx_1st = Wire(UInt(2.W))
	val rs1_idx_1st = Wire(UInt(2.W))
	val rs2_idx_1st = Wire(UInt(2.W))
	val rd0_idx_2nd = Wire(UInt(2.W))
	val rs1_idx_2nd = Wire(UInt(2.W))
	val rs2_idx_2nd = Wire(UInt(2.W))

	rd0_idx_1st := rename_i.malloc_1st(rd0_raw(0))
	rd0_idx_2nd := rename_i.malloc_2nd(rd0_raw(1))
	
	rs1_idx_1st := rename_i.lookup(rs1_raw(0))
	rs2_idx_1st := rename_i.lookup(rs2_raw(0))
	


	rs1_idx_2nd := Mux( ( rd0_raw(0) === rs1_raw(1) ), rd0_idx_1st, rename_i.lookup(rs1_raw(1)) )
	rs2_idx_2nd := Mux( ( rd0_raw(0) === rs2_raw(1) ), rd0_idx_1st, rename_i.lookup(rs2_raw(1)) )

	
	


	for ( i <- 0 until 32; j <- 0 until 4 ) yield {
		io.rn_op_i(i)(j) := 
			((i.U === rd0_raw(0)) & ( j.U === rd0_idx_1st ) & is_dpt_1st & is_iwb(0)) | 
			((i.U === rd0_raw(1)) & ( j.U === rd0_idx_2nd ) & is_dpt_2nd & is_iwb(1))

	}






	
	
	def is_rod_ready(i: Int) = reOrder_fifo_i.io.enq(i).ready //Mux(is_iwb(i), reOrder_fifo_i.io.enq(i).ready, reOrder_fifo_f.io.enq(i).ready)



	is_alu_dpt_1st    := 
				(io.id_dpt(0).valid & instruction_info(0).alu_isa.is_alu & rename_i.is_free_1st(rd0_raw(0)) & is_rod_ready(0) & alu_dpt_iss_fifo.io.enq.ready)

	is_bru_dpt_1st    := 
				(io.id_dpt(0).valid & instruction_info(0).bru_isa.is_bru & rename_i.is_free_1st(rd0_raw(0)) & is_rod_ready(0) & bru_dpt_iss_fifo.io.enq.ready)

	is_lsu_dpt_1st    := 
				(io.id_dpt(0).valid & instruction_info(0).lsu_isa.is_lsu & rename_i.is_free_1st(rd0_raw(0)) & is_rod_ready(0) & lsu_dpt_iss_fifo.io.enq.ready)

	is_csr_dpt_1st    := 
				(io.id_dpt(0).valid & instruction_info(0).csr_isa.is_csr & rename_i.is_free_1st(rd0_raw(0)) & is_rod_ready(0) & csr_dpt_iss_fifo.io.enq.ready)

	is_mul_dpt_1st    := 
				(io.id_dpt(0).valid & instruction_info(0).mul_isa.is_mul & rename_i.is_free_1st(rd0_raw(0)) & is_rod_ready(0) & mul_dpt_iss_fifo.io.enq.ready)

	is_privil_dpt_1st := 
				(io.id_dpt(0).valid & (instruction_info(0).privil_isa.is_privil | io.id_dpt(0).bits.is_iFAccessFault | io.id_dpt(0).bits.is_illeage ) & is_rod_ready(0))
	
	is_dpt_1st        := 
				(is_alu_dpt_1st | is_bru_dpt_1st | is_lsu_dpt_1st | is_csr_dpt_1st | is_mul_dpt_1st | is_privil_dpt_1st) //| is_fpu_dpt_1st




	is_alu_dpt_2nd    := 
				(~is_alu_dpt_1st & is_dpt_1st) &
				(io.id_dpt(1).valid & instruction_info(1).alu_isa.is_alu & rename_i.is_free_2nd(rd0_raw(0), rd0_raw(1)) & is_rod_ready(1) & alu_dpt_iss_fifo.io.enq.ready )
	
	is_bru_dpt_2nd    := 
				(~is_bru_dpt_1st & is_dpt_1st) &
				(io.id_dpt(1).valid & instruction_info(1).bru_isa.is_bru & rename_i.is_free_2nd(rd0_raw(0), rd0_raw(1)) & is_rod_ready(1) & bru_dpt_iss_fifo.io.enq.ready)

	is_lsu_dpt_2nd    := 
				(~is_lsu_dpt_1st & is_dpt_1st) &
				(io.id_dpt(1).valid & instruction_info(1).lsu_isa.is_lsu & rename_i.is_free_2nd(rd0_raw(0), rd0_raw(1)) & is_rod_ready(1) & lsu_dpt_iss_fifo.io.enq.ready)


	is_csr_dpt_2nd    := 
				(~is_csr_dpt_1st & is_dpt_1st) &
				(io.id_dpt(1).valid & instruction_info(1).csr_isa.is_csr & rename_i.is_free_2nd(rd0_raw(0), rd0_raw(1)) & is_rod_ready(1) & csr_dpt_iss_fifo.io.enq.ready)


	is_mul_dpt_2nd    := 
				(~is_mul_dpt_1st & is_dpt_1st) &
				(io.id_dpt(1).valid & instruction_info(1).mul_isa.is_mul & rename_i.is_free_2nd(rd0_raw(0), rd0_raw(1)) & is_rod_ready(1) & mul_dpt_iss_fifo.io.enq.ready)


	is_privil_dpt_2nd := 
				(is_dpt_1st) &
				(io.id_dpt(1).valid & (instruction_info(1).privil_isa.is_privil | io.id_dpt(1).bits.is_iFAccessFault | io.id_dpt(1).bits.is_illeage) & is_rod_ready(1))


	is_dpt_2nd        := is_alu_dpt_2nd | is_bru_dpt_2nd | is_lsu_dpt_2nd | is_csr_dpt_2nd | is_mul_dpt_2nd | is_privil_dpt_2nd //| is_fpu_dpt_2nd






	alu_dpt_iss_fifo.io.enq.bits := Mux(
										is_alu_dpt_1st,
										dpt_mux_alu(id_dpt_info(0), rd0_idx_1st, rs1_idx_1st, rs2_idx_1st),
										dpt_mux_alu(id_dpt_info(1), rd0_idx_2nd, rs1_idx_2nd, rs2_idx_2nd)
										)
	bru_dpt_iss_fifo.io.enq.bits := Mux(
										is_bru_dpt_1st,
										dpt_mux_bru(id_dpt_info(0), rd0_idx_1st, rs1_idx_1st, rs2_idx_1st),
										dpt_mux_bru(id_dpt_info(1), rd0_idx_2nd, rs1_idx_2nd, rs2_idx_2nd)
									)
	lsu_dpt_iss_fifo.io.enq.bits := Mux(
										is_lsu_dpt_1st,
										dpt_mux_lsu(id_dpt_info(0), rd0_idx_1st, rs1_idx_1st, rs2_idx_1st),
										dpt_mux_lsu(id_dpt_info(1), rd0_idx_2nd, rs1_idx_2nd, rs2_idx_2nd)
									)
	csr_dpt_iss_fifo.io.enq.bits := Mux(
										is_csr_dpt_1st,
										dpt_mux_csr(id_dpt_info(0), rd0_idx_1st, rs1_idx_1st),
										dpt_mux_csr(id_dpt_info(1), rd0_idx_2nd, rs1_idx_2nd)
									)
	mul_dpt_iss_fifo.io.enq.bits := Mux(
										is_mul_dpt_1st,
										dpt_mux_mul(id_dpt_info(0), rd0_idx_1st, rs1_idx_1st, rs2_idx_1st),
										dpt_mux_mul(id_dpt_info(1), rd0_idx_2nd, rs1_idx_2nd, rs2_idx_2nd)
									)

	// fpu_dpt_iss_fifo.io.enq(0).bits := dpt_mux_fpu(id_dpt_info(0), rd0_idx_1st, rs1_idx_1st, rs2_idx_1st, rs3_idx_1st)



	reOrder_fifo_i.io.enq(0).bits := rod_mux_i(id_dpt_info(0), rd0_idx_1st)
	reOrder_fifo_i.io.enq(1).bits := rod_mux_i(id_dpt_info(1), rd0_idx_2nd)

	override def is_1st_solo = false.B
	override def is_2nd_solo = false.B


	io.id_dpt(0).ready := is_dpt_1st
	io.id_dpt(1).ready := is_dpt_2nd



	alu_dpt_iss_fifo.io.enq.valid := is_alu_dpt_1st | is_alu_dpt_2nd
	bru_dpt_iss_fifo.io.enq.valid := is_bru_dpt_1st | is_bru_dpt_2nd
	lsu_dpt_iss_fifo.io.enq.valid := is_lsu_dpt_1st | is_lsu_dpt_2nd
	csr_dpt_iss_fifo.io.enq.valid := is_csr_dpt_1st | is_csr_dpt_2nd
	mul_dpt_iss_fifo.io.enq.valid := is_mul_dpt_1st | is_mul_dpt_2nd
	// fpu_dpt_iss_fifo.io.enq(0).valid := is_fpu_dpt_1st


	reOrder_fifo_i.io.enq(0).valid := is_iwb(0) & is_dpt_1st
	reOrder_fifo_i.io.enq(1).valid := is_iwb(1) & is_dpt_2nd



}


