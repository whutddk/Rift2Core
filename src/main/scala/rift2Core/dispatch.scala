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



class Rename(ptr: Vec[UInt], log: Vec[Vec[UInt]] ) {

	
	def is_full_1st (rd0: UInt): Bool = return log(rd0).exists( (x:UInt) => (x === 0.U) )
	def is_full_2nd (rd0: UInt, rd1: UInt): Bool = {
		return Mux(
				(rd0 === rd1),
				(log(rd1).count((x: UInt) => (x === 0.U)) >= 2.U),
				is_full_1st (rd1)
			)
	}

	def malloc_1st(rd0: UInt): UInt = return log(rd0).indexWhere( (x:UInt) => (x === 0.U) )
	def malloc_2nd(rd0: UInt): UInt = return log(rd0).lastIndexWhere( (x:UInt) => (x === 0.U) )

	def lookup_1st( rs: UInt ): UInt = return ptr(rs)
	def lookup_2nd( rs: UInt, rd_1st: UInt ): UInt = {
		return Mux(
				( rs === rd_1st ),
				malloc_1st(rd_1st),
				lookup_1st(rs)
			)
	}
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
		reorder_i_info.is_accessFault := id_dpt_info.isIFAccessFault
		reorder_i_info.is_illeage     := id_dpt_info.isIlleage

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
		mul_dpt_info.param      := DontCare
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



class Dispatch_ss extends Module with Superscalar with ReOrder with Dpt{
	val io = IO(new Bundle{
		val id_dpt = Vec(2, Flipped(new DecoupledIO(new Info_id_dpt())))

		val alu_dpt_iss = new DecoupledIO(new Alu_dpt_info())
		val bru_dpt_iss = new DecoupledIO(new Bru_dpt_info())
		val lsu_dpt_iss = new DecoupledIO(new Lsu_dpt_info())
		val csr_dpt_iss = new DecoupledIO(new Csr_dpt_info())
		val mul_dpt_iss = new DecoupledIO(new Mul_dpt_info())
		val fpu_dpt_iss = new DecoupledIO(new Fpu_dpt_info())

		val rod_i = Vec(2,new DecoupledIO(new Info_reorder_i))
		val rod_f = Vec(2,new DecoupledIO(new Info_reorder_f))

		val rn_ptr_i = Vec(32, Input(UInt(2.W))) 
		val log_i = Vec(32,Vec(4, Input(UInt(2.W))) )
		val rn_op_i = Vec(32, Vec(4, Output(Bool())))

		val rn_ptr_f = Vec(32, Input(UInt(2.W))) 
		val log_f = Vec(32,Vec(4, Input(UInt(2.W))) )
		val rn_op_f = Vec(32, Vec(4, Output(Bool())))

		val flush = Input(Bool())

	})

	val rename_i = new Rename(io.rn_ptr_i, io.log_i)
	val rename_f = new Rename(io.rn_ptr_f, io.log_f)

	val alu_dpt_iss_fifo = Module(new MultiPortFifo(new Alu_dpt_info, 2, 2, 1))
	val bru_dpt_iss_fifo = Module(new MultiPortFifo(new Bru_dpt_info, 2, 2, 1))
	val lsu_dpt_iss_fifo = Module(new MultiPortFifo(new Lsu_dpt_info, 2, 2, 1))
	val csr_dpt_iss_fifo = Module(new MultiPortFifo(new Csr_dpt_info, 2, 2, 1))
	val mul_dpt_iss_fifo = Module(new MultiPortFifo(new Mul_dpt_info, 2, 2, 1))
	val fpu_dpt_iss_fifo = Module(new MultiPortFifo(new Fpu_dpt_info, 2, 2, 1))

	val reOrder_fifo_i = Module(new MultiPortFifo(new Info_reorder_i, 4, 2, 2))
	val reOrder_fifo_f = Module(new MultiPortFifo(new Info_reorder_f, 4, 2, 2))

	alu_dpt_iss_fifo.io.flush := io.flush
	bru_dpt_iss_fifo.io.flush := io.flush
	lsu_dpt_iss_fifo.io.flush := io.flush
	csr_dpt_iss_fifo.io.flush := io.flush
	mul_dpt_iss_fifo.io.flush := io.flush
	fpu_dpt_iss_fifo.io.flush := io.flush
	reOrder_fifo_i.io.flush := io.flush
	reOrder_fifo_f.io.flush := io.flush


	io.alu_dpt_iss <> alu_dpt_iss_fifo.io.deq(0)
	io.bru_dpt_iss <> bru_dpt_iss_fifo.io.deq(0)
	io.lsu_dpt_iss <> lsu_dpt_iss_fifo.io.deq(0)
	io.csr_dpt_iss <> csr_dpt_iss_fifo.io.deq(0)
	io.mul_dpt_iss <> mul_dpt_iss_fifo.io.deq(0)
	io.fpu_dpt_iss <> fpu_dpt_iss_fifo.io.deq(0)

	io.rod_i <> reOrder_fifo_i.io.deq
	io.rod_f <> reOrder_fifo_f.io.deq


	def is_iwb(i: Int) = io.id_dpt(i).bits.info.is_iwb
	def is_fwb(i: Int) = io.id_dpt(i).bits.info.is_fwb

	def id_dpt_info(i: Int) = io.id_dpt(i).bits
	def instruction_info(i: Int) = id_dpt_info(i).info


	def rd0_raw(i: Int) = instruction_info(i).param.rd0_raw
	def rs1_raw(i: Int) = instruction_info(i).param.rs1_raw	
	def rs2_raw(i: Int) = instruction_info(i).param.rs2_raw
	def rs3_raw(i: Int) = instruction_info(i).param.rs3_raw



	// val rodmux = new ReOrder_mux(id_dpt_info, rd0_idx)

	def rd0_idx_1st = Mux( is_iwb(0), rename_i.malloc_1st(rd0_raw(0)), rename_f.malloc_1st(rd0_raw(0)) )
	def rs1_idx_1st = Mux( is_iwb(0), rename_i.lookup_1st(rs1_raw(0)), rename_f.lookup_1st(rs1_raw(0)) )
	def rs2_idx_1st = Mux( is_iwb(0), rename_i.lookup_1st(rs2_raw(0)), rename_f.lookup_1st(rs2_raw(0)) )
	def rs3_idx_1st = Mux( is_iwb(0), rename_i.lookup_1st(rs3_raw(0)), rename_f.lookup_1st(rs3_raw(0)) )

	def rd0_idx_2nd = Mux( is_iwb(1), rename_i.malloc_2nd(rd0_raw(1)), rename_f.malloc_2nd(rd0_raw(1)) )
	def rs1_idx_2nd = Mux( is_iwb(1), rename_i.lookup_2nd(rs1_raw(1), rd0_raw(0)), rename_f.malloc_2nd(rs1_raw(1)) )
	def rs2_idx_2nd = Mux( is_iwb(1), rename_i.lookup_2nd(rs2_raw(1), rd0_raw(0)), rename_f.malloc_2nd(rs2_raw(1)) )
	def rs3_idx_2nd = Mux( is_iwb(1), rename_i.lookup_2nd(rs3_raw(1), rd0_raw(0)), rename_f.malloc_2nd(rs3_raw(1)) )
	// val dptmux = new Dpt_mux ( id_dpt_info, rd0_idx, rs1_idx, rs2_idx, rs3_idx )





	for ( i <- 0 until 32; j <- 0 until 4 ) yield {
		io.rn_op_i(i)(j) := 
			((i.U === rd0_raw(0)) & ( j.U === rd0_idx_1st ) & is_dpt_1st & is_iwb(0)) | 
			((i.U === rd0_raw(1)) & ( j.U === rd0_idx_2nd ) & is_dpt_2nd & is_iwb(1))

		io.rn_op_f(i)(j) := 
			((i.U === rd0_raw(0)) & ( j.U === rd0_idx_1st ) & is_dpt_1st & is_fwb(0)) | 
			((i.U === rd0_raw(1)) & ( j.U === rd0_idx_2nd ) & is_dpt_2nd & is_fwb(1))
	}






	
	
	def is_rod_ready(i: Int) = Mux(is_iwb(i), reOrder_fifo_i.io.enq(i).ready, reOrder_fifo_f.io.enq(i).ready)


	def is_alu_dpt_1st    = instruction_info(0).alu_isa.is_alu & ~rename_i.is_full_1st(rd0_raw(0)) & is_rod_ready(0) & alu_dpt_iss_fifo.io.enq(0).ready
	def is_bru_dpt_1st    = instruction_info(0).bru_isa.is_bru & ~rename_i.is_full_1st(rd0_raw(0)) & is_rod_ready(0) & bru_dpt_iss_fifo.io.enq(0).ready
	def is_lsu_dpt_1st    = instruction_info(0).lsu_isa.is_lsu & Mux( is_iwb(0), ~rename_i.is_full_1st(rd0_raw(0)), ~rename_f.is_full_1st(rd0_raw(0)) ) & is_rod_ready(0) & lsu_dpt_iss_fifo.io.enq(0).ready
	def is_csr_dpt_1st    = instruction_info(0).csr_isa.is_csr & ~rename_i.is_full_1st(rd0_raw(0)) & is_rod_ready(0) & csr_dpt_iss_fifo.io.enq(0).ready
	def is_mul_dpt_1st    = instruction_info(0).mul_isa.is_mul & ~rename_i.is_full_1st(rd0_raw(0)) & is_rod_ready(0) & mul_dpt_iss_fifo.io.enq(0).ready
	def is_fpu_dpt_1st    = instruction_info(0).fpu_isa.is_fpu & Mux( is_iwb(0), ~rename_i.is_full_1st(rd0_raw(0)), ~rename_f.is_full_1st(rd0_raw(0)) ) & is_rod_ready(0) & fpu_dpt_iss_fifo.io.enq(0).ready
	def is_privil_dpt_1st = instruction_info(0).privil_isa.is_privil & is_rod_ready(0)
	def is_dpt_1st = is_alu_dpt_1st | is_bru_dpt_1st | is_lsu_dpt_1st | is_csr_dpt_1st | is_mul_dpt_1st | is_fpu_dpt_1st | is_privil_dpt_1st

	def is_alu_dpt_2nd    = instruction_info(1).alu_isa.is_alu & ~rename_i.is_full_2nd(rd0_raw(0), rd0_raw(1)) & is_rod_ready(1) & alu_dpt_iss_fifo.io.enq(1).ready & is_dpt_1st
	def is_bru_dpt_2nd    = instruction_info(1).bru_isa.is_bru & ~rename_i.is_full_2nd(rd0_raw(0), rd0_raw(1)) & is_rod_ready(1) & bru_dpt_iss_fifo.io.enq(1).ready & is_dpt_1st
	def is_lsu_dpt_2nd    = instruction_info(1).lsu_isa.is_lsu & Mux( is_iwb(1), ~rename_i.is_full_2nd(rd0_raw(0), rd0_raw(1)), ~rename_f.is_full_2nd(rd0_raw(0), rd0_raw(1)) ) & is_rod_ready(1) & lsu_dpt_iss_fifo.io.enq(1).ready & is_dpt_1st
	def is_csr_dpt_2nd    = instruction_info(1).csr_isa.is_csr & ~rename_i.is_full_2nd(rd0_raw(0), rd0_raw(1)) & is_rod_ready(1) & csr_dpt_iss_fifo.io.enq(1).ready & is_dpt_1st
	def is_mul_dpt_2nd    = instruction_info(1).mul_isa.is_mul & ~rename_i.is_full_2nd(rd0_raw(0), rd0_raw(1)) & is_rod_ready(1) & mul_dpt_iss_fifo.io.enq(1).ready & is_dpt_1st
	def is_fpu_dpt_2nd    = instruction_info(1).fpu_isa.is_fpu & Mux( is_iwb(1), ~rename_i.is_full_2nd(rd0_raw(0), rd0_raw(1)), ~rename_f.is_full_2nd(rd0_raw(0), rd0_raw(1)) ) & is_rod_ready(1) & fpu_dpt_iss_fifo.io.enq(1).ready & is_dpt_1st
	def is_privil_dpt_2nd = instruction_info(1).privil_isa.is_privil & is_rod_ready(1) & is_dpt_1st
	def is_dpt_2nd = is_alu_dpt_2nd | is_bru_dpt_2nd | is_lsu_dpt_2nd | is_csr_dpt_2nd | is_mul_dpt_2nd | is_fpu_dpt_2nd | is_privil_dpt_2nd






	alu_dpt_iss_fifo.io.enq(0).bits := dpt_mux_alu(id_dpt_info(0), rd0_idx_1st, rs1_idx_1st, rs2_idx_1st)
	bru_dpt_iss_fifo.io.enq(0).bits := dpt_mux_bru(id_dpt_info(0), rd0_idx_1st, rs1_idx_1st, rs2_idx_1st)
	lsu_dpt_iss_fifo.io.enq(0).bits := dpt_mux_lsu(id_dpt_info(0), rd0_idx_1st, rs1_idx_1st, rs2_idx_1st)
	csr_dpt_iss_fifo.io.enq(0).bits := dpt_mux_csr(id_dpt_info(0), rd0_idx_1st, rs1_idx_1st)
	mul_dpt_iss_fifo.io.enq(0).bits := dpt_mux_mul(id_dpt_info(0), rd0_idx_1st, rs1_idx_1st, rs2_idx_1st)
	fpu_dpt_iss_fifo.io.enq(0).bits := dpt_mux_fpu(id_dpt_info(0), rd0_idx_1st, rs1_idx_1st, rs2_idx_1st, rs3_idx_1st)

	alu_dpt_iss_fifo.io.enq(1).bits := dpt_mux_alu(id_dpt_info(1), rd0_idx_2nd, rs1_idx_2nd, rs2_idx_2nd)
	bru_dpt_iss_fifo.io.enq(1).bits := dpt_mux_bru(id_dpt_info(1), rd0_idx_2nd, rs1_idx_2nd, rs2_idx_2nd)
	lsu_dpt_iss_fifo.io.enq(1).bits := dpt_mux_lsu(id_dpt_info(1), rd0_idx_2nd, rs1_idx_2nd, rs2_idx_2nd)
	csr_dpt_iss_fifo.io.enq(1).bits := dpt_mux_csr(id_dpt_info(1), rd0_idx_2nd, rs1_idx_2nd)
	mul_dpt_iss_fifo.io.enq(1).bits := dpt_mux_mul(id_dpt_info(1), rd0_idx_2nd, rs1_idx_2nd, rs2_idx_2nd)
	fpu_dpt_iss_fifo.io.enq(1).bits := dpt_mux_fpu(id_dpt_info(1), rd0_idx_2nd, rs1_idx_2nd, rs2_idx_2nd, rs3_idx_2nd)


	reOrder_fifo_i.io.enq(0).bits := rod_mux_i(id_dpt_info(0), rd0_idx_1st)
	reOrder_fifo_f.io.enq(0).bits := rod_mux_f(id_dpt_info(0), rd0_idx_1st)
	reOrder_fifo_i.io.enq(1).bits := rod_mux_i(id_dpt_info(1), rd0_idx_2nd)
	reOrder_fifo_f.io.enq(1).bits := rod_mux_f(id_dpt_info(1), rd0_idx_2nd)

	override def is_1st_solo = false.B
	override def is_2nd_solo = is_1st_solo & false.B


	io.id_dpt(0).ready := is_dpt_1st
	io.id_dpt(1).ready := is_dpt_2nd



	alu_dpt_iss_fifo.io.enq(0).valid := is_alu_dpt_1st
	bru_dpt_iss_fifo.io.enq(0).valid := is_bru_dpt_1st
	lsu_dpt_iss_fifo.io.enq(0).valid := is_lsu_dpt_1st
	csr_dpt_iss_fifo.io.enq(0).valid := is_csr_dpt_1st
	mul_dpt_iss_fifo.io.enq(0).valid := is_mul_dpt_1st
	fpu_dpt_iss_fifo.io.enq(0).valid := is_fpu_dpt_1st

	alu_dpt_iss_fifo.io.enq(1).valid := is_alu_dpt_2nd
	bru_dpt_iss_fifo.io.enq(1).valid := is_bru_dpt_2nd
	lsu_dpt_iss_fifo.io.enq(1).valid := is_lsu_dpt_2nd
	csr_dpt_iss_fifo.io.enq(1).valid := is_csr_dpt_2nd
	mul_dpt_iss_fifo.io.enq(1).valid := is_mul_dpt_2nd
	fpu_dpt_iss_fifo.io.enq(1).valid := is_fpu_dpt_2nd

	reOrder_fifo_i.io.enq(0).valid := is_iwb(0) & is_dpt_1st
	reOrder_fifo_i.io.enq(1).valid := is_iwb(1) & is_dpt_2nd
	reOrder_fifo_f.io.enq(0).valid := is_fwb(0) & is_dpt_1st
	reOrder_fifo_f.io.enq(1).valid := is_fwb(1) & is_dpt_2nd





}


