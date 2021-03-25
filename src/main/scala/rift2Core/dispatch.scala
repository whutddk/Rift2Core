/*
* @Author: Ruige Lee
* @Date:   2021-03-23 09:31:02
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-03-24 12:02:49
*/



package rift2Core

import chisel3._
import chisel3.util._
import rift2Core.basicElement._



class Rename(ptr: Vec[UInt], log: Vec[Vec[UInt]] ) {

	// val rn_ptr = Wire(Vec(32, UInt(2.W)))
	// val regLog = Wire(Vec(32,Vec(4, UInt(2.W)) ))


	def full (rd0: UInt): Bool = {
		for ( j <- 0 until 4) {
			if (( log(rd0)(j) == 0.U )) {return false.B}
		}
		return true.B; 
	}

	def malloc(rd0: UInt): UInt = {
		MuxCase(0.U, Array(
			(log(rd0)(0) === 0.U) -> 0.U,
			(log(rd0)(1) === 0.U) -> 1.U,
			(log(rd0)(2) === 0.U) -> 2.U,
			(log(rd0)(3) === 0.U) -> 3.U,
		))
	}

	def lookup( rs: UInt ): UInt = {
		return ptr(rs)
	}


}

class Reorder_mux (id_dpt_info: Info_id_dpt, rd0_idx: UInt) {
	// val instruction_info = new Info_id_dpt
	val reorder_i_info = Wire(new Info_reorder_i)
	val reorder_f_info = Wire(new Info_reorder_f)
	// val rd0_idx = UInt(2.W)


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



		reorder_f_info.pc             := id_dpt_info.info.param.pc
		reorder_f_info.rd0_raw        := id_dpt_info.info.param.rd0_raw 
		reorder_f_info.rd0_idx        := rd0_idx
		reorder_f_info.is_lu          := id_dpt_info.info.lsu_isa.is_lu
		reorder_f_info.is_su          := id_dpt_info.info.lsu_isa.is_su


}


class Dpt_mux ( id_dpt_info: Info_id_dpt, rd0_idx: UInt, rs1_idx: UInt, rs2_idx: UInt, rs3_idx: UInt ) {

	val alu_dpt_info = Wire(new Alu_dpt_info)
	val bru_dpt_info = Wire(new Bru_dpt_info)
	val lsu_dpt_info = Wire(new Lsu_dpt_info)
	val csr_dpt_info = Wire(new Csr_dpt_info)
	val mul_dpt_info = Wire(new Mul_dpt_info)
	val fpu_dpt_info = Wire(new Fpu_dpt_info)

	alu_dpt_info.isa        := id_dpt_info.info.alu_isa
	alu_dpt_info.param      := id_dpt_info.info.param
	alu_dpt_info.rn.rd0_idx := rd0_idx
	alu_dpt_info.rn.rs1_idx := rs1_idx
	alu_dpt_info.rn.rs2_idx := rs2_idx
	alu_dpt_info.rn.rs3_idx := DontCare

	bru_dpt_info.isa        := id_dpt_info.info.bru_isa
	bru_dpt_info.param      := id_dpt_info.info.param
	bru_dpt_info.rn.rd0_idx := rd0_idx
	bru_dpt_info.rn.rs1_idx := rs1_idx
	bru_dpt_info.rn.rs2_idx := rs2_idx
	bru_dpt_info.rn.rs3_idx := DontCare

	lsu_dpt_info.isa        := id_dpt_info.info.lsu_isa
	lsu_dpt_info.param      := id_dpt_info.info.param
	lsu_dpt_info.rn.rd0_idx := rd0_idx
	lsu_dpt_info.rn.rs1_idx := rs1_idx
	lsu_dpt_info.rn.rs2_idx := rs2_idx
	lsu_dpt_info.rn.rs3_idx := DontCare

	csr_dpt_info.isa        := id_dpt_info.info.csr_isa
	csr_dpt_info.param      := id_dpt_info.info.param
	csr_dpt_info.rn.rd0_idx := rd0_idx
	csr_dpt_info.rn.rs1_idx := rs1_idx
	csr_dpt_info.rn.rs2_idx := DontCare
	csr_dpt_info.rn.rs3_idx := DontCare

	mul_dpt_info.isa        := id_dpt_info.info.mul_isa
	mul_dpt_info.param      := DontCare
	mul_dpt_info.rn.rd0_idx := rd0_idx
	mul_dpt_info.rn.rs1_idx := rs1_idx
	mul_dpt_info.rn.rs2_idx := rs2_idx
	mul_dpt_info.rn.rs3_idx := DontCare

	fpu_dpt_info.isa        := id_dpt_info.info.fpu_isa
	fpu_dpt_info.param      := id_dpt_info.info.param
	fpu_dpt_info.rn.rd0_idx := rd0_idx
	fpu_dpt_info.rn.rs1_idx := rs1_idx
	fpu_dpt_info.rn.rs2_idx := rs2_idx
	fpu_dpt_info.rn.rs3_idx := rs3_idx
}
























class Dispatch extends Module {
	val io = IO(new Bundle{
		val id_dpt = Flipped(new DecoupledIO(new Info_id_dpt()))

		val alu_dpt_iss = new DecoupledIO(new Alu_dpt_info())
		val bru_dpt_iss = new DecoupledIO(new Bru_dpt_info())
		val lsu_dpt_iss = new DecoupledIO(new Lsu_dpt_info())
		val csr_dpt_iss = new DecoupledIO(new Csr_dpt_info())
		val mul_dpt_iss = new DecoupledIO(new Mul_dpt_info())
		val fpu_dpt_iss = new DecoupledIO(new Fpu_dpt_info())

		val rod_i = new DecoupledIO(new Info_reorder_i)
		val rod_f = new DecoupledIO(new Info_reorder_f)

		val irn   = new Info_dpt_reg
		val frn   = new Info_dpt_reg

	})

	def is_iwb = io.id_dpt.bits.info.is_iwb
	def is_fwb = io.id_dpt.bits.info.is_fwb

	def id_dpt_info = io.id_dpt.bits
	def instruction_info = id_dpt_info.info

	def ptr = Mux(is_fwb, io.frn.ptr, io.irn.ptr)
	def log = Mux(is_fwb, io.frn.log, io.irn.log)

	def rd0_raw = instruction_info.param.rd0_raw
	def rs1_raw = instruction_info.param.rs1_raw	
	def rs2_raw = instruction_info.param.rs2_raw
	def rs3_raw = instruction_info.param.rs3_raw



	val rename = new Rename(ptr, log)
	val rodmux = new Reorder_mux(id_dpt_info, rd0_idx)

	def rd0_idx = rename.malloc(rd0_raw)
	def rs1_idx = rename.lookup(rs1_raw)
	def rs2_idx = rename.lookup(rs2_raw)
	def rs3_idx = rename.lookup(rs3_raw)

	val dptmux = new Dpt_mux ( id_dpt_info, rd0_idx, rs1_idx, rs2_idx, rs3_idx )




	{
		for ( i <- 0 until 32; j <- 0 until 4 ) {
			io.irn.opr(i)(j) := false.B
			io.frn.opr(i)(j) := false.B
		}

		io.irn.opr(rd0_raw)(rd0_idx) := true.B & is_dpt	& is_iwb
		io.frn.opr(rd0_raw)(rd0_idx) := true.B & is_dpt	& is_fwb
	}





	
	def is_rod_ready = Mux(instruction_info.is_fwb, io.rod_f.ready, io.rod_i.ready) //when the rd0 write back to f reg, just check f rod
	def canbe_dpt = io.id_dpt.valid & is_rod_ready

	def alu_dpt     = instruction_info.alu_isa.is_alu & ~rename.full(rd0_raw) & io.alu_dpt_iss.ready & canbe_dpt
	def bru_dpt     = instruction_info.bru_isa.is_bru & ~rename.full(rd0_raw) & io.bru_dpt_iss.ready & canbe_dpt
	def lsu_dpt     = instruction_info.lsu_isa.is_lsu & ~rename.full(rd0_raw) & io.lsu_dpt_iss.ready & canbe_dpt
	def csr_dpt     = instruction_info.csr_isa.is_csr & ~rename.full(rd0_raw) & io.csr_dpt_iss.ready & canbe_dpt
	def mul_dpt     = instruction_info.mul_isa.is_mul & ~rename.full(rd0_raw) & io.mul_dpt_iss.ready & canbe_dpt
	def fpu_dpt     = instruction_info.fpu_isa.is_fpu & ~rename.full(rd0_raw) & io.fpu_dpt_iss.ready & canbe_dpt
	def privil_dpt  = instruction_info.privil_isa.is_privil & canbe_dpt

	def is_dpt = alu_dpt | bru_dpt | lsu_dpt | csr_dpt | mul_dpt | fpu_dpt | privil_dpt




	io.id_dpt.ready := is_dpt 

	io.alu_dpt_iss.bits := dptmux.alu_dpt_info
	io.bru_dpt_iss.bits := dptmux.bru_dpt_info
	io.lsu_dpt_iss.bits := dptmux.lsu_dpt_info
	io.csr_dpt_iss.bits := dptmux.csr_dpt_info
	io.mul_dpt_iss.bits := dptmux.mul_dpt_info
	io.fpu_dpt_iss.bits := dptmux.fpu_dpt_info

	io.rod_i.bits := Mux(is_iwb, rodmux.reorder_i_info, DontCare)
	io.rod_f.bits := Mux(is_fwb, rodmux.reorder_f_info, DontCare)

	io.alu_dpt_iss.valid := alu_dpt
	io.bru_dpt_iss.valid := bru_dpt
	io.lsu_dpt_iss.valid := lsu_dpt
	io.csr_dpt_iss.valid := csr_dpt
	io.mul_dpt_iss.valid := mul_dpt
	io.fpu_dpt_iss.valid := fpu_dpt

	io.rod_i.valid := is_iwb & is_dpt
	io.rod_f.valid := is_fwb & is_dpt

}


