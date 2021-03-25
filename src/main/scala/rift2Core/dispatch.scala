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



class Rename () {

	val rn_ptr = Wire(Vec(32, UInt(2.W)))
	val regLog = Wire(Vec(32,Vec(4, UInt(2.W)) ))


	def full (rd0: UInt): Bool = {
		for ( j <- 0 until 4) {
			if (( regLog(rd0)(j) == 0.U )) {return false.B}
		}
		return true.B; 
	}

	def malloc(rd0: UInt): UInt = {
		MuxCase(0.U, Array(
			(regLog(rd0)(0) === 0.U) -> 0.U,
			(regLog(rd0)(1) === 0.U) -> 1.U,
			(regLog(rd0)(2) === 0.U) -> 2.U,
			(regLog(rd0)(3) === 0.U) -> 3.U,
		))
	}

	def lookup( rs: UInt ): UInt = {
		return rn_ptr(rs)
	}


}


















class Dispatch extends Module {
	val io = IO(new Bundle{
		val id_dpt = Flipped(new DecoupledIO(new Info_id_dpt()))
		// val dpt_isu = new Info_dpt_iss()
		val alu_dpt_iss = new DecoupledIO(new Alu_dpt_info())
		val bru_dpt_iss = new DecoupledIO(new Bru_dpt_info())
		val lsu_dpt_iss = new DecoupledIO(new Lsu_dpt_info())
		val csr_dpt_iss = new DecoupledIO(new Csr_dpt_info())
		val mul_dpt_iss = new DecoupledIO(new Mul_dpt_info())
		val fpu_dpt_iss = new DecoupledIO(new Fpu_dpt_info())

		val rod_i = new DecoupledIO(new Info_reorder_i)
		val rod_f = new DecoupledIO(new Info_reorder_f)

		val irn_op = Vec(32, Vec(4, Output(Bool())))
		val irn_ptr = Vec(32, Input(UInt(2.W)))
		val iregLog = Vec(32,Vec(4, Input(UInt(2.W))) )

		val frn_op = Vec(32, Vec(4, Output(Bool())))
		val frn_ptr = Vec(32, Input(UInt(2.W)))
		val fregLog = Vec(32,Vec(4, Input(UInt(2.W))) )
	})

	def is_fpu = io.id_dpt.bits.info.fpu_isa.is_fpu

	def id_dpt_info = io.id_dpt.bits.info
	def rename_ptr = Mux(is_fpu, io.frn_ptr, io.irn_ptr)
	def register_log = Mux(is_fpu, io.fregLog, io.iregLog)

	def rd0_raw = id_dpt_info.param.rd0
	def rs1_raw = id_dpt_info.param.rs1	
	def rs2_raw = id_dpt_info.param.rs2
	def rs3_raw = id_dpt_info.param.rs3

	val rename = new Rename()


	rename.rn_ptr := rename_ptr
	rename.regLog := register_log




	def rd0_rename = rename.malloc(rd0_raw)
	def rs1_rename = rename.lookup(rs1_raw)
	def rs2_rename = rename.lookup(rs2_raw)
	def rs3_rename = rename.lookup(rs3_raw)

	{
		for ( i <- 0 until 32; j <- 0 until 4 ) {
			io.irn_op(i)(j) := false.B
		}

		io.irn_op(rd0_raw)(rd0_rename) := true.B & io.id_dpt.valid & io.id_dpt.ready	
	}




	val alu_dpt_info = new Alu_dpt_info 
	val bru_dpt_info = new Bru_dpt_info 
	val lsu_dpt_info = new Lsu_dpt_info
	val csr_dpt_info = new Csr_dpt_info
	val mul_dpt_info = new Mul_dpt_info
	val fpu_dpt_info = new Fpu_dpt_info

	alu_dpt_info.isa        := id_dpt_info.alu_isa
	alu_dpt_info.param      := id_dpt_info.param
	alu_dpt_info.rn.rd0_idx := rd0_rename
	alu_dpt_info.rn.rs1_idx := rs1_rename
	alu_dpt_info.rn.rs2_idx := rs2_rename
	alu_dpt_info.rn.rs3_idx := DontCare

	bru_dpt_info.isa        := id_dpt_info.bru_isa
	bru_dpt_info.param      := id_dpt_info.param
	bru_dpt_info.rn.rd0_idx := rd0_rename
	bru_dpt_info.rn.rs1_idx := rs1_rename
	bru_dpt_info.rn.rs2_idx := rs2_rename
	bru_dpt_info.rn.rs3_idx := DontCare

	lsu_dpt_info.isa        := id_dpt_info.lsu_isa
	lsu_dpt_info.param      := id_dpt_info.param
	lsu_dpt_info.rn.rd0_idx := rd0_rename
	lsu_dpt_info.rn.rs1_idx := rs1_rename
	lsu_dpt_info.rn.rs2_idx := rs2_rename
	lsu_dpt_info.rn.rs3_idx := DontCare

	csr_dpt_info.isa        := id_dpt_info.csr_isa
	csr_dpt_info.param      := id_dpt_info.param
	csr_dpt_info.rn.rd0_idx := rd0_rename
	csr_dpt_info.rn.rs1_idx := rs1_rename
	csr_dpt_info.rn.rs2_idx := DontCare
	csr_dpt_info.rn.rs3_idx := DontCare

	mul_dpt_info.isa        := id_dpt_info.mul_isa
	mul_dpt_info.param      := DontCare
	mul_dpt_info.rn.rd0_idx := rd0_rename
	mul_dpt_info.rn.rs1_idx := rs1_rename
	mul_dpt_info.rn.rs2_idx := rs2_rename
	mul_dpt_info.rn.rs3_idx := DontCare

	fpu_dpt_info.isa := id_dpt_info.fpu_isa
	fpu_dpt_info.param      := id_dpt_info.param
	fpu_dpt_info.rn.rd0_idx := rd0_rename
	fpu_dpt_info.rn.rs1_idx := rs1_rename
	fpu_dpt_info.rn.rs2_idx := rs2_rename
	fpu_dpt_info.rn.rs3_idx := rs3_rename

	def is_dpt_f = id_dpt_info.fpu_isa.is_fpu //Warning, not all fpu will wb to f register
	printf ("Warning, not all fpu will wb to f register")
	
	def is_rod_ready = Mux(is_dpt_f, io.rod_f.ready, io.rod_i.ready) //when the rd0 write back to f reg, just check f rod
	def canbe_dpt = io.id_dpt.valid & io.rod_i.ready & is_rod_ready

	def alu_dpt     = id_dpt_info.alu_isa.is_alu & ~rename.full(rd0_raw) & io.alu_dpt_iss.ready & canbe_dpt
	def bru_dpt     = id_dpt_info.bru_isa.is_bru & ~rename.full(rd0_raw) & io.bru_dpt_iss.ready & canbe_dpt
	def lsu_dpt     = id_dpt_info.lsu_isa.is_lsu & ~rename.full(rd0_raw) & io.lsu_dpt_iss.ready & canbe_dpt
	def csr_dpt     = id_dpt_info.csr_isa.is_csr & ~rename.full(rd0_raw) & io.csr_dpt_iss.ready & canbe_dpt
	def mul_dpt     = id_dpt_info.mul_isa.is_mul & ~rename.full(rd0_raw) & io.mul_dpt_iss.ready & canbe_dpt
	def fpu_dpt     = id_dpt_info.fpu_isa.is_fpu & ~rename.full(rd0_raw) & io.fpu_dpt_iss.ready & canbe_dpt
	def privil_dpt  = id_dpt_info.privil_isa.is_privil & canbe_dpt

	def is_dpt = alu_dpt | bru_dpt | lsu_dpt | csr_dpt | mul_dpt | fpu_dpt | privil_dpt

	io.id_dpt.ready := is_dpt 

	io.alu_dpt_iss.bits := alu_dpt_info
	io.bru_dpt_iss.bits := bru_dpt_info
	io.lsu_dpt_iss.bits := lsu_dpt_info
	io.csr_dpt_iss.bits := csr_dpt_info
	io.mul_dpt_iss.bits := mul_dpt_info
	io.fpu_dpt_iss.bits := fpu_dpt_info

	io.alu_dpt_iss.valid := alu_dpt
	io.bru_dpt_iss.valid := bru_dpt
	io.lsu_dpt_iss.valid := lsu_dpt
	io.csr_dpt_iss.valid := csr_dpt
	io.mul_dpt_iss.valid := mul_dpt
	io.fpu_dpt_iss.valid := fpu_dpt

}


