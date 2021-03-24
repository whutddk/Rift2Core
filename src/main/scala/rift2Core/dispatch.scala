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
		val dpt_isu = new Info_dpt_iss()

		val irn_op = Vec(32, Vec(4, Output(Bool())))
		val irn_ptr = Vec(32, Input(UInt(2.W)))
		val iregLog = Vec(32,Vec(4, Input(UInt(2.W))) )

		val frn_op = Vec(32, Vec(4, Output(Bool())))
		val frn_ptr = Vec(32, Input(UInt(2.W)))
		val fregLog = Vec(32,Vec(4, Input(UInt(2.W))) )
	})

	def is_fpu = io.id_dpt.bits.info.fpu_isa.is_Fpu

	def id_dpt_info = io.id_dpt.bits.info
	def rename_ptr = Mux(is_fpu, io.frn_ptr, io.irn_ptr)
	def register_log = Mux(is_fpu, io.fregLog, io.iregLog)
	def rd0_raw = id_dpt_info.rd0
	def rs1_raw = id_dpt_info.rs1	
	def rs2_raw = id_dpt_info.rs2
	def rs3_raw = id_dpt_info.rs3

	val rename = new Rename()


	rename.rn_ptr := rename_ptr
	rename.regLog := register_log


	{
		for ( i <- 0 until 32; j <- 0 until 4 ) {
			io.irn_op(i)(j) := false.B
		}

		io.irn_op(rd0_raw)(rename.malloc(rd0_raw)) := true.B & io.id_dpt.valid & io.id_dpt.ready	
	}

	def rs1_rename = rename.lookup(rs1_raw)
	def rs2_rename = rename.lookup(rs2_raw)
	def rs3_rename = rename.lookup(rs3_raw)

	// io.id_dpt.ready := ~rename.full(rd0_raw) & 

	val alu_dpt_info = new Alu_dpt_info 
	val bru_dpt_info = new Bru_dpt_info 
	val lsu_dpt_info = new Lsu_dpt_info
	val csr_dpt_info = new Csr_dpt_info
	val mul_dpt_info = new Mul_dpt_info
	val fpu_dpt_info = new Fpu_dpt_info

	alu_dpt_info.isa := id_dpt_info.alu_isa
	bru_dpt_info.isa := id_dpt_info.bru_isa
	lsu_dpt_info.isa := id_dpt_info.lsu_isa
	csr_dpt_info.isa := id_dpt_info.csr_isa
	mul_dpt_info.isa := id_dpt_info.mul_isa
	fpu_dpt_info.isa := id_dpt_info.fpu_isa

	// alu_dpt_info.param := id_dpt_info.param
	// bru_dpt_info.param := id_dpt_info.param
	// lsu_dpt_info.param := id_dpt_info.param
	// csr_dpt_info.param := id_dpt_info.param
	// mul_dpt_info.param := id_dpt_info.param
	// fpu_dpt_info.param := id_dpt_info.param


}


