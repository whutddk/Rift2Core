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



class rename (rd0: UInt, rs1: UInt, rs2: UInt) {

	val rn_X = Wire(Vec(32, UInt(2.W)))
	val regLog = Wire(Vec(32,Vec(4, UInt(2.W)) ))


	def full (): Bool = {
		for ( j <- 0 until 4) {
			if (( regLog(rd0)(j) == 0.U )) {return false.B}
		}
		return true.B; 
	}

	def malloc(): UInt = {
		MuxCase(0.U, Array(
			(regLog(rd0)(0) === 0.U) -> 0.U,
			(regLog(rd0)(1) === 0.U) -> 1.U,
			(regLog(rd0)(2) === 0.U) -> 2.U,
			(regLog(rd0)(3) === 0.U) -> 3.U,
		))
	}

	def rslookup( i: UInt ): UInt = {
		return rn_X(i)
	}


}


class Dptbuf {
	val alu_buf = 
	val bru_fifo = 
	val lsu_fifo = 
	val mul_buf = 
	val fpu_buf = 
}














class Dispatch extends Module {
	val io = IO(new Bundle{
		val id_dpt = Flipped(new DecoupledIO(new Info_id_dpt()))
		val dpt_isu = new DecoupledIO(new Info_dpt_iss())

		val rn_op = Vec(32, Vec(4, Output(Bool())))
		val rn_X = Vec(32, Input(UInt(2.W)))
		val regLog = Vec(32,Vec(4, Input(UInt(2.W))) )
	})

	def rd0_raw = io.id_dpt.bits.info.rd0
	def rs1_raw = io.id_dpt.bits.info.rs1	
	def rs2_raw = io.id_dpt.bits.info.rs2

	val irn = new rename(rd0_raw, rs1_raw, rs2_raw)
	val frn = new rename(rd0_raw, rs1_raw, rs2_raw)

	rn.rn_X := io.rn_X
	rn.regLog := io.regLog

	{
		for ( i <- 0 until 32; j <- 0 until 4 ) {
			io.rn_op(i)(j) := false.B
		}

		io.rn_op(rd0_raw)(rn.malloc()) := true.B & io.id_dpt.valid & io.id_dpt.ready	
	}

	io.id_dpt.ready := ~rn.full() & 


}


