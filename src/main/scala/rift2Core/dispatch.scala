/*
* @Author: Ruige Lee
* @Date:   2021-03-23 09:31:02
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-03-23 20:43:56
*/



package rift2Core

import chisel3._
import chisel3.util._
import rift2Core.basicElement._



class rename (rd0: UInt, rs1: UInt, rs2: UInt) {
	// val rn_op = Vec(32, Vec(4, Bool()))
	val rn_X = Wire(Vec(32, UInt(2.W)))
	val regLog = Wire(Vec(32,Vec(4, UInt(2.W)) ))
	// val rn_op = Wire(Vec(32, Vec(4, Bool())))

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




class Dispatch extends Module {
	val io = IO(new Bundle{
		val id_dpt = Flipped(new DecoupledIO(new Info_id_dpt()))

		val rn_op = Vec(32, Vec(4, Output(Bool())))
		val rn_X = Vec(32, Input(UInt(2.W)))
		val regLog = Vec(32,Vec(4, Input(UInt(2.W))) )
	})


	val rn = new rename(io.id_dpt.bits.info.rd0, io.id_dpt.bits.info.rs1, io.id_dpt.bits.info.rs2)
	rn.rn_X := io.rn_X
	rn.regLog := io.regLog

	for ( i <- 0 until 32; j <- 0 until 4 ) {
		io.rn_op(i)(j) := false.B
	}

	io.rn_op(io.id_dpt.bits.info.rd0)(rn.malloc) := true.B




	io.id_dpt.ready := true.B

}


