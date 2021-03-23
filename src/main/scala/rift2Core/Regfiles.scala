package rift2Core

/*
* @Author: Ruige Lee
* @Date:   2021-03-23 10:42:59
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-03-23 19:17:18
*/


import chisel3._
import chisel3.util._

class Regfiles extends Module{
	val io = IO(new Bundle{


		val wb_files = Vec(32, Vec(4, Input(UInt(64.W))))

		val rn_op = Vec(32, Vec(4, Input(Bool())))
		val wb_op = Vec(32, Vec(4, Input(Bool())))
		val cm_op = Vec(32, Vec(4, Input(Bool())))


		val files = Vec(32, Vec(4, Output(UInt(64.W))))
		val log = Vec(32,Vec(4, Output(UInt(2.W))) )
		val rn_X = Vec(32, Output(UInt(2.W))) 
		val ar_X = Vec(32, Output(UInt(2.W))) 

		val flush = Input(Bool())
	})





	val register = Reg(Vec(32,Vec(4, UInt(64.W)) ))
	val regLog = Reg(Vec(32,Vec(4, UInt(2.W)) ))

	val rename_X = Reg(Vec(32, UInt(2.W)) )
	val archit_X = Reg(Vec(32, UInt(2.W)) )



	io.files := register
	io.log := regLog
	io.rn_X := rename_X
	io.ar_X := archit_X




		when(reset.asBool) {
			for ( i <- 0 until 32;  j  <- 0 until 4) yield 
				register(i)(j) := 0.U(64.W)
		} 
		.otherwise {
			register := io.wb_files
		}



	for ( i <- 0 until 32;  j  <- 0 until 4) yield {
		when(reset.asBool) {
			regLog(i)(0) := 2.U(2.W)		//when reset, the first reg is wb
			regLog(i)(1) := 0.U(2.W)
			regLog(i)(2) := 0.U(2.W)
			regLog(i)(3) := 0.U(2.W)

			rename_X(i) := 0.U(2.W)
			archit_X(i) := 0.U(2.W)
		}
		.elsewhen(io.flush) {
			regLog(i)(j) := Mux( archit_X(i) === j.U , 2.U , 0.U)
			rename_X(i) := archit_X(i)
		}
		.elsewhen(io.rn_op(i)(j)) {
			regLog(i)(j) := 1.U(2.W)
			rename_X(i) := j.asUInt()
		}
		.elsewhen(io.wb_op(i)(j)) {
			regLog(i)(j) := 2.U(2.W)
		}
		.elsewhen(io.cm_op(i)(j)) {
			regLog(i)( archit_X(i) ) := 0.U(2.W) //when reg i is commit, the last one should be free
			archit_X(i) := j.U
		}
		.otherwise {

		}		
	}





}




