package rift2Core

/*
* @Author: Ruige Lee
* @Date:   2021-03-23 10:42:59
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-03-23 19:17:18
*/


import chisel3._
import chisel3.util._

import rift2Core.define._


class Regfiles extends Module{
	val io = IO(new Bundle{

		val wb_reg = Input(new Info_wb_reg)



		val rn_op = Vec(32, Vec(4, Input(Bool())))
		val cm_op = Vec(32, Vec(4, Input(Bool())))

		// val hint_op = Input(Vec(32, Vec(4, Bool())))  //hint operation for marking an instr special


		val files = Vec(32, Vec(4, Output(UInt(64.W))))
		val log = Vec(32,Vec(4, Output(UInt(2.W))) )
		val rn_ptr = Vec(32, Output(UInt(2.W))) 
		// val ar_ptr = Vec(32, Output(UInt(2.W))) 

		val flush = Input(Bool())
	})


	val wb_op = Wire(Vec(32, Vec(4, Bool())))
	wb_op := io.wb_reg.enable

	val files = RegInit( VecInit(Seq.fill(32)(VecInit(Seq.fill(4)(0.U(64.W)) )))   )
	val regLog = RegInit( VecInit(Seq.fill(32)( VecInit( (0.U(2.W)),(0.U(2.W)),(0.U(2.W)),(3.U(2.W)) )))   ) // the first reg is wb

	val rename_ptr = RegInit( VecInit( Seq.fill(32)(0.U(2.W))) )
	val archit_ptr = RegInit( VecInit( Seq.fill(32)(0.U(2.W))) )



	io.files := files
	io.log := regLog
	io.rn_ptr := rename_ptr
	// io.ar_ptr := archit_ptr

	for ( i <- 0 until 32; j <- 0 until 4) yield {
		files(i)(j) := Mux( io.wb_reg.enable(i)(j), io.wb_reg.dnxt(i)(j), files(i)(j) )
	}
	


	for ( i <- 0 until 32; j <- 0 until 4) yield {
		when(io.cm_op(i)(j)) {
			regLog(i)( archit_ptr(i) ) := 0.U(2.W) //when reg i is commit, the last one should be free
			archit_ptr(i) := j.U
			when( io.flush ) {
				rename_ptr(i) := j.U				
			}

		}
		.elsewhen(io.flush) {
			regLog(i)(j) := Mux( archit_ptr(i) === j.U , 3.U , 0.U)
			when(~(io.cm_op(i).contains(true.B))) {
				rename_ptr(i) := archit_ptr(i)				
			}
				


		}
		.otherwise{
			when(io.rn_op(i)(j)) {
				regLog(i)(j) := 1.U(2.W)
				rename_ptr(i) := j.asUInt()
			}
			when(wb_op(i)(j)) {
				regLog(i)(j) := (regLog(i)(j) | "b10".U) //when hint will stay on 2, when normal wb will be 3
			}


		}

	
	}





}




