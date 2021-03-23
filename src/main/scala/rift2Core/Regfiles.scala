package rift2Core

/*
* @Author: Ruige Lee
* @Date:   2021-03-23 10:42:59
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-03-23 16:27:21
*/


import chisel3._
import chisel3.util._

class Regfiles extends Module{
	val io = IO(new Bundle{
		val dnxt =  Vec(4, Vec(32, Input(UInt(64.W))))	
		val files = Vec(4, Vec(32, Output(UInt(64.W))))	
	})





	val register = Reg(Vec(4,Vec(32, UInt(64.W)) ))
	val regLog = Reg(Vec(4,Vec(32, UInt(2.W)) ))

	val rename_X = Reg(Vec(32, UInt(2.W)) )
	val archit_X = Reg(Vec(32, UInt(2.W)) )



	when(reset.asBool) {
		for ( i <- 0 until 4;  j  <- 0 until 32) yield
		register(i)(j) := 0.U(64.W)
	} 
	// // .elsewhen {

	// // } 
	.otherwise {
		register := io.dnxt
	}


	io.files := register



}




