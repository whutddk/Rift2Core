package rift2Core.basicElement

/*
* @Author: Ruige Lee
* @Date:   2021-03-24 11:59:20
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-03-24 17:09:36
*/



import chisel3._
import chisel3.util._


abstract class Ppbuf[T]( dp: UInt) extends Module {
	val io = IO(new Bundle{
		val push = Input(Bool())


		val pop  = Input(Bool())


		val data_i = Input(T)
		val buf_o   = Vec(dp, Output(T))
		val valid_o = Vec(dp, Output(Bool()))

		val flush = Output(Bool())
	})

	def pop_idx: UInt

	val buf = Reg(Vec(dp, T))
	val valid = Reg(Vec(dp, Bool()))

	def full = valid_o.andR
	def empty = (~valid_o).andR


	def push_idx = {
		Int i = 0
		while( i < dp && valid(i) === true.B ){
			i ++
		}
		return i.U
	}
	// MuxCase(0.U, {val pushArray = for ( i <- 0 until dp ) yield Array( valid(i) === true.B -> i.U)} )

	io.buf_o := buf
	io.valid_o := valid

	when( reset ) {
		for ( i <- 0 until dp) yield 
			buf(i) := 0.U
			valid(i) := false.B 
	}
	elsewhen( flush ) {
		for ( i <- 0 until dp) yield 
			buf(i) := 0.U
			valid(i) := false.B 
	}
	elsewhen( io.push ) {
		Reg(push_idx) := data_i
		valid(push_idx) := true.B 
	}
	elsewhen( io.pop ) {
		valid(pop_idx) := false.B 
	}

	assert( io.push )
}

