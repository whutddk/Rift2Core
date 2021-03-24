package rift2Core.basicElement

/*
* @Author: Ruige Lee
* @Date:   2021-03-24 11:59:20
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-03-24 17:09:36
*/



import chisel3._
import chisel3.util._


abstract class Ppbuf[T <:Data](dw:T , dp: Int) extends Module {
	val io = IO(new Bundle{
		val push = Input(Bool())


		val pop  = Input(Bool())


		val data_i = Input(dw)
		val buf_o   = Vec(dp, Output(dw))
		val valid_o = Output(UInt(dp.W))

		val flush = Output(Bool())
	})

	def pop_idx: UInt

	val buf = Reg(Vec(dp, dw))
	val valid = Wire(UInt(dp.W))

	def full = valid.andR
	def empty = (~valid).andR


	def push_idx: UInt = {
		var i = 0
		while( i < dp && valid(i) == 1.U ){
			i = i + 1
		}
		return i.U
	}
	// MuxCase(0.U, {val pushArray = for ( i <- 0 until dp ) yield Array( valid(i) === true.B -> i.U)} )

	io.buf_o := buf
	io.valid_o := valid

	when( reset.asBool ) {
		for ( i <- 0 until dp) yield 
		{
			buf(i) := 0.U
			valid(i) := 0.U 			
		}

	}
	.elsewhen( io.flush ) {
		for ( i <- 0 until dp) yield {
			buf(i) := 0.U
			valid(i) := 0.U			
		}

	}
	.elsewhen( io.push ) {
		Reg(push_idx) := io.data_i
		valid(push_idx) := 1.U
	}
	.elsewhen( io.pop ) {
		valid(pop_idx) := 0.U
	}

	assert( ((io.push & ~full) || (io.pop & ~empty)), "Assert Fail at ppbuf" )
}

