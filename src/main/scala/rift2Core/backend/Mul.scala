/*
* @Author: Ruige Lee
* @Date:   2021-03-29 14:37:46
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-03-29 14:39:35
*/


/*
  Copyright (c) 2020 - 2021 Ruige Lee <wut.ruigeli@gmail.com>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

package rift2Core.backend


import chisel3._
import chisel3.util._
import rift2Core.basic._






class Mul extends Module {
	val io = IO(new Bundle {
		val mul_iss_exe = Flipped(new DecoupledIO(new Mul_iss_info))
		val mul_exe_iwb = new DecoupledIO(new Exe_iwb_info)

		val flush = Input(Bool())
	})

	def iss_ack = io.mul_exe_iwb.valid & io.mul_exe_iwb.ready
	def iwb_ack = io.mul_exe_iwb.valid & io.mul_exe_iwb.ready

	def op1 = io.mul_iss_exe.bits.param.op1
	def op2 = io.mul_iss_exe.bits.param.op2

	def cutTo32(in: UInt) = Cat(Fill( 32, in(31) ), in(31, 0))

	def mul_op1_sign =
		Mux(
			( io.mul_iss_exe.bits.fun.mul | io.mul_iss_exe.bits.fun.mulh | io.mul_iss_exe.bits.fun.mulhsu | io.mul_iss_exe.bits.fun.mulw),
			Mux(io.mul_iss_exe.bits.fun.mulw, op1(31), op1(63)),
			0.U
		)

	def mul_op2_sign =
		Mux(
			(io.mul_iss_exe.bits.fun.mul | io.mul_iss_exe.bits.fun.mulh | io.mul_iss_exe.bits.fun.mulw),
			Mux(io.mul_iss_exe.bits.fun.mulw, op2(31), op2(63)),
			0.U
		)

	def mul_op1 =
		Mux(
			io.mul_iss_exe.bits.fun.mulw,
			Cat(Fill(33, mul_op1_sign), op1(31,0)),
			Cat(mul_op1_sign, op1)
		)

	def mul_op2 =
		Mux(
			io.mul_iss_exe.bits.fun.mulw,
			Cat(Fill(33, mul_op2_sign), op2(31,0)),
			Cat(mul_op2_sign, op2)
		)

	def mul_res_128w = mul_op1.asSInt * mul_op2.asSInt



	def is_32w = io.mul_iss_exe.bits.fun.divw | io.mul_iss_exe.bits.fun.divuw | io.mul_iss_exe.bits.fun.remw | io.mul_iss_exe.bits.fun.remuw;
	def is_usi = io.mul_iss_exe.bits.fun.divu | io.mul_iss_exe.bits.fun.remu | io.mul_iss_exe.bits.fun.divuw | io.mul_iss_exe.bits.fun.remuw;
	def is_div = io.mul_iss_exe.bits.fun.div | io.mul_iss_exe.bits.fun.divu | io.mul_iss_exe.bits.fun.rem | io.mul_iss_exe.bits.fun.remu | io.mul_iss_exe.bits.fun.divw | io.mul_iss_exe.bits.fun.divuw | io.mul_iss_exe.bits.fun.remw | io.mul_iss_exe.bits.fun.remuw

	def dividend_load =
		Cat ( Fill(64,0.U),
			Mux(
				is_usi, 
				op1,
				Mux(  
					is_32w,
					Cat( Fill(32, 0.U), Mux(op1(31), (~op1(31,0) + 1.U), op1(31,0))),
					Mux( op1(63), (~op1 + 1.U), op1)
				)
			)
		)

	def divisor_load =
		Mux(
			is_usi,
			op2,
			Mux( 
				is_32w,
				Cat( Fill(32, 0.U), Mux(op2(31), (~op2(31,0) + 1.U), op2(31,0))),
				Mux(op2(63), (~op2 + 1.U), op2)
			)
		)

	val ( cnt, isEnd ) = Counter( io.mul_iss_exe.valid & is_div, 64 )


	val dividend = Reg(UInt(128.W))
	val divisor = Reg(UInt(64.W))

	def dividend_shift = dividend << 1;
	def div_cmp = (dividend_shift(127,64) >= divisor);
	def divided = 
		Mux(
			div_cmp,
			Cat((dividend_shift(127,64) - divisor), dividend_shift(63,1), 1.U(1.W)),
			dividend_shift
		)



	def dividend_sign = Mux(is_usi, false.B, Mux(is_32w, op1(31).asBool, op1(63).asBool))
	def divisor_sign  = Mux(is_usi, false.B, Mux(is_32w, op2(31).asBool, op2(63).asBool))
	def div_by_zero = (op2 === 0.U)
	def div_overflow = ~is_usi & 
							(
								( is_32w & (op1(31).asBool & (op1(30,0) === 0.U) ) & (op2(31,0).andR.asBool))
								|
								(~is_32w & (op1(63).asBool & (op1(62,0) === 0.U) ) & (op2(63,0).andR.asBool))								
							); 

	when(reset.asBool() | io.flush) {
		dividend := 0.U
		divisor := 0.U	
	}
	.elsewhen( cnt === 0.U ) {
		dividend := dividend_load 
		divisor := divisor_load
	}
	.otherwise {
		dividend := divided
	}

	def quot_sign_corrcet = 
		Mux(dividend_sign^divisor_sign, ~dividend(63,0) + 1.U, dividend(63,0))

	def rema_sign_corrcet = 
		Mux(dividend_sign, ~dividend(127,64) + 1.U, dividend(127,64))

	def quot_res = MuxCase(0.U, Array(
		div_by_zero -> -1.S.asUInt,
		div_overflow -> Mux( is_32w, Cat( Fill(33, 1.U(1.W)), 0.U(31.W)), Cat(1.U, 0.U(63.W))),
		(~div_by_zero & ~div_overflow) -> quot_sign_corrcet
		))

	def rema_res = MuxCase(0.U, Array(
		div_by_zero -> Mux(is_32w, cutTo32(op1), op1),
		div_overflow -> 0.U,
		(~div_by_zero & ~div_overflow) -> rema_sign_corrcet
		))













	def res = MuxCase(DontCare, Array(
		io.mul_iss_exe.bits.fun.mul    -> mul_res_128w(63,0),
		io.mul_iss_exe.bits.fun.mulh   -> mul_res_128w(127,64),
		io.mul_iss_exe.bits.fun.mulhsu -> mul_res_128w(127,64),
		io.mul_iss_exe.bits.fun.mulhu  -> mul_res_128w(127,64),
		io.mul_iss_exe.bits.fun.div    -> quot_res,
		io.mul_iss_exe.bits.fun.divu   -> quot_res,
		io.mul_iss_exe.bits.fun.rem    -> rema_res,
		io.mul_iss_exe.bits.fun.remu   -> rema_res,
		io.mul_iss_exe.bits.fun.mulw   -> cutTo32(mul_res_128w(63,0) ),
		io.mul_iss_exe.bits.fun.divw   -> cutTo32(quot_res),
		io.mul_iss_exe.bits.fun.divuw  -> cutTo32(quot_res),
		io.mul_iss_exe.bits.fun.remw   -> cutTo32(rema_res),
		io.mul_iss_exe.bits.fun.remuw  -> cutTo32(rema_res)
	))




	val iwb_valid = Reg(Bool())
	val iwb_res = Reg(UInt(64.W))
	val iwb_rd0 = Reg(UInt(7.W))


	def is_fun_end: Bool = {
		if ( io.mul_iss_exe.valid == true.B ) {
			if ( ~is_div == true.B ) {
				true.B
			}
			else {
				if ( (div_by_zero | div_overflow) == true.B ) {
					true.B
				}
				else if ( cnt == 64.U ) {
					true.B
				}
				else {
					false.B
				}
			}
		}
		else {
			false.B
		}
	}



	when( reset.asBool | io.flush ) {
		iwb_valid := false.B
		iwb_res := 0.U
		iwb_rd0 := 0.U
	}
	.elsewhen( ~iwb_valid & is_fun_end ) {
		iwb_valid := true.B
		iwb_res := res
		iwb_rd0 := Cat( io.mul_iss_exe.bits.param.rd0_idx, io.mul_iss_exe.bits.param.rd0_raw ) 
	}
	.elsewhen( iwb_ack ) {
		iwb_valid := false.B
	}

	io.mul_iss_exe.ready := iwb_ack
	io.mul_exe_iwb.valid := iwb_valid
	io.mul_exe_iwb.bits.res := iwb_res
	io.mul_exe_iwb.bits.rd0_raw := iwb_rd0(4,0)
	io.mul_exe_iwb.bits.rd0_idx := iwb_rd0(6,5)





}



