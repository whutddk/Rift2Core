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
import rift2Core.basicElement._

class Mul extends Module {
	val io = IO(new Bundle {
		val mul_iss_exe = Flipped(new DecoupledIO(new Mul_iss_info))
		val mul_exe_iwb = new DecoupledIO(new Exe_iwb_info)

		val flush = Input(Bool())
	})

	val iwb_valid = Reg(Bool())
	val iwb_res = Reg(UInt(64.W))
	val iwb_rd0 = Reg(UInt(7.W))

	def op1 = io.mul_iss_exe.bits.param.op1
	def op2 = io.mul_iss_exe.bits.param.op2

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
		Mux(
			is_usi, 
			op1,
			Mux(  
				is_32w,
				Cat( Fill(32, 0.U), Mux(op1(31), (~op1(31,0) + 1.U), op1(31,0))),
				Mux( op1(63), (~op1 + 1.U), op1)
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

	val ( cnt, isEnd ) = Counter( io.mul_iss_exe.valid & is_div, 33 )



















	def res = MuxCase(DontCare, Array(
		io.mul_iss_exe.bits.fun.mul    -> mul_res_128w(63,0),
		io.mul_iss_exe.bits.fun.mulh   -> mul_res_128w(127,64),
		io.mul_iss_exe.bits.fun.mulhsu -> mul_res_128w(127,64),
		io.mul_iss_exe.bits.fun.mulhu  -> mul_res_128w(127,64),
		io.mul_iss_exe.bits.fun.div    -> quot_res,
		io.mul_iss_exe.bits.fun.divu   -> quot_res,
		io.mul_iss_exe.bits.fun.rem    -> rema_res,
		io.mul_iss_exe.bits.fun.remu   -> rema_res,
		io.mul_iss_exe.bits.fun.mulw   -> Cat( Fill(32,mul_res_128w(31)), mul_res_128w(31,0) ),
		io.mul_iss_exe.bits.fun.divw   -> quot_res,
		io.mul_iss_exe.bits.fun.divuw  -> quot_res,
		io.mul_iss_exe.bits.fun.remw   -> rema_res,
		io.mul_iss_exe.bits.fun.remuw  -> rema_res
	))
















}



