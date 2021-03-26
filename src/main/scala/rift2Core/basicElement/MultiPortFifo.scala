/*
* @Author: Ruige Lee
* @Date:   2021-03-26 17:41:24
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-03-26 17:46:47
*/


package rift2Core.basicElement

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


import chisel3._
import chisel3.util._


class Fifo_port_i[T<:Data](dw: T) extends Bundle {
	val valid = Input(Bool())
	val data = Input(dw)
	val ready = Output(Bool())
}

class Fifo_port_o[T<:Data](dw: T) extends Bundle {
	val valid = Output(Bool())
	val data = Output(dw)
	val ready = Input(Bool())
}

class MultiPortFifo[T<:Data]( dw: T, aw: Int, in: Int, out: Int ) extends Module {
	val io = IO(new Bundle{
		val push = Vec(in,  new Fifo_port_i(dw))
		val pop  = Vec(out, new Fifo_port_o(dw))
	})


	val dp = 2^aw

	val buf = Vec(dp, Reg(dw))
	val 








}

