
package rift2Core

/*
* @Author: Ruige Lee
* @Date:   2021-03-19 09:55:43
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-03-22 20:02:27
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


import chisel3._
import chisel3.util._


class Syntax extends Module {
	val io = IO(new Bundle{
		val a = Input(Bool())
		val b = Input(Bool())
		val c = Input(Bool())
		val d = Output(Bool())

		val e = Input(Vec(3, Bool()))
	})

	// val temp = WireDefault(false.B)
	// io.d := temp

	// def sela() = {
	// 	temp := io.a
	// }

	// def selb() = {
	// 	temp := io.b
	// }

	// when ( io.c === true.B ) {
	// 	sela()
	// }
	// .otherwise {
	// 	selb()
	// }	

	def check_lgc(a: Bool, b: Bool, c: Bool): Bool = {
		val cmp = Wire(Vec(3, Bool()))

			cmp(0) := a
			cmp(1) := b
			cmp(2) := c

		return cmp.forall(x => x === false.B)	
	}

 

	io.d := check_lgc(io.a, io.b, io.c)
}
