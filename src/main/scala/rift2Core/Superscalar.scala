/*
* @Author: Ruige Lee
* @Date:   2021-04-15 17:15:02
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-28 11:19:48
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


package rift2Core

import chisel3._
import chisel3.util._


trait Superscalar {
	def ss = 2

	def is_1st_solo: Bool
	def is_2nd_solo: Bool

	// val is_act = Wire(Vec(num, Bool()))  //valid next
	// val is_out = Wire(Vec(num, Bool()))  //ready perivous

	// val is_stall = Wire(Vec(num, Bool()))
	// val is_no_source = Wire(Vec(num, Bool()))
	// val is_no_sink = Wire(Vec(num, Bool()))


	// for ( i <- 0 until num  ) yield {
	// 	is_act(i) := ~is_stall(i) & ~is_no_source(i)
	// 	is_out(i) := ~is_stall(i) & ~is_no_source(i) & is_no_sink
	// }
}
