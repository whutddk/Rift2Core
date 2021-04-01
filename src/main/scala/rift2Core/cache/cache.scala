/*
* @File name: cache
* @Author: Ruige Lee
* @Email: wut.ruigeli@gmail.com
* @Date:   2021-03-11 19:52:35
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-03-11 19:52:39
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

package rift2Core.cache


import chisel3._
import chisel3.util._
import chisel3.experimental.chiselName
import rift2Core.basicElement._

@chiselName
class Cache_mem( dw: Int, aw: Int, bk: Int, cb: Int, cl: Int ) {
	val addr_lsb = log2Ceil(dw*bk/8)
	val line_w   = log2Ceil(cl)
	val tag_w    = aw - addr_lsb - line_w

	val tag_ram = VecInit( Seq.fill(cb)(Module(new Sram(tag_w, aw)).io)) 
	val dat_ram = Vec( cb, Module(new Sram(tag_w, aw)).io)



}






