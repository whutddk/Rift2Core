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


	val cache_addr = Wire(UInt(aw.W))

	val dat_en_w = Wire( Vec(cb, Bool()) )
	val dat_en_r = Wire( Vec(cb, Bool()) )
	val dat_info_wstrb = Wire(UInt((dw/8).W))
	val dat_info_w = Wire(UInt(dw.W))
	val dat_info_r = Wire( Vec(cb, UInt(dw.W)) )

	val tag_en_w = Wire( Vec(cb, Bool()) )
	val tag_en_r = Wire( Vec(cb, Bool()) )	
	val tag_info_r = Wire( Vec(cb, UInt(tag_w.W)) )







	def addr_sel = cache_addr(addr_lsb+aw, addr_lsb)
	def bank_sel = cache_addr(addr_lsb-1, addr_lsb-log2Ceil(bk) )
	def data_sel = cache_addr(addr_lsb-log2Ceil(bk)-1,0)
	def tag_info_w = cache_addr(31, 32-tag_w)







	val tag_ram = Vec( cb, Module(new Sram((tag_w+7)/8,line_w)).io ) 
	val dat_ram = Vec( cb, Vec(bk, Module(new Sram(dw, line_w)).io) )

	for ( i <- 0 until cb ) yield {
		tag_ram(i).addr_r := addr_sel
		tag_ram(i).addr_w := addr_sel

		tag_ram(i).en_r   := tag_en_r(i)
		tag_ram(i).en_w   := tag_en_w(i)

		tag_ram(i).mask_w := Fill((tag_w+7)/8/8,1.U)
		tag_ram(i).data_w := tag_info_w
		tag_info_r(i)     := tag_ram(i).data_r

		for ( j <- 0 until bk ) yield {

			dat_ram(i)(j).addr_w := addr_sel
			dat_ram(i)(j).addr_r := addr_sel

			if ( j.U == bank_sel ){
				dat_ram(i)(j).en_w   := dat_en_w(i)
				dat_ram(i)(j).en_r   := dat_en_r(i)				
			} else {
				dat_ram(i)(j).en_w   := false.B
				dat_ram(i)(j).en_r   := false.B				
			}


			dat_ram(i)(j).mask_w := dat_info_wstrb
			dat_ram(i)(j).data_w := dat_info_w
			dat_info_r(i) := dat_ram(i)(bank_sel).data_r


		}





	}








}






