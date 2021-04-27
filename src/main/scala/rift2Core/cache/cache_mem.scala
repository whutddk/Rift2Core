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
import rift2Core.basic._

@chiselName
class Cache_mem( dw: Int, aw: Int, bk: Int, cb: Int, cl: Int ) {

	val addr_lsb = log2Ceil(dw*bk/8)
	val line_w   = log2Ceil(cl)
	val tag_w    = aw - addr_lsb - line_w


	val cache_addr = Wire(UInt(aw.W))

	val dat_en_w = Wire( Vec(cb, Bool()) )
	val dat_en_r = Wire( Vec(cb, Bool()) )
	val dat_info_wstrb = Wire(UInt((128/8).W))
	val dat_info_w = Wire(UInt(128.W))
	val dat_info_r = Wire( Vec(cb, UInt(dw.W)) )

	val tag_en_w = Wire( Vec(cb, Bool()) )
	val tag_en_r = Wire( Vec(cb, Bool()) )	
	val tag_info_r = Wire( Vec(cb, UInt(tag_w.W)) )






	val addr_sel = cache_addr(addr_lsb+line_w-1, addr_lsb)
	val bank_sel = cache_addr(addr_lsb-1, addr_lsb-log2Ceil(bk) )
	val data_sel = cache_addr(addr_lsb-log2Ceil(bk)-1,0)
	val tag_info_w = cache_addr(31, 32-tag_w)


	val tag_ram = {
		for ( i <- 0 until cb ) yield { val mdl = Module(new Gen_sram(tag_w,line_w)); mdl }
	}

	val dat_ram = {
		for ( i <- 0 until cb; j <- 0 until bk ) yield { 

			val mdl = Module(new Gen_sram(dw, line_w))
			mdl
		}
	}


	for ( i <- 0 until cb ) yield {
		tag_ram(i).io.addr_r := addr_sel
		tag_ram(i).io.addr_w := addr_sel

		tag_ram(i).io.en_r   := tag_en_r(i)
		tag_ram(i).io.en_w   := tag_en_w(i)

		tag_ram(i).io.data_wstrb := Fill((tag_w+7)/8,1.U)
		tag_ram(i).io.data_w := tag_info_w
		tag_info_r(i)     := tag_ram(i).io.data_r






		for ( j <- 0 until bk ) yield {

			dat_ram(i*bk+j).io.addr_w := addr_sel
			dat_ram(i*bk+j).io.addr_r := addr_sel

			when ( j.U === bank_sel ){
				dat_ram(i*bk+j).io.en_w   := dat_en_w(i)
				dat_ram(i*bk+j).io.en_r   := dat_en_r(i)
				

			}
			.otherwise {
				dat_ram(i*bk+j).io.en_w   := false.B
				dat_ram(i*bk+j).io.en_r   := false.B				
			}


			dat_ram(i*bk+j).io.data_wstrb := dat_info_wstrb << (data_sel & ~("b1111".U(32.W)) )
			dat_ram(i*bk+j).io.data_w := Fill( dw/128, dat_info_w)

		}

		val bank_num = for ( j <- 0 until bk ) yield { j.U === bank_sel }
		val dat_bank = for ( j <- 0 until bk ) yield { dat_ram( i*bk+j).io.data_r }
		dat_info_r(i) := MuxCase(DontCare, bank_num zip dat_bank )
		
		
		// dat_ram( i*bk + bank_sel).io.data_r
	}








}






