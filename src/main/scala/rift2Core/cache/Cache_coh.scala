
/*
  Copyright (c) 2020 - 2021 Ruige Lee <m201772520@hust.edu.cn>

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
import base._


class Cache_coh( dw: Int, aw: Int, bk: Int, cb: Int, cl: Int ) {

  val addr_lsb = log2Ceil(dw*bk/8)
  val line_w   = log2Ceil(cl)


  val coh_addr_w = Wire(UInt(aw.W))
  val coh_addr_r = Wire(UInt(aw.W))

  val coh_en_w = Wire( Vec(cb, Bool()) )
  val coh_en_r = Wire( Vec(cb, Bool()) )

  val coh_info_w = Wire(UInt(4.W))
  val coh_info_r = Wire( Vec(cb, UInt(4.W)) )





  val addr_sel_w = coh_addr_w(addr_lsb+line_w-1, addr_lsb)
  val bank_sel_w = coh_addr_w(addr_lsb-1, addr_lsb-log2Ceil(bk) )
  val data_sel_w = coh_addr_w(addr_lsb-log2Ceil(bk)-1,0)

  val addr_sel_r = coh_addr_r(addr_lsb+line_w-1, addr_lsb)
  val bank_sel_r = coh_addr_r(addr_lsb-1, addr_lsb-log2Ceil(bk) )
  val data_sel_r = coh_addr_r(addr_lsb-log2Ceil(bk)-1,0)



  val coh_ram = {
    for ( i <- 0 until cb; j <- 0 until bk ) yield { 
      val mdl = Module(new Sram( 4, line_w))
      mdl
    }
  }


  for ( i <- 0 until cb ) yield {
    for ( j <- 0 until bk ) yield {

      coh_ram(i*bk+j).io.addr_w := addr_sel_w
      coh_ram(i*bk+j).io.addr_r := addr_sel_r

      coh_ram(i*bk+j).io.en_w := Mux( j.U === bank_sel_w, coh_en_w(i), false.B)
      coh_ram(i*bk+j).io.en_r := Mux( j.U === bank_sel_r, coh_en_r(i), false.B)

      coh_ram(i*bk+j).io.data_wstrb := "hff".U
      coh_ram(i*bk+j).io.data_w := coh_info_w

    }

  }








}






