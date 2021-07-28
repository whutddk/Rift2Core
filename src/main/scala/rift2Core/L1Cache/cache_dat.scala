

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

package rift2Core.L1Cache


import chisel3._
import chisel3.util._
import base._


class Cache_dat( dw: Int, aw: Int, bk: Int, cb: Int, cl: Int ) {

  val addr_lsb = log2Ceil(dw*bk/8)
  val line_w   = log2Ceil(cl)


  val dat_addr_w = Wire(UInt(aw.W))
  val dat_addr_r = Wire(UInt(aw.W))

  val dat_en_w = Wire( Vec(cb, Vec(bk, Bool()) ))
  val dat_en_r = Wire( Vec(cb, Vec(bk, Bool()) ))
  val dat_info_wstrb = Wire( Vec(bk, UInt((dw/8).W)) )
  val dat_info_w = Wire( Vec(bk, UInt(dw.W)))
  val dat_info_r = Wire( Vec(cb, Vec(bk, UInt(dw.W)) ))





  val addr_sel_w = dat_addr_w(addr_lsb+line_w-1, addr_lsb)
  // val bank_sel_w = dat_addr_w(addr_lsb-1, addr_lsb-log2Ceil(bk) )
  // val data_sel_w = dat_addr_w(addr_lsb-log2Ceil(bk)-1,0)

  val addr_sel_r = dat_addr_r(addr_lsb+line_w-1, addr_lsb)
  // val bank_sel_r = dat_addr_r(addr_lsb-1, addr_lsb-log2Ceil(bk) )
  // val data_sel_r = dat_addr_r(addr_lsb-log2Ceil(bk)-1,0)



  val dat_ram = {
    for ( i <- 0 until cb; j <- 0 until bk ) yield { 
      val mdl = Mem( cl, Vec( dw/8, UInt(8.W) ) )
      mdl
    }
  }

  for ( i <- 0 until cb; j <- 0 until bk ) yield {
    val data_i = VecInit(for ( k <- 0 until dw/8 ) yield dat_info_w(j)(8*k+7, 8*k))
    val data_o = Cat( for ( k <- 0 until dw/8 ) yield { dat_ram(i*bk+j).read(addr_sel_r)(dw/8-1-k) } )

    when( dat_en_w(i)(j) ) {
      dat_ram(i*bk+j).write(addr_sel_w, data_i, dat_info_wstrb(j).asBools)
    }

    dat_info_r(i)(j) := {
      val mask = dat_info_wstrb(j).asBools
      val ext_mask = Cat( for ( k <- 0 until dw/8 ) yield Fill(8, mask(dw/8-1-k)) )

      RegEnable(
        Mux(
          addr_sel_r === addr_sel_w & dat_en_w(i)(j) & dat_en_r(i)(j),
          (dat_info_w(j) & ext_mask) | (data_o & ~ext_mask),
          data_o
        ),
        0.U(dw.W),
        dat_en_r(i)(j)
      )    
    }    
  }


}






