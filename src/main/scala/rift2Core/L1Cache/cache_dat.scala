

/*
  Copyright (c) 2020 - 2022 Wuhan University of Technology <295054118@whut.edu.cn>

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


class Cache_dat( dw: Int, aw: Int, cb: Int, cl: Int, bk: Int ) {

  val addr_lsb = log2Ceil(dw/8)
  val line_w   = log2Ceil(cl)
  val bk_w     = log2Ceil(bk)


  val dat_addr_w = Wire(UInt(aw.W))
  val dat_addr_r = Wire(UInt(aw.W))

  val dat_en_w = Wire( Vec(cb, Bool()) )
  val dat_en_r = Wire( Vec(cb, Bool()) )
  val dat_info_wstrb = Wire(  UInt((dw/8).W))
  val dat_info_w = Wire(  UInt(dw.W))
  val dat_info_r = Wire( Vec(cb,  UInt(dw.W)) )





  val addr_sel_w = dat_addr_w(addr_lsb+bk_w+line_w-1, addr_lsb+bk_w)
  val addr_sel_r = dat_addr_r(addr_lsb+bk_w+line_w-1, addr_lsb+bk_w)




  val dat_ram = {
    for ( i <- 0 until cb ) yield { 
      val mdl = SyncReadMem( cl, Vec( dw/8, UInt(8.W) ) )
      mdl
    }
  }

  for ( i <- 0 until cb ) yield {
    val data_i = VecInit(for ( k <- 0 until dw/8 ) yield dat_info_w(8*k+7, 8*k))
    val data_o = Cat( for ( k <- 0 until dw/8 ) yield { dat_ram(i).read(addr_sel_r, dat_en_r(i))(dw/8-1-k) } )

    when( dat_en_w(i) ) {
      dat_ram(i).write(addr_sel_w, data_i, dat_info_wstrb.asBools)
    }

    dat_info_r(i) := {
      val mask = dat_info_wstrb.asBools

      val wdata    = RegNext( dat_info_w )
      val ext_mask = RegNext(Cat( for ( k <- 0 until dw/8 ) yield Fill(8, mask(dw/8-1-k)) ))

      val isBypass = RegNext(addr_sel_r === addr_sel_w & dat_en_w(i) & dat_en_r(i), false.B)
      val isEnable = RegNext(dat_en_r(i), false.B)

      Mux( isEnable, Mux(isBypass, (wdata & ext_mask) | (data_o & ~ext_mask), data_o), 0.U )

    }    
  }


}






