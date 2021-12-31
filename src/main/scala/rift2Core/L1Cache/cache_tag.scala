
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


class Cache_tag( dw: Int, aw: Int, bk: Int, cb: Int, cl: Int ) {

  val addr_lsb = log2Ceil(dw*bk/8)
  val line_w   = log2Ceil(cl)
  val tag_w   = aw - addr_lsb - line_w


  val tag_addr_r = Wire(UInt(aw.W))
  val tag_addr_w = Wire(UInt(aw.W))

  val tag_en_w = Wire( Vec(cb, Bool()) )
  val tag_en_r = Wire( Vec(cb, Bool()) )	
  val tag_info_r = Wire( Vec(cb, UInt(tag_w.W)) )


  val addr_sel_w = tag_addr_w(addr_lsb+line_w-1, addr_lsb)
  val addr_sel_r = tag_addr_r(addr_lsb+line_w-1, addr_lsb)

  val tag_info_w = tag_addr_w(aw-1, aw-tag_w)


  val tag_ram = {
    for ( i <- 0 until cb ) yield { val ram = Mem( cl, UInt(tag_w.W) ); ram }
  }

  for ( i <- 0 until cb ) yield {
    when( tag_en_w(i) ) {
      tag_ram(i).write(addr_sel_w, tag_info_w)
    }


    tag_info_r(i) := 
      RegEnable(
        Mux( addr_sel_r === addr_sel_w & tag_en_w(i) & tag_en_r(i), tag_info_w, tag_ram(i).read(addr_sel_r)),
        0.U(tag_w.W),
        tag_en_r(i)
      )

    }

  

  assert(PopCount(tag_en_w) <= 1.U)






}






