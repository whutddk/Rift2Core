
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


class Coher extends Bundle{
  // def NONE = 0.U
  // def TRNK = 1.U
  // def TTIP = 2.U

  // val modified = Bool()
  val exclusive = UInt(8.W)
  // val shared = false.B

}

class Cache_coh( dw: Int, aw: Int, bk: Int, cb: Int, cl: Int ) {

  println("dw is"+dw);
  println("aw is"+aw);
  println("bk is"+bk);
  println("cb is"+cb);
  println("cl is"+cl);


  val addr_lsb = log2Ceil(dw*bk/8)
  val line_w   = log2Ceil(cl)


  val coh_addr_w = Wire(UInt(aw.W))
  val coh_addr_r = Wire(UInt(aw.W))

  val coh_en_w = Wire( Vec(cb, Vec(bk, Bool()) ))
  val coh_en_r = Wire( Vec(cb, Bool()) )

  val coh_info_w = Wire( UInt(8.W) ) 
  val coh_info_r = Wire( Vec(bk, UInt(8.W)) )





  val addr_sel_w = coh_addr_w(addr_lsb+line_w-1, addr_lsb)
  // val bank_sel_w = coh_addr_w(addr_lsb-1, addr_lsb-log2Ceil(bk) )


  val addr_sel_r = coh_addr_r(addr_lsb+line_w-1, addr_lsb)
  // val bank_sel_r = coh_addr_r(addr_lsb-1, addr_lsb-log2Ceil(bk) )

  val data_o = Reg( Vec(bk, UInt(8.W)))
  
  val ram = for ( i <- 0 until cb; j <- 0 until bk ) yield { Mem( cl, UInt(8.W) ) }
  
  for ( i <- 0 until cb; j <- 0 until bk ) yield {

    when( coh_en_w(i)(j) === true.B ) {
      ram(i*bk+j).write(addr_sel_w, coh_info_w)
    }

    when( coh_en_r(i) === true.B ){
      data_o(j) := ram(i*bk+j).read(addr_sel_r)
    }
  }


  coh_info_r := data_o
}






