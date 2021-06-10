
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
import chisel3.util.random._


import tilelink._
import axi._
import base._




trait TLC_mst {

}

trait AXI_mst {

}




class TLC_L3 ( dw:Int = 1024, bk:Int = 4, cb:Int = 4, cl:Int = 256, mst_num:Int = 3, mst_size:Int = 1024 ) extends TLC_slv_port( dw, bk, cb, cl, mst_size ) {
  val io = IO(new Bundle{

    val slv_chn_a = Vec(mst_num, Flipped(new DecoupledIO(new TLchannel_a(128, 32))))
    val slv_chn_b = Vec(mst_num, new DecoupledIO(new TLchannel_b(128, 32)))
    val slv_chn_c = Vec(mst_num, Flipped(new DecoupledIO(new TLchannel_c(128, 32))))
    val slv_chn_d = Vec(mst_num, new DecoupledIO( new TLchannel_d(128)))
    val slv_chn_e = Vec(mst_num, Flipped(new DecoupledIO( new TLchannel_e)))
    
    val mem_chn_ar = new DecoupledIO(new AXI_chn_a( 32, 1, 1 ))
    val mem_chn_r = Flipped( new DecoupledIO(new AXI_chn_r( 128, 1, 1)) )

    val mem_chn_aw = new DecoupledIO(new AXI_chn_a( 32, 1, 1 ))
    val mem_chn_w = new DecoupledIO(new AXI_chn_w( 128, 1 )) 
    val mem_chn_b = Flipped( new DecoupledIO(new AXI_chn_b( 1, 1 )))

    val l3c_fence = Flipped(DecoupledIO(Bool()))


  })


  


  val mem_mst_r = Module( new AXI_mst_r( 32, 128, 1, 1, 63 ))
  val mem_mst_w = Module( new AXI_mst_w( 32, 128, 1, 1, 63 ))


  override val is_fence_req = ~is_op_aqblk & ~is_op_wbblk & ~is_op_fence & l3c_fence.valid

  override val is_flash_bus_fire = io.mem_chn_r.fire
  override val is_evict_bus_fire = io.mem_chn_w.fire
 

  override val flash_data = io.mem_chn_r.bits.data

}

