/*
* @Author: Ruige Lee
* @Date:   2021-04-19 14:43:41
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-20 12:04:14
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


package rift2Chip

import chisel3._
import chisel3.util._
import rift2Core._
import cache._
import axi._


class Rift2Chip extends Module {
	val io = IO( new Bundle{
		val mem_chn_ar = new DecoupledIO(new AXI_chn_a( 32, 1, 1 ))
		val mem_chn_r = Flipped( new DecoupledIO(new AXI_chn_r( 128, 1, 1)) )
		val mem_chn_aw = new DecoupledIO(new AXI_chn_a( 32, 1, 1 ))
		val mem_chn_w = new DecoupledIO(new AXI_chn_w( 128, 1 )) 
		val mem_chn_b = Flipped( new DecoupledIO(new AXI_chn_b( 1, 1 )))		
	})


	val i_rift2Core = Module( new Rift2Core )
	val l2cache = Module( new L2Cache )
	val l3cache = Module( new L3Cache )


	l2cache.io.il1_chn_a <> i_rift2Core.io.il1_chn_a
	l2cache.io.il1_chn_d <> i_rift2Core.io.il1_chn_d
	l2cache.io.dl1_chn_a <> i_rift2Core.io.dl1_chn_a
	l2cache.io.dl1_chn_d <> i_rift2Core.io.dl1_chn_d
	l2cache.io.l2c_chn_a <> l3cache.io.l2c_chn_a
	l2cache.io.l2c_chn_d <> l3cache.io.l2c_chn_d
	l2cache.io.l2c_fence_req := i_rift2Core.io.l2c_fence_req
	l2cache.io.l2c_fence_end := i_rift2Core.io.l3c_fence_req



	l3cache.io.mem_chn_ar <> io.mem_chn_ar
	l3cache.io.mem_chn_r  <> io.mem_chn_r
	l3cache.io.mem_chn_aw <> io.mem_chn_aw
	l3cache.io.mem_chn_w  <> io.mem_chn_w
	l3cache.io.mem_chn_b  <> io.mem_chn_b
	l3cache.io.l3c_fence_req := i_rift2Core.io.l2c_fence_end
	l3cache.io.l3c_fence_end := i_rift2Core.io.l3c_fence_end
	
}


