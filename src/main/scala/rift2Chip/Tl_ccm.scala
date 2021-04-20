/*
* @Author: Ruige Lee
* @Date:   2021-04-19 14:49:41
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-19 15:06:05
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
import rift2Core.basic._
import tilelink._

class Tl_CCM extends Module {
	val io = IO( new Bundle{
		val ccm_chn_a = Flipped(new DecoupledIO(new TLchannel_a(128, 32)))
		val ccm_chn_d = new DecoupledIO( new TLchannel_d(128) )
	})

	val tl_slv = new TileLink_slv(128, 32)

	io.ccm_chn_a.bits <> tl_slv.a
	io.ccm_chn_d.bits <> tl_slv.d
	io.ccm_chn_a.ready := tl_slv.a_ready
	io.ccm_chn_d.valid := tl_slv.d_valid
	tl_slv.a_valid := io.ccm_chn_a.valid
	tl_slv.d_ready := io.ccm_chn_d.ready	



	val ram = Module( new Gen_sram(128, 10) )




	ram.io.data_w := tl_slv.a.data
	ram.io.addr_w := tl_slv.rsp_addr
	ram.io.data_wstrb := tl_slv.a.mask
	ram.io.en_w   := tl_slv.is_putFullData

		
	ram.io.addr_r := tl_slv.rsp_addr
	ram.io.en_r   := tl_slv.is_d_busy & tl_slv.is_getData

    // tl_slv.d.data := ram.io.data_r

	when( tl_slv.is_d_busy ) {
		when( tl_slv.state === tl_slv.Get ) {
			tl_slv.op_accessAck(0.U)
			tl_slv.d_valid_set
		}
		when( tl_slv.state === tl_slv.PutFullData ) {
			tl_slv.op_accessDataAck(0.U, ram.io.data_r)
			tl_slv.d_valid_set
		}
	}
	.otherwise{
		tl_slv.d_valid_rst
	}


}
