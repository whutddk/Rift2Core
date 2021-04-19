// /*
// * @Author: Ruige Lee
// * @Date:   2021-04-19 14:49:41
// * @Last Modified by:   Ruige Lee
// * @Last Modified time: 2021-04-19 15:06:05
// */


// /*
//   Copyright (c) 2020 - 2021 Ruige Lee <wut.ruigeli@gmail.com>

//    Licensed under the Apache License, Version 2.0 (the "License");
//    you may not use this file except in compliance with the License.
//    You may obtain a copy of the License at

// 	   http://www.apache.org/licenses/LICENSE-2.0

//    Unless required by applicable law or agreed to in writing, software
//    distributed under the License is distributed on an "AS IS" BASIS,
//    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//    See the License for the specific language governing permissions and
//    limitations under the License.
// */


// package rift2Chip

// import chisel3._
// import chisel3.util._
// import rift2Core.basic._
// import rift2Core.frontend._
// import rift2Core.backend._
// import rift2Core.cache._
// import tilelink._

// class Tl_CCM extends Module {
// 	val io = IO( new Bundle{
// 		val il1_chn_a = Flipped(new DecoupledIO(new TLchannel_a(128, 32)))
// 		val il1_chn_d = new DecoupledIO( new TLchannel_d(128) )
// 	})

// 	val tl_slv = new TileLink_slv(128, 32)

// 	io.il1_chn_a <> tl_slv.a
// 	io.il1_chn_d <> tl_slv.d


// 	val opcode  = UInt(3.W)
// 	val param   = UInt(3.W)
// 	val size    = UInt(8.W)
// 	val source  = UInt(3.W)
// 	val address = UInt(aw.W)
// 	val mask    = UInt(8.W)
// 	val data    = UInt(dw.W)
// 	val corrupt = Bool()



// 	val opcode  = UInt(3.W)
// 	val param   = UInt(2.W)
// 	val size    = UInt(8.W)
// 	val source  = UInt(3.W)
// 	val sink    = UInt(3.W)
// 	val denied  = Bool()
// 	val data    = UInt(dw.W)
// 	val corrupt = Bool()



// 	val ram = Module( new Gen_sram(128, 14) )






// 	ram.io.clk := clock.asBool


// 	ram.io.data_w := tl_slv.a.data
// 	ram.io.addr_w := tl_slv.a.address
// 	ram.io.data_wstrb := tl_slv.a.mask
// 	ram.io.en_w   = Input(Bool())

		
// 	ram.io.addr_r := tl_slv.a.address
// 	ram.io.en_r   = Input(Bool())

// val data_r = Output(UInt(dw.W))

// }
