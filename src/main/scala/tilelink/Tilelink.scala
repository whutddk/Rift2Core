
/*
* @Author: Ruige Lee
* @Date:   2021-04-01 09:24:57
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-06 19:36:38
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

package tilelink


import chisel3._
import chisel3.util._
import rift2Core.basicElement._


class TLchannel_a(dw:Int, aw:Int = 32) extends Bundle {

	val opcode  = Output( UInt(3.W) )
	val param   = Output( UInt(3.W) )
	val size    = Output( UInt(8.W) )
	val source  = Output( UInt(3.W) )
	val address = Output( UInt(aw.W) )
	val mask    = Output( UInt(8.W) )
	val data    = Output( UInt(dw.W) )
	val corrupt = Output( Bool() )
	val valid   = Output( Bool() )
	val ready   = Input(Bool()) 		
}

class TLchannel_d_out extends Bundle {
	
}

class TLchannel_d(dw:Int) extends Bundle {

	val opcode = Output( UInt(3.W) )
	val param  = Output( UInt(2.W) )
	val size   = Output( UInt(8.W) )
	val source = Output( UInt(3.W) )
	val sink   = Output( UInt(3.W) )
	val denied = Output( Bool() )
	val data   = Output( UInt(dw.W) )
	val corrupt  = Output( Bool() )
	val valid  = Output( Bool() )	


	val ready  =  Input( Bool())


}

trait Opcode {
	def PutFullData    = 0.U
	def PutPartialData = 1.U
	def ArithmeticData = 2.U
	def LogicalData    = 3.U
	def Get            = 4.U
	def Intent         = 5.U
	def Acquire        = 6.U
	def AccessAck     = 0.U
	def AccessAckData = 1.U
	def HintAck       = 2.U
	def Grant         = 4.U
	def GrantData     = 5.U
	def ReleaseAck    = 6.U

	def MIN  = 0.U
	def MAX  = 1.U
	def MINU = 2.U
	def MAXU = 3.U
	def ADD  = 4.U

	def XOR  = 0.U
	def OR   = 1.U
	def AND  = 2.U
	def SWAP = 3.U
}


class TileLink_mst(dw: Int, aw: Int, id: Int) extends Opcode{

	val a = new TLchannel_a(dw, aw)
	val d = Flipped(new TLchannel_d(dw))


	def op_getData(addr: UInt, size: UInt) = {

		a.opcode  := Get
		a.param   := 0.U
		a.size    := size
		a.source  := id.U
		a.address := addr
		a.mask    := "b11111111".U
		a.data    := 0.U
		a.corrupt := false.B

	}

	def op_putFullData(addr: UInt, size: UInt, data: UInt, mask: UInt) = {

		a.opcode  := PutFullData
		a.param   := 0.U 
		a.size    := size
		a.source  := id.U
		a.address := addr
		a.mask    := mask
		a.data    := data
		a.corrupt := false.B


	}

	def op_putPartialData(addr: UInt, size: UInt, data: UInt, mask: UInt) = {

		a.opcode  := PutPartialData
		a.param   := 0.U 
		a.size    := size
		a.source  := id.U
		a.address := addr
		a.mask    := mask
		a.data    := data
		a.corrupt := false.B


	}

	def op_arithmeticData(addr: UInt, op: UInt, size: UInt, data: UInt, mask: UInt) = {
		a.opcode  := ArithmeticData
		a.param   := op
		a.size    := size
		a.source  := id.U
		a.address := addr
		a.mask    := mask
		a.data    := data
		a.corrupt := false.B


	}

	def op_logicalData(addr: UInt, op: UInt, size: UInt, data: UInt, mask: UInt) = {
		a.opcode  := LogicalData
		a.param   := op
		a.size    := size
		a.source  := id.U
		a.address := addr
		a.mask    := mask
		a.data    := data
		a.corrupt := false.B
	}

	val a_remain = RegInit(0.U(8.W))
	val d_remain = RegInit(0.U(8.W))

	when( a.valid & ~a.ready ) {
		a_remain := 1.U << a.size
	}
	.elsewhen ( is_chn_a_ack ) {
		if( a_remain == 0.U ) {
			a_remain := (1.U << a.size) - (dw/8).U
		}
		else {
			a_remain := a_remain - (dw/8).U
		}
	}

	when ( is_chn_a_ack ) {
		d_remain := 1.U << a.size
	}
	.elsewhen( is_chn_d_ack ) {
		d_remain := d_remain - (dw/8).U
	}

	def is_a_busy = a_remain === 0.U
	def is_d_busy = d_remain === 0.U

	def is_free = ~is_a_busy & ~is_d_busy
	def is_busy =  is_a_busy |  is_d_busy

	def a_valid_set() = a.valid := true.B
	def a_valid_rst() = a.valid := false.B

	def d_ready_set() = d.ready := true.B
	def d_ready_rst() = d.ready := false.B


	def is_chn_a_ack:  Bool = a.valid &  a.ready
	def is_chn_d_ack:  Bool = d.valid &  d.ready
	def is_chn_a_nack: Bool = a.valid & ~a.ready
	def is_chn_d_nack: Bool = d.valid & ~d.ready

	def is_accessAck     = (d.opcode === AccessAck) & is_chn_d_ack
	def is_accessAckData = (d.opcode === AccessAckData) & is_chn_d_ack
	def data_ack = d.data



	assert( a.size % Log2((dw/8).U) == 0.U, "a.size is upsupport"  )

}




class TileLink_slv(dw: Int, aw: Int) extends Opcode{
	val a = Flipped(new TLchannel_a(dw, aw))
	val d = new TLchannel_d(dw)


	def is_chn_a_ack: Bool = a.valid & a.ready
	def is_chn_d_ack: Bool = d.ready & d.valid

	def op_accessAck(sid: UInt, size: UInt) = {
		d.opcode  := AccessAck
		d.param   := 0.U
		d.size    := size
		d.source  := sid
		d.sink    := DontCare
		d.data    := DontCare
		d.corrupt := false.B
		d.denied  := false.B

	}


	def op_accessDataAck(sid: UInt, size: UInt, data: UInt) = {
		d.opcode  := AccessAckData
		d.param   := 0.U
		d.size    := size
		d.source  := sid
		d.sink    := DontCare
		d.data    := data
		d.corrupt := false.B
		d.denied  := false.B

	}

	def is_getData        = (a.opcode === Get)            & is_chn_a_ack 
	def is_putFullData    = (a.opcode === PutFullData)    & is_chn_a_ack 
	def is_putPartialData = (a.opcode === PutPartialData) & is_chn_a_ack 
	def is_arithmeticData = (a.opcode === ArithmeticData) & is_chn_a_ack 
	def is_logicalData    = (a.opcode === LogicalData)    & is_chn_a_ack 


	def a_ready_set = a.ready := true.B
	def a_ready_rst = a.ready := false.B

	def d_valid_set = d.valid := true.B
	def d_valid_rst = d.valid := false.B


}

