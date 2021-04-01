
/*
* @Author: Ruige Lee
* @Date:   2021-04-01 09:24:57
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-01 09:25:13
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


class TLchannel_a(aw:Int = 64) extends Bundle {

	val opcode  = Output( UInt(3.W) )
	val param   = Output( UInt(3.W) )
	val size    = Output( UInt(8.W) )
	val source  = Output( UInt(3.W) )
	val address = Output( UInt(aw.W) )
	val mask    = Output( UInt(8.W) )
	val data    = Output( UInt(64.W) )
	val corrupt = Output( Bool() )
	val valid   = Output( Bool() )
	val ready   = Input(Bool()) 		
}

class TLchannel_d_out extends Bundle {
	
}

class TLchannel_d extends Bundle {

	val opcode = Output( UInt(3.W) )
	val param  = Output( UInt(2.W) )
	val size   = Output( UInt(8.W) )
	val source = Output( UInt(3.W) )
	val sink   = Output( UInt(3.W) )
	val denied = Output( Bool() )
	val data   = Output( UInt(64.W) )
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


class TileLink_mst(id: Int) extends Opcode{

	val a = new TLchannel_a
	val d = Flipped(new TLchannel_d)


	def op_getData(addr: UInt, size: UInt ) = {

		a.opcode  := Get
		a.param   := 0.U
		a.size    := size
		a.source  := id.U
		a.address := addr
		a.mask    := "b11111111".U
		a.data    := 0.U
		a.corrupt := false.B
		a.valid   := true.B

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
		a.valid   := true.B

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
		a.valid   := true.B

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
		a.valid   := true.B
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
		a.valid   := true.B
	}

	def is_chn_a_ack: Bool = a.valid & a.ready
	def is_chn_d_ack: Bool = d.ready & d.valid

	def is_accessAck     = (d.opcode === AccessAck) & is_chn_d_ack
	def is_accessAckData = (d.opcode === AccessAckData) & is_chn_d_ack





}




class TileLink_slv extends Opcode{
	val a = new TLchannel_a
	val d = Flipped(new TLchannel_d)


	def is_chn_a_ack: Bool = a.valid & a.ready
	def is_chn_d_ack: Bool = d.ready & d.valid

	def op_accessAck(sid: UInt) = {
		d.opcode  := AccessAck
		d.param   := 0.U
		d.size    := 3.U
		d.source  := sid
		d.sink    := DontCare
		d.data    := DontCare
		d.corrupt := false.B
		d.denied  := false.B
		d.valid   := true.B
	}


	def op_accessDataAck(sid: UInt, data: UInt) = {
		d.opcode  := AccessAckData
		d.param   := 0.U
		d.size    := 3.U
		d.source  := sid
		d.sink    := DontCare
		d.data    := data
		d.corrupt := false.B
		d.denied  := false.B
		d.valid   := true.B
	}

	def is_getData        = (a.opcode === Get)            & is_chn_a_ack 
	def is_putFullData    = (a.opcode === PutFullData)    & is_chn_a_ack 
	def is_putPartialData = (a.opcode === PutPartialData) & is_chn_a_ack 
	def is_arithmeticData = (a.opcode === ArithmeticData) & is_chn_a_ack 
	def is_logicalData    = (a.opcode === LogicalData)    & is_chn_a_ack 
}

