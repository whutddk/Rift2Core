package rift2Core

/*
* @Author: Ruige Lee
* @Date:   2021-03-19 09:55:43
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-03-22 20:02:27
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


import chisel3._
import chisel3.util._
import rift2Core.basic._
import rift2Core.frontend._


class Decode extends Module {
	val io = IO( new Bundle {
		val ib_id = Input ( new Info_ib_id())
		val id_dpt = Output ( new Info_id_dpt())
	})

	io.id_dpt.info := 
		Mux( io.ib_id.is_rvc,
		new Decode16(io.ib_id.instr).info,
		new Decode32(io.ib_id.instr).info
		)

	io.id_dpt.is_iFAccessFault  := io.ib_id.pc(63,32) =/= (0.U)
	io.id_dpt.is_illeage        := io.id_dpt.info.is_illeage
	io.id_dpt.info.param.is_rvc := io.ib_id.is_rvc
	io.id_dpt.info.param.pc     := io.ib_id.pc

	

}

class Decode_ss extends Module with Superscalar {
	val io = IO( new Bundle {
		val ib_id = Vec(2, Flipped(new DecoupledIO(new Info_ib_id)))
		val id_dpt = Vec(2, new DecoupledIO(new Info_id_dpt))

		val flush = Input(Bool())
	})

	val decode = for ( i <- 0 until 2 ) yield { val mdl = Module(new Decode); mdl }
	val id_dpt_fifo = Module ( new MultiPortFifo( new Info_id_dpt, 4, 2, 2 ) )

	io.id_dpt <> id_dpt_fifo.io.deq
	id_dpt_fifo.io.flush := io.flush

	for ( i <- 0 until 2 ) yield {
		decode(i).io.ib_id := io.ib_id(i).bits
		id_dpt_fifo.io.enq(i).bits := decode(i).io.id_dpt
	}




	override def is_1st_solo = false.B
	override def is_2nd_solo = is_1st_solo & false.B

	id_dpt_fifo.io.enq(0).valid := ~io.flush & io.ib_id(0).valid
	id_dpt_fifo.io.enq(1).valid := ~io.flush & io.ib_id(0).valid & io.ib_id(1).valid & is_1st_solo

	io.ib_id(0).ready := id_dpt_fifo.is_enq_ack(0)
	io.ib_id(1).ready := id_dpt_fifo.is_enq_ack(0) & id_dpt_fifo.is_enq_ack(1) & is_1st_solo

	assert( ~(~id_dpt_fifo.is_enq_ack(0) & id_dpt_fifo.is_enq_ack(1)), "Assert Fail! When id_dpt_fifo push(0) failed, When id_dpt_fifo push(1) must failed," )

}



