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
		val iq_id = Flipped(new DecoupledIO(new Info_ib_id()))
		val id_dpt = new DecoupledIO(new Info_id_dpt())
	})

	val info_id_dpt = Wire(new Info_id_dpt)

	info_id_dpt.info := 
		Mux( io.iq_id.bits.is_rvc,
		new Decode16(io.iq_id.bits.instr).info,
		new Decode32(io.iq_id.bits.instr).info
		)

	info_id_dpt.isIFAccessFault := (io.iq_id.bits.pc(63,32)) =/= (0.U)
	info_id_dpt.isIlleage := io.id_dpt.bits.info.is_illeage
	info_id_dpt.info.param.is_rvc := io.iq_id.bits.is_rvc
	info_id_dpt.info.param.pc := io.iq_id.bits.pc


	val instr_info_fifo = Module(new Queue(new Info_id_dpt, 16))

	instr_info_fifo.io.enq.valid := io.iq_id.valid
	io.iq_id.ready               := instr_info_fifo.io.enq.ready
	instr_info_fifo.io.enq.bits  := info_id_dpt


	io.id_dpt              <> instr_info_fifo.io.deq

}

// class Decode_ss extends Module with Superscalar {
// 	val io = IO( new Bundle {
// 		val iq_id = Vec(2, Flipped(new DecoupledIO(new Info_ib_id())))
// 		val id_dpt = Vec(2, new DecoupledIO(new Info_id_dpt()))
// 	})

// 	val decode = for ( i <- 0 until 2 ) yield { val mdl = Module(new Decode); mdl }

// 	for ( i <- 0 until 2 ) yield { io.id_dpt(i).bits := decode(i).io. }




// }



