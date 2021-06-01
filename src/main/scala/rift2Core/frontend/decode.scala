

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

package rift2Core.frontend

import chisel3._
import chisel3.util._
import base._
import rift2Core.define._



class Decode extends Module {
  val io = IO( new Bundle {
    val ib_id = Input ( new Info_ib_id())
    val id_dpt = Output ( new Info_bd_dpt())
  })

  io.id_dpt.info := 
    Mux( io.ib_id.is_rvc,
    new Decode16(io.ib_id.instr, io.ib_id.pc).info,
    new Decode32(io.ib_id.instr, io.ib_id.pc).info
    )

  io.id_dpt.is_iFAccessFault  := io.ib_id.pc(63,32) =/= (0.U)
  io.id_dpt.is_illeage        := io.id_dpt.info.is_illeage

  

}


object Decode {
  def apply( ib_id: Info_ib_id ) = {
    val mdl = Module(new Decode)
    mdl.io.ib_id := ib_id

    mdl.io.id_dpt
  }
}


