

/*
* @Author: Ruige Lee
* @Date:   2021-04-14 09:19:20
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-14 09:20:03
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


package rift2Core.backend

import chisel3._
import chisel3.util._

import rift2Core.define._

class WriteBack extends Module {
  val io = IO(new Bundle{
    val exe_iwb = Vec(5, (Flipped(new DecoupledIO(new Exe_iwb_info))))

    val wb_op = Vec(5, ValidIO( new Info_writeback_op))
    // val wb_reg = Output(new Info_writeback_op)
  })


  for ( i <- 0 until 5 ) yield { io.exe_iwb(i).ready := true.B }

  for ( i <- 0 until 5 ) yield {
    io.wb_op(i).valid :=  io.exe_iwb(i).fire
  }


  for ( i <- 0 until 5 ) yield {
    io.wb_op(i).bits.phy  := io.exe_iwb(i).bits.rd0_phy
    io.wb_op(i).bits.dnxt := io.exe_iwb(i).bits.res

  }
  //alu, lsu and bru must always ready
}


