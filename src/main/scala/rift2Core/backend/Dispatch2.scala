
/*
  Copyright (c) 2020 - 2022 Wuhan University of Technology <295054118@whut.edu.cn>

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
import base._
import rift2Core.define._
import rift2Core.frontend._


class Dispatch() extends Module {
  val io = IO(new Bundle{
    val bd_dpt = Vec(2, Flipped(new DecoupledIO(new Info_bd_dpt())))

    val alu_dpt_iss = new DecoupledIO(new Alu_dpt_info())
    val bru_dpt_iss = new DecoupledIO(new Bru_dpt_info())
    val lsu_dpt_iss = new DecoupledIO(new Lsu_dpt_info())
    val csr_dpt_iss = new DecoupledIO(new Csr_dpt_info())
    val mul_dpt_iss = new DecoupledIO(new Mul_dpt_info())

    val rod_i = Vec(2,new DecoupledIO(new Info_reorder_i))

    val rn_ptr_i = Vec(32, Input(UInt(6.W)))
    val log_i = Vec(64, Input(UInt(2.W)))
    val rn_op_i = Vec(2, ValidIO( new Info_rename_op ))

    val flush = Input(Bool())    
  })

}


