




/*
* @Author: Ruige Lee
* @Date:   2021-04-09 10:34:13
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-09 10:34:13
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
import rift2Core.basic._

class BHT() {

    val io = new Bundle {
        val is_branch_valid = Wire(Bool())
        val is_branch_taken = Wire(Bool())

        val bhq_pop_pc = Wire(UInt(64.W))
    }

    val bht_buf = RegInit(VecInit(Fill(4096, "b01".U(2.W) ) ))

    def idx = io.bhq_pop_pc(12,1)

    when( io.is_branch_valid ) {
        bht_buf(idx) := Mux1H(Seq(
            (bht_buf(idx) === "b00".U) -> Mux( io.is_branch_taken, "b01".U, "b00".U ),
            (bht_buf(idx) === "b01".U) -> Mux( io.is_branch_taken, "b10".U, "b00".U ),
            (bht_buf(idx) === "b10".U) -> Mux( io.is_branch_taken, "b11".U, "b01".U ),
            (bht_buf(idx) === "b11".U) -> Mux( io.is_branch_taken, "b11".U, "b10".U )
        ))
    }
}

class Info_BHQ extends Bundle {
    val dir = Wire(Bool())
    val opp_pc = Wire(UInt(64.W))
}


class BHQ() {

    val fifo = new MultiPortFifo( new Info_BHQ, 4, 2, 1 )



}

class JHQ() {

}

class JTB() { //only used in jalr and only when RAS is down

}

class RAS() {

}


class BranchPredict() {

}
