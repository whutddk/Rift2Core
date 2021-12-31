


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

import rift2Core.define._

class WriteBack extends Module {
  val io = IO(new Bundle{
    val exe_iwb = Vec(5, (Flipped(new DecoupledIO(new Exe_iwb_info))))

    val wb_op = Vec(2, ValidIO( new Info_writeback_op))
  })


  io.exe_iwb(0).ready := PopCount(Seq( io.exe_iwb(1).fire, io.exe_iwb(2).fire ) ) < 2.U
  io.exe_iwb(1).ready := true.B
  io.exe_iwb(2).ready := true.B
  io.exe_iwb(3).ready := PopCount(Seq( io.exe_iwb(0).fire, io.exe_iwb(1).fire, io.exe_iwb(2).fire ) ) < 2.U
  io.exe_iwb(4).ready := PopCount(Seq( io.exe_iwb(0).fire, io.exe_iwb(1).fire, io.exe_iwb(2).fire, io.exe_iwb(3).fire ) ) < 2.U




  io.wb_op(0).valid := PopCount(Seq( io.exe_iwb(0).fire, io.exe_iwb(1).fire, io.exe_iwb(2).fire, io.exe_iwb(3).fire, io.exe_iwb(4).fire ) ) > 0.U
  io.wb_op(1).valid := PopCount(Seq( io.exe_iwb(0).fire, io.exe_iwb(1).fire, io.exe_iwb(2).fire, io.exe_iwb(3).fire, io.exe_iwb(4).fire ) ) === 2.U

  val idx = VecInit(
    io.exe_iwb.indexWhere    ( (x:DecoupledIO[Exe_iwb_info]) => (x.fire === true.B)),
    io.exe_iwb.lastIndexWhere( (x:DecoupledIO[Exe_iwb_info]) => (x.fire === true.B))
  )

  for ( i <- 0 until 2 ) yield {
    io.wb_op(i).bits.phy  := io.exe_iwb(idx(i)).bits.rd0_phy
    io.wb_op(i).bits.dnxt := io.exe_iwb(idx(i)).bits.res

  }
  //alu, lsu and bru must always ready
}


