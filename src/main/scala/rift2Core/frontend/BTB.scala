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

package rift2Core.frontend

import chisel3._
import chisel3.util._


class Info_BTB extends IFetchBundle {
  val targets = UInt(64.W)
  val tag     = UInt(8.W)
  val is_active = Vec(4, Bool()) 
  val is_jump   = Vec(4, Bool())
  val is_takens = Vec(4, Bool())
  val is_branch = Vec(4, Bool())
  val is_RVC    = Vec(4, Bool())

  def tagHash = 
}


class BTB extends IFetchModule {
  def cl: Int = { var res = 1; for ( i <- 0 until btb_cl_w ) { res = res * 2 }; return res }

  val io = IO(new Bundle{
    val reqFromIF1  = Flipped(Decoupled())
    val reqFromIF3  = Flipped(Decoupled())
    val respToIF1 = Valid()
    val respToIF3 = Valid()

  })

  val rd_cl_sel = io.reqFromIF1.bits.pchash
  val wr_cl_sel = io.reqFromIF3.bits.pchash
  val btb_table = Mem( cl, new Info_BTB )


  btb_table.read(rd_cl_sel)

    when( io.reqFromIF3.fire ) {
      btb_table.write(wr_cl_sel, io.reqFromIF3.bits)
  }


}
