


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

package rift2Core.backend.mem

import chisel3._
import chisel3.util._
import rift2Core.define._
import rift2Core.backend._

class cacheMux(implicit p: Parameters) extends DcacheModule{
  val io = IO(new Bundle{
    val enq = Flipped(new DecoupledIO(new Reservation_Info))
    val deq = Vec(nm, new DecoupledIO(new Reservation_Info))
  })

  val chn = io.enq.bits.paddr(12+nm_w-1,12)

  for ( i <- 0 until nm ) yield {
    when( i.U === chn ) {
      io.deq(i) <> io.enq
    } .otherwise {
      io.deq(i).valid := false.B
      io.deq(i).bits  := DontCare
    }
  }
}


