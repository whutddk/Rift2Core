


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


class Store_buffer(dp: Int = 16) extends Module {
  val io = IO(new Bundle{
    val enq = DecoupledIO(new Reservation_Info)
    val rls = DecoupledIO(UInt(log2Ceil(dp).W))

    val forward_paddr = ValidIO(UInt(64.W))
    val forward_wdata = Flipped(ValidIO(UInt(64.W)))
    val forward_wstrb = Flipped(ValidIO(UInt(64.W)))
  })

  val buff = RegInit(VecInit(Seq.fill(dp)(0.U.asTypeof(new Reservation_Info))))
  val valid = RegInit(VecInit(Seq.fill(dp)(false.B)))


  for ( i <- 0 until dp ) yield {
    when( valid(i) == ture.B ) {
      assert( buff.count( (x: Reservation_Info) => (x.paddr === buff(i).paddr) ) === 1.U )
    }
  }




  {
    val is_hazard = buff.map( x => (x.paddr === io.forward_paddr.paddr) ).reduce(_|_)
    val idx = valid.indexWhere( (x:Bool) => (x === true.B) )

    io.forward_wdata.valid := io.forward_paddr.valid & is_hazard
    io.forward_wstrb.valid := io.forward_paddr.valid & is_hazard

    io.forward_wdata.bits := Mux( io.forward_wdata.valid, buff(idx), 0.U ) 
    io.forward_wstrb.bits := Mux( io.forward_wstrb.valid, buff(idx), 0.U ) 
  }



  for ( i <- 0 until dp ) yield {
    val is_hazard = buff.map( x => (x.paddr === io.enq.bits.paddr) ).reduce(_|_)
    val idx = valid.indexWhere( (x:Bool) => (x === false.B) )
    io.enq.ready := 
      valid.exists( (x:Bool) => (x === false.B) ) &
      ~is_hazard

    when( io.enq.fire ) {
      buff(idx)  := io.enq.bits
      valid(idx) := true.B
    }
    when( io.rls.fire ) {
      buff(io.rls.bits)  := 0.U.asTypeOf( new Reservation_Info )
      valid(io.rls.bits) := false.B
    }
  }


}

