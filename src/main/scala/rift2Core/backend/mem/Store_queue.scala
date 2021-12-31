
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

package rift2Core.backend.mmu

import chisel3._
import chisel3.util._
import rift2Core.define._
import rift2Core.backend._

class lsu_wr_Info extends Bundle {
  val paddr = UInt(64.W)
  val wdata = UInt(64.W)
  val wmask = UInt(64.W)
}


/**
  * @note if no store addr harazd, load req will be sent to dcache 
  * @note if    store addr harazd, load req will get res from forward store req
  * @note if no store addr harazd, write req will be push into write fifo
  * @note if    store addr harazd, write req will be push into write fifo
  * @note if no store addr harazd, amo req will be sent to dcache as a load, and write req will be push into write fifo
  * @note if    store addr harazd, load req will get res from forward store req, and write req will be push into write fifo
  * @note if a forward instr is flushed, reset all reservation
  * 
  * @note haraze will happeded at commit queue and wrirt-in queue
  */ 

class Reservation_lsu() extends Module {





}

/**
  * bound to every cache 
  */
class Store_queue(dp: Int = 64) extends Module {
  def dp_w = log2Ceil(dp)

  val io = IO( new Bundle{
    val enq = DecoupledIO(new Reservation_Info)

    /** only when the data is written into cache will this pop */
    val deq = Flipped(DecoupledIO(new Reservation_Info))

    val is_commited = Input(Vec(2,Bool()))

    val forward_paddr = ValidIO(UInt(64.W))
    val forward_wdata = Flipped(ValidIO(UInt(64.W)))
    val forward_wstrb = Flipped(ValidIO(UInt(64.W)))

    /** prefetch is not guarantee to be accepted by cache*/
    val preFetch = ValidIO( UInt(64.W) )
  } )


  val buff = RegInit(VecInit(Seq.fill(dp)(new Reservation_Info)))
  val cm_ptr_reg = RegInit( 0.U((dp_w+1).W) )
  val wr_ptr_reg = RegInit( 0.U((dp_w+1).W) )
  val rd_ptr_reg = RegInit( 0.U((dp_w+1).W) )

  val cm_ptr = cm_ptr_reg(dp_w-1,0)
  val wr_ptr = wr_ptr_reg(dp_w-1,0)
  val rd_ptr = rd_ptr_reg(dp_w-1,0)

  def full = (wr_ptr(dp_w) =/= rd_ptr(dp_w)) & (wr_ptr(dp_w-1,0) === rd_ptr(dp_w-1,0))
  def emty = cm_ptr === rd_ptr

  io.enq.ready := ~full
  io.deq.valid := ~emty

  when( io.enq.fire ) {
    buff(wr_ptr) := io.enq.bits
    wr_ptr_reg := wr_ptr_reg + 1.U
  }

  io.preFetch.valid := io.enq.fire
  io.preFetch.bits := io.enq.bits.paddr

  when( io.is_commited(1) ) {
    cm_ptr_reg := cm_ptr_reg + 2.U
  } .elsewhen( io.is_commited(0) ) {
    cm_ptr_reg := cm_ptr_reg + 1.U
  }

  when( io.deq.fire ) {
    rd_ptr_reg := rd_ptr_reg + 1.U
  }


  val forward_buff = Wire(Vec(dp, new Reservation_Info))

  val forward_paddr = ValidIO(UInt(64.W))
  val forward_wdata = Flipped(ValidIO(UInt(64.W)))
  val forward_wstrb = Flipped(ValidIO(UInt(64.W)))


  when( rd_ptr_reg(dp_w) =/= wr_ptr_reg(dp_w) ) {
    assert( rd_ptr >= wr_ptr )
    for ( i <- 0 until dp ) yield {
      val ro_ptr = (rd_ptr_reg + i)(dp_w-1,0)
      when( (ro_ptr >= rd_ptr || ro_ptr < wr_ptr) && (buff.paddr === io.forward_paddr) ) {
        forward_buff(i) := buff(ro_ptr)
      } .otherwise {
        forward_buff(i) := 0.U.asTypeOf(new Reservation_Info)
      }
    }
  } .otherwise {
    assert( rd_ptr <= wr_ptr )
    for ( i <- 0 until dp ) yield {
      val ro_ptr = (rd_ptr_reg + i)(dp_w-1,0)
      when( ro_ptr >= rd_ptr && ro_ptr < wr_ptr && (buff.paddr === io.forward_paddr) ) {
        forward_buff(i) := buff(ro_ptr)
      } .otherwise {
        forward_buff(i) := 0.U.asTypeOf(new Reservation_Info)
      }
    }
  }


  io.forward_wdata := {
    def fw_wr( ori: UInt, wdata: UInt, wmask: UInt): UInt = {
      (ori & ~wmask) | (wdata & wmask)
    }

    val temp_res = Wire(UInt(64.W))
    for ( i <- 0 until dp ) yield {
      if ( i == 0 ) {
        temp_res(0) := fw_wr( 0.U, forward_buff(0).wdata, forward_buff(0).wmask)
      } else {
        temp_res(i) := fw_wr( temp_res(i-1), forward_buff(i).wdata, forward_buff(i).wmask)
      }
    }
    temp_res(dp-1)
  }

  io.forward_wmask := forward_buff.map(x => x.wmask).reduce(_|_)


}

