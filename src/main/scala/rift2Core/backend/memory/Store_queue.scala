
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

package rift2Core.backend.memory

import chisel3._
import chisel3.util._
import rift2Core.define._
import rift2Core.backend._
import rift2Core.L1Cache._
import rift2Core.privilege._
import rift._
import base._
import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._



/**
  * bound to every cache 
  */
class Store_queue(dp: Int = 16)(implicit p: Parameters) extends RiftModule{
  def dp_w = log2Ceil(dp)
  def nm = 8

  val io = IO( new Bundle{
    val enq = Flipped(DecoupledIO(new Lsu_iss_info))
    val deq = DecoupledIO(new Info_cache_s0s1)

    // val is_st_commited = Input(Vec(2,Bool()))
    val cmm_lsu = Input(new Info_cmm_lsu)
    val is_empty = Output(Bool())

    val overlap =  Flipped(new Info_overlap)

    val flush = Input(Bool())

    /** prefetch is not guarantee to be accepted by cache*/
    // val preFetch = ValidIO( UInt(64.W) )
  } )


  val buff = RegInit(VecInit(Seq.fill(dp)(0.U.asTypeOf(new Info_cache_s0s1))))
  
  val cm_ptr_reg = RegInit( 0.U((dp_w+1).W) )
  val wr_ptr_reg = RegInit( 0.U((dp_w+1).W) )
  val rd_ptr_reg = RegInit( 0.U((dp_w+1).W) )

  val cm_ptr = cm_ptr_reg(dp_w-1,0)
  val wr_ptr = wr_ptr_reg(dp_w-1,0)
  val rd_ptr = rd_ptr_reg(dp_w-1,0)

  def full = (wr_ptr_reg(dp_w) =/= rd_ptr_reg(dp_w)) & (wr_ptr_reg(dp_w-1,0) === rd_ptr_reg(dp_w-1,0))
  def emty = cm_ptr_reg === rd_ptr_reg

  val rd_buff = buff(rd_ptr)

  val is_amo = {
    val is_amo_pre = RegNext(io.cmm_lsu.is_amo_pending & ~io.flush & ~io.is_empty, false.B)
    (is_amo_pre === false.B) & (io.cmm_lsu.is_amo_pending === true.B  & ~io.flush & ~io.is_empty)
  }

  val is_st_commited = VecInit( io.cmm_lsu.is_store_commit(0), io.cmm_lsu.is_store_commit(1) )
  io.enq.ready := ~full
  io.deq.valid := ~emty

  io.deq.bits := Mux( io.deq.valid, rd_buff, DontCare )

  when( io.flush ) {
    wr_ptr_reg := cm_ptr_reg
    // assert( ~is_st_commited(0) & ~is_st_commited(1) & ~is_amo )
  } .elsewhen( io.enq.fire ) {
    buff(wr_ptr) := pkg_Info_cache_s0s1(io.enq.bits)
    wr_ptr_reg := wr_ptr_reg + 1.U
  }

  when( io.deq.fire ) {
    rd_ptr_reg := rd_ptr_reg + 1.U
  }



  when( is_st_commited(1) & is_st_commited(0) ) {
    cm_ptr_reg := cm_ptr_reg + 2.U
    // assert( is_st_commited(0) )
    assert( ~is_amo )
  } .elsewhen( is_st_commited(0) | is_amo | is_st_commited(1) ) {
    cm_ptr_reg := cm_ptr_reg + 1.U
    assert( ~((is_st_commited(0) | is_st_commited(1)) & is_amo), "Assert Failed, is_amo only launch at chn 0!\n" )
  }

    io.is_empty := (cm_ptr_reg === wr_ptr_reg) & (cm_ptr_reg === rd_ptr_reg)

  // for ( chn <- 0 until (nm+2) ) yield {
    val overlap_buff = Wire(Vec(dp, (new Info_cache_s0s1)))
    // val overlap_buff = RegInit(VecInit(Seq.fill(dp)(0.U.asTypeOf(new Info_cache_s0s1) )))


    when( rd_ptr_reg(dp_w) =/= wr_ptr_reg(dp_w) ) {
      assert( rd_ptr >= wr_ptr )
      for ( i <- 0 until dp ) yield {
        val ro_ptr = (rd_ptr_reg + i.U)(dp_w-1,0)
        when( (ro_ptr >= rd_ptr || ro_ptr < wr_ptr) && (buff(ro_ptr).paddr(63,3) === io.overlap.paddr(63,3)) ) {
          overlap_buff(i) := buff(ro_ptr)
        } .otherwise {
          overlap_buff(i) := 0.U.asTypeOf(new Info_cache_s0s1)
        }
      }
    } .otherwise {
      assert( rd_ptr <= wr_ptr )
      for ( i <- 0 until dp ) yield {
        val ro_ptr = (rd_ptr_reg + i.U)(dp_w-1,0)
        when( ro_ptr >= rd_ptr && ro_ptr < wr_ptr && (buff(ro_ptr).paddr(63,3) === io.overlap.paddr(63,3)) ) {
          overlap_buff(i) := buff(ro_ptr)
        } .otherwise {
          overlap_buff(i) := 0.U.asTypeOf(new Info_cache_s0s1)
        }
      }
    }



    val temp_wdata = Wire(Vec(dp, UInt(64.W)))
    val temp_wstrb = Wire(Vec(dp, UInt(8.W)))
    for ( i <- 0 until dp ) yield {
      if ( i == 0 ) {
        val (wdata, wstrb) = overlap_wr( 0.U, 0.U, overlap_buff(0).wdata(0), overlap_buff(0).wstrb)
        temp_wdata(0) := wdata
        temp_wstrb(0) := wstrb
      } else {
        val (wdata, wstrb) = overlap_wr( temp_wdata(i-1), temp_wstrb(i-1), overlap_buff(i).wdata(0), overlap_buff(i).wstrb)
        temp_wdata(i) := wdata
        temp_wstrb(i) := wstrb
      }
    }
    io.overlap.wdata := temp_wdata(dp-1)
    io.overlap.wstrb := temp_wstrb(dp-1)
  // }

 





}

