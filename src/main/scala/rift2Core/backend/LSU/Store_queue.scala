
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

package rift2Core.backend.lsu

import chisel3._
import chisel3.util._

import rift2Core.define._

import base._
import rift2Chip._

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._


abstract class Stq_Base()(implicit p: Parameters) extends DcacheModule{

  def st_w = log2Ceil(stEntry)

  val io = IO( new Bundle{
    val enq = Flipped(DecoupledIO(new Lsu_iss_info))
    val deq = DecoupledIO(new Lsu_iss_info)

    val cmm_lsu = Input(new Info_cmm_lsu)
    val is_empty = Output(Bool())



    val overlapReq  = Flipped(Valid(new Stq_req_Bundle))
    val overlapResp = Valid(new Stq_resp_Bundle)

    val flush = Input(Bool())

    /** prefetch is not guarantee to be accepted by cache*/
    val preFetch = ValidIO( new PreFetch_Req_Bundle )
  } )

  val buff = RegInit(VecInit(Seq.fill(stEntry)(0.U.asTypeOf(new Lsu_iss_info))))
  
  val cm_ptr_reg = RegInit( 0.U((st_w+1).W) )
  val wr_ptr_reg = RegInit( 0.U((st_w+1).W) )
  val rd_ptr_reg = RegInit( 0.U((st_w+1).W) )

  val cm_ptr = cm_ptr_reg(st_w-1,0)
  val wr_ptr = wr_ptr_reg(st_w-1,0)
  val rd_ptr = rd_ptr_reg(st_w-1,0)

  val full = (wr_ptr_reg(st_w) =/= rd_ptr_reg(st_w)) & (wr_ptr_reg(st_w-1,0) === rd_ptr_reg(st_w-1,0))
  val emty = cm_ptr_reg === rd_ptr_reg

  val rd_buff = buff(rd_ptr)

  val is_amo = RegInit(false.B)


}

trait Stq_Ptr { this: Stq_Base => 
  {
    val is_amo_pre = RegNext(io.cmm_lsu.is_amo_pending, false.B)
    val is_amo_pos = io.cmm_lsu.is_amo_pending & ~is_amo_pre
    when( io.flush ) {
      is_amo := false.B
    } .elsewhen( is_amo_pos ) {
      is_amo := true.B
    } .elsewhen(is_amo & ~io.is_empty) {
      is_amo := false.B
    }
  }


  val is_st_commited = io.cmm_lsu.is_store_commit
  io.enq.ready := ~full
  io.deq.valid := ~emty

  io.deq.bits := Mux( io.deq.valid, rd_buff, 0.U.asTypeOf(new Lsu_iss_info) )

  when( io.flush ) {
    if( cm_chn ==2 ) {
      when( is_st_commited(1) & is_st_commited(0) ) { wr_ptr_reg := cm_ptr_reg + 2.U }
      .elsewhen( is_st_commited(0) | (is_amo & ~io.is_empty) | is_st_commited(1) ) { wr_ptr_reg := cm_ptr_reg + 1.U } //amo only resolved at chn0
      .otherwise{ wr_ptr_reg := cm_ptr_reg }      
    } else if ( cm_chn == 1 ) {
      when( is_st_commited(0) | (is_amo & ~io.is_empty) ) { wr_ptr_reg := cm_ptr_reg + 1.U } //amo only resolved at chn0
      .otherwise{ wr_ptr_reg := cm_ptr_reg }         
    } else {
      require(false)
    }

  } .elsewhen( io.enq.fire ) {
    buff(wr_ptr) := io.enq.bits
    wr_ptr_reg := wr_ptr_reg + 1.U
  }

  when( io.deq.fire ) {
    rd_ptr_reg := rd_ptr_reg + 1.U
    buff(rd_ptr) := 0.U.asTypeOf(new Lsu_iss_info)
  }

  if( cm_chn == 2 ) {
    when( is_st_commited(1) & is_st_commited(0) ) {
      cm_ptr_reg := cm_ptr_reg + 2.U
      assert( ~is_amo )
      assert( cm_ptr_reg =/= wr_ptr_reg & cm_ptr_reg =/= (wr_ptr_reg-1.U) )
    } .elsewhen( is_st_commited(0) | (is_amo & ~io.is_empty) | is_st_commited(1) ) { //amo only resolved at chn0
      cm_ptr_reg := cm_ptr_reg + 1.U
      assert( ~((is_st_commited(0) | is_st_commited(1)) & (is_amo & ~io.is_empty)), "Assert Failed, is_amo only launch at chn 0!\n" )
      assert( cm_ptr_reg =/= wr_ptr_reg )
    }    
  } else if( cm_chn == 1 ) {
    when( is_st_commited(0) ) {
      cm_ptr_reg := cm_ptr_reg + 1.U
      assert( ~is_amo )
      assert( cm_ptr_reg =/= wr_ptr_reg )
    } .elsewhen( (is_amo & ~io.is_empty) ) { //amo only resolved at chn0
      cm_ptr_reg := cm_ptr_reg + 1.U
      assert( ~((is_st_commited(0)) & (is_amo & ~io.is_empty)), "Assert Failed, is_amo only launch at chn 0!\n" )
      assert( cm_ptr_reg =/= wr_ptr_reg )
    }
  } else {
    require(false)
  }


    io.is_empty := (cm_ptr_reg === wr_ptr_reg) & (cm_ptr_reg === rd_ptr_reg)
}

trait Stq_Overlap{ this: Stq_Base => 
    val overlap_buff = Wire(Vec(stEntry, (new Lsu_iss_info)))
    io.overlapResp.valid := io.overlapReq.valid //may be overlapped

    when( rd_ptr_reg(st_w) =/= wr_ptr_reg(st_w) ) {
      assert( rd_ptr >= wr_ptr )
      for ( i <- 0 until stEntry ) yield {
        val ro_ptr = (rd_ptr_reg + i.U)(st_w-1,0)
        when( (ro_ptr >= rd_ptr || ro_ptr < wr_ptr) && (buff(ro_ptr).param.dat.op1(plen-1,3) === io.overlapReq.bits.paddr(plen-1,3)) ) {
          overlap_buff(i) := buff(ro_ptr)

          when( buff(ro_ptr).fun.is_amo ) { io.overlapResp.valid := false.B }
          // assert( ~(buff(ro_ptr).fun.is_amo & io.overlapResp.valid), "Assert Failed at Store-queue, overlapping an amo-instr, that is not allow!" )
        } .otherwise {
          overlap_buff(i) := 0.U.asTypeOf(new Lsu_iss_info)
        }
      }
    } .otherwise {
      assert( rd_ptr <= wr_ptr )
      for ( i <- 0 until stEntry ) yield {
        val ro_ptr = (rd_ptr_reg + i.U)(st_w-1,0)
        when( ro_ptr >= rd_ptr && ro_ptr < wr_ptr && (buff(ro_ptr).param.dat.op1(plen-1,3) === io.overlapReq.bits.paddr(plen-1,3)) ) {
          overlap_buff(i) := buff(ro_ptr)

          when( buff(ro_ptr).fun.is_amo ) { io.overlapResp.valid := false.B }
          // assert( ~(buff(ro_ptr).fun.is_amo & io.overlapResp.valid), "Assert Failed at Store-queue, overlapping an amo-instr, that is not allow!" )
        } .otherwise {
          overlap_buff(i) := 0.U.asTypeOf(new Lsu_iss_info)
        }
      }
    }

    val temp_wdata = Wire(Vec(stEntry, UInt(64.W)))
    val temp_wstrb = Wire(Vec(stEntry, UInt(8.W)))
    for ( i <- 0 until stEntry ) yield {
      if ( i == 0 ) {
        val (wdata, wstrb) = overlap_wr( 0.U(64.W), 0.U(8.W), overlap_buff(0).wdata_align(64), overlap_buff(0).wstrb_align(64) )
        temp_wdata(0) := wdata
        temp_wstrb(0) := wstrb
      } else {
        val (wdata, wstrb) = overlap_wr( temp_wdata(i-1), temp_wstrb(i-1), overlap_buff(i).wdata_align(64), overlap_buff(i).wstrb_align(64) )
        temp_wdata(i) := wdata
        temp_wstrb(i) := wstrb
      }
    }
    io.overlapResp.bits.wdata := temp_wdata(stEntry-1)
    io.overlapResp.bits.wstrb := temp_wstrb(stEntry-1)
}


/**
  * bound to every cache 
  */
class Store_queue()(implicit p: Parameters) extends Stq_Base with Stq_Ptr with Stq_Overlap {
  if(hasPreFetch) {
    io.preFetch.valid      := io.enq.fire
    io.preFetch.bits.paddr := io.enq.bits.param.dat.op1    
  } else {
    io.preFetch.valid      := false.B
    io.preFetch.bits.paddr := 0.U
  }

}










 




