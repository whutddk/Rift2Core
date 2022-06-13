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

import rift2Core.diff._


class dpt_lookup_info(dp: Int) extends Bundle{
  val rsp = Input(new Register_source(dp))
  val req = Output(new Register_source(32))
}

/**
  * rename channel is located in dpt-stage,
  * the raw-rs needs lookup the phy-rs num ( 1x, 2x, 3x raw-rs -> phy-rs ),
  * the rd should rename and malloc 1 new phy, 
  */
class dpt_rename_info(dp: Int) extends Bundle{
  val rsp = Input(new Register_dstntn(dp))
  val req = Decoupled(new Register_dstntn(32))
}

class iss_readOp_info(dw: Int, dp: Int) extends Bundle{
  val reg = Decoupled(new Register_source(dp))
  val dat = Input(new Operation_source(dw))
}

class Info_commit_op(dp:Int) extends Bundle{
  val is_comfirm = Output(Bool())
  val is_MisPredict = Output(Bool())
  val is_abort   = Output(Bool())
  val raw        = Output(UInt(5.W)  )
  val phy        = Output(UInt((log2Ceil(dp)).W))
  val toX        = Output(Bool())
  val toF        = Output(Bool())
  val is_writeback = Input(Bool())
}

class RegFiles(dw: Int, dp: Int=64, rn_chn: Int = 2, rop_chn: Int=6, wb_chn: Int = 6, cmm_chn: Int = 2) extends Module{
  val io = IO( new Bundle{

    val dpt_lookup = Vec( rn_chn, Flipped(new dpt_lookup_info(dp)) )
    val dpt_rename = Vec( rn_chn, Flipped(new dpt_rename_info(dp)) )
    /** read operators based on idx, must success */
    val iss_readOp = Vec(rop_chn, Flipped( new iss_readOp_info(dw, dp)) )
    /** writeBack request from exeUnit */
    val exe_writeBack = Vec(wb_chn, Flipped(new DecoupledIO(new WriteBack_info(dw,dp))))
    /** Commit request from commitUnit */
    val commit = Vec(cmm_chn, Flipped(new Info_commit_op(dp)))

    val diffReg = Output(Vec(32, UInt(dw.W)))
  })

  /**
    * dp-1 (63) files exist in this version,'
    * @note the file(dp) is assert to be Zero
    */
  val files_reg = RegInit( VecInit( Seq.fill(dp-1)(0.U(dw.W)) ))
  val files = {
    val res = Wire( Vec(dp, UInt(dw.W)) )
    for ( i <- 0 until dp-1 ) yield { res(i) := files_reg(i) }
    res(dp-1) := 0.U
    res
  }
  

  /**
    * dp-1 (63) log exist in this version,
    * @note the log(dp) is assert to be "b11".U
    */ 
  val log_reg = 
    RegInit( VecInit( Seq.fill(32)("b11".U(2.W) ) ++ Seq.fill(dp-32-1)(0.U(2.W) )))

  val log = {
    val res = Wire( Vec(dp, UInt(2.W)) )
    for ( i <- 0 until dp-1 ) yield { res(i) := log_reg(i) }
    res(dp-1) := "b11".U
    res
  }

  /**
    * index that 32 renamed register-sources point to
    */
  val rename_ptr = RegInit( VecInit( for( i <- 0 until 32 ) yield {i.U(log2Ceil(dp).W)} ) )

  /**
    * index that 32 commited register-sources point to
    */  
  val archit_ptr = RegInit( VecInit( for( i <- 0 until 32 ) yield {i.U(log2Ceil(dp).W)} ) )

  /**
    * finding out the first Free-phy-register
    */ 
  val molloc_idx = Wire(Vec(rn_chn, UInt((log2Ceil(dp)).W)))
  for ( i <- 0 until rn_chn ) {
    molloc_idx(i) := 0.U
    for ( j <- dp-1 to 0 by -1 ) {
      if ( i == 0 ) { when( log(j) === 0.U ) { molloc_idx(i) := j.U }  }
      else { when( log(j) === 0.U && j.U > molloc_idx(i-1) ) { molloc_idx(i) := j.U } }
    }
  }


  for ( i <- 0 until rn_chn ) {


    when( io.dpt_rename(i).req.fire ) {
      val idx = io.dpt_rename(i).req.bits.rd0
      assert( log(molloc_idx(i)) === "b00".U )
      log_reg(molloc_idx(i)) := "b01".U //may be override by flush
      rename_ptr(idx) := molloc_idx(i) //may be override by flush
    }

    io.dpt_rename(i).req.ready := log.count( (x:UInt) => ( x === 0.U ) ) > i.U
    io.dpt_rename(i).rsp.rd0 := molloc_idx(i)

  }


  for ( i <- 0 until rop_chn ) yield {
    val idx1 = io.iss_readOp(i).reg.bits.rs1
    val idx2 = io.iss_readOp(i).reg.bits.rs2
    val idx3 = io.iss_readOp(i).reg.bits.rs3

    when( io.iss_readOp(i).reg.fire ) {
      io.iss_readOp(i).dat.op1 := Mux(idx1 === 63.U, 0.U, files(idx1))
      io.iss_readOp(i).dat.op2 := Mux(idx2 === 63.U, 0.U, files(idx2))
      io.iss_readOp(i).dat.op3 := Mux(idx3 === 63.U, 0.U, files(idx3))
    } .otherwise {
      io.iss_readOp(i).dat.op1 := 0.U
      io.iss_readOp(i).dat.op2 := 0.U
      io.iss_readOp(i).dat.op3 := 0.U
    }
    io.iss_readOp(i).reg.ready :=
      (log(idx1) === "b11".U | idx1 === 63.U ) &
      (log(idx2) === "b11".U | idx2 === 63.U ) &
      (log(idx3) === "b11".U | idx3 === 63.U ) 

  }

  for ( i <- 0 until wb_chn ) yield {
    when( io.exe_writeBack(i).fire ) {
      val idx = io.exe_writeBack(i).bits.rd0
      assert( log(idx) === "b01".U, "Assert Failed when writeback at chn" + i + ", log(" + idx + ")" )
      log_reg(idx) := "b11".U
      files_reg(idx) := io.exe_writeBack(i).bits.res
    }
    io.exe_writeBack(i).ready := true.B
  }

  {
    val raw = io.commit.map{ x => x.raw }
    val phy = io.commit.map{ x => x.phy }
    val idx_pre = io.commit.map{ x => archit_ptr(x.raw) }

    for ( i <- 0 until cmm_chn ) {
      def m = cmm_chn-1-i

      io.commit(m).is_writeback := log(phy(m)) === "b11".U

      when( io.commit(m).is_MisPredict | io.commit(m).is_abort ) {
        /** clear all log to 0, except that archit_ptr point to, may be override */
        for ( j <- 0 until dp-1 ) yield {log_reg(j) := Mux( archit_ptr.exists( (x:UInt) => (x === j.U) ), log(j), "b00".U )}
      }
      when( io.commit(m).is_MisPredict | io.commit(m).is_comfirm ) {
        /** override the log(clear) */
        assert( io.commit(m).is_writeback )
        for ( j <- 0 until dp-1 ) yield {
          when(j.U === idx_pre(m) ) {log_reg(j) := 0.U} // the log, that used before commit, will be clear to 0
        }
        assert( log_reg(phy(m)) === "b11".U, "log_reg which going to commit to will be overrided to \"b11\" if there is an abort in-front." )
        log_reg(phy(m)) := "b11".U //the log, that going to use after commit should keep to be "b11"
      }


      when( io.commit(i).is_MisPredict | io.commit(i).is_comfirm ) {
        archit_ptr(raw(i)) := phy(i)
      }


      when ( io.commit(m).is_MisPredict | io.commit(m).is_abort ) {
        for ( j <- 0 until 32 ) yield {
          rename_ptr(j) := archit_ptr(j)
          for ( n <- 0 until m ) {
            when( j.U === raw(n) & io.commit(n).is_comfirm ) {
              rename_ptr(j) := phy(n) //override renme_ptr when the perivious chn comfirm
            } 
          }
          when( j.U === raw(m) & io.commit(m).is_MisPredict ) { rename_ptr(j) := phy(m) } //override renme_ptr when the this chn is mispredict
        }

      }
    }
  }


  archit_ptr.map{
    i => assert( log(i) === "b11".U, "Assert Failed, archit point to should be b11.U! i = "+i+"\n")
  }

  for ( i <- 0 until 32 ) yield {
    io.diffReg(i) := files(archit_ptr(i))    
  }
}

class XRegFiles (dw: Int, dp: Int=64, rn_chn: Int = 2, rop_chn: Int=6, wb_chn: Int = 6, cmm_chn: Int = 2) extends RegFiles(dw, dp, rn_chn, rop_chn, wb_chn, cmm_chn ) {

  for ( i <- 0 until rn_chn ) {
    val idx1 = io.dpt_lookup(i).req.rs1
    val idx2 = io.dpt_lookup(i).req.rs2

    if ( i == 0) {
      io.dpt_lookup(i).rsp.rs1 := Mux( idx1 === 0.U, 63.U, rename_ptr(idx1) )
      io.dpt_lookup(i).rsp.rs2 := Mux( idx2 === 0.U, 63.U, rename_ptr(idx2) )
      io.dpt_lookup(i).rsp.rs3 := 63.U
    } else {
      io.dpt_lookup(i).rsp.rs1 := Mux( idx1 === 0.U, 63.U, rename_ptr(idx1) )
      io.dpt_lookup(i).rsp.rs2 := Mux( idx2 === 0.U, 63.U, rename_ptr(idx2) )
      io.dpt_lookup(i).rsp.rs3 := 63.U
      for ( j <- 0 until i ) {
        when( io.dpt_rename(j).req.valid && (io.dpt_rename(j).req.bits.rd0 === idx1) && (idx1 =/= 0.U) ) { io.dpt_lookup(i).rsp.rs1 := molloc_idx(j) }
        when( io.dpt_rename(j).req.valid && (io.dpt_rename(j).req.bits.rd0 === idx2) && (idx2 =/= 0.U) ) { io.dpt_lookup(i).rsp.rs2 := molloc_idx(j) }
      }
    }
  }



}

class FRegFiles (dw: Int, dp: Int=64, rn_chn: Int = 2, rop_chn: Int=6, wb_chn: Int = 6, cmm_chn: Int = 2) extends RegFiles(dw, dp, rn_chn, rop_chn, wb_chn, cmm_chn ) {


  for ( i <- 0 until rn_chn ) {
    val idx1 = io.dpt_lookup(i).req.rs1
    val idx2 = io.dpt_lookup(i).req.rs2
    val idx3 = io.dpt_lookup(i).req.rs3

    if ( i == 0) {
      io.dpt_lookup(i).rsp.rs1 := rename_ptr(idx1)
      io.dpt_lookup(i).rsp.rs2 := rename_ptr(idx2)
      io.dpt_lookup(i).rsp.rs3 := rename_ptr(idx3)
    } else {
      io.dpt_lookup(i).rsp.rs1 := rename_ptr(idx1)
      io.dpt_lookup(i).rsp.rs2 := rename_ptr(idx2)
      io.dpt_lookup(i).rsp.rs3 := rename_ptr(idx3) 
      for ( j <- 0 until i ) {
        when( io.dpt_rename(j).req.valid && (io.dpt_rename(j).req.bits.rd0 === idx1) ) { io.dpt_lookup(i).rsp.rs1 := molloc_idx(j) }
        when( io.dpt_rename(j).req.valid && (io.dpt_rename(j).req.bits.rd0 === idx2) ) { io.dpt_lookup(i).rsp.rs2 := molloc_idx(j) }
        when( io.dpt_rename(j).req.valid && (io.dpt_rename(j).req.bits.rd0 === idx3) ) { io.dpt_lookup(i).rsp.rs3 := molloc_idx(j) }
      }
    }
  }


}


class FakeFRegFiles(dw: Int, dp: Int=64, rn_chn: Int = 2, rop_chn: Int=6, wb_chn: Int = 6, cmm_chn: Int = 2) extends Module{
  val io = IO( new Bundle{

    val dpt_lookup = Vec( rn_chn, Flipped(new dpt_lookup_info(dp)) )
    val dpt_rename = Vec( rn_chn, Flipped(new dpt_rename_info(dp)) )
    val iss_readOp = Vec(rop_chn, Flipped( new iss_readOp_info(dw, dp)) )
    val exe_writeBack = Vec(wb_chn, Flipped(new DecoupledIO(new WriteBack_info(dw,dp))))

    val commit = Vec(cmm_chn, Flipped(new Info_commit_op(dp)))

    val diffReg = Output(Vec(32, UInt(dw.W)))
  })



  for( i <- 0 until rn_chn ) {
    io.dpt_lookup(i).rsp := 0.U.asTypeOf(new Register_source(dp))
    io.dpt_rename(i).rsp := 0.U.asTypeOf(new Register_dstntn(dp))
    io.dpt_rename(i).req.ready := true.B
  }

  for( i <- 0 until rop_chn ) {
    io.iss_readOp(i).dat := 0.U.asTypeOf(new Operation_source(dw))
    io.iss_readOp(i).reg.ready := true.B
    assert( ~io.iss_readOp(i).reg.valid )
  }

  for( i <- 0 until wb_chn ) {
    io.exe_writeBack(i).ready := true.B
    assert( ~io.exe_writeBack(i).valid )
  }

  for( i <- 0 until cmm_chn ) {
    io.commit(i).is_writeback := false.B
  }

  io.diffReg := 0.U.asTypeOf(Vec(32, UInt(dw.W)))

}

