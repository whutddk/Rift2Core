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

import rift2Chip._
import chipsalliance.rocketchip.config.Parameters

class Lookup_Bundle()(implicit p: Parameters) extends RiftBundle{
  val rsp = Input(new RS_PHY)
  val req = Output(new RS_RAW)
}

/**
  * rename channel is located in dpt-stage,
  * the raw-rs needs lookup the phy-rs num ( 1x, 2x, 3x raw-rs -> phy-rs ),
  * the rd should rename and malloc 1 new phy, 
  */
class Rename_Bundle()(implicit p: Parameters) extends RiftBundle{
  val rsp = Input(new RD_PHY)
  val req = Decoupled(new RD_RAW)
}

// class iss_readOp_info(dw: Int)(implicit p: Parameters) extends RiftBundle{
//   val reg = Decoupled(new RS_PHY)
//   val dat = Input(new Operation_source(dw))
// }

// class ReadOp_req_Bundle(implicit p: Parameters) extends RiftBundle{
//   val rs = new RS_PHY
// }

// class ReadOp_rsp_Bundle(dw: Int)(implicit p: Parameters) extends ReadOp_req_Bundle{
//   val dat = new Operation_source(dw)
// }

class Info_commit_op(implicit p: Parameters) extends RiftBundle{
  val is_comfirm = Output(Bool())
  val is_MisPredict = Output(Bool())
  val is_abort   = Output(Bool())
  val raw        = Output(UInt(5.W)  )
  val phy        = Output(UInt((log2Ceil(regNum)).W))
  val toX        = Output(Bool())
  val toF        = Output(Bool())
  val is_writeback = Input(Bool())
}

abstract class RegFilesBase(dw: Int, rop_chn: Int, wb_chn: Int)(implicit p: Parameters) extends RiftModule{
  def opc = rop_chn
  def wbc = wb_chn
  val io = IO( new Bundle{

    val lookup = Vec( rnChn, Flipped(new Lookup_Bundle) )
    val rename = Vec( rnChn, Flipped(new Rename_Bundle) )

    /** read operators based on idx, must success */
    // val iss_readOp = Vec(opc, Flipped( new iss_readOp_info(dw)) )
    val rgLog = Output( Vec(regNum, UInt(2.W)) )
    val rgReq = Flipped(Vec( opc, Valid( UInt((log2Ceil(regNum)).W) ) ))
    val rgRsp =         Vec( opc, Valid(new ReadOp_Rsp_Bundle(dw) ))


    /** writeBack request from exeUnit */
    val exe_writeBack = Vec(wbc, Flipped(new DecoupledIO(new WriteBack_info(dw))))
    /** Commit request from commitUnit */
    val commit = Vec(cm_chn, Flipped(new Info_commit_op))

    val diffReg = Output(Vec(32, UInt(dw.W)))
  })
}

abstract class RegFilesReal(dw: Int, rop_chn: Int, wb_chn: Int)(implicit p: Parameters) extends RegFilesBase(dw, rop_chn, wb_chn){

  val raw = io.commit.map{ x => x.raw }
  val phy = io.commit.map{ x => x.phy }

  /**
    * there are regNum-1 log,
    * @note the log(regNum) is assert to be "b11".U
    */ 
  val log_reg = 
    RegInit( VecInit( Seq.fill(32)("b11".U(2.W) ) ++ Seq.fill(regNum-32-1)(0.U(2.W) )))

  val log = {
    val res = Wire( Vec(regNum, UInt(2.W)) )
    for ( i <- 0 until regNum-1 ) yield { res(i) := log_reg(i) }
    res(regNum-1) := "b11".U
    res
  }

  /**
    * there are regNum-1 files,
    * @note the file(regNum) is assert to be Zero
    */
  val files_reg = RegInit( VecInit( Seq.fill(regNum-1)(0.U(dw.W)) ))
  val files = {
    val res = Wire( Vec(regNum, UInt(dw.W)) )
    for ( i <- 0 until regNum-1 ) yield { res(i) := files_reg(i) }
    res(regNum-1) := 0.U
    res
  }

  /**
    * index that 32 renamed register-sources point to
    */
  val rename_ptr = RegInit( VecInit( for( i <- 0 until 32 ) yield {i.U(log2Ceil(regNum).W)} ) )

  /**
    * index that 32 commited register-sources point to
    */  
  val archit_ptr = RegInit( VecInit( for( i <- 0 until 32 ) yield {i.U(log2Ceil(regNum).W)} ) )

  for ( i <- 0 until 32 ) yield {
    io.diffReg(i) := files(archit_ptr(i))
  }


}


trait RegFilesReName{ this: RegFilesReal => 


  /**
    * finding out the first Free-phy-register
    */ 
  val molloc_idx = Wire(Vec(rnChn, UInt((log2Ceil(regNum)).W)))
  for ( i <- 0 until rnChn ) {
    molloc_idx(i) := 0.U
    for ( j <- (regNum-1) to 0 by -1 ) {
      if ( i == 0 ) { when( log(j) === 0.U ) { molloc_idx(i) := j.U }  }
      else { when( log(j) === 0.U && j.U > molloc_idx(i-1) ) { molloc_idx(i) := j.U } }
    }
  }


  for ( i <- 0 until rnChn ) {

    when( io.rename(i).req.fire ) {
      val idx = io.rename(i).req.bits.rd0
      assert( log(molloc_idx(i)) === "b00".U )
      log_reg(molloc_idx(i)) := "b01".U //may be override by flush
      rename_ptr(idx) := molloc_idx(i) //may be override by flush
    }

    io.rename(i).req.ready := log.count( (x:UInt) => ( x === 0.U ) ) > i.U
    io.rename(i).rsp.rd0 := molloc_idx(i)

  }

  for ( i <- 0 until cm_chn ) {
    def m = cm_chn-1-i
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

trait RegFilesReadOP{ this:RegFilesReal =>

  io.rgLog := log

  for( i <- 0 until opc ) {
    io.rgRsp(i).valid    := RegNext( io.rgReq(i).valid, init = false.B )
    io.rgRsp(i).bits.phy := RegEnable( io.rgReq(i).bits,        io.rgReq(i).valid )
    io.rgRsp(i).bits.op  := RegEnable( files(io.rgReq(i).bits), io.rgReq(i).valid )
    when( io.rgRsp(i).valid ) {  assert( log(io.rgReq(i).bits) === "b11".U, "Assert Failed while reading operator, log is not ready!" ) }
  }
}


trait RegFilesWriteBack{ this: RegFilesReal =>
  for ( i <- 0 until wbc ) {
    when( io.exe_writeBack(i).fire ) {
      val idx = io.exe_writeBack(i).bits.rd0
      assert( log(idx) === "b01".U, "Assert Failed when writeback at chn" + i + ", log(" + idx + ")" )
      log_reg(idx) := "b11".U
      files_reg(idx) := io.exe_writeBack(i).bits.res
    }
    io.exe_writeBack(i).ready := true.B
  }
}

trait RegFilesCommit{ this: RegFilesReal =>


  val idx_pre = io.commit.map{ x => archit_ptr(x.raw) }

  for ( i <- 0 until cm_chn ) {
    def m = cm_chn-1-i

    io.commit(m).is_writeback := log(phy(m)) === "b11".U

    when( io.commit(m).is_MisPredict | io.commit(m).is_abort ) {
      /** clear all log to 0, except that archit_ptr point to, may be override */
      for ( j <- 0 until (regNum-1) ) yield {log_reg(j) := Mux( archit_ptr.exists( (x:UInt) => (x === j.U) ), log(j), "b00".U )}
    }
    when( io.commit(m).is_MisPredict | io.commit(m).is_comfirm ) {
      /** override the log(clear) */
      assert( io.commit(m).is_writeback )
      for ( j <- 0 until (regNum-1) ) yield {
        when(j.U === idx_pre(m) ) {log_reg(j) := 0.U} // the log, that used before commit, will be clear to 0
      }
      assert( log_reg(phy(m)) === "b11".U, "log_reg which going to commit to will be overrided to \"b11\" if there is an abort in-front." )
      log_reg(phy(m)) := "b11".U //the log, that going to use after commit should keep to be "b11"
    }

    when( io.commit(i).is_MisPredict | io.commit(i).is_comfirm ) {
      archit_ptr(raw(i)) := phy(i)
    }
  }
  

  archit_ptr.map{
    i => assert( log(i) === "b11".U, "Assert Failed, archit point to should be b11.U! i = "+i+"\n")
  }



}



class XRegFiles (dw: Int, rop_chn: Int, wb_chn: Int)(implicit p: Parameters) extends RegFilesReal(dw, rop_chn, wb_chn ) with RegFilesReName with RegFilesReadOP with RegFilesWriteBack with RegFilesCommit{

  for ( i <- 0 until rnChn ) {
    val idx1 = io.lookup(i).req.rs1
    val idx2 = io.lookup(i).req.rs2

    if ( i == 0) {
      io.lookup(i).rsp.rs1 := Mux( idx1 === 0.U, (regNum-1).U, rename_ptr(idx1) )
      io.lookup(i).rsp.rs2 := Mux( idx2 === 0.U, (regNum-1).U, rename_ptr(idx2) )
      io.lookup(i).rsp.rs3 := (regNum-1).U
    } else {
      io.lookup(i).rsp.rs1 := Mux( idx1 === 0.U, (regNum-1).U, rename_ptr(idx1) )
      io.lookup(i).rsp.rs2 := Mux( idx2 === 0.U, (regNum-1).U, rename_ptr(idx2) )
      io.lookup(i).rsp.rs3 := (regNum-1).U
      for ( j <- 0 until i ) {
        when( io.rename(j).req.valid && (io.rename(j).req.bits.rd0 === idx1) && (idx1 =/= 0.U) ) { io.lookup(i).rsp.rs1 := molloc_idx(j) }
        when( io.rename(j).req.valid && (io.rename(j).req.bits.rd0 === idx2) && (idx2 =/= 0.U) ) { io.lookup(i).rsp.rs2 := molloc_idx(j) }
      }
    }
  }



}

class FRegFiles (dw: Int, rop_chn: Int, wb_chn: Int)(implicit p: Parameters) extends RegFilesReal(dw, rop_chn, wb_chn ) with RegFilesReName with RegFilesReadOP with RegFilesWriteBack with RegFilesCommit{


  for ( i <- 0 until rnChn ) {
    val idx1 = io.lookup(i).req.rs1
    val idx2 = io.lookup(i).req.rs2
    val idx3 = io.lookup(i).req.rs3

    if ( i == 0) {
      io.lookup(i).rsp.rs1 := rename_ptr(idx1)
      io.lookup(i).rsp.rs2 := rename_ptr(idx2)
      io.lookup(i).rsp.rs3 := rename_ptr(idx3)
    } else {
      io.lookup(i).rsp.rs1 := rename_ptr(idx1)
      io.lookup(i).rsp.rs2 := rename_ptr(idx2)
      io.lookup(i).rsp.rs3 := rename_ptr(idx3) 
      for ( j <- 0 until i ) {
        when( io.rename(j).req.valid && (io.rename(j).req.bits.rd0 === idx1) ) { io.lookup(i).rsp.rs1 := molloc_idx(j) }
        when( io.rename(j).req.valid && (io.rename(j).req.bits.rd0 === idx2) ) { io.lookup(i).rsp.rs2 := molloc_idx(j) }
        when( io.rename(j).req.valid && (io.rename(j).req.bits.rd0 === idx3) ) { io.lookup(i).rsp.rs3 := molloc_idx(j) }
      }
    }
    assert( io.lookup(i).rsp.rs1 =/= (regNum-1).U )
    assert( io.lookup(i).rsp.rs2 =/= (regNum-1).U )
    assert( io.lookup(i).rsp.rs3 =/= (regNum-1).U )
  }


}


class FakeFRegFiles(dw: Int, rop_chn: Int=6, wb_chn: Int = 6)(implicit p: Parameters) extends RegFilesBase(dw, rop_chn, wb_chn ){

  for( i <- 0 until rnChn ) {
    io.lookup(i).rsp := 0.U.asTypeOf(new RS_PHY)
    io.rename(i).rsp := 0.U.asTypeOf(new RD_PHY)
    io.rename(i).req.ready := true.B
  }

  for( i <- 0 until regNum ) io.rgLog(i) := "b11".U 
  for( i <- 0 until rop_chn ) {
    io.rgRsp(i).valid := false.B
    io.rgRsp(i).bits.phy := DontCare
    io.rgRsp(i).bits.op := DontCare
  }

  for( i <- 0 until wb_chn ) {
    io.exe_writeBack(i).ready := true.B
    assert( ~io.exe_writeBack(i).valid )
  }

  for( i <- 0 until cm_chn ) {
    io.commit(i).is_writeback := false.B
  }

  io.diffReg := 0.U.asTypeOf(Vec(32, UInt(dw.W)))

}

