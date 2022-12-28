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


class Info_commit_op(arc: Int, dp: Int)(implicit p: Parameters) extends RiftBundle{
  val is_comfirm = Output(Bool())
  val is_MisPredict = Output(Bool())
  val is_abort   = Output(Bool())
  val raw        = Output(UInt(( log2Ceil(arc)       ).W))
  val phy        = Output(UInt(( log2Ceil(maxRegNum) ).W))
  val toX        = Output(Bool())
  val toF        = Output(Bool())
  val toV        = Output(Bool())
  val is_writeback = Input(Bool())
}

abstract class RegFilesBase(dw: Int, dp: Int, arc: Int, rnc: Int, rop: Int, wbc: Int, cmm: Int )(implicit p: Parameters) extends RiftModule{
  val io = IO( new Bundle{

    val lookup = Vec( rnc, Flipped(new Lookup_Bundle) )
    val rename = Vec( rnc, Flipped(new Rename_Bundle) )

    /** read operators based on idx, must success */
    val rgLog = Output( Vec(dp, UInt(2.W)) )
    val rgReq = Flipped(Vec( rop, Valid( UInt((log2Ceil(dp)).W) ) ))
    val rgRsp =         Vec( rop, Valid(new ReadOp_Rsp_Bundle(dw) ))


    /** writeBack request from exeUnit */
    val exe_writeBack = Vec(wbc, Flipped(new DecoupledIO(new WriteBack_info(dw))))
    /** Commit request from commitUnit */
    val commit = Vec(cmm, Flipped(new Info_commit_op(arc, dp)))

    val diffReg = Output(Vec(arc, UInt(dw.W)))
  })
}

abstract class RegFilesReal(val dw: Int, val dp: Int, val arc: Int, val rnc: Int, val rop: Int, val wbc: Int, val cmm: Int)(implicit p: Parameters) extends RegFilesBase(dw, dp, arc, rnc, rop, wbc, cmm){

  val raw = io.commit.map{ x => x.raw }
  val phy = io.commit.map{ x => x.phy }

  /**
    * there are dp-1 log,
    * @note the log(dp) is assert to be "b11".U
    */ 
  val log_reg = 
    RegInit( VecInit( Seq.fill(arc+1)("b11".U(2.W) ) ++ Seq.fill(dp-arc)(0.U(2.W) )))

  val log = {
    val res = Wire( Vec(dp, UInt(2.W)) )
    for ( i <- 1 until dp ) yield { res(i) := log_reg(i) }
    res(0) := "b11".U
    res
  }
  assert( log(0) === "b11".U )

  /**
    * there are dp-1 files,
    * @note the file(dp) is assert to be Zero
    */
  val files_reg = RegInit( VecInit( Seq.fill(dp)(0.U(dw.W)) ))
  val files = {
    val res = Wire( Vec(dp, UInt(dw.W)) )
    for ( i <- 1 until dp ) yield { res(i) := files_reg(i) }
    res(0) := 0.U
    res
  }

  /** index that arc(32) renamed register-sources point to, Avoid pointing to file-0*/
  val rename_ptr = RegInit( VecInit( for( i <- 0 until arc ) yield {(i+1).U(log2Ceil(dp).W)} ) )

  /** index that arc(32) commited register-sources point to, Avoid pointing to file-0 */  
  val archit_ptr = RegInit( VecInit( for( i <- 0 until arc ) yield {(i+1).U(log2Ceil(dp).W)} ) )

  for ( i <- 0 until arc ) yield {
    io.diffReg(i) := files(archit_ptr(i))
  }


}


trait RegFilesReName{ this: RegFilesReal => 


  /**
    * finding out the first Free-phy-register
    */ 
  val mollocIdx = Wire(Vec(rnc, UInt((log2Ceil(dp)).W)))
  for ( i <- 0 until rnc ) {
    mollocIdx(i) := 0.U
    for ( j <- (dp-1) to 1 by -1 ) {
      if ( i == 0 ) { when( log(j) === 0.U ) { mollocIdx(i) := j.U }  }
      else { when( log(j) === 0.U && j.U > mollocIdx(i-1) ) { mollocIdx(i) := j.U } }
    }
  }


  for ( i <- 0 until rnc ) {

    when( io.rename(i).req.fire ) {
      val idx = io.rename(i).req.bits.rd0
      assert( log(mollocIdx(i)) === "b00".U )
      log_reg(mollocIdx(i)) := "b01".U //may be override by flush
      rename_ptr(idx) := mollocIdx(i) //may be override by flush
    }

    io.rename(i).req.ready := log.count( (x:UInt) => ( x === 0.U ) ) > i.U
    io.rename(i).rsp.rd0 := mollocIdx(i)

  }

  for ( i <- 0 until cmm ) {
    def m = cmm-1-i
    when ( io.commit(m).is_MisPredict | io.commit(m).is_abort ) {
      for ( j <- 0 until arc ) yield {
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

trait XRegFilesLookup{ this: RegFilesReal =>
  for ( i <- 0 until rnc ) {
    val idx1 = io.lookup(i).req.rs1
    val idx2 = io.lookup(i).req.rs2

    if ( i == 0) {
      io.lookup(i).rsp.rs1 := Mux( idx1 === 0.U, 0.U, rename_ptr(idx1) )
      io.lookup(i).rsp.rs2 := Mux( idx2 === 0.U, 0.U, rename_ptr(idx2) )
      io.lookup(i).rsp.rs3 := 0.U
      io.lookup(i).rsp.rs4 := 0.U
    } else {
      io.lookup(i).rsp.rs1 := Mux( idx1 === 0.U, 0.U, rename_ptr(idx1) )
      io.lookup(i).rsp.rs2 := Mux( idx2 === 0.U, 0.U, rename_ptr(idx2) )
      io.lookup(i).rsp.rs3 := 0.U
      io.lookup(i).rsp.rs4 := 0.U
      for ( j <- 0 until i ) {
        when( io.rename(j).req.valid && (io.rename(j).req.bits.rd0 === idx1) && (idx1 =/= 0.U) ) { io.lookup(i).rsp.rs1 := mollocIdx(j) }
        when( io.rename(j).req.valid && (io.rename(j).req.bits.rd0 === idx2) && (idx2 =/= 0.U) ) { io.lookup(i).rsp.rs2 := mollocIdx(j) }
      }
    }
  }
}

trait FRegFilesLookup{ this: RegFilesReal =>
  for ( i <- 0 until rnc ) {
    val idx1 = io.lookup(i).req.rs1
    val idx2 = io.lookup(i).req.rs2
    val idx3 = io.lookup(i).req.rs3

    if ( i == 0) {
      io.lookup(i).rsp.rs1 := rename_ptr(idx1)
      io.lookup(i).rsp.rs2 := rename_ptr(idx2)
      io.lookup(i).rsp.rs3 := rename_ptr(idx3)
      io.lookup(i).rsp.rs4 := 0.U
    } else {
      io.lookup(i).rsp.rs1 := rename_ptr(idx1)
      io.lookup(i).rsp.rs2 := rename_ptr(idx2)
      io.lookup(i).rsp.rs3 := rename_ptr(idx3) 
      io.lookup(i).rsp.rs4 := 0.U
      for ( j <- 0 until i ) {
        when( io.rename(j).req.valid && (io.rename(j).req.bits.rd0 === idx1) ) { io.lookup(i).rsp.rs1 := mollocIdx(j) }
        when( io.rename(j).req.valid && (io.rename(j).req.bits.rd0 === idx2) ) { io.lookup(i).rsp.rs2 := mollocIdx(j) }
        when( io.rename(j).req.valid && (io.rename(j).req.bits.rd0 === idx3) ) { io.lookup(i).rsp.rs3 := mollocIdx(j) }
      }
    }
    when( io.rename(i).req.fire ) {
      assert( io.lookup(i).rsp.rs1 =/= 0.U )
      assert( io.lookup(i).rsp.rs2 =/= 0.U )
      assert( io.lookup(i).rsp.rs3 =/= 0.U )      
    }

  }
}


trait RegFilesReadOP{ this:RegFilesReal =>

  io.rgLog := log

  for( i <- 0 until rop ) {
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

  for ( i <- 0 until cmm ) {
    def m = cmm-1-i

    io.commit(m).is_writeback := log(phy(m)) === "b11".U

    when( io.commit(m).is_MisPredict | io.commit(m).is_abort ) {
      /** clear all log to 0, except that archit_ptr point to, may be override */
      for ( j <- 1 until dp ) yield {log_reg(j) := Mux( archit_ptr.exists( (x:UInt) => (x === j.U) ), log(j), "b00".U )}
    }
    when( io.commit(m).is_MisPredict | io.commit(m).is_comfirm ) {
      /** override the log(clear) */
      assert( io.commit(m).is_writeback )
      for ( j <- 1 until dp ) yield {
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



class XRegFiles (dw: Int, dp: Int, rnc: Int, rop: Int, wbc: Int, cmm: Int)(implicit p: Parameters) extends RegFilesReal(dw, dp, arc = 32, rnc, rop, wbc, cmm)
  with RegFilesReName
  with XRegFilesLookup
  with RegFilesReadOP
  with RegFilesWriteBack
  with RegFilesCommit{

}

class FRegFiles (dw: Int, dp: Int, rnc: Int, rop: Int, wbc: Int, cmm: Int)(implicit p: Parameters) extends RegFilesReal(dw, dp, arc = 32, rnc, rop, wbc, cmm)
  with RegFilesReName
  with FRegFilesLookup
  with RegFilesReadOP
  with RegFilesWriteBack
  with RegFilesCommit{


}


class FakeFRegFiles(dw: Int, dp: Int, rnc: Int, rop: Int = 6, wbc: Int = 6, cmm: Int)(implicit p: Parameters) extends RegFilesBase(dw, dp, arc = 32, rnc, rop, wbc, cmm){

  for( i <- 0 until rnc ) {
    io.lookup(i).rsp := 0.U.asTypeOf(new RS_PHY)
    io.rename(i).rsp := 0.U.asTypeOf(new RD_PHY)
    io.rename(i).req.ready := true.B
  }

  for( i <- 0 until dp ) io.rgLog(i) := "b11".U 
  for( i <- 0 until rop ) {
    io.rgRsp(i).valid := false.B
    io.rgRsp(i).bits.phy := DontCare
    io.rgRsp(i).bits.op := DontCare
  }

  for( i <- 0 until wbc ) {
    io.exe_writeBack(i).ready := true.B
    assert( ~io.exe_writeBack(i).valid )
  }

  for( i <- 0 until cmm ) {
    io.commit(i).is_writeback := false.B
  }

  io.diffReg := 0.U.asTypeOf(Vec(32, UInt(dw.W)))

}

