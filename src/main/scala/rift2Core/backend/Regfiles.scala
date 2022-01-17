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

/**
  * rename channel is located in dpt-stage,
  * the raw-rs needs lookup the phy-rs num ( 1x, 2x, 3x raw-rs -> phy-rs ),
  * the rd should rename and malloc 1 new phy, 
  */
class dpt_rename_info(dp: Int) extends Bundle{
  val raw = Input(new Register_source(32))
  val phy = Output(new Register_source(dp))

  val rsp = Output(new Register_dstntn(dp))
  val req = Decoupled(new Register_dstntn(32))
}

class iss_readOp_info(dp: Int) extends Bundle {
  val reg = DecoupledIO(new Register_source(dp))
  val dat = Output(new Operation_source)
}


class RegFiles(dp: Int=64, rn_chn: Int = 2, rop_chn: Int=2, wb_chn: Int = 6, cmm_chn: Int = 2) extends Module{
  val io = IO( new Bundle{

    val dpt_rename = Vec( rn_chn, new dpt_rename_info(dp) )
    /** read operators based on idx, must success */
    val iss_readOp = Vec(rop_chn, Flipped( new iss_readOp_info) )
    /** writeBack request from exeUnit */
    val exe_writeBack = Vec(wb_chn, Flipped(new DecoupledIO(new WriteBack_info)))
    /** Commit request from commitUnit */
    val commit = Vec(cmm_chn, Flipped(Valid(new Info_commit_op(dp))))

  })

  /**
    * dp-1 (63) files exist in this version,'
    * @note the file(dp) is assert to be Zero
    */
  val files = RegInit( VecInit( Seq.fill(dp-1)(0.U(64.W)) ))

  /**
    * dp-1 (63) log exist in this version,
    * @note the log(dp) is assert to be "b11".U
    */ 
  val log   = RegInit( VecInit( Seq.fill(dp-1)(0.U( 2.W)) ))

  /**
    * index that 32 renamed register-sources point to
    */
  val rename_ptr = RegInit( VecInit( for( i <- 0 until 32 ) yield {i} ) )

  /**
    * index that 32 commited register-sources point to
    */  
  val archit_ptr = RegInit( VecInit( for( i <- 0 until 32 ) yield {i} ) )

  /**
    * finding out the first Free-phy-register
    */ 
  val molloc_idx = Wire(Vec(rn_chn, UInt((log2Ceil(dp)).W)))
  for ( i <- 0 until rn_chn ) {
    molloc_idx(i) := 0.U
    for ( j <- dp-1 to 0 by -1 ) {
      if ( i == 0 ) { when( log(j) === 0.U ) { molloc_idx(i) := j.U }  }
      else { when( log(j) === 0.U & j.U > molloc_idx(i-1) ) { molloc_idx(i) := j.U } }
    }
  }
  //   VecInit(
  //   log.indexWhere( (x:UInt) => ( x === 0.U )),
  //   log.lastIndexWhere( (x:UInt) => ( x === 0.U ))
  // )

  for ( i <- 0 until rn_chn ) yield {
    val idx1 = io.dpt_rename(i).raw.rs1
    val idx2 = io.dpt_rename(i).raw.rs2
    val idx3 = io.dpt_rename(i).raw.rs3

    io.dpt_rename(i).phy.rs1 := rename_ptr(idx1)
    io.dpt_rename(i).phy.rs2 := rename_ptr(idx2)
    io.dpt_rename(i).phy.rs3 := rename_ptr(idx3)

    when( io.dpt_rename(i).req.fire ) {
      val idx = io.dpt_rename(i).req.rd0
      assert( log(idx) === "b00".U )
      log(idx) := "b01".U //may be override by flush
      rename_ptr(idx) := molloc_idx(i) //may be override by flush
    }

    io.dpt_rename(i).ready := log.count( (x:UInt) => ( x === 0.U ) ) > i.U
    io.dpt_rename(i).rsp.rd := molloc_idx(i)

  }


  for ( i <- 0 until rop_chn ) yield {
    val idx1 = io.iss_readOp(i).reg.rs1
    val idx2 = io.iss_readOp(i).reg.rs2
    val idx3 = io.iss_readOp(i).reg.rs3

    when( io.iss_readOp(i).fire ) {
      io.iss_readOp(i).dat.op1 := Mux(idx1 === 63.U, 0.U, files(idx1))
      io.iss_readOp(i).dat.op2 := Mux(idx2 === 63.U, 0.U, files(idx2))
      io.iss_readOp(i).dat.op3 := Mux(idx3 === 63.U, 0.U, files(idx3))
    } .otherwise {
      io.iss_readOp(i).dat.op1 := 0.U
      io.iss_readOp(i).dat.op2 := 0.U
      io.iss_readOp(i).dat.op3 := 0.U
    }
    io.iss_readOp(i).ready :=
      (log(idx1) === "b10".U | idx1 === 63.U ) &
      (log(idx2) === "b10".U | idx2 === 63.U ) &
      (log(idx3) === "b10".U | idx3 === 63.U ) 

  }

  for ( i <- 0 until wb_chn ) yield {
    when( io.exe_writeBack(i).valid ) {
      val idx = io.exe_writeBack(i).bits.dest.rd
      assert( log(idx) === "b10".U )
      files(idx) := io.exe_writeBack(i).bits.res
      io.exe_writeBack(i).raedy := true.B
    }
  }

  {
    val raw = io.fpu_cmm_fwb.map{ x => x.bits.raw }
    val phy = io.fpu_cmm_fwb.map{ x => x.bits.phy }
    val idx_pre = io.fpu_cmm_fwb.map{ x => archit_ptr(x.bits.raw) }

    for ( i <- 0 until cmm_chn ) {
      def m = cmm_chn-1-i
      when( io.commit(m).valid ) {
        when( io.commit(m).bits.is_abort ) {
          /** clear all log to 0, except that archit_ptr point to, may be override */
          for ( j <- 0 unitl dp-1 ) yield {log(j) := Mux( archit_ptr.exist( (x:UInt) => (x === j.U) ), log(j), "b00".U )}
          for ( n <- 0 until m ) yield { assert( io.commit(n).valid & ~io.commit(n).bits.is_abort) }
        } .otherwise {
          /** override the log(clear) */
          for ( j <- 0 unitl dp-1 ) yield { log(j) := Mux( j.U === idx_pre(m), 0.U, log(j) ) }
        }
      }

      when( io.commit(m).valid & ~io.commit(m).bits.is_abort) {
        archit_ptr(raw(m)) := phy(m)
      }

      when( io.commit(m).valid ) {
        when (io.commit(m).bits.is_abort) {
          for ( j <- 0 until 32 ) yield {
            rename_ptr(j) := archit_ptr(j)
            for ( n <- 0 until m ) {
              when( j.U === raw(n) ) { rename_ptr(j) := phy(n) }
            }

          }
        }
      }
    }
  }









}



