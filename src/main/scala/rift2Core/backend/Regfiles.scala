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
/**
  * rename channel is located in dpt-stage,
  * the raw-rs needs lookup the phy-rs num ( 1x, 2x, 3x raw-rs -> phy-rs ),
  * the rd should rename and malloc 1 new phy, 
  */
class dpt_rename_info(dp: Int) extends Bundle{
  // val raw = Input(new Register_source(32))
  // val phy = Output(new Register_source(dp))

  val rsp = Input(new Reg_phy(dp))
  val req = Decoupled(new Reg_raw)

  override def cloneType = ( new dpt_rename_info(dp:Int) ).asInstanceOf[this.type]
}

class iss_readOp_info(dp: Int) extends Bundle {
  val reg = Decoupled(new Register_source(dp))
  val dat = Input(new Operation_source)

  override def cloneType = ( new iss_readOp_info(dp:Int) ).asInstanceOf[this.type]
}


class RegFiles(dp: Int=64, rn_chn: Int = 2, rop_chn: Int=6, wb_chn: Int = 6, cmm_chn: Int = 2) extends Module{
  val io = IO( new Bundle{

    val dpt_rename = Vec( rn_chn, Flipped(new dpt_rename_info(dp) ))
    /** read operators based on idx, must success */
    val iss_readOp = Vec(rop_chn, Flipped( new iss_readOp_info(dp)) )
    /** writeBack request from exeUnit */
    val exe_writeBack = Vec(wb_chn, Flipped(new DecoupledIO(new WriteBack_info(64))))
    /** Commit request from commitUnit */
    val commit = Vec(cmm_chn, Flipped(Decoupled(new Info_commit_op(dp))))

    val diff_register = Output(new Info_abi_reg)
  })

  /**
    * dp-1 (63) files exist in this version,'
    * @note the file(dp) is assert to be Zero
    */
  val files_reg = RegInit( VecInit( Seq.fill(dp-1)(0.U(64.W)) ))
  val files = {
    val res = Wire( Vec(dp, UInt(64.W)) )
    for ( i <- 0 until dp-1 ) yield { res(i) := files_reg(i) }
    res(dp-1) := 0.U
    res
  }
  

  /**
    * dp-1 (63) log exist in this version,
    * @note the log(dp) is assert to be "b11".U
    */ 
  val log_reg = RegInit( VecInit( Seq.fill(dp-1)(0.U( 2.W)) ))
  val log = {
    val res = Wire( Vec(dp, UInt(2.W)) )
    for ( i <- 0 until dp-1 ) yield { res(i) := log_reg(i) }
    res(dp-1) := "b11".U
    res
  }

  /**
    * index that 32 renamed register-sources point to
    */
  val rename_ptr = RegInit( VecInit( for( i <- 0 until 32 ) yield {i.U} ) )

  /**
    * index that 32 commited register-sources point to
    */  
  val archit_ptr = RegInit( VecInit( for( i <- 0 until 32 ) yield {i.U} ) )

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
  //   VecInit(
  //   log.indexWhere( (x:UInt) => ( x === 0.U )),
  //   log.lastIndexWhere( (x:UInt) => ( x === 0.U ))
  // )

  for ( i <- 0 until rn_chn ) {
    val idx1 = io.dpt_rename(i).req.bits.rs1
    val idx2 = io.dpt_rename(i).req.bits.rs2
    val idx3 = io.dpt_rename(i).req.bits.rs3

    if ( i == 0) {
      io.dpt_rename(i).rsp.rs1 := Mux( idx1 === 0.U, 63.U, rename_ptr(idx1) )
      io.dpt_rename(i).rsp.rs2 := Mux( idx2 === 0.U, 63.U, rename_ptr(idx2) )
      io.dpt_rename(i).rsp.rs3 := Mux( idx3 === 0.U, 63.U, rename_ptr(idx3) )
    } else {
      io.dpt_rename(i).rsp.rs1 := Mux( idx1 === 0.U, 63.U, rename_ptr(idx1) )
      io.dpt_rename(i).rsp.rs2 := Mux( idx2 === 0.U, 63.U, rename_ptr(idx2) )
      io.dpt_rename(i).rsp.rs3 := Mux( idx3 === 0.U, 63.U, rename_ptr(idx3) ) 
      for ( j <- 0 until i ) {
        when( io.dpt_rename(j).req.bits.rd0 === idx1 ) { io.dpt_rename(i).rsp.rs1 := molloc_idx(j) }
        when( io.dpt_rename(j).req.bits.rd0 === idx2 ) { io.dpt_rename(i).rsp.rs2 := molloc_idx(j) }
        when( io.dpt_rename(j).req.bits.rd0 === idx3 ) { io.dpt_rename(i).rsp.rs3 := molloc_idx(j) }
      }
    }


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
      (log(idx1) === "b10".U | idx1 === 63.U ) &
      (log(idx2) === "b10".U | idx2 === 63.U ) &
      (log(idx3) === "b10".U | idx3 === 63.U ) 

  }

  for ( i <- 0 until wb_chn ) yield {
    when( io.exe_writeBack(i).fire ) {
      val idx = io.exe_writeBack(i).bits.rd0
      assert( log(idx) === "b01".U, "Assert Failed when writeback log(" + i + ")" )
      log_reg(idx) := "b10".U
      files_reg(idx) := io.exe_writeBack(i).bits.res
    }
    io.exe_writeBack(i).ready := true.B
  }

  {
    val raw = io.commit.map{ x => x.bits.raw }
    val phy = io.commit.map{ x => x.bits.phy }
    val idx_pre = io.commit.map{ x => archit_ptr(x.bits.raw) }

    for ( i <- 0 until cmm_chn ) {
      def m = cmm_chn-1-i

      io.commit(m).ready := log(phy(m)) === "b10".U


      when( io.commit(m).fire ) {
        when( io.commit(m).bits.is_abort ) {
          /** clear all log to 0, except that archit_ptr point to, may be override */
          for ( j <- 0 until dp-1 ) yield {log_reg(j) := Mux( archit_ptr.exists( (x:UInt) => (x === j.U) ), log(j), "b00".U )}
          for ( n <- 0 until m ) yield { assert( io.commit(n).valid & ~io.commit(n).bits.is_abort) }
        } .otherwise {
          /** override the log(clear) */
          for ( j <- 0 until dp-1 ) yield { when(j.U === idx_pre(m) ) {log_reg(j) := 0.U}  }
        }
      }

      when( io.commit(m).fire & ~io.commit(m).bits.is_abort) {
        archit_ptr(raw(m)) := phy(m)
      }

      when( io.commit(m).fire ) {
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





  io.diff_register.zero := files(archit_ptr(0))
  io.diff_register.ra   := files(archit_ptr(1))
  io.diff_register.sp   := files(archit_ptr(2))
  io.diff_register.gp   := files(archit_ptr(3))
  io.diff_register.tp   := files(archit_ptr(4))
  io.diff_register.t0   := files(archit_ptr(5))
  io.diff_register.t1   := files(archit_ptr(6))
  io.diff_register.t2   := files(archit_ptr(7))
  io.diff_register.s0   := files(archit_ptr(8))
  io.diff_register.s1   := files(archit_ptr(9))
  io.diff_register.a0   := files(archit_ptr(10))
  io.diff_register.a1   := files(archit_ptr(11))
  io.diff_register.a2   := files(archit_ptr(12))
  io.diff_register.a3   := files(archit_ptr(13))
  io.diff_register.a4   := files(archit_ptr(14))
  io.diff_register.a5   := files(archit_ptr(15))
  io.diff_register.a6   := files(archit_ptr(16))
  io.diff_register.a7   := files(archit_ptr(17))
  io.diff_register.s2   := files(archit_ptr(18))
  io.diff_register.s3   := files(archit_ptr(19))
  io.diff_register.s4   := files(archit_ptr(20))
  io.diff_register.s5   := files(archit_ptr(21))
  io.diff_register.s6   := files(archit_ptr(22))
  io.diff_register.s7   := files(archit_ptr(23))
  io.diff_register.s8   := files(archit_ptr(24))
  io.diff_register.s9   := files(archit_ptr(25))
  io.diff_register.s10  := files(archit_ptr(26))
  io.diff_register.s11  := files(archit_ptr(27))
  io.diff_register.t3   := files(archit_ptr(28))
  io.diff_register.t4   := files(archit_ptr(29))
  io.diff_register.t5   := files(archit_ptr(30))
  io.diff_register.t6   := files(archit_ptr(31))






}



