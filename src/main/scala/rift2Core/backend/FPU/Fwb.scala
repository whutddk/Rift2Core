/*
  Copyright (c) 2020 - 2021 Ruige Lee <wut.ruigeli@gmail.com>

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

package rift2Core.backend.fpu

import chisel3._
import chisel3.util._
import rift2Core.define._


class dpt_rn_info extends Bundle{
  val rs1_raw = Input( UInt(5.W) )
  val rs2_raw = Input( UInt(5.W) )
  val rs3_raw = Input( UInt(5.W) )

  val rs1_phy = Output( UInt(6.W) )
  val rs2_phy = Output( UInt(6.W) )
  val rs3_phy = Output( UInt(6.W) )

  val rd0_phy = Output( UInt(6.W) )
  val rd0_rn = DecoupledIO( UInt(5.W) )
}




class Fwb(dp: Int=64, pp: Int=2) extends Module{
  val io = IO( new Bundle{

    val dpt_rn = Vec( 2, new dpt_rn_info )

    /** are operators written back */
    val is_op_rsl = Vec(pp, Flipped(new DecoupledIO(new Reg_phy )))
    /** read operators based on idx, must success */
    val fpu_iss_fwb = Vec(2, Flipped(new fpu_iss_fwb_info) )

    val fpu_exe_fwb = Vec(2, Flipped(new ValidIO(new Exe_fwb_info)))

    val fpu_cmm_fwb = Vec(2, Flipped(Valid(new Info_commit_op)))

  })

  /**
    * dp-1 (63) files exist in this version, the file(dp) is assert to be Zero
    */
  val files = RegInit( VecInit( Seq.fill(dp-1)(0.U(64.W)) ))

  /**
  * dp-1 (63) log exist in this version, the log(dp) is assert to be "b11".U
  */ 
  val log   = RegInit( VecInit( Seq.fill(dp-1)(0.U( 2.W)) ))

  
  val rename_ptr = RegInit( VecInit( for( i <- 0 until 32 ) yield {i} ) )
  val archit_ptr = RegInit( VecInit( for( i <- 0 until 32 ) yield {i} ) )


  val molloc_idx = VecInit(
    log.indexWhere( (x:UInt) => ( x === 0.U )),
    log.lastIndexWhere( (x:UInt) => ( x === 0.U ))
  )

  for ( i <- 0 until 2 ) yield {
    val idx1 = io.dpt_rn(i).rs1_raw
    val idx2 = io.dpt_rn(i).rs2_raw
    val idx3 = io.dpt_rn(i).rs3_raw

    io.dpt_rn(i).rs1_phy := rename_ptr(idx1)
    io.dpt_rn(i).rs2_phy := rename_ptr(idx2)
    io.dpt_rn(i).rs3_phy := rename_ptr(idx3)

    when( io.dpt_rn(i).fire ) {
      val idx = io.dpt_rn(i).rd0_raw
      assert( log(idx) === "b00".U )
      log(idx) := "b01".U //may be override by flush
      rename_ptr(idx) := molloc_idx(i) //may be override by flush
    }

  }


  io.dpt_rn(0).rd0_phy := molloc_idx(0)
  io.dpt_rn(1).rd0_phy := molloc_idx(1)
  io.dpt_rn(0).ready := log.count( (x:UInt) => ( x === 0.U ) ) > 0.U
  io.dpt_rn(1).ready := log.count( (x:UInt) => ( x === 0.U ) ) > 1.U

  for ( i <- 0 until 2 ) yield {
    when( io.fpu_exe_fwb(i).valid ) {
      val idx = io.fpu_exe_fwb(i).bits.rd0_phy
      assert( log(idx) === "b10".U )
      files(idx) := io.fpu_exe_fwb(i).bits.res
    }
  }

  for ( i <- 0 until pp ) yield {
    val idx1 = io.is_op_rsl(i).bits.rs1_raw
    val idx2 = io.is_op_rsl(i).bits.rs2_raw
    val idx3 = io.is_op_rsl(i).bits.rs3_raw

    io.is_op_rsl(i).ready :=
      io.is_op_rsl(i).valid & 
      (log(idx1) === "b10".U | idx1 === 63.U ) &
      (log(idx2) === "b10".U | idx2 === 63.U ) &
      (log(idx3) === "b10".U | idx3 === 63.U )
  }

  for ( i <- 0 until 2 ) yield {
    val idx1 = io.fpu_iss_fwb(i).rs1
    val idx2 = io.fpu_iss_fwb(i).rs2
    val idx3 = io.fpu_iss_fwb(i).rs3

    io.fpu_iss_fwb(i).op1 := Mux(idx1 === 63.U, 0.U, files(idx1))
    io.fpu_iss_fwb(i).op2 := Mux(idx2 === 63.U, 0.U, files(idx2))
    io.fpu_iss_fwb(i).op3 := Mux(idx3 === 63.U, 0.U, files(idx3))
  }












 


  {
    val raw = io.fpu_cmm_fwb.map{ x => x.bits.raw }
    val phy = io.fpu_cmm_fwb.map{ x => x.bits.phy }
    val idx_pre = io.fpu_cmm_fwb.map{ x => archit_ptr(x.bits.raw) }

    for ( i <- 0 until dp-1 ) yield {


      when( io.fpu_cmm_fwb(0).valid ) {
        when( io.fpu_cmm_fwb(0).is_abort ) {
          log(i) := Mux( archit_ptr.exist( (x:UInt) => (x === i.U) ), log(i), "b00".U  )
          for ( j <- 0 until 32 ) yield { rename_ptr(j) := archit_ptr(j) }
        } .otherwise {
          log(i) := Mux( i.U === idx_pre(0), 0.U, log(i) )
          archit_ptr(raw(0)) := phy(0)
        }

      } 
      when( io.fpu_cmm_fwb(1).valid ) {
        when( io.fpu_cmm_fwb(1).is_abort & i.U =/= idx_pre(0) ) {
          log(i) := Mux( archit_ptr.exist( (x:UInt) => (x === i.U) ), log(i), "b00".U  )
          for ( j <- 0 until 32 ) yield {
            when( j.U === raw(0) ) { rename_ptr(j) := phy(0) }
            .otherwise { rename_ptr(j) := archit_ptr(j) }
          }
        } .otherwise {
          log(i) := Mux( i.U =/= idx_pre(1), 0.U, log(i) )
          archit_ptr(raw(1)) := phy(1)
        }
      }

    }
  }









}



