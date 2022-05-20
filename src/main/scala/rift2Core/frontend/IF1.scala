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

package rift2Core.frontend

import chisel3._
import chisel3.util._
import rift2Core.define._
import chipsalliance.rocketchip.config.Parameters

/**
  * instract fetch stage 1, generate pc
  */
abstract class IF1Base()(implicit p: Parameters) extends IFetchModule {
  val io = IO(new Bundle{


    val if4Redirect = Flipped(Valid(new IF4_Redirect_Bundle))
    val cmmRedirect = Flipped(Valid(new Commit_Redirect_Bundle))


    val pc_gen = Decoupled(new IF1_Bundle)

    val jcmm_update = Flipped(Valid(new Jump_CTarget_Bundle))
    val bcmm_update = Flipped(Valid(new Branch_CTarget_Bundle))
  })

  // val pc_dnxt = Wire(UInt(64.W))
  val pc_qout = RegInit("h80000000".U(64.W))

}

trait IF1Predict { this: IF1Base => 
  val uBTB = Module(new uBTB)

  uBTB.io.req.pc := pc_qout

  io.pc_gen.bits.isRedirect := uBTB.io.resp.isRedirect
  io.pc_gen.bits.isActive := uBTB.io.resp.isActive
  io.pc_gen.bits.target := uBTB.io.resp.target

  uBTB.io.update.valid := io.bcmm_update.fire | io.jcmm_update.fire

  uBTB.io.update.bits.target :=
    Mux( io.bcmm_update.fire, Mux( io.bcmm_update.bits.isMisPredict, io.bcmm_update.bits.revertTarget, io.bcmm_update.bits.predicTarget),
      Mux( io.jcmm_update.fire, io.jcmm_update.bits.finalTarget, 0.U ) )

  uBTB.io.update.bits.pc     :=
    Mux( io.bcmm_update.fire, io.bcmm_update.bits.pc, 
      Mux( io.jcmm_update.fire, io.jcmm_update.bits.pc, 0.U ) )

  uBTB.io.update.bits.isTaken := 
    ( io.jcmm_update.fire ) |
    ( io.bcmm_update.fire & io.bcmm_update.bits.isFinalTaken )

  uBTB.io.if4Redirect := io.if4Redirect

}


class IF1()(implicit p: Parameters) extends IF1Base with IF1Predict{
  val any_reset = reset.asBool


  when( any_reset ) { pc_qout := "h80000000".U }
  .elsewhen( io.cmmRedirect.fire ) { pc_qout := io.cmmRedirect.bits.pc }
  .elsewhen( io.if4Redirect.fire ) { pc_qout := io.if4Redirect.bits.target }
  .elsewhen( io.pc_gen.fire ) {
    when( uBTB.io.resp.isRedirect.reduce(_|_) ){
      pc_qout := uBTB.io.resp.target
    } .otherwise {
      pc_qout := (pc_qout + 16.U) >> 4 << 4        
    }

  }
  io.pc_gen.valid   := true.B
  io.pc_gen.bits.pc := pc_qout
}
