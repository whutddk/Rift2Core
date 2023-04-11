/*
  Copyright (c) 2020 - 2023 Wuhan University of Technology <295054118@whut.edu.cn>

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
import org.chipsalliance.cde.config._
import base._

/**
  * instract fetch stage 1, generate pc
  */
abstract class IF1Base()(implicit p: Parameters) extends IFetchModule {

  class IF1IO extends Bundle{
    val if4Redirect = Flipped(Valid(new IF4_Redirect_Bundle))
    val cmmRedirect = Flipped(Valid(new Commit_Redirect_Bundle))


    val pc_gen = Decoupled(new IF1_Bundle)

    val jcmm_update = Flipped(Valid(new Jump_CTarget_Bundle))
    val bcmm_update = Flipped(Valid(new Branch_CTarget_Bundle))    
  }

  val io: IF1IO = IO(new IF1IO)

  val pc_qout = RegInit("h80000000".U(64.W))

}

trait IF1uBTB { this: IF1Base => 
  val uBTB = Module(new uBTB)

  uBTB.io.req.pc := pc_qout

  io.pc_gen.bits.isRedirect := uBTB.io.resp.isRedirect
  io.pc_gen.bits.isActive := uBTB.io.resp.isActive
  io.pc_gen.bits.target := uBTB.io.resp.target

  uBTB.io.update.valid := io.bcmm_update.fire | io.jcmm_update.fire

  uBTB.io.update.bits.target :=
    Mux( io.bcmm_update.fire, io.bcmm_update.bits.finalTarget,
      Mux( io.jcmm_update.fire, io.jcmm_update.bits.finalTarget, 0.U ) )

  uBTB.io.update.bits.pc     :=
    Mux( io.bcmm_update.fire, io.bcmm_update.bits.pc, 
      Mux( io.jcmm_update.fire, io.jcmm_update.bits.pc, 0.U ) )

  uBTB.io.update.bits.isTaken := 
    ( io.jcmm_update.fire ) |
    ( io.bcmm_update.fire & io.bcmm_update.bits.isFinalTaken )

  uBTB.io.if4Redirect := io.if4Redirect

}

trait IF1NuBTB{ this: IF1Base =>
  val uBTB = Module(new FakeuBTB)

  io.pc_gen.bits.isRedirect := uBTB.io.resp.isRedirect
  io.pc_gen.bits.isActive := uBTB.io.resp.isActive
  io.pc_gen.bits.target := uBTB.io.resp.target

  uBTB.io.req.pc := 0.U
  uBTB.io.update.valid := false.B
  uBTB.io.update.bits.target := 0.U
  uBTB.io.update.bits.pc     := 0.U
  uBTB.io.update.bits.isTaken := false.B
  uBTB.io.if4Redirect.valid := false.B
  uBTB.io.if4Redirect.bits  := 0.U.asTypeOf(new IF4_Redirect_Bundle)
}


class IF1Predict()(implicit p: Parameters) extends IF1Base with IF1uBTB {
  val any_reset = reset.asBool


  when( any_reset ) { pc_qout := "h80000000".U }
  .elsewhen( io.cmmRedirect.fire ) { pc_qout := io.cmmRedirect.bits.pc }
  .elsewhen( io.if4Redirect.fire ) { pc_qout := extVaddr(io.if4Redirect.bits.target, vlen) }
  .elsewhen( io.pc_gen.fire ) {
    when( uBTB.io.resp.isRedirect.reduce(_|_) ){
      pc_qout :=  extVaddr(uBTB.io.resp.target, vlen)
    } .otherwise {
      pc_qout := (pc_qout + (1.U << log2Ceil(ftChn*16/8))   ) >> (log2Ceil(ftChn*16/8)) << (log2Ceil(ftChn*16/8))
    }

  }
  io.pc_gen.valid   := true.B
  io.pc_gen.bits.pc := pc_qout
}

class IF1NPredict()(implicit p: Parameters) extends IF1Base with IF1NuBTB {
  val any_reset = reset.asBool


  when( any_reset ) { pc_qout := "h80000000".U }
  .elsewhen( io.cmmRedirect.fire ) { pc_qout := io.cmmRedirect.bits.pc }
  .elsewhen( io.if4Redirect.fire ) { pc_qout := extVaddr(io.if4Redirect.bits.target, vlen) }
  .elsewhen( io.pc_gen.fire ) {
    pc_qout := (pc_qout + (1.U << log2Ceil(ftChn*16/8))   ) >> (log2Ceil(ftChn*16/8)) << (log2Ceil(ftChn*16/8))
  }
  io.pc_gen.valid   := true.B
  io.pc_gen.bits.pc := pc_qout
}
