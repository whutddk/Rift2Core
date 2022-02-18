



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
import base._

import rift2Core.define._
import rift2Core.diff._

class WriteBack(dp: Int=64, rn_chn: Int = 2, rop_chn: Int=6, wb_chn: Int=4, cmm_chn: Int = 2) extends Module {
  val io = IO(new Bundle{
    val dpt_Xlookup = Vec( rn_chn, Flipped(new dpt_lookup_info(dp)) )
    val dpt_Flookup = Vec( rn_chn, Flipped(new dpt_lookup_info(dp)) )
    val dpt_Xrename = Vec( rn_chn, Flipped(new dpt_rename_info(dp)) )
    val dpt_Frename = Vec( rn_chn, Flipped(new dpt_rename_info(dp)) )


    val ooo_readOp  = Vec(2, Flipped( new iss_readOp_info(dp)))
    val bru_readOp  = Flipped( new iss_readOp_info(dp))
    val csr_readOp  = Flipped( new iss_readOp_info(dp))
    val lsu_readXOp  = Flipped( new iss_readOp_info(dp))
    val lsu_readFOp  = Flipped( new iss_readOp_info(dp))
    val fpu_readXOp  = Flipped( new iss_readOp_info(dp))
    val fpu_readFOp  = Flipped( new iss_readOp_info(dp))

    val alu_iWriteBack = Flipped(new DecoupledIO(new WriteBack_info(dp)))
    val bru_iWriteBack = Flipped(new DecoupledIO(new WriteBack_info(dp)))
    val csr_iWriteBack = Flipped(new DecoupledIO(new WriteBack_info(dp)))
    val mem_iWriteBack = Flipped(new DecoupledIO(new WriteBack_info(dp)))
    val mul_iWriteBack = Flipped(new DecoupledIO(new WriteBack_info(dp)))
    val fpu_iWriteBack = Flipped(new DecoupledIO(new WriteBack_info(dp)))

    val mem_fWriteBack = Flipped(new DecoupledIO(new WriteBack_info(dp)))
    val fpu_fWriteBack = Flipped(new DecoupledIO(new WriteBack_info(dp)))

    val commit = Vec(cmm_chn, Flipped(Decoupled(new Info_commit_op(dp))))

    val diffXReg = Output(Vec(32, UInt(64.W)))
    val diffFReg = Output(Vec(32, UInt(64.W)))
  })


  val iReg = Module(new RegFiles(dp, rn_chn, rop_chn, wb_chn, cmm_chn))
  val fReg = Module(new RegFiles(dp, rn_chn, rop_chn=2, wb_chn=2, cmm_chn))

  {
    iReg.io.dpt_rename.req.valid := false.B
    iReg.io.dpt_rename.req.bits := 0.U.asTypeOf(Reg_raw)
    fReg.io.dpt_rename.req.valid := false.B
    fReg.io.dpt_rename.req.bits := 0.U.asTypeOf(Reg_raw)
    io.dpt_rename.req.ready := false.B
    io.dpt_rename.rsp := 0.U.asTypeOf(Reg_phy(64))
    when( io.dpt_rename.toX === true.B ) {iReg.io.dpt_rename <> io.dpt_rename}
    .elsewhen( io.dpt_rename.toF === true.B ) {fReg.io.dpt_rename <> io.dpt_rename}    
  }

  {
    for ( i <- 0 until cmm_chn ) yield {
      iReg.io.commit(i).valid := false.B
      iReg.io.commit(i).bits  := 0.U.asTypeOf(Info_commit_op(dp))
      fReg.io.commit(i).valid := false.B
      fReg.io.commit(i).bits  := 0.U.asTypeOf(Info_commit_op(dp))
      io.commit(i).ready := false.B

      when( io.commit.bits.toX === true.B ) {iReg.io.commit <> io.commit}
      .elsewhen( io.commit.bits.toF === true.B ) {fReg.io.commit <> io.commit}
    }
  }


  iReg.io.diffReg <> io.diffXReg
  fReg.io.diffReg <> io.diffFReg


  iReg.io.iss_readOp(0) <> io.ooo_readOp(0)
  iReg.io.iss_readOp(1) <> io.ooo_readOp(1)
  iReg.io.iss_readOp(2) <> io.bru_readOp
  iReg.io.iss_readOp(3) <> io.csr_readOp
  iReg.io.iss_readOp(4) <> io.lsu_readXOp
  iReg.io.iss_readOp(5) <> io.fpu_readXOp

  fReg.io.iss_readOp(0) <> io.lsu_readFOp
  fReg.io.iss_readOp(1) <> io.fpu_readFOp


  val iwriteBack_arb = {
    val mdl = Module(new XArbiter(new WriteBack_info(dp), in = 6, out = wb_chn))
    mdl.io.enq(0) <> io.alu_iWriteBack
    mdl.io.enq(1) <> io.bru_iWriteBack
    mdl.io.enq(2) <> io.csr_iWriteBack
    mdl.io.enq(3) <> io.mem_iWriteBack
    mdl.io.enq(4) <> io.mul_iWriteBack
    mdl.io.enq(5) <> io.fpu_iWriteBack
    mdl.io.deq <> iReg.io.exe_writeBack

    mdl
  }

  fReg.io.exe_writeBack(0) <> io.mem_fWriteBack
  fReg.io.exe_writeBack(1) <> io.fpu_fWriteBack

  // {
  //   iwriteBack_arb.io.enq(3).valid := false.B
  //   iwriteBack_arb.io.enq(3).bits  := 0.U.asTypeOf(WriteBack_info(dp))
  //   fReg.io.exe_writeBack(0).valid := false.B
  //   fReg.io.exe_writeBack(0).bits  := 0.U.asTypeOf(WriteBack_info(dp)) 
  //   io.mem_aWriteBack.ready := false.B

  //   iwriteBack_arb.io.enq(4).valid := false.B
  //   iwriteBack_arb.io.enq(4).bits  := 0.U.asTypeOf(WriteBack_info(dp))
  //   fReg.io.exe_writeBack(1).valid := false.B
  //   fReg.io.exe_writeBack(1).bits  := 0.U.asTypeOf(WriteBack_info(dp)) 
  //   io.fpu_aWriteBack.ready := false.B

  //   when(io.mem_aWriteBack.bits.is_iwb === true.B) {iwriteBack_arb.io.enq(3) <> io.mem_aWriteBack}
  //   .elsewhen(io.mem_aWriteBack.bits.is_fwb === true.B) {}

  //   when(io.fpu_aWriteBack.bits.is_iwb === true.B) {iwriteBack_arb.io.enq(5) <> io.fpu_aWriteBack}
  //   .elsewhen(io.fpu_aWriteBack.bits.is_fwb === true.B) {}    
  // }



}
