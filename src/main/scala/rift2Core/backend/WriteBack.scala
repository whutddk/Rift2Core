



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

class WriteBack( dp: Int=64, rn_chn: Int = 2, rop_chn: Int=6, wb_chn: Int=4, cmm_chn: Int = 2) extends Module {
  val io = IO(new Bundle{
    val dpt_Xlookup = Vec( rn_chn, Flipped(new dpt_lookup_info(dp)) )
    val dpt_Flookup = Vec( rn_chn, Flipped(new dpt_lookup_info(dp)) )
    val dpt_Xrename = Vec( rn_chn, Flipped(new dpt_rename_info(dp)) )
    val dpt_Frename = Vec( rn_chn, Flipped(new dpt_rename_info(dp)) )


    val ooo_readOp  = Vec(2, Flipped( new iss_readOp_info(dw = 64,dp)))
    val bru_readOp  = Flipped( new iss_readOp_info(dw = 64,dp))
    val csr_readOp  = Flipped( new iss_readOp_info(dw = 64,dp))
    val lsu_readXOp  = Flipped( new iss_readOp_info(dw = 64,dp))
    val lsu_readFOp  = Flipped( new iss_readOp_info(dw = 65,dp))
    val fpu_readXOp  = Flipped( new iss_readOp_info(dw = 64,dp))
    val fpu_readFOp  = Flipped( new iss_readOp_info(dw = 65,dp))

    val alu_iWriteBack = Flipped(new DecoupledIO(new WriteBack_info(dw = 64,dp)))
    val bru_iWriteBack = Flipped(new DecoupledIO(new WriteBack_info(dw = 64,dp)))
    val csr_iWriteBack = Flipped(new DecoupledIO(new WriteBack_info(dw = 64,dp)))
    val mem_iWriteBack = Flipped(new DecoupledIO(new WriteBack_info(dw = 64,dp)))
    val mul_iWriteBack = Flipped(new DecoupledIO(new WriteBack_info(dw = 64,dp)))
    val fpu_iWriteBack = Flipped(new DecoupledIO(new WriteBack_info(dw = 64,dp)))

    val mem_fWriteBack = Flipped(new DecoupledIO(new WriteBack_info(dw = 65, dp)))
    val fpu_fWriteBack = Flipped(new DecoupledIO(new WriteBack_info(dw = 65, dp)))

    val commit = Vec(cmm_chn, Flipped((new Info_commit_op(dp))))

    val diffXReg = Output(Vec(32, UInt(64.W)))
    val diffFReg = Output(Vec(32, UInt(65.W)))
  })


  val iReg = Module(new XRegFiles(dw = 64, dp, rn_chn, rop_chn, wb_chn, cmm_chn))
  val fReg = Module(new FRegFiles(dw = 65, dp, rn_chn, rop_chn=2, wb_chn=2, cmm_chn))

  for ( i <- 0 until rn_chn ) yield {
    iReg.io.dpt_rename(i) <> io.dpt_Xrename(i)
    fReg.io.dpt_rename(i) <> io.dpt_Frename(i)
    iReg.io.dpt_lookup(i) <> io.dpt_Xlookup(i)
    fReg.io.dpt_lookup(i) <> io.dpt_Flookup(i)
  }


  for ( i <- 0 until cmm_chn ) yield {
    iReg.io.commit(i).is_comfirm    := false.B
    iReg.io.commit(i).is_MisPredict := io.commit(i).is_MisPredict
    iReg.io.commit(i).is_abort      := io.commit(i).is_abort
    iReg.io.commit(i).raw           := 0.U
    iReg.io.commit(i).phy           := 0.U
    iReg.io.commit(i).toX           := false.B
    iReg.io.commit(i).toF           := false.B

    fReg.io.commit(i).is_comfirm    := false.B
    fReg.io.commit(i).is_MisPredict := false.B
    fReg.io.commit(i).is_abort      := io.commit(i).is_abort | io.commit(i).is_MisPredict
    fReg.io.commit(i).raw           := 0.U
    fReg.io.commit(i).phy           := 0.U
    fReg.io.commit(i).toX           := false.B
    fReg.io.commit(i).toF           := false.B

    io.commit(i).is_writeback := false.B

    when( io.commit(i).toX === true.B ) {iReg.io.commit(i) <> io.commit(i)}
    .elsewhen( io.commit(i).toF === true.B ) {fReg.io.commit(i) <> io.commit(i)}

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
    val mdl = Module(new XArbiter(new WriteBack_info(dw=64,dp), in = 6, out = wb_chn))
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



}
