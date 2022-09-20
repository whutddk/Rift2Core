



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

import rift._
import chipsalliance.rocketchip.config._

class WriteBack(implicit p: Parameters) extends RiftModule {
  val io = IO(new Bundle{
    val dpt_Xlookup = Vec( rn_chn, Flipped(new dpt_lookup_info) )
    val dpt_Flookup = Vec( rn_chn, Flipped(new dpt_lookup_info) )
    val dpt_Xrename = Vec( rn_chn, Flipped(new dpt_rename_info) )
    val dpt_Frename = Vec( rn_chn, Flipped(new dpt_rename_info) )

    
      
    val ooo_readOp  = ( if ( opChn > 1)  { Some(Vec( opChn/2, Flipped( new iss_readOp_info(dw = 64)) ) ) } else {None})
    val ito_readOp  = ( if ( opChn > 1)  { Some(Vec( opChn/2, Flipped( new iss_readOp_info(dw = 64)) ) ) } else {None})
    val sig_readOp  = ( if ( opChn == 1) { Some(Vec( 1,       Flipped( new iss_readOp_info(dw = 64)) ) ) } else {None})


    val frg_readOp  = Vec( (if (opChn > 1) {opChn/2} else {1}) , Flipped( new iss_readOp_info(dw = 65)) )


    val alu_iWriteBack = Flipped(new DecoupledIO(new WriteBack_info(dw = 64)))
    val bru_iWriteBack = Flipped(new DecoupledIO(new WriteBack_info(dw = 64)))
    val csr_iWriteBack = Flipped(new DecoupledIO(new WriteBack_info(dw = 64)))
    val mem_iWriteBack = Flipped(new DecoupledIO(new WriteBack_info(dw = 64)))
    val mul_iWriteBack = Flipped(new DecoupledIO(new WriteBack_info(dw = 64)))
    val fpu_iWriteBack = Flipped(new DecoupledIO(new WriteBack_info(dw = 64)))

    val mem_fWriteBack = Flipped(new DecoupledIO(new WriteBack_info(dw = 65)))
    val fpu_fWriteBack = Flipped(new DecoupledIO(new WriteBack_info(dw = 65)))

    val commit = Vec(cm_chn, Flipped((new Info_commit_op)))

    val diffXReg = Output(Vec(32, UInt(64.W)))
    val diffFReg = Output(Vec(32, UInt(65.W)))
  })


  val iReg = Module(new XRegFiles(dw = 64, opChn, wbChn))
  val fRegIO = 
    if( hasFpu ) {
      val mdl = Module(new FRegFiles(dw = 65, (if ( opChn > 1 ) {opChn/2} else {1}), wb_chn=2))
      mdl.io
    } else {
      val mdl = Module(new FakeFRegFiles(dw = 65, (if ( opChn > 1 ) {opChn/2} else {1}), wb_chn=2) )
      mdl.io
    }



  for ( i <- 0 until rn_chn ) yield {
    iReg.io.dpt_rename(i) <> io.dpt_Xrename(i)
    fRegIO.dpt_rename(i) <> io.dpt_Frename(i)
    iReg.io.dpt_lookup(i) <> io.dpt_Xlookup(i)
    fRegIO.dpt_lookup(i) <> io.dpt_Flookup(i)
  }


  for ( i <- 0 until cm_chn ) yield {
    iReg.io.commit(i).is_comfirm    := false.B
    iReg.io.commit(i).is_MisPredict := io.commit(i).is_MisPredict
    iReg.io.commit(i).is_abort      := io.commit(i).is_abort
    iReg.io.commit(i).raw           := 0.U
    iReg.io.commit(i).phy           := 0.U
    iReg.io.commit(i).toX           := false.B
    iReg.io.commit(i).toF           := false.B

    fRegIO.commit(i).is_comfirm    := false.B
    fRegIO.commit(i).is_MisPredict := false.B
    fRegIO.commit(i).is_abort      := io.commit(i).is_abort | io.commit(i).is_MisPredict
    fRegIO.commit(i).raw           := 0.U
    fRegIO.commit(i).phy           := 0.U
    fRegIO.commit(i).toX           := false.B
    fRegIO.commit(i).toF           := false.B

    io.commit(i).is_writeback := false.B

    when( io.commit(i).toX === true.B ) {iReg.io.commit(i) <> io.commit(i)}
    .elsewhen( io.commit(i).toF === true.B ) {fRegIO.commit(i) <> io.commit(i)}

  }



  iReg.io.diffReg <> io.diffXReg
  fRegIO.diffReg <> io.diffFReg

  if( opChn > 1 ) {
    for ( i <- 0 until opChn/2 ) {
      iReg.io.iss_readOp(2*i)   <> io.ooo_readOp.get(i)
      iReg.io.iss_readOp(2*i+1) <> io.ito_readOp.get(i)

      fRegIO.iss_readOp(i) <> io.frg_readOp(i)
    }    
  } else if( opChn == 1 ) {
    iReg.io.iss_readOp(0)   <> io.sig_readOp.get(0)
    fRegIO.iss_readOp(0) <> io.frg_readOp(0)
  }






  val iwriteBack_arb = {
    val mdl = Module(new XArbiter(new WriteBack_info(dw=64), in = 6, out = wbChn))
    mdl.io.enq(0) <> io.alu_iWriteBack
    mdl.io.enq(1) <> io.bru_iWriteBack
    mdl.io.enq(2) <> io.csr_iWriteBack
    mdl.io.enq(3) <> io.mem_iWriteBack
    mdl.io.enq(4) <> io.mul_iWriteBack
    mdl.io.enq(5) <> io.fpu_iWriteBack
    mdl.io.deq <> iReg.io.exe_writeBack

    mdl
  }

  fRegIO.exe_writeBack(0) <> io.mem_fWriteBack
  fRegIO.exe_writeBack(1) <> io.fpu_fWriteBack



}
