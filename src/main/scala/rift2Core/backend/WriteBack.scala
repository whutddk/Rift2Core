



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

import rift2Chip._
import chipsalliance.rocketchip.config._

class WriteBack(implicit p: Parameters) extends RiftModule {

  val io = IO(new Bundle{
    val xLookup = Vec( rnChn, Flipped(new Lookup_Bundle) )
    val fLookup = Vec( rnChn, Flipped(new Lookup_Bundle) )
    val cLookup = Vec( rnChn, new SeqReg_Lookup_Bundle(4))

    val xRename = Vec( rnChn, Flipped(new Rename_Bundle) )
    val fRename = Vec( rnChn, Flipped(new Rename_Bundle) )
    val cRename = Vec( rnChn, new SeqReg_Rename_Bundle(4))

    val irgLog = Output( Vec(xRegNum, UInt(2.W)) )
    val frgLog = Output( Vec(fRegNum, UInt(2.W)) )

    val irgReq = Flipped(Vec( opChn, Valid( UInt((log2Ceil(xRegNum)).W) ) ))
    val frgReq = Flipped(Vec( opChn, Valid( UInt((log2Ceil(fRegNum)).W) ) ))

    val irgRsp =  Vec( opChn, Valid(new ReadOp_Rsp_Bundle(64) ))
    val frgRsp =  Vec( opChn, Valid(new ReadOp_Rsp_Bundle(65) ))


    val alu_iWriteBack = Vec(aluNum, Flipped(new DecoupledIO(new WriteBack_info(dw = 64))))
    val bru_iWriteBack = Flipped(new DecoupledIO(new WriteBack_info(dw = 64)))
    val csr_iWriteBack = Flipped(new DecoupledIO(new WriteBack_info(dw = 64)))
    val mem_iWriteBack = Flipped(new DecoupledIO(new WriteBack_info(dw = 64)))
    val mul_iWriteBack = Vec(mulNum max 1, Flipped(new DecoupledIO(new WriteBack_info(dw = 64))))
    val fpu_iWriteBack = Vec(mulNum max 1, Flipped(new DecoupledIO(new WriteBack_info(dw = 64))))

    val mem_fWriteBack = Flipped(new DecoupledIO(new WriteBack_info(dw = 65)))
    val fpu_fWriteBack = Vec(mulNum max 1, Flipped(new DecoupledIO(new WriteBack_info(dw = 65))))

    val csr_cWriteBack = Flipped(Valid(new SeqReg_WriteBack_Bundle(64, 4)))


    
    val commit = Vec(cmChn, Flipped((new Info_commit_op)))
    val cCommit = Vec(cmChn, Flipped(new SeqReg_Commit_Bundle(4)))

    val csrIsReady = Output(new CSR_LOG_Bundle)
    val csrOp= Output(Vec(cmChn, new Exe_Port) )

    val diffXReg = Output(Vec(32, UInt(64.W)))
    val diffFReg = Output(Vec(32, UInt(65.W)))
  })


  val iReg = Module(new XRegFiles(dw = 64, dp = xRegNum, rnChn, opChn, wbChn, cmChn))
  val fReg = if( fpuNum > 0 ) { Module(new FRegFiles(dw = 65, dp = fRegNum, rnChn, opChn, 2, cmChn)) } else {  Module(new FakeFRegFiles(dw = 65, dp = fRegNum, rnChn, opChn, 2, cmChn) ) }
  val cReg = Module(new CRegfiles)

  io.cLookup <> cReg.io.lookup
  io.cRename <> cReg.io.rename

  cReg.io.writeBack(0) := io.csr_cWriteBack

  cReg.io.writeBack(1).valid := false.B
  cReg.io.writeBack(1).bits  := DontCare

  cReg.io.writeBack(2).valid := false.B
  cReg.io.writeBack(2).bits  := DontCare

  cReg.io.writeBack(3).valid := false.B
  cReg.io.writeBack(3).bits  := DontCare

  io.cCommit    <> cReg.io.commit
  io.csrOp      <> cReg.io.csrOp
  io.csrIsReady := cReg.isReady


  for ( i <- 0 until rnChn ) yield {
    iReg.io.rename(i) <> io.xRename(i)
    fReg.io.rename(i) <> io.fRename(i)
    iReg.io.lookup(i) <> io.xLookup(i)
    fReg.io.lookup(i) <> io.fLookup(i)
  }


  for ( i <- 0 until cmChn ) yield {
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

  iReg.io.rgReq := io.irgReq
  io.irgRsp := iReg.io.rgRsp
  
  fReg.io.rgReq := io.frgReq
  io.frgRsp := fReg.io.rgRsp

  io.irgLog := iReg.io.rgLog
  io.frgLog := fReg.io.rgLog






  val iwriteBack_arb = {
    val mdl = Module(new XArbiter(new WriteBack_info(dw=64), in = 3 + aluNum + (mulNum max 1) + (fpuNum max 1), out = wbChn))
    val iwbIO = io.alu_iWriteBack ++ Seq(io.bru_iWriteBack, io.csr_iWriteBack, io.mem_iWriteBack) ++ io.mul_iWriteBack ++ io.fpu_iWriteBack
    mdl.io.enq <> iwbIO
    // mdl.io.enq(0) <> io.alu_iWriteBack
    // mdl.io.enq(1) <> io.bru_iWriteBack
    // mdl.io.enq(2) <> io.csr_iWriteBack
    // mdl.io.enq(3) <> io.mem_iWriteBack
    // mdl.io.enq(4) <> io.mul_iWriteBack
    // mdl.io.enq(5) <> io.fpu_iWriteBack
    mdl.io.deq <> iReg.io.exe_writeBack

    mdl
  }

  fReg.io.exe_writeBack(0) <> io.mem_fWriteBack
  fReg.io.exe_writeBack(1) <> io.fpu_fWriteBack(0); require( fpuNum <= 1 )



}
