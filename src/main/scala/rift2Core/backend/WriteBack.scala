



/*
  Copyright (c) 2020 - 2024 Wuhan University of Technology <295054118@whut.edu.cn>

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

import rift2Chip._
import org.chipsalliance.cde.config._

import rift2Core.privilege._






class WriteBack(implicit p: Parameters) extends RiftModule {

  class WriteBackIO extends Bundle{
    val xLookup = Vec( rnChn, Flipped(new Lookup_Bundle) )
    val fLookup = Vec( rnChn, Flipped(new Lookup_Bundle) )
              // val vLookup = Vec( rnChn, Flipped(new Lookup_Bundle) )
              // val cLookup = Vec( rnChn, new SeqReg_Lookup_Bundle(cRegNum))

    val xRename = Vec( rnChn, Flipped(new Rename_Bundle) )
    val fRename = Vec( rnChn, Flipped(new Rename_Bundle) )
              // val vRename = Vec( rnChn, Flipped(new Rename_Bundle) )
              // val cRename = Vec( rnChn, new SeqReg_Rename_Bundle(cRegNum))

    val irgLog = Output( Vec(xRegNum, UInt(2.W)) )
    val frgLog = Output( Vec(fRegNum, UInt(2.W)) )
              // val vrgLog = Output( Vec(fRegNum, UInt(2.W)) )

    val irgReq = Flipped(Vec( opChn, Valid( UInt((log2Ceil(xRegNum)).W) ) ))
    val frgReq = Flipped(Vec( opChn, Valid( UInt((log2Ceil(fRegNum)).W) ) ))
              // val vrgReq = Flipped(Vec( vParams.opChn, Valid( UInt((log2Ceil(vRegNum)).W) ) ))

    val irgRsp =  Vec( opChn, Valid(new ReadOp_Rsp_Bundle(64) ))
    val frgRsp =  Vec( opChn, Valid(new ReadOp_Rsp_Bundle(65) ))
              // val vrgRsp =  Vec( vParams.opChn, Valid(new ReadOp_Rsp_Bundle(vParams.vlen) ))

    val vMolloc = Flipped(Decoupled(new Vector_Molloc_Bundle ) ) //molloc at read op
    val vReadOp = Vec( 32, Valid(UInt( (vParams.vlen).W )) )




    val alu_iWriteBack = Vec(aluNum, Flipped(new DecoupledIO(new WriteBack_info(dw = 64))))
    val bru_iWriteBack = Flipped(new DecoupledIO(new WriteBack_info(dw = 64)))
    val csr_iWriteBack = Flipped(new DecoupledIO(new WriteBack_info(dw = 64)))
    val mem_iWriteBack = Flipped(new DecoupledIO(new WriteBack_info(dw = 64)))
    val mul_iWriteBack = Vec(mulNum max 1, Flipped(new DecoupledIO(new WriteBack_info(dw = 64))))
    val fpu_iWriteBack = Vec(fpuNum max 1, Flipped(new DecoupledIO(new WriteBack_info(dw = 64))))

    val mem_fWriteBack = Flipped(new DecoupledIO(new WriteBack_info(dw = 65)))
    val fpu_fWriteBack = Vec(fpuNum max 1, Flipped(new DecoupledIO(new WriteBack_info(dw = 65))))

    val mem_vWriteBack = Flipped(new DecoupledIO(new Vector_WriteBack_Bundle))

                // val cWriteBack     = Vec(3, Flipped(Valid(new SeqReg_WriteBack_Bundle(64, cRegNum))))


    
    val xCommit = Vec(cmChn, Flipped((new Info_commit_op(32, xRegNum))))
    val fCommit = Vec(cmChn, Flipped((new Info_commit_op(32, fRegNum))))
    val vCommit = Vec(cmChn, Flipped(new Vector_Commit_Bundle))
                // val cCommit = Vec(cmChn, Flipped(new SeqReg_Commit_Bundle(cRegNum)))

                // val csrIsReady = Output(new CSR_LOG_Bundle(cRegNum))
                // val csrOp= Output(Vec(cmChn, new Exe_Port) )

    val xpuCsrMolloc    = Flipped(Vec(rnChn, Decoupled(UInt(12.W))))
    val xpuCsrWriteBack = Flipped(Valid(new Exe_Port))
    val xpuCsrCommit    = Decoupled(new Exe_Port)
    val fpuCsrMolloc    = Flipped(Vec(rnChn, Decoupled(Bool())))
    val fpuCsrWriteBack = Flipped(Valid(new Exe_Port))
    val fpuCsrCommit    = Decoupled(new Exe_Port)
    val vpuCsrMolloc    = Flipped(Vec(rnChn, Decoupled(Bool())))
    val vpuCsrWriteBack = Flipped(Valid(new Exe_Port))
    val vpuCsrCommit    = Decoupled(new Exe_Port)
    val isCSRMMUReady   = Output(Bool())

    val diffXReg = Output(Vec(32, UInt(64.W)))
    val diffFReg = Output(Vec(32, UInt(65.W)))
    val diffVReg = Output(Vec(32, UInt((vParams.vlen).W)))
  }

  val io: WriteBackIO = IO(new WriteBackIO)


  val iReg = Module(new XRegFiles(dw = 64, dp = xRegNum, rnChn, opChn, wbChn, cmChn))
  val fReg = if( fpuNum > 0 ) { Module(new FRegFiles(dw = 65, dp = fRegNum, rnChn, opChn, 2, cmChn)) } else {  Module(new FakeRegFiles(dw = 65, dp = fRegNum, rnChn, opChn, 2, cmChn) ) }
  val vReg = if( hasVector )  { Module(new VRegFiles) } else {  Module(new FakeVRegFiles ) }
  val cReg = Module(new CsrScoreBoard)



  io.xpuCsrMolloc    <> cReg.io.xpu.molloc
  io.xpuCsrWriteBack <> cReg.io.xpu.writeBack
  io.xpuCsrCommit    <> cReg.io.xpu.commit
  io.fpuCsrMolloc    <> cReg.io.fpu.molloc
  io.fpuCsrWriteBack <> cReg.io.fpu.writeBack
  io.fpuCsrCommit    <> cReg.io.fpu.commit
  io.vpuCsrMolloc    <> cReg.io.vpu.molloc
  io.vpuCsrWriteBack <> cReg.io.vpu.writeBack
  io.vpuCsrCommit    <> cReg.io.vpu.commit
  io.isCSRMMUReady   := cReg.io.isMMUReady

  iReg.io.rename <> io.xRename
  iReg.io.lookup <> io.xLookup
  iReg.io.commit <> io.xCommit

  fReg.io.rename <> io.fRename
  fReg.io.lookup <> io.fLookup
  fReg.io.commit <> io.fCommit


  vReg.io.molloc <> io.vMolloc
  vReg.io.readOp <> io.vReadOp
  vReg.io.commit <> io.vCommit


  // for ( i <- 0 until cmChn ) yield {
    // iReg.io.commit(i).is_comfirm    := false.B
    // iReg.io.commit(i).is_MisPredict := io.commit(i).is_MisPredict
    // iReg.io.commit(i).is_abort      := io.commit(i).is_abort
    // iReg.io.commit(i).raw           := 0.U
    // iReg.io.commit(i).phy           := 0.U
    // iReg.io.commit(i).toX           := false.B
    // iReg.io.commit(i).toF           := false.B
    // iReg.io.commit(i).toV           := false.B

    // fReg.io.commit(i).is_comfirm    := false.B
    // fReg.io.commit(i).is_MisPredict := false.B
    // fReg.io.commit(i).is_abort      := io.commit(i).is_abort | io.commit(i).is_MisPredict
    // fReg.io.commit(i).raw           := 0.U
    // fReg.io.commit(i).phy           := 0.U
    // fReg.io.commit(i).toX           := false.B
    // fReg.io.commit(i).toF           := false.B
    // fReg.io.commit(i).toV           := false.B

    

    // io.commit(i).is_writeback := false.B

    // when( io.commit(i).toX === true.B ) {iReg.io.commit(i) <> io.commit(i)}
    // .elsewhen( io.commit(i).toF === true.B ) {fReg.io.commit(i) <> io.commit(i)}

  // }

  cReg.io.isAbort := ( 0 until cmChn ).map{ i => io.xCommit(i).is_abort | io.xCommit(i).is_MisPredict }.foldLeft(false.B)(_|_)

  iReg.io.diffReg <> io.diffXReg
  fReg.io.diffReg <> io.diffFReg
  vReg.io.diffReg <> io.diffVReg

  iReg.io.rgReq := io.irgReq
  io.irgRsp := iReg.io.rgRsp
  
  fReg.io.rgReq := io.frgReq
  io.frgRsp := fReg.io.rgRsp

      // vReg.io.rgReq := io.vrgReq
      // io.vrgRsp := vReg.io.rgRsp

  io.irgLog := iReg.io.rgLog
  io.frgLog := fReg.io.rgLog
      // io.vrgLog := vReg.io.rgLog






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


  vReg.io.writeBack <> io.mem_vWriteBack

              // cReg.io.writeBack <> io.cWriteBack

}
