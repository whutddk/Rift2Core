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
import rift2Core.privilege._

import rift2Chip._
import chipsalliance.rocketchip.config.Parameters

abstract class CRegfilesBase( val rnc: Int, val wbc: Int, val cmm: Int )(implicit p: Parameters) extends RiftModule {
  val io = IO(new Bundle{
    val lookup = Vec( rnc, new SeqReg_Lookup_Bundle(4))
    val rename = Vec( rnc, new SeqReg_Rename_Bundle(4))

    val writeBack = Vec(wbc, Flipped(Valid(new SeqReg_WriteBack_Bundle(64, 4))))

    val commit = Vec(cmm, Flipped(new SeqReg_Commit_Bundle(4)))

    val csrOp= Output(Vec(cmm, new Exe_Port) )
  })

  for( i <- 0 until rnc ){
    io.lookup(i).rsp := DontCare
    io.rename(i).req.ready := false.B
    io.rename(i).rsp := DontCare    
  }

  for( i <- 0 until cmm ){
    io.csrOp(i).addr  := DontCare
    io.csrOp(i).dat_i := DontCare
    io.csrOp(i).op_rw := false.B
    io.csrOp(i).op_rs := false.B
    io.csrOp(i).op_rc := false.B
  }



}


trait MSTATUS_Reg{ this: CRegfilesBase =>
  

  val mstatusReg = Module( new SeqCsr(dw = 64, 4, 0.U, 0x300, rnc, wbc, cmm) )

  

  for( i <- 0 until rnc ){
    mstatusReg.io.lookup(i).req := DontCare
    mstatusReg.io.rename(i).req.valid := false.B
    mstatusReg.io.rename(i).req.bits  := DontCare

    when( io.lookup(i).req === "h300".U ){ io.lookup(i).rsp := mstatusReg.io.lookup(i).rsp }
    when( io.rename(i).req.bits === "h300".U ){ io.rename(i) <> mstatusReg.io.rename(i) }
  }

  for( i <- 0 until wbc ){
      mstatusReg.io.writeBack(i).bits.dati  := DontCare
      mstatusReg.io.writeBack(i).bits.addr  := DontCare
      mstatusReg.io.writeBack(i).bits.op_rw := false.B
      mstatusReg.io.writeBack(i).bits.op_rs := false.B
      mstatusReg.io.writeBack(i).bits.op_rc := false.B
      mstatusReg.io.writeBack(i).bits.idx   := DontCare
      mstatusReg.io.writeBack(i).valid      := false.B

    when( io.writeBack(i).bits.addr === "h300".U ){
      mstatusReg.io.writeBack(i) := io.writeBack(i)
    }
  }

  for( i <- 0 until cmm ){
    mstatusReg.io.commit(i).isComfirm := false.B
    mstatusReg.io.commit(i).isAbort := false.B
    mstatusReg.io.commit(i).idx := DontCare
    mstatusReg.io.commit(i).addr := DontCare
    io.commit(i).isWroteback := false.B

    when( io.commit(i).addr === "h300".U ){
      mstatusReg.io.commit(i) <> io.commit(i)
      io.csrOp(i) := mstatusReg.io.csrOp(i)      
    }
  }
}

class CSR_LOG_Bundle(dp: Int = 4) extends Bundle{
  val mstatus = Vec( dp, Bool() )
}

class CRegfiles( rnc: Int, wbc: Int, cmm: Int )(implicit p: Parameters) extends CRegfilesBase( rnc, wbc, cmm )
with MSTATUS_Reg{
  val isReady = IO(Output(new CSR_LOG_Bundle))

  isReady.mstatus := mstatusReg.io.isReady




}

