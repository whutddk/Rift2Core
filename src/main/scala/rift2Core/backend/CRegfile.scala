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

abstract class CRegfilesBase(implicit p: Parameters) extends RiftModule {
  val io = IO(new Bundle{
    val lookup = Vec( rnc, new SeqReg_Lookup_Bundle(dp))
    val rename = Vec( rnc, new SeqReg_Rename_Bundle(dp))

    val writeBack = Vec(wbc, Flipped(Valid(new SeqReg_WriteBack_Bundle(dw, dp))))

    val commit = Vec(cmm, Flipped(new SeqReg_Commit_Bundle(dp)))

    val csrOp= Vec(cmm, new Exe_Port) 
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
  val mstatusLog = IO(Output( Vec( dp, UInt(2.W)) ))

  val mstatusReg = Module( SeqCsr(dw = 64, 4, 0.U, rnChn, wbChn, cmChn) )

  for( i <- 0 until rnc ){
    mstatusReg.io.lookup(i).req := DontCare
    mstatusReg.io.rename(i).req.valid := false.B
    mstatusReg.io.rename(i).req.bits  := DontCare

    when( io.lookup(i).req === "h300".U ){
      io.lookup(i).rsp := mstatusReg.io.lookup(i).rsp
    }

    when( io.rename(i).req.bits === "h300".U ){
      io.rename(i) <> mstatusReg.io.rename(i)
    }
  }

  for( i <- 0 until wbc ){
      mstatusReg.io.dati               := DontCare
      mstatusReg.io.writeBack(i).op_rw := false.B
      mstatusReg.io.writeBack(i).op_rs := false.B
      mstatusReg.io.writeBack(i).op_rc := false.B
      mstatusReg.io.writeBack(i).idx   := DontCare

    when( io.writeBack(i).addr === "h300".U ){
      mstatusReg.io.dati               := io.writeBack(i).dati
      mstatusReg.io.writeBack(i).op_rw := io.writeBack(i).op_rw
      mstatusReg.io.writeBack(i).op_rs := io.writeBack(i).op_rs
      mstatusReg.io.writeBack(i).op_rc := io.writeBack(i).op_rc
      mstatusReg.io.writeBack(i).idx   := io.writeBack(i).idx
    }
  }


  for( i <- 0 unitl cmm ){
    mstatusReg.io.commit(i).isComfirm := false.B
    mstatusReg.io.commit(i).isAbort := false.B
    mstatusReg.io.commit(i).idx := DontCare


    when( io.commit(i).addr === "h300".U ){
      mstatusReg.io.commit(i) := io.commit(i)
      io.csrOp(i) <> mstatusReg.io.csrOp(i)      
    }


  }
}


class CRegfiles extends CRegfilesBase
with MSTATUS_Reg{


}

