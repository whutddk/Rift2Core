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

import rift2Chip._
import chipsalliance.rocketchip.config._



trait VRegFilesReName{ this: RegFilesReal => 


  /** finding out the first Free-phy-register */ 
  val mollocIdx = Wire(Vec( 2*rnc, UInt((log2Ceil(dp)).W)))

  for ( i <- 0 until 2*rnc ) {
    mollocIdx(i) := 0.U
    for ( j <- (dp-1) to 1 by -1 ) {
      if ( i == 0 ) { when( log(j) === 0.U ) { mollocIdx(i) := j.U }  }
      else { when( log(j) === 0.U && j.U > mollocIdx(i-1) ) { mollocIdx(i) := j.U } }
    }
  }


  for ( i <- 0 until rnc ) {
    val idx0 = io.rename(i).req.bits.rd0
    val idx1 = io.rename(i).req.bits.rd1
    when( io.rename(i).req.fire ) {

      assert( log(mollocIdx(2*i)) === "b00".U )
      log_reg(mollocIdx(2*i)) := "b01".U //may be override by flush
      rename_ptr(idx0) := mollocIdx(2*i) //may be override by flush

      when( idx1 =/= 0.U ){
        assert( log(mollocIdx(2*i+1)) === "b00".U )
        log_reg(mollocIdx(2*i+1)) := "b01".U //may be override by flush
        rename_ptr(idx1) := mollocIdx(2*i+1)
      }
    }

    io.rename(i).req.ready :=
      Mux(
        idx1 =/= 0.U,
        log.count( (x:UInt) => ( x === 0.U ) ) > (2*i+1).U,
        log.count( (x:UInt) => ( x === 0.U ) ) > (2*i).U,
      )

    io.rename(i).rsp.rd0 := mollocIdx(2*i)
    io.rename(i).rsp.rd1 := Mux( idx1 =/= 0.U, mollocIdx(2*i+1), 0.U )

  }

  for ( i <- 0 until cmm ) {
    def m = cmm-1-i
    when ( io.commit(m).is_MisPredict | io.commit(m).is_abort ) {
      for ( j <- 0 until 32 ) yield {
        rename_ptr(j) := archit_ptr(j)
        for ( n <- 0 until m ) {
          when( j.U === raw(n) & io.commit(n).is_comfirm ) {
            rename_ptr(j) := phy(n) //override renme_ptr when the perivious chn comfirm
          } 
        }
        when( j.U === raw(m) & io.commit(m).is_MisPredict ) { rename_ptr(j) := phy(m) } //override renme_ptr when the this chn is mispredict
      }
    }
  }
}

trait VRegFilesLookup{ this: RegFilesReal => 
  val vio = IO( new Bundle{
    val lookup_v0_rsp = Output( Vec( rnc, UInt((log2Ceil(vRegNum)).W)) )
  })

  val mollocIdx: Vec[UInt]

  for ( i <- 0 until rnc ) {
    val idx1 = io.lookup(i).req.rs1
    val idx2 = io.lookup(i).req.rs2
    val idx3 = io.lookup(i).req.rs3
    val idx4 = io.lookup(i).req.rs4


    if ( i == 0 ) {
      io.lookup(i).rsp.rs1 := rename_ptr(idx1)
      io.lookup(i).rsp.rs2 := rename_ptr(idx2)
      io.lookup(i).rsp.rs3 := rename_ptr(idx3)
      io.lookup(i).rsp.rs4 := rename_ptr(idx4)
      vio.lookup_v0_rsp(i)  := rename_ptr(0)
    } else {
      io.lookup(i).rsp.rs1 := rename_ptr(idx1)
      io.lookup(i).rsp.rs2 := rename_ptr(idx2)
      io.lookup(i).rsp.rs3 := rename_ptr(idx3)
      io.lookup(i).rsp.rs4 := rename_ptr(idx4)
      vio.lookup_v0_rsp(i)  := rename_ptr(0)

      for ( j <- 0 until i ) {
        when( io.rename(j).req.valid && (io.rename(j).req.bits.rd0 === idx1) ) { io.lookup(i).rsp.rs1 := mollocIdx(2*j) }
        when( io.rename(j).req.valid && (io.rename(j).req.bits.rd0 === idx2) ) { io.lookup(i).rsp.rs2 := mollocIdx(2*j) }
        when( io.rename(j).req.valid && (io.rename(j).req.bits.rd0 === idx3) ) { io.lookup(i).rsp.rs3 := mollocIdx(2*j) }
        when( io.rename(j).req.valid && (io.rename(j).req.bits.rd0 === idx4) ) { io.lookup(i).rsp.rs4 := mollocIdx(2*j) }
        when( io.rename(j).req.valid && (io.rename(j).req.bits.rd0 === 0.U ) ) { vio.lookup_v0_rsp(i) := mollocIdx(2*j) }

        when( io.rename(j).req.valid && (io.rename(j).req.bits.rd1 === idx1) ) { io.lookup(i).rsp.rs1 := mollocIdx(2*j+1) }
        when( io.rename(j).req.valid && (io.rename(j).req.bits.rd1 === idx2) ) { io.lookup(i).rsp.rs2 := mollocIdx(2*j+1) }
        when( io.rename(j).req.valid && (io.rename(j).req.bits.rd1 === idx3) ) { io.lookup(i).rsp.rs3 := mollocIdx(2*j+1) }
        when( io.rename(j).req.valid && (io.rename(j).req.bits.rd1 === idx4) ) { io.lookup(i).rsp.rs4 := mollocIdx(2*j+1) }
        when( io.rename(j).req.valid && (io.rename(j).req.bits.rd1 === 0.U ) ) { vio.lookup_v0_rsp(i) := mollocIdx(2*j+1) }
      }
    }
    when( io.rename(i).req.fire ) {
      assert( io.lookup(i).rsp.rs1 =/= 0.U )
      assert( io.lookup(i).rsp.rs2 =/= 0.U )
      assert( io.lookup(i).rsp.rs3 =/= 0.U )
      assert( io.lookup(i).rsp.rs4 =/= 0.U )
      assert( vio.lookup_v0_rsp(i)  =/= 0.U )      
    }
  }
}


class VRegFiles(dw: Int, dp: Int, rnc: Int, rop: Int, wbc: Int, cmm: Int)(implicit p: Parameters) extends RegFilesReal(dw, dp, arc = 32, rnc, rop, wbc, cmm)
with VRegFilesReName
with VRegFilesLookup
with RegFilesReadOP
with RegFilesWriteBack
with RegFilesCommit{


}

