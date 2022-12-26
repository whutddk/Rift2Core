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

class SeqReg_Lookup_Bundle(dp: Int)(implicit p: Parameters) extends RiftBundle{
  val rsp = Output(UInt(log2Ceil(dp).W))
  val req = Input (UInt(12.W))
}


class SeqReg_Rename_Bundle(dp: Int)(implicit p: Parameters) extends RiftBundle{
  val req = Flipped(Decoupled(UInt(12.W)))
  val rsp = Output(UInt(log2Ceil(dp).W))
}

class SeqReg_ReadOp_Bundle(dw: Int, dp: Int)(implicit p: Parameters) extends RiftBundle{
  val req = Flipped(Valid(UInt((log2Ceil(dp)).W)))
  val rsp = Valid(UInt(dw.W))
}

class SeqReg_WriteBack_Bundle(dw:Int, dp: Int)(implicit p: Parameters) extends RiftBundle{
  // val res = UInt(dw.W)
  val addr = UInt(12.W)
  val dati = UInt(dw.W)
  val op_rw = Bool()
  val op_rs = Bool()
  val op_rc = Bool()

  val idx = UInt(log2Ceil(dp).W)
}

class SeqReg_Commit_Bundle(dp: Int)(implicit p: Parameters) extends RiftBundle{
  val isComfirm = Output(Bool())
  val isAbort   = Output(Bool())
  val idx        = Output(UInt(( log2Ceil(dp) ).W))
  val addr = Output(UInt(12.W))
  // val isWroteback = Input(Bool())
}


trait SRegFilesLookup{ this: RegFilesReal =>
  for ( i <- 0 until rnc ) {

    if ( i == 0 ) {
      io.lookup(i).rsp.rs1 := rename_ptr(0)
      io.lookup(i).rsp.rs2 := 0.U
      io.lookup(i).rsp.rs3 := 0.U
      io.lookup(i).rsp.rs4 := 0.U
    } else {
      io.lookup(i).rsp.rs1 := rename_ptr(0)
      io.lookup(i).rsp.rs2 := 0.U
      io.lookup(i).rsp.rs3 := 0.U
      io.lookup(i).rsp.rs4 := 0.U
      for ( j <- 0 until i ) {
        when( io.rename(j).req.valid ) { io.lookup(i).rsp.rs1 := mollocIdx(i-1) }
      }
    }
    when( io.rename(i).req.fire ) {
      assert( io.lookup(i).rsp.rs1 =/= 0.U )
    }

  }
}


class SRegFiles (dw: Int, dp: Int, rnc: Int, rop: Int, wbc: Int, cmm: Int)(implicit p: Parameters) extends RegFilesReal(dw, dp, arc = 1, rnc, rop, wbc, cmm)
  with RegFilesReName
  with SRegFilesLookup
  with RegFilesReadOP
  with RegFilesWriteBack
  with RegFilesCommit{

}





class SeqCsr(dw: Int, dp: Int, init: UInt, addr: Int, rnc: Int, wbc: Int, cmm: Int)(implicit p: Parameters) extends RiftModule{
  val io = IO(new Bundle{

    val lookup = Vec( rnc, new SeqReg_Lookup_Bundle(dp) )
    val rename = Vec( rnc, new SeqReg_Rename_Bundle(dp))

    val writeBack = Vec(wbc, Flipped(Valid(new SeqReg_WriteBack_Bundle(dw, dp))))

    val log = Output( Vec( dp, UInt(2.W)) )

    val commit = Vec(cmm, Flipped(new SeqReg_Commit_Bundle(dp)))

    val csrOp= Vec(cmm, Valid(new Exe_Port))

  })


  
  val mdl = Module(new SRegFiles(3+dw, dp, rnc, 1, wbc, cmm)){
    val csrOp = IO(Vec(cmm, Valid(new Exe_Port)))
    for( i <- 0 until cmm ) {
      when( io.commit(i).isComfirm ){
        csrOp(i).valid := true.B
        csrOp(i).bits.addr  := addr.U
        csrOp(i).bits.dat_i := files_reg(phy(i)).extract(dw-1, 0)
        csrOp(i).bits.op_rw := files_reg(phy(i)).extract(dw+2)
        csrOp(i).bits.op_rs := files_reg(phy(i)).extract(dw+1)
        csrOp(i).bits.op_rc := files_reg(phy(i)).extract(dw+0)
      } .otherwise{
        csrOp(i).valid := false.B
        csrOp(i).bits  := DontCare
      }
    }
  }

  io.csrOp  := mdl.csrOp



  for( i <- 0 until rnc ){
    mdl.io.lookup(i).req.rs1 := 0.U
    mdl.io.lookup(i).req.rs2 := 0.U
    mdl.io.lookup(i).req.rs3 := 0.U
    mdl.io.lookup(i).req.rs4 := 0.U

    io.lookup(i).rsp := mdl.io.lookup(i).rsp.rs1
  }

  for( i <- 0 until rnc ){
    mdl.io.rename(i).req.valid := io.rename(i).req.valid
    io.rename(i).req.ready := mdl.io.rename(i).req.ready
    mdl.io.rename(i).req.bits.rd0 := 0.U

    io.rename(i).rsp := mdl.io.rename(i).rsp.rd0
  }

  for( i <- 0 until 1 ){
    mdl.io.readOp(i).rgReq := DontCare
  }

  for( i <- 0 until wbc ){
    mdl.io.exe_writeBack(i).valid := io.writeBack(i).valid
    mdl.io.exe_writeBack(i).res   :=
      Cat(
        io.writeBack(i).bits.op_rw,
        io.writeBack(i).bits.op_rs,
        io.writeBack(i).bits.op_rc,
        io.writeBack(i).bits.dati
      )
    mdl.io.exe_writeBack(i).idx   := io.writeBack(i).idx
  }

  for( i <- 0 until cmm ){
    mdl.io.commit(i).is_comfirm    := io.commit(i).isComfirm
    mdl.io.commit(i).is_MisPredict := false.B
    mdl.io.commit(i).is_abort      := io.commit(i).isAbort
    mdl.io.commit(i).raw           := 0.U
    mdl.io.commit(i).phy           := io.commit(i).idx
    mdl.io.commit(i).toX           := DontCare
    mdl.io.commit(i).toF           := DontCare
    mdl.io.commit(i).toV           := DontCare
    // io.commit(i).isWroteback       := mdl.io.commit(i).is_writeback
  }


}




// abstract class SeqRegReal(val dw: Int, val dp: Int, val init: UInt, val rnc: Int, val rop: Int, val wbc: Int, val cmm: Int)(implicit p: Parameters) extends SeqRegBase(dw, rnc, rop, wbc, cmm, regNum){

//   val log = 
//     for( i <- 0 until dp ) yield {
//       if ( i == 0) RegInit("b11".U(2.W))
//       else RegInit("b00".U(2.W))
//     }

//   val file =
//     for( i <- 0 until dp ) yield {
//       RegInit(init(dw.W))
//     }

//   val renamePtr = RegInit( 0.U(log2Ceil(dp).W) )
//   val architPtr = RegInit( 0.U(log2Ceil(dp).W) )


//   regr := file(architPtr)

// }



// trait SeqRegReName{ this: SeqRegReal => 

//   /**
//     * finding out the first Free-phy-register
//     */ 
//   val mollocIdx = Wire(Vec(rnc, UInt((log2Ceil(dp)).W)))
//   for ( i <- 0 until rnc ) {
//     mollocIdx(i) := 0.U
//     for ( j <- (dp-1) to 1 by -1 ) {
//       if ( i == 0 ) { when( log(j) === 0.U ) { mollocIdx(i) := j.U }  }
//       else { when( log(j) === 0.U && j.U > mollocIdx(i-1) ) { mollocIdx(i) := j.U } }
//     }
//   }


//   for ( i <- 0 until rnc ) {
//     when( io.rename(i).req === true.B ) {
//       assert( log(mollocIdx(i)) === "b00".U )
//       log(mollocIdx(i)) := "b01".U //may be override by flush
//       renamePtr := mollocIdx(i) //may be override by flush
//     }

//     io.rename(i).req.ready := log.count( (x:UInt) => ( x === 0.U ) ) > i.U
//     io.rename(i).rsp.bits  := mollocIdx(i)
//   }

//   for ( i <- 0 until cmm ) {
//     def m = cmm-1-i
//     when ( io.commit(m).isMisPredict | io.commit(m).isAbort ) {
//       renamePtr := architPtr
//       for ( n <- 0 until m ) { //for all preivious commit-chn
//         when( io.commit(n).isComfirm ) { renamePtr(j) := io.commit(n).idx }//override renmePtr when the perivious chn comfirm
//       }
//       when( io.commit(m).isMisPredict ) { renamePtr(j) := io.commit(m).idx } //override renmePtr when the this chn is mispredict
//     }
//   }
// }

// trait SeqRegLookup{ this: SeqRegReal =>
//   for ( i <- 0 until rnc ) {
//     if ( i == 0) {
//       io.lookup(i) := renamePtr
//     } else {
//       io.lookup(i) := mollocIdx(i-1)
//     }
//   }
// }




// trait SeqRegReadOP{ this:SeqRegReal =>

//   io.log := log

//   for( i <- 0 until rop ) {
//     io.rdRsp(i).valid    := RegNext  ( io.rdReq(i).valid, init = false.B )
//     io.rdRsp(i).bits     := RegEnable( files(io.rdReq(i).bits), io.rdReq(i).valid )

//     when( io.rdRsp(i).valid ) {  assert( log(io.rdReq(i).bits) === "b11".U, "Assert Failed while reading operator, log is not ready!" ) }
//   }
// }


// trait SeqRegWriteBack{ this: SeqRegReal =>
//   for ( i <- 0 until wbc ) {
//     when( io.writeBack(i).fire ) {
//       val idx = io.exe_writeBack(i).bits.idx
//       assert( log(idx) === "b01".U, "Assert Failed when writeback at chn" + i + ", log(" + idx + ")" )
//       log(idx) := "b11".U
//       files(idx) := io.writeBack(i).bits.res
//     }
//   }
// }

// trait SeqRegCommit{ this: SeqRegReal =>


//   val idx = io.commit.map{ x => x.idx }

//   for ( i <- 0 until cmm ) {
//     def m = cmm-1-i

//     io.commit(m).isWriteback := log(phy(m)) === "b11".U

//     when( io.commit(m).isMisPredict | io.commit(m).isAbort ) {
//       /** clear all log to 0, except that archit_ptr point to, may be override */
//       for ( j <- 0 until dp ) { log(j) := Mux( architPtr === j.U, log(j), "b00".U ) }
//     }
//     when( io.commit(m).isMisPredict | io.commit(m).isComfirm ) {
//       /** override the log(clear) */
//       assert( io.commit(m).isWriteback )
//       for ( j <- 0 until dp ) {
//         when(j.U === architPtr ) {log(j) := 0.U} // the log, that used before commit, will be clear to 0
//       }
//       assert( log(idx) === "b11".U, "log which going to commit to will be overrided to \"b11\" if there is an abort in-front." )
//       log(idx) := "b11".U //the log, that going to use after commit should keep to be "b11"
//     }

//     when( io.commit(i).isMisPredict | io.commit(i).isComfirm ) {
//       architPtr := io.commit(i).idx
//     }
//   }
  

//   assert( log(architPtr) === "b11".U, "Assert Failed, archit point to should be b11.U!\n")



// }




class SeqFRM(dp: Int, rnc: Int, wbc: Int, cmm: Int) extends SeqCsr(dw = 3, dp, 0.U, rnc, wbc, cmm)

class SeqFFLAG(dp: Int, rnc: Int, wbc: Int, cmm: Int) extends SeqCsr(dw = 5, dp, 0.U, rnc, wbc, cmm)

class SeqVXRM(dp: Int, rnc: Int, wbc: Int, cmm: Int) extends SeqCsr(dw = 2, dp, 0.U, rnc, wbc, cmm)

class SeqVXSAT(dp: Int, rnc: Int, wbc: Int, cmm: Int) extends SeqCsr(dw = 1, dp, 0.U, rnc, wbc, cmm)

class Seqvstart(dp: Int, rnc: Int, wbc: Int, cmm: Int) extends SeqCsr(dw = 1, dp, 0.U, rnc, wbc, cmm)


