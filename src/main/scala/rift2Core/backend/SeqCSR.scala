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
  val isWroteback = Input(Bool())
}


trait SRegFilesLookup{ this: RegFilesReal =>
  val mollocIdx: Vec[UInt]

  for ( i <- 0 until rnc ) {

    if ( i == 0 ) {
      io.lookup(i).rsp.rs1 := rename_ptr(0)
      io.lookup(i).rsp.rs2 := 0.U
      io.lookup(i).rsp.rs3 := 0.U
      io.lookup(i).rsp.rs4 := 0.U
      io.lookup(i).rsp.rs5 := 0.U
    } else {
      io.lookup(i).rsp.rs1 := rename_ptr(0)
      io.lookup(i).rsp.rs2 := 0.U
      io.lookup(i).rsp.rs3 := 0.U
      io.lookup(i).rsp.rs4 := 0.U
      io.lookup(i).rsp.rs5 := 0.U
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


    val commit = Vec(cmm, Flipped(new SeqReg_Commit_Bundle(dp)))

    val csrOp = Output(Vec(cmm, new Exe_Port))

    val isReady = Output(Vec(dp, Bool()))
  })


  def dwi = dw
  val mdl = Module(new SRegFiles(3+dw, dp, rnc, 1, wbc, cmm){

    val csrOp = IO(Output(Vec(cmm, new Exe_Port)))
    val isReady = IO(Output(Vec(dp, Bool())))

    for( i <- 0 until cmm ) {
      csrOp(i).addr  := addr.U
      csrOp(i).dat_i := files_reg(phy(i)).apply(  dwi-1, 0)
      csrOp(i).op_rw := files_reg(phy(i)).extract(dwi+2)
      csrOp(i).op_rs := files_reg(phy(i)).extract(dwi+1)
      csrOp(i).op_rc := files_reg(phy(i)).extract(dwi+0)
    }
    for( i <- 0 until dp ) {
      isReady(i) := (archit_ptr(0) === i.U)
    }
  })

  io.csrOp  := mdl.csrOp
  io.isReady := mdl.isReady
  assert( PopCount(io.isReady) === 1.U )


  for( i <- 0 until rnc ){
    mdl.io.lookup(i).req.rs1 := 0.U
    mdl.io.lookup(i).req.rs2 := 0.U
    mdl.io.lookup(i).req.rs3 := 0.U
    mdl.io.lookup(i).req.rs4 := 0.U
    mdl.io.lookup(i).req.rs5 := 0.U

    io.lookup(i).rsp := mdl.io.lookup(i).rsp.rs1
  }

  for( i <- 0 until rnc ){
    mdl.io.rename(i).req.valid := io.rename(i).req.valid
    io.rename(i).req.ready := mdl.io.rename(i).req.ready
    mdl.io.rename(i).req.bits.rd0 := 0.U
    mdl.io.rename(i).req.bits.rd1 := DontCare //XRegBase will not accept rd1, So Dontcare

    io.rename(i).rsp := mdl.io.rename(i).rsp.rd0
  }

  for( i <- 0 until 1 ){
    mdl.io.rgReq(i).valid := false.B
    mdl.io.rgReq(i).bits := DontCare
  }

  for( i <- 0 until wbc ){
    mdl.io.exe_writeBack(i).valid := io.writeBack(i).valid
    mdl.io.exe_writeBack(i).bits.res   :=
      Cat(
        io.writeBack(i).bits.op_rw,
        io.writeBack(i).bits.op_rs,
        io.writeBack(i).bits.op_rc,
        io.writeBack(i).bits.dati
      )
    mdl.io.exe_writeBack(i).bits.rd0   := io.writeBack(i).bits.idx
    mdl.io.exe_writeBack(i).bits.rd1   := DontCare
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
    io.commit(i).isWroteback       := mdl.io.commit(i).is_writeback
  }


}


abstract class CRegfilesBase( val rnc: Int, val wbc: Int, val cmm: Int )(implicit p: Parameters) extends RiftModule {
  val io = IO(new Bundle{
    val lookup = Vec( rnc, new SeqReg_Lookup_Bundle(4))
    val rename = Vec( rnc, new SeqReg_Rename_Bundle(4))

    val writeBack = Vec(wbc, Flipped(Valid(new SeqReg_WriteBack_Bundle(64, 4))))

    val commit = Vec(cmm, Flipped(new SeqReg_Commit_Bundle(4)))

    val csrOp= Output(Vec(cmm, new Exe_Port) )

    val isReady = Output(new CSR_LOG_Bundle)
  })

  for( i <- 0 until rnc ){
    io.lookup(i).rsp := DontCare
    io.rename(i).req.ready := false.B
    io.rename(i).rsp := DontCare    
  }

  for( i <- 0 until cmm ){
    io.commit(i).isWroteback := false.B

    io.csrOp(i).addr  := DontCare
    io.csrOp(i).dat_i := DontCare
    io.csrOp(i).op_rw := false.B
    io.csrOp(i).op_rs := false.B
    io.csrOp(i).op_rc := false.B
  }

          

  object CreateRWCSRRegfiles{
    def apply(name: String, dw: Int, dp: Int, addr: Int, rnc: Int, wbc: Int, cmm: Int) = {
      val mdl = Module( new SeqCsr(dw = dw, dp = dp, init = 0.U, addr = addr, rnc, wbc, cmm){
        // override def desiredName = s"${name}-regfiles"
      } )

      for( i <- 0 until rnc ){
        mdl.io.lookup(i).req := DontCare
        mdl.io.rename(i).req.valid := false.B
        mdl.io.rename(i).req.bits  := DontCare

        when( io.lookup(i).req === addr.U ){ io.lookup(i).rsp  := mdl.io.lookup(i).rsp }
        when( io.rename(i).req.bits === addr.U ){ io.rename(i) <> mdl.io.rename(i) }
      }

      for( i <- 0 until wbc ){
          mdl.io.writeBack(i).bits.dati  := DontCare
          mdl.io.writeBack(i).bits.addr  := DontCare
          mdl.io.writeBack(i).bits.op_rw := false.B
          mdl.io.writeBack(i).bits.op_rs := false.B
          mdl.io.writeBack(i).bits.op_rc := false.B
          mdl.io.writeBack(i).bits.idx   := DontCare
          mdl.io.writeBack(i).valid      := false.B

        when( io.writeBack(i).bits.addr === addr.U ){
          mdl.io.writeBack(i) := io.writeBack(i)
        }
      }

      for( i <- 0 until cmm ){
        mdl.io.commit(i).isComfirm := false.B
        mdl.io.commit(i).isAbort := false.B
        mdl.io.commit(i).idx := DontCare
        mdl.io.commit(i).addr := DontCare

        when( io.commit(i).addr === addr.U ){
          mdl.io.commit(i) <> io.commit(i)
          io.csrOp(i) := mdl.io.csrOp(i)      
        }
      }

      io.isReady.elements.map{ ele => 
        ele._1 match{
          case `name` => ele._2 := mdl.io.isReady
          case _      => require(false)          
        }
      }

    }
  }


  object CreateROCSRRegfiles{
    def apply(name: String, dw: Int, dp: Int, addr: Int, rnc: Int, wbc: Int, cmm: Int) = {
      for( i <- 0 until rnc ){
        when( io.lookup(i).req === addr.U ){ io.lookup(i).rsp  := 1.U }
        when( io.rename(i).req.bits === addr.U ){
          io.rename(i).req.ready := true.B
          io.rename(i).rsp := 1.U
        }
      }

      io.isReady.elements.map{ ele => 
        ele._1 match{
          case `name` => ele._2 := VecInit(Seq.fill(dp){true.B})
          case _      => require(false)
        }
      }

    }
  }
}



class CSR_LOG_Bundle(dp: Int = 4) extends Bundle{
  val mstatus = Vec( dp, Bool() )

  val fcsr    = new FCSRBundle

  val sstatus     = UInt(64.W)
  val sedeleg     = UInt(64.W)
  val sideleg     = UInt(64.W)
  val stvec       = new TVecBundle
  val scounteren  = new CounterenBundle
  val sscratch    = UInt(64.W)
  val sepc        = UInt(64.W)
  val scause      = new CauseBundle
  val stval       = UInt(64.W)
  val satp        = new SatpBundle

  val mvendorid   = UInt(64.W)
  val marchid     = UInt(64.W)
  val mimpid      = UInt(64.W)
  val mhartid     = UInt(64.W)
  val mstatus     = new MStatusBundle
  val misa        = UInt(64.W)
  val medeleg     = UInt(64.W)
  val mideleg     = UInt(64.W)
  val mie         = new MSIntBundle
  val mtvec       = new TVecBundle
  val mcounteren  = new CounterenBundle
  val mscratch    = UInt(64.W)
  val mepc        = UInt(64.W)
  val mcause      = new CauseBundle
  val mtval       = UInt(64.W)
  val mip         = new MSIntBundle
  val mtinst      = UInt(64.W)
  val mtval2      = UInt(64.W)
  val mcycle      = UInt(64.W)
  val minstret    = UInt(64.W)
  val mcountinhibit = UInt(64.W)
  val tselect     = UInt(64.W)
  val tdata1      = UInt(64.W)
  val tdata2      = UInt(64.W)
  val tdata3      = UInt(64.W)
  val dcsr        = new DcsrBundle
  val dpc         = UInt(64.W)
  val dscratch0   = UInt(64.W)
  val dscratch1   = UInt(64.W)
  val dscratch2   = UInt(64.W)


  val pmpcfg  = (if(pmpNum==0) { Vec( 1, Vec(8, new PmpcfgBundle) ) } else {Vec( pmpNum, Vec(8, new PmpcfgBundle) )})
  val pmpaddr = (if(pmpNum==0) { Vec( 8, UInt(64.W)) }      else {Vec( 8*pmpNum, UInt(64.W))})



  val mhpmcounter = Vec( 32, UInt(64.W))
  val mhpmevent   = Vec( 32, UInt(64.W))





}

class CRegfiles( rnc: Int, wbc: Int, cmm: Int )(implicit p: Parameters) extends CRegfilesBase( rnc, wbc, cmm ){

  CreateROCSRRegfiles( "mvendorid", dw = 64, dp = 4, 0xf11, rnc, wbc, cmm)
  CreateROCSRRegfiles( "marchid",   dw = 64, dp = 4, 0xf12, rnc, wbc, cmm)
  CreateROCSRRegfiles( "mimpid",    dw = 64, dp = 4, 0xf13, rnc, wbc, cmm)
  CreateROCSRRegfiles( "mhartid",   dw = 64, dp = 4, 0xf14, rnc, wbc, cmm)

  CreateRWCSRRegfiles( "mstatus",   dw = 64, dp = 4, 0x300, rnc, wbc, cmm)
  CreateROCSRRegfiles( "misa",      dw = 64, dp = 4, 0x301, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "medeleg",   dw = 64, dp = 4, 0x302, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "mideleg",   dw = 64, dp = 4, 0x303, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "mie",       dw = 64, dp = 4, 0x304, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "mtvec",     dw = 64, dp = 4, 0x305, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "mcounteren",dw = 64, dp = 4, 0x306, rnc, wbc, cmm)
  
  CreateRWCSRRegfiles( "mscratch",  dw = 64, dp = 4, 0x340, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "mepc",      dw = 64, dp = 4, 0x341, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "mcause",    dw = 64, dp = 4, 0x342, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "mtval",     dw = 64, dp = 4, 0x343, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "mip",       dw = 64, dp = 4, 0x344, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "mtinst",    dw = 64, dp = 4, 0x34A, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "mtval2",    dw = 64, dp = 4, 0x34B, rnc, wbc, cmm)

  for( i <- 0 until pmpNum by 2 ){
    CreateRWCSRRegfiles( s"pmpcfg$i",   dw = 64, dp = 4, (0x3A0 + i), rnc, wbc, cmm)
  }

  for( i <- 0 until pmpNum*8 ){
    CreateRWCSRRegfiles( s"pmpaddr$i",  dw = 64, dp = 4, (0x3B0 + i), rnc, wbc, cmm)
  }

  CreateRWCSRRegfiles( "mcycle",    dw = 64, dp = 4, 0xB00, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "minstret",  dw = 64, dp = 4, 0xB02, rnc, wbc, cmm)

  for( i <- 3 until 32 ){
    CreateRWCSRRegfiles( s"mhpmcounter$i",  dw = 64, dp = 4, 0xB00+i, rnc, wbc, cmm)
  }

  CreateRWCSRRegfiles( "mcountinhibit",  dw = 64, dp = 4, 0x320, rnc, wbc, cmm)

  for( i <- 3 until 32 ){
    CreateRWCSRRegfiles( s"mhpmevent$i",    dw = 64, dp = 4, 0x320+i, rnc, wbc, cmm)
  }


  CreateRWCSRRegfiles( "sstatus",    dw = 64, dp = 4, 0.U, 0x100, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "sedeleg",    dw = 64, dp = 4, 0.U, 0x102, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "sideleg",    dw = 64, dp = 4, 0.U, 0x103, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "sie",        dw = 64, dp = 4, 0.U, 0x104, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "stvec",      dw = 64, dp = 4, 0.U, 0x105, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "scounteren", dw = 64, dp = 4, 0.U, 0x106, rnc, wbc, cmm)

  CreateRWCSRRegfiles( "sscratch",   dw = 64, dp = 4, 0.U, 0x140, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "sepc",       dw = 64, dp = 4, 0.U, 0x141, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "scause",     dw = 64, dp = 4, 0.U, 0x142, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "stval",      dw = 64, dp = 4, 0.U, 0x143, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "sip",        dw = 64, dp = 4, 0.U, 0x144, rnc, wbc, cmm)

  CreateRWCSRRegfiles( "satp",       dw = 64, dp = 4, 0.U, 0x180, rnc, wbc, cmm)



  CreateRWCSRRegfiles( "tselect",    dw = 64, dp = 4, 0.U, 0x7A0, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "tdata1",     dw = 64, dp = 4, 0.U, 0x7A1, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "tdata2",     dw = 64, dp = 4, 0.U, 0x7A2, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "tdata3",     dw = 64, dp = 4, 0.U, 0x7A3, rnc, wbc, cmm)

  CreateRWCSRRegfiles( "dcsr",       dw = 64, dp = 4, 0.U, 0x7B0, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "dpc",        dw = 64, dp = 4, 0.U, 0x7B1, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "dscratch0",  dw = 64, dp = 4, 0.U, 0x7B2, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "dscratch1",  dw = 64, dp = 4, 0.U, 0x7B3, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "dscratch2",  dw = 64, dp = 4, 0.U, 0x7B4, rnc, wbc, cmm)

  CreateRWCSRRegfiles( "fflag",      dw = 64, dp = 4, 0.U, 0x001, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "frm",        dw = 64, dp = 4, 0.U, 0x002, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "fcsr",       dw = 64, dp = 4, 0.U, 0x003, rnc, wbc, cmm)



}