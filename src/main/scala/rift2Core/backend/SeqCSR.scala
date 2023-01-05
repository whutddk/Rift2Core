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
    io.rename(i).req.ready := true.B
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
        if(ele._1.matches(`name`)){
          ele._2 := mdl.io.isReady
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
        if(ele._1.matches(`name`)){
          ele._2 := VecInit(Seq.fill(dp){true.B})
        }
      }

    }
  }
}



class CSR_LOG_Bundle(dp: Int = 4) extends Bundle{
  // val mstatus = Vec( dp, Bool() )

  val mvendorid  = Vec( dp, Bool() )
  val marchid    = Vec( dp, Bool() )
  val mimpid     = Vec( dp, Bool() )
  val mhartid    = Vec( dp, Bool() )
  val mstatus    = Vec( dp, Bool() )
  val misa       = Vec( dp, Bool() )
  val medeleg    = Vec( dp, Bool() )
  val mideleg    = Vec( dp, Bool() )
  val mie        = Vec( dp, Bool() )
  val mtvec      = Vec( dp, Bool() )
  val mcounteren = Vec( dp, Bool() )
  val mscratch   = Vec( dp, Bool() )
  val mepc       = Vec( dp, Bool() )
  val mcause     = Vec( dp, Bool() )
  val mtval      = Vec( dp, Bool() )
  val mip        = Vec( dp, Bool() )
  val mtinst     = Vec( dp, Bool() )
  val mtval2     = Vec( dp, Bool() )

  val pmpcfg0    = Vec( dp, Bool() )
  val pmpcfg2    = Vec( dp, Bool() )
  val pmpcfg4    = Vec( dp, Bool() )
  val pmpcfg6    = Vec( dp, Bool() )
  val pmpcfg8    = Vec( dp, Bool() )
  val pmpcfg10   = Vec( dp, Bool() )
  val pmpcfg12   = Vec( dp, Bool() )
  val pmpcfg14   = Vec( dp, Bool() )

  val pmpaddr0   = Vec( dp, Bool() )
  val pmpaddr1   = Vec( dp, Bool() )
  val pmpaddr2   = Vec( dp, Bool() )
  val pmpaddr3   = Vec( dp, Bool() )
  val pmpaddr4   = Vec( dp, Bool() )
  val pmpaddr5   = Vec( dp, Bool() )
  val pmpaddr6   = Vec( dp, Bool() )
  val pmpaddr7   = Vec( dp, Bool() )
  val pmpaddr8   = Vec( dp, Bool() )
  val pmpaddr9   = Vec( dp, Bool() )
  val pmpaddr10   = Vec( dp, Bool() )
  val pmpaddr11   = Vec( dp, Bool() )
  val pmpaddr12   = Vec( dp, Bool() )
  val pmpaddr13   = Vec( dp, Bool() )
  val pmpaddr14   = Vec( dp, Bool() )
  val pmpaddr15   = Vec( dp, Bool() )
  val pmpaddr16   = Vec( dp, Bool() )
  val pmpaddr17   = Vec( dp, Bool() )
  val pmpaddr18   = Vec( dp, Bool() )
  val pmpaddr19   = Vec( dp, Bool() )
  val pmpaddr20   = Vec( dp, Bool() )
  val pmpaddr21   = Vec( dp, Bool() )
  val pmpaddr22   = Vec( dp, Bool() )
  val pmpaddr23   = Vec( dp, Bool() )
  val pmpaddr24   = Vec( dp, Bool() )
  val pmpaddr25   = Vec( dp, Bool() )
  val pmpaddr26   = Vec( dp, Bool() )
  val pmpaddr27   = Vec( dp, Bool() )
  val pmpaddr28   = Vec( dp, Bool() )
  val pmpaddr29   = Vec( dp, Bool() )
  val pmpaddr30   = Vec( dp, Bool() )
  val pmpaddr31   = Vec( dp, Bool() )
  val pmpaddr32   = Vec( dp, Bool() )
  val pmpaddr33   = Vec( dp, Bool() )
  val pmpaddr34   = Vec( dp, Bool() )
  val pmpaddr35   = Vec( dp, Bool() )
  val pmpaddr36   = Vec( dp, Bool() )
  val pmpaddr37   = Vec( dp, Bool() )
  val pmpaddr38   = Vec( dp, Bool() )
  val pmpaddr39   = Vec( dp, Bool() )
  val pmpaddr40   = Vec( dp, Bool() )
  val pmpaddr41   = Vec( dp, Bool() )
  val pmpaddr42   = Vec( dp, Bool() )
  val pmpaddr43   = Vec( dp, Bool() )
  val pmpaddr44   = Vec( dp, Bool() )
  val pmpaddr45   = Vec( dp, Bool() )
  val pmpaddr46   = Vec( dp, Bool() )
  val pmpaddr47   = Vec( dp, Bool() )
  val pmpaddr48   = Vec( dp, Bool() )
  val pmpaddr49   = Vec( dp, Bool() )
  val pmpaddr50   = Vec( dp, Bool() )
  val pmpaddr51   = Vec( dp, Bool() )
  val pmpaddr52   = Vec( dp, Bool() )
  val pmpaddr53   = Vec( dp, Bool() )
  val pmpaddr54   = Vec( dp, Bool() )
  val pmpaddr55   = Vec( dp, Bool() )
  val pmpaddr56   = Vec( dp, Bool() )
  val pmpaddr57   = Vec( dp, Bool() )
  val pmpaddr58   = Vec( dp, Bool() )
  val pmpaddr59   = Vec( dp, Bool() )
  val pmpaddr60   = Vec( dp, Bool() )
  val pmpaddr61   = Vec( dp, Bool() )
  val pmpaddr62   = Vec( dp, Bool() )
  val pmpaddr63   = Vec( dp, Bool() )

  val mcycle     = Vec( dp, Bool() )
  val minstret   = Vec( dp, Bool() )

  val mhpmcounter3  = Vec( dp, Bool() )
  val mhpmcounter4  = Vec( dp, Bool() )
  val mhpmcounter5  = Vec( dp, Bool() )
  val mhpmcounter6  = Vec( dp, Bool() )
  val mhpmcounter7  = Vec( dp, Bool() )
  val mhpmcounter8  = Vec( dp, Bool() )
  val mhpmcounter9  = Vec( dp, Bool() )
  val mhpmcounter10 = Vec( dp, Bool() )
  val mhpmcounter11 = Vec( dp, Bool() )
  val mhpmcounter12 = Vec( dp, Bool() )
  val mhpmcounter13 = Vec( dp, Bool() )
  val mhpmcounter14 = Vec( dp, Bool() )
  val mhpmcounter15 = Vec( dp, Bool() )
  val mhpmcounter16 = Vec( dp, Bool() )
  val mhpmcounter17 = Vec( dp, Bool() )
  val mhpmcounter18 = Vec( dp, Bool() )
  val mhpmcounter19 = Vec( dp, Bool() )
  val mhpmcounter20 = Vec( dp, Bool() )
  val mhpmcounter21 = Vec( dp, Bool() )
  val mhpmcounter22 = Vec( dp, Bool() )
  val mhpmcounter23 = Vec( dp, Bool() )
  val mhpmcounter24 = Vec( dp, Bool() )
  val mhpmcounter25 = Vec( dp, Bool() )
  val mhpmcounter26 = Vec( dp, Bool() )
  val mhpmcounter27 = Vec( dp, Bool() )
  val mhpmcounter28 = Vec( dp, Bool() )
  val mhpmcounter29 = Vec( dp, Bool() )
  val mhpmcounter30 = Vec( dp, Bool() )
  val mhpmcounter31 = Vec( dp, Bool() )

  val mcountinhibit = Vec( dp, Bool() )

  val mhpmevent3  = Vec( dp, Bool() )
  val mhpmevent4  = Vec( dp, Bool() )
  val mhpmevent5  = Vec( dp, Bool() )
  val mhpmevent6  = Vec( dp, Bool() )
  val mhpmevent7  = Vec( dp, Bool() )
  val mhpmevent8  = Vec( dp, Bool() )
  val mhpmevent9  = Vec( dp, Bool() )
  val mhpmevent10 = Vec( dp, Bool() )
  val mhpmevent11 = Vec( dp, Bool() )
  val mhpmevent12 = Vec( dp, Bool() )
  val mhpmevent13 = Vec( dp, Bool() )
  val mhpmevent14 = Vec( dp, Bool() )
  val mhpmevent15 = Vec( dp, Bool() )
  val mhpmevent16 = Vec( dp, Bool() )
  val mhpmevent17 = Vec( dp, Bool() )
  val mhpmevent18 = Vec( dp, Bool() )
  val mhpmevent19 = Vec( dp, Bool() )
  val mhpmevent20 = Vec( dp, Bool() )
  val mhpmevent21 = Vec( dp, Bool() )
  val mhpmevent22 = Vec( dp, Bool() )
  val mhpmevent23 = Vec( dp, Bool() )
  val mhpmevent24 = Vec( dp, Bool() )
  val mhpmevent25 = Vec( dp, Bool() )
  val mhpmevent26 = Vec( dp, Bool() )
  val mhpmevent27 = Vec( dp, Bool() )
  val mhpmevent28 = Vec( dp, Bool() )
  val mhpmevent29 = Vec( dp, Bool() )
  val mhpmevent30 = Vec( dp, Bool() )
  val mhpmevent31 = Vec( dp, Bool() )


  val sstatus     = Vec( dp, Bool() )
  // val sedeleg     = Vec( dp, Bool() )
  // val sideleg     = Vec( dp, Bool() )
  val sie         = Vec( dp, Bool() )
  val stvec       = Vec( dp, Bool() )
  val scounteren  = Vec( dp, Bool() )
  val sscratch    = Vec( dp, Bool() )
  val sepc        = Vec( dp, Bool() )
  val scause      = Vec( dp, Bool() )
  val stval       = Vec( dp, Bool() )
  val sip         = Vec( dp, Bool() )
  val satp        = Vec( dp, Bool() )
  val tselect     = Vec( dp, Bool() )
  val tdata1      = Vec( dp, Bool() )
  val tdata2      = Vec( dp, Bool() )
  val tdata3      = Vec( dp, Bool() )
  val dcsr        = Vec( dp, Bool() )
  val dpc         = Vec( dp, Bool() )
  val dscratch0   = Vec( dp, Bool() )
  val dscratch1   = Vec( dp, Bool() )
  val dscratch2   = Vec( dp, Bool() )
  val fflags      = Vec( dp, Bool() )
  val frm         = Vec( dp, Bool() )
  val fcsr        = Vec( dp, Bool() )





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

  for( i <- 0 until 16 by 2 ){
    CreateRWCSRRegfiles( s"pmpcfg$i",   dw = 64, dp = 4, (0x3A0 + i), rnc, wbc, cmm)
  }

  for( i <- 0 until 16*8 ){
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


  CreateRWCSRRegfiles( "sstatus",    dw = 64, dp = 4, 0x100, rnc, wbc, cmm)
  // CreateRWCSRRegfiles( "sedeleg",    dw = 64, dp = 4, 0x102, rnc, wbc, cmm)
  // CreateRWCSRRegfiles( "sideleg",    dw = 64, dp = 4, 0x103, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "sie",        dw = 64, dp = 4, 0x104, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "stvec",      dw = 64, dp = 4, 0x105, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "scounteren", dw = 64, dp = 4, 0x106, rnc, wbc, cmm)

  CreateRWCSRRegfiles( "sscratch",   dw = 64, dp = 4, 0x140, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "sepc",       dw = 64, dp = 4, 0x141, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "scause",     dw = 64, dp = 4, 0x142, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "stval",      dw = 64, dp = 4, 0x143, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "sip",        dw = 64, dp = 4, 0x144, rnc, wbc, cmm)

  CreateRWCSRRegfiles( "satp",       dw = 64, dp = 4, 0x180, rnc, wbc, cmm)



  CreateRWCSRRegfiles( "tselect",    dw = 64, dp = 4, 0x7A0, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "tdata1",     dw = 64, dp = 4, 0x7A1, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "tdata2",     dw = 64, dp = 4, 0x7A2, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "tdata3",     dw = 64, dp = 4, 0x7A3, rnc, wbc, cmm)

  CreateRWCSRRegfiles( "dcsr",       dw = 64, dp = 4, 0x7B0, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "dpc",        dw = 64, dp = 4, 0x7B1, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "dscratch0",  dw = 64, dp = 4, 0x7B2, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "dscratch1",  dw = 64, dp = 4, 0x7B3, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "dscratch2",  dw = 64, dp = 4, 0x7B4, rnc, wbc, cmm)

  CreateRWCSRRegfiles( "fflags",     dw = 64, dp = 4, 0x001, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "frm",        dw = 64, dp = 4, 0x002, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "fcsr",       dw = 64, dp = 4, 0x003, rnc, wbc, cmm)



}