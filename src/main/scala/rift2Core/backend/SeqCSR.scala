/*
  Copyright (c) 2020 - 2023 Wuhan University of Technology <295054118@whut.edu.cn>

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
import org.chipsalliance.cde.config._


import chisel3.util.experimental._

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
      io.lookup(i).rsp.vm0 := 0.U
      io.lookup(i).rsp.rs1 := rename_ptr(0)
      io.lookup(i).rsp.rs2 := 0.U
      io.lookup(i).rsp.rs3 := 0.U
    } else {
      io.lookup(i).rsp.vm0 := 0.U
      io.lookup(i).rsp.rs1 := rename_ptr(0)
      io.lookup(i).rsp.rs2 := 0.U
      io.lookup(i).rsp.rs3 := 0.U
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

  class SeqCsrIO extends Bundle{
    val lookup = Vec( rnc, new SeqReg_Lookup_Bundle(dp) )
    val rename = Vec( rnc, new SeqReg_Rename_Bundle(dp))

    val writeBack = Vec(wbc, Flipped(Valid(new SeqReg_WriteBack_Bundle(dw, dp))))


    val commit = Vec(cmm, Flipped(new SeqReg_Commit_Bundle(dp)))

    val csrOp = Output(Vec(cmm, new Exe_Port))

    val isReady = Output(new Log_Status_Bundle(cRegNum))    
  }

  val io: SeqCsrIO = IO(new SeqCsrIO)


  def dwi = dw
  val mdl = Module(new SRegFiles(3+dw, dp, rnc, 1, wbc, cmm) with FlattenInstance {

    val csrOp   = IO(Output(Vec(cmm, new Exe_Port)))

    val isReady = IO(Output(new Log_Status_Bundle(cRegNum)))

    for( i <- 0 until cmm ) {
      csrOp(i).addr  := addr.U
      csrOp(i).dat_i := files(phy(i)).apply(  dwi-1, 0)
      csrOp(i).op_rw := files(phy(i)).extract(dwi+2)
      csrOp(i).op_rs := files(phy(i)).extract(dwi+1)
      csrOp(i).op_rc := files(phy(i)).extract(dwi+0)
    }
    for( i <- 0 until dp ) {
      isReady.isCmm(i) := (archit_ptr(0) === i.U)
    }

    isReady.isFree := archit_ptr(0) === rename_ptr(0)
      
    assert( (log(0) === "b11".U) & (log(archit_ptr(0)) === "b11".U) )
    
  }  )

  io.csrOp  := mdl.csrOp
  io.isReady := mdl.isReady

  assert( PopCount(io.isReady.isCmm) === 1.U )


  for( i <- 0 until rnc ){
    mdl.io.lookup(i).req.vm0 := 0.U
    mdl.io.lookup(i).req.rs1 := 0.U
    mdl.io.lookup(i).req.rs2 := 0.U
    mdl.io.lookup(i).req.rs3 := 0.U

    io.lookup(i).rsp := mdl.io.lookup(i).rsp.rs1
  }

  for( i <- 0 until rnc ){
    mdl.io.rename(i).req.valid := io.rename(i).req.valid
    io.rename(i).req.ready := mdl.io.rename(i).req.ready
    mdl.io.rename(i).req.bits.rd0 := 0.U

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

  class CRegfilesIO extends Bundle{
    val lookup = Vec( rnc, new SeqReg_Lookup_Bundle(cRegNum))
    val rename = Vec( rnc, new SeqReg_Rename_Bundle(cRegNum))

    val writeBack = Vec(wbc, Flipped(Valid(new SeqReg_WriteBack_Bundle(64, cRegNum))))

    val commit = Vec(cmm, Flipped(new SeqReg_Commit_Bundle(cRegNum)))

    val csrOp= Output(Vec(cmm, new Exe_Port) )

    val isReady = Output(new CSR_LOG_Bundle(cRegNum))    
  }

  val io: CRegfilesIO = IO(new CRegfilesIO)

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
      val mdl = Module( new SeqCsr(dw = dw, dp = dp, init = 0.U, addr = addr, 1, 1, cmm) with FlattenInstance{
        // override def desiredName = s"${name}-regfiles"
      }  )

      mdl.io.lookup(0).req := DontCare
      mdl.io.rename(0).req.valid := false.B
      mdl.io.rename(0).req.bits  := DontCare

      mdl.io.writeBack(0).bits.dati  := DontCare
      mdl.io.writeBack(0).bits.addr  := DontCare
      mdl.io.writeBack(0).bits.op_rw := false.B
      mdl.io.writeBack(0).bits.op_rs := false.B
      mdl.io.writeBack(0).bits.op_rc := false.B
      mdl.io.writeBack(0).bits.idx   := DontCare
      mdl.io.writeBack(0).valid      := false.B



      for( i <- 0 until rnc ){
        if( i == 0 ){
          when( io.lookup(0).req      === addr.U ){ io.lookup(0).rsp  := mdl.io.lookup(0).rsp }
          when( io.rename(0).req.bits === addr.U ){ io.rename(0) <> mdl.io.rename(0) }
        } else {
          when( io.lookup(i).req      === addr.U & (( 0 until i ).map{ j => io.lookup(j).req      =/= addr.U }.reduce(_&_)) ){ io.lookup(i).rsp  := mdl.io.lookup(0).rsp }
          when( io.rename(i).req.bits === addr.U ){
            when(( 0 until i ).map{ j => io.rename(j).req.bits =/= addr.U }.reduce(_&_)){
              io.rename(i) <> mdl.io.rename(0)
            } .otherwise{
              io.rename(i).req.ready := false.B
            }
          }
        }
      }

      for( i <- 0 until wbc ){
        if( i == 0 ){
          when( io.writeBack(0).bits.addr === addr.U ){ mdl.io.writeBack(0) := io.writeBack(0) }
        } else {
          when( io.writeBack(i).bits.addr === addr.U & ( (0 until i).map{ j => io.writeBack(j).bits.addr =/= addr.U}.reduce(_&_) ) ){ mdl.io.writeBack(0) := io.writeBack(i) }
        }
      }

      for( i <- 0 until cmm ){
        mdl.io.commit(i).isComfirm := false.B
        mdl.io.commit(i).isAbort := io.commit(i).isAbort
        mdl.io.commit(i).idx := DontCare
        mdl.io.commit(i).addr := DontCare


        when( io.commit(i).addr === addr.U ){
          mdl.io.commit(i).isComfirm := io.commit(i).isComfirm
          mdl.io.commit(i).idx       := io.commit(i).idx
          mdl.io.commit(i).addr      := io.commit(i).addr
          io.commit(i).isWroteback   := mdl.io.commit(i).isWroteback

          io.csrOp(i) := mdl.io.csrOp(i)      
        }
      }

      io.isReady.elements.map{ ele => 
        if(ele._1.matches(`name`)){
          ele._2 := mdl.io.isReady//.asTypeOf(new Log_Status_Bundle)
        }
      }

    }
  }


  object CreateROCSRRegfiles{
    def apply(name: String, dw: Int, dp: Int, addr: Int, rnc: Int, wbc: Int, cmm: Int) = {
      val mdl = Module( new SeqCsr(dw = dw, dp = dp, init = 0.U, addr = addr, 1, 1, cmm) with FlattenInstance {
        // override def desiredName = s"${name}-regfiles"
      }  )

      mdl.io.lookup(0).req := DontCare
      mdl.io.rename(0).req.valid := false.B
      mdl.io.rename(0).req.bits  := DontCare

      mdl.io.writeBack(0).bits.dati  := DontCare
      mdl.io.writeBack(0).bits.addr  := DontCare
      mdl.io.writeBack(0).bits.op_rw := false.B
      mdl.io.writeBack(0).bits.op_rs := false.B
      mdl.io.writeBack(0).bits.op_rc := false.B
      mdl.io.writeBack(0).bits.idx   := DontCare
      mdl.io.writeBack(0).valid      := false.B

      for( i <- 0 until rnc ){
        if( i == 0 ){
          when( io.lookup(0).req      === addr.U ){ io.lookup(0).rsp  := mdl.io.lookup(0).rsp }
          when( io.rename(0).req.bits === addr.U ){ io.rename(0) <> mdl.io.rename(0) }
        } else {
          when( io.lookup(i).req      === addr.U & (( 0 until i ).map{ j => io.lookup(j).req      =/= addr.U }.reduce(_&_)) ){ io.lookup(i).rsp  := mdl.io.lookup(0).rsp }
          when( io.rename(i).req.bits === addr.U ){
            when(( 0 until i ).map{ j => io.rename(j).req.bits =/= addr.U }.reduce(_&_)){
              io.rename(i) <> mdl.io.rename(0)
            } .otherwise{
              io.rename(i).req.ready := false.B
            }
          }
        }       
      }

      for( i <- 0 until wbc ){
        if( i == 0 ){
          when( io.writeBack(0).bits.addr === addr.U ){
            mdl.io.writeBack(0).bits.dati  := DontCare
            mdl.io.writeBack(0).bits.addr  := addr.U
            mdl.io.writeBack(0).bits.op_rw := io.writeBack(0).bits.op_rw
            mdl.io.writeBack(0).bits.op_rs := io.writeBack(0).bits.op_rs
            mdl.io.writeBack(0).bits.op_rc := io.writeBack(0).bits.op_rc
            mdl.io.writeBack(0).bits.idx   := io.writeBack(0).bits.idx
            mdl.io.writeBack(0).valid      := io.writeBack(0).valid
          }
        } else {
          when( io.writeBack(i).bits.addr === addr.U & ( (0 until i).map{ j => io.writeBack(j).bits.addr =/= addr.U}.reduce(_&_) ) ){
            mdl.io.writeBack(0).bits.dati  := DontCare
            mdl.io.writeBack(0).bits.addr  := addr.U
            mdl.io.writeBack(0).bits.op_rw := io.writeBack(i).bits.op_rw
            mdl.io.writeBack(0).bits.op_rs := io.writeBack(i).bits.op_rs
            mdl.io.writeBack(0).bits.op_rc := io.writeBack(i).bits.op_rc
            mdl.io.writeBack(0).bits.idx   := io.writeBack(i).bits.idx
            mdl.io.writeBack(0).valid      := io.writeBack(i).valid
          }
        }
      }


      for( i <- 0 until cmm ){
        mdl.io.commit(i).isComfirm := false.B
        mdl.io.commit(i).isAbort := io.commit(i).isAbort
        mdl.io.commit(i).idx := DontCare
        mdl.io.commit(i).addr := DontCare


        when( io.commit(i).addr === addr.U ){
          mdl.io.commit(i).isComfirm := io.commit(i).isComfirm
          mdl.io.commit(i).idx       := io.commit(i).idx
          mdl.io.commit(i).addr      := io.commit(i).addr
          io.commit(i).isWroteback   := mdl.io.commit(i).isWroteback

          io.csrOp(i).addr  := addr.U
          io.csrOp(i).dat_i := DontCare
          io.csrOp(i).op_rw := mdl.io.csrOp(i).op_rw
          io.csrOp(i).op_rs := mdl.io.csrOp(i).op_rs
          io.csrOp(i).op_rc := mdl.io.csrOp(i).op_rc
        }

      }

      io.isReady.elements.map{ ele => 
        if(ele._1.matches(`name`)){
          ele._2  := mdl.io.isReady//.asTypeOf(new Log_Status_Bundle)
        }
      }
    }

  }
}


class Log_Status_Bundle(dp: Int) extends Bundle{
  val isCmm = Vec( dp, Bool() )
  val isFree  = Bool()
}

class CSR_LOG_Bundle(dp: Int) extends Bundle{


  // val mstatus = Vec( dp, Bool() )

  val mvendorid  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val marchid    = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mimpid     = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhartid    = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mstatus    = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val misa       = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val medeleg    = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mideleg    = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mie        = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mtvec      = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mcounteren = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mscratch   = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mepc       = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mcause     = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mtval      = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mip        = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mtinst     = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mtval2     = new Log_Status_Bundle(dp) //Vec( dp, Bool() )

  val pmpcfg0    = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpcfg2    = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpcfg4    = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpcfg6    = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpcfg8    = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpcfg10   = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpcfg12   = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpcfg14   = new Log_Status_Bundle(dp) //Vec( dp, Bool() )

  val pmpaddr0   = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr1   = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr2   = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr3   = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr4   = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr5   = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr6   = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr7   = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr8   = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr9   = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr10  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr11  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr12  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr13  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr14  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr15  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr16  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr17  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr18  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr19  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr20  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr21  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr22  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr23  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr24  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr25  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr26  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr27  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr28  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr29  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr30  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr31  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr32  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr33  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr34  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr35  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr36  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr37  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr38  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr39  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr40  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr41  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr42  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr43  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr44  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr45  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr46  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr47  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr48  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr49  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr50  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr51  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr52  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr53  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr54  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr55  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr56  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr57  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr58  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr59  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr60  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr61  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr62  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val pmpaddr63  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )

  val mcycle     = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val minstret   = new Log_Status_Bundle(dp) //Vec( dp, Bool() )

  val mhpmcounter3  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmcounter4  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmcounter5  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmcounter6  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmcounter7  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmcounter8  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmcounter9  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmcounter10 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmcounter11 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmcounter12 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmcounter13 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmcounter14 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmcounter15 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmcounter16 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmcounter17 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmcounter18 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmcounter19 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmcounter20 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmcounter21 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmcounter22 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmcounter23 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmcounter24 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmcounter25 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmcounter26 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmcounter27 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmcounter28 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmcounter29 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmcounter30 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmcounter31 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )

  val mcountinhibit = new Log_Status_Bundle(dp) //Vec( dp, Bool() )

  val mhpmevent3  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmevent4  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmevent5  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmevent6  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmevent7  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmevent8  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmevent9  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmevent10 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmevent11 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmevent12 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmevent13 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmevent14 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmevent15 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmevent16 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmevent17 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmevent18 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmevent19 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmevent20 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmevent21 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmevent22 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmevent23 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmevent24 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmevent25 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmevent26 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmevent27 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmevent28 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmevent29 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmevent30 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val mhpmevent31 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )


  val sstatus     = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  // val sedeleg     = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  // val sideleg     = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val sie         = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val stvec       = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val scounteren  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val sscratch    = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val sepc        = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val scause      = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val stval       = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val sip         = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val satp        = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val tselect     = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val tdata1      = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val tdata2      = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val tdata3      = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val dcsr        = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val dpc         = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val dscratch0   = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val dscratch1   = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val dscratch2   = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val fflags      = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val frm         = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val fcsr        = new Log_Status_Bundle(dp) //Vec( dp, Bool() )

  val cycle   = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val time    = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val instret = new Log_Status_Bundle(dp) //Vec( dp, Bool() )

  val hpmcounter3  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val hpmcounter4  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val hpmcounter5  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val hpmcounter6  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val hpmcounter7  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val hpmcounter8  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val hpmcounter9  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val hpmcounter10 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val hpmcounter11 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val hpmcounter12 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val hpmcounter13 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val hpmcounter14 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val hpmcounter15 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val hpmcounter16 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val hpmcounter17 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val hpmcounter18 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val hpmcounter19 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val hpmcounter20 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val hpmcounter21 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val hpmcounter22 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val hpmcounter23 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val hpmcounter24 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val hpmcounter25 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val hpmcounter26 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val hpmcounter27 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val hpmcounter28 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val hpmcounter29 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val hpmcounter30 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val hpmcounter31 = new Log_Status_Bundle(dp) //Vec( dp, Bool() )

  val lsuExc  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )

  val vstart  = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val vxsat   = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val vxrm    = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val vcsr    = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val vl      = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val vtype   = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
  val vlenb   = new Log_Status_Bundle(dp) //Vec( dp, Bool() )

  val vConfig = new Log_Status_Bundle(dp) //Vec( dp, Bool() )
}

class CRegfiles( rnc: Int, wbc: Int, cmm: Int )(implicit p: Parameters) extends CRegfilesBase( rnc, wbc, cmm ) with FlattenInstance{

  // println("Warning, writing to the read-only csr will take no effort. And No illegal exception will be caused!\n")

  CreateROCSRRegfiles( "mvendorid", dw = 64, dp = cRegNum, 0xf11, rnc, wbc, cmm)
  CreateROCSRRegfiles( "marchid",   dw = 64, dp = cRegNum, 0xf12, rnc, wbc, cmm)
  CreateROCSRRegfiles( "mimpid",    dw = 64, dp = cRegNum, 0xf13, rnc, wbc, cmm)
  CreateROCSRRegfiles( "mhartid",   dw = 64, dp = cRegNum, 0xf14, rnc, wbc, cmm)

  CreateRWCSRRegfiles( "mstatus",   dw = 64, dp = cRegNum, 0x300, rnc, wbc, cmm)
  CreateROCSRRegfiles( "misa",      dw = 64, dp = cRegNum, 0x301, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "medeleg",   dw = 64, dp = cRegNum, 0x302, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "mideleg",   dw = 64, dp = cRegNum, 0x303, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "mie",       dw = 64, dp = cRegNum, 0x304, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "mtvec",     dw = 64, dp = cRegNum, 0x305, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "mcounteren",dw = 64, dp = cRegNum, 0x306, rnc, wbc, cmm)
  
  CreateRWCSRRegfiles( "mscratch",  dw = 64, dp = cRegNum, 0x340, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "mepc",      dw = 64, dp = cRegNum, 0x341, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "mcause",    dw = 64, dp = cRegNum, 0x342, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "mtval",     dw = 64, dp = cRegNum, 0x343, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "mip",       dw = 64, dp = cRegNum, 0x344, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "mtinst",    dw = 64, dp = cRegNum, 0x34A, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "mtval2",    dw = 64, dp = cRegNum, 0x34B, rnc, wbc, cmm)

  for( i <- 0 until 16 by 2 ){
    CreateRWCSRRegfiles( s"pmpcfg$i",   dw = 64, dp = cRegNum, (0x3A0 + i), rnc, wbc, cmm)
  }

  for( i <- 0 until 16*8 ){
    CreateRWCSRRegfiles( s"pmpaddr$i",  dw = 64, dp = cRegNum, (0x3B0 + i), rnc, wbc, cmm)
  }

  CreateRWCSRRegfiles( "mcycle",    dw = 64, dp = cRegNum, 0xB00, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "minstret",  dw = 64, dp = cRegNum, 0xB02, rnc, wbc, cmm)

  for( i <- 3 until 32 ){
    CreateRWCSRRegfiles( s"mhpmcounter$i",  dw = 64, dp = cRegNum, 0xB00+i, rnc, wbc, cmm)
  }

  CreateRWCSRRegfiles( "mcountinhibit",  dw = 64, dp = cRegNum, 0x320, rnc, wbc, cmm)

  for( i <- 3 until 32 ){
    CreateRWCSRRegfiles( s"mhpmevent$i",    dw = 64, dp = cRegNum, 0x320+i, rnc, wbc, cmm)
  }


  CreateRWCSRRegfiles( "sstatus",    dw = 64, dp = cRegNum, 0x100, rnc, wbc, cmm)
  // CreateRWCSRRegfiles( "sedeleg",    dw = 64, dp = cRegNum, 0x102, rnc, wbc, cmm)
  // CreateRWCSRRegfiles( "sideleg",    dw = 64, dp = cRegNum, 0x103, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "sie",        dw = 64, dp = cRegNum, 0x104, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "stvec",      dw = 64, dp = cRegNum, 0x105, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "scounteren", dw = 64, dp = cRegNum, 0x106, rnc, wbc, cmm)

  CreateRWCSRRegfiles( "sscratch",   dw = 64, dp = cRegNum, 0x140, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "sepc",       dw = 64, dp = cRegNum, 0x141, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "scause",     dw = 64, dp = cRegNum, 0x142, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "stval",      dw = 64, dp = cRegNum, 0x143, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "sip",        dw = 64, dp = cRegNum, 0x144, rnc, wbc, cmm)

  CreateRWCSRRegfiles( "satp",       dw = 64, dp = cRegNum, 0x180, rnc, wbc, cmm)



  CreateRWCSRRegfiles( "tselect",    dw = 64, dp = cRegNum, 0x7A0, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "tdata1",     dw = 64, dp = cRegNum, 0x7A1, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "tdata2",     dw = 64, dp = cRegNum, 0x7A2, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "tdata3",     dw = 64, dp = cRegNum, 0x7A3, rnc, wbc, cmm)

  CreateRWCSRRegfiles( "dcsr",       dw = 64, dp = cRegNum, 0x7B0, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "dpc",        dw = 64, dp = cRegNum, 0x7B1, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "dscratch0",  dw = 64, dp = cRegNum, 0x7B2, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "dscratch1",  dw = 64, dp = cRegNum, 0x7B3, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "dscratch2",  dw = 64, dp = cRegNum, 0x7B4, rnc, wbc, cmm)





  CreateROCSRRegfiles( "cycle",      dw = 64, dp = cRegNum, 0xC00, rnc, wbc, cmm)
  CreateROCSRRegfiles( "time",       dw = 64, dp = cRegNum, 0xC01, rnc, wbc, cmm)
  CreateROCSRRegfiles( "instret",    dw = 64, dp = cRegNum, 0xC02, rnc, wbc, cmm)

  for( i <- 3 until 32 ){
    CreateROCSRRegfiles( s"hpmcounter$i",  dw = 64, dp = cRegNum, 0xC00+i, rnc, wbc, cmm)
  }


  CreateRWCSRRegfiles( "fflags",     dw = 64, dp = cRegNum, 0x001, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "frm",        dw = 64, dp = cRegNum, 0x002, rnc, wbc, cmm)
  CreateRWCSRRegfiles( "fcsr",       dw = 64, dp = cRegNum, 0x003, rnc, wbc, cmm)

  CreateRWCSRRegfiles( "vstart",  dw = 64, dp = cRegNum, 0x008, rnc, wbc, cmm )
  CreateRWCSRRegfiles( "vxsat",   dw = 64, dp = cRegNum, 0x009, rnc, wbc, cmm )
  CreateRWCSRRegfiles( "vxrm",    dw = 64, dp = cRegNum, 0x00A, rnc, wbc, cmm )
  CreateRWCSRRegfiles( "vcsr",    dw = 64, dp = cRegNum, 0x00F, rnc, wbc, cmm )
  CreateROCSRRegfiles( "vl",      dw = 64, dp = cRegNum, 0xC20, rnc, wbc, cmm )
  CreateROCSRRegfiles( "vtype",   dw = 64, dp = cRegNum, 0xC21, rnc, wbc, cmm )
  CreateROCSRRegfiles( "vlenb",   dw = 64, dp = cRegNum, 0xC22, rnc, wbc, cmm )


  CreateRWCSRRegfiles( "vConfig",   dw = 64, dp = cRegNum, 0xFFE, rnc, wbc, cmm )
  CreateRWCSRRegfiles( "lsuExc",    dw = 64, dp = cRegNum, 0xFFF, rnc, wbc, cmm)

  
}
