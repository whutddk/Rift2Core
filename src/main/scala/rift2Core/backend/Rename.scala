
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
import base._
import rift2Core.define._
import rift2Chip._
import org.chipsalliance.cde.config._


abstract class RenameBase()(implicit p: Parameters) extends RiftModule {

  class RenameIO extends Bundle{
    val rnReq = Vec(rnChn, Flipped(new DecoupledIO(new IF4_Bundle)))
    val rnRsp = Vec(rnChn, (new DecoupledIO(new Dpt_info)))

    val xLookup = Vec( rnChn, new Lookup_Bundle )
    val xRename = Vec( rnChn, new Rename_Bundle )

    val fLookup = Vec( rnChn, new Lookup_Bundle )
    val fRename = Vec( rnChn, new Rename_Bundle )

    val xpuCsrMolloc    = Vec(rnChn, Decoupled(UInt(12.W)))
    val fpuCsrMolloc    = Vec(rnChn, Decoupled(Bool()))

    val rod_i = Vec(cmChn,new DecoupledIO(new Info_reorder_i))
  }

  val io: RenameIO = IO(new RenameIO)

  val rnRspFifo = Module(new MultiPortFifo(new Dpt_info, aw = (if(!isMinArea) 4 else 1 ), rnChn, rnChn))
  rnRspFifo.io.deq <> io.rnRsp

  val reOrder_fifo_i = {
    val mdl = Module(new MultiPortFifo(new Info_reorder_i, aw = (if(!isMinArea) 4 else 1 ), rnChn, cmChn))
    mdl.io.deq <> io.rod_i
    mdl
  }

  val reg_phy = Wire(Vec(rnChn, new Reg_PHY ) )

}

trait RenameMalloc { this: RenameBase =>
  for ( i <- 0 until rnChn ) {

    rnRspFifo.io.enq(i).valid :=
      io.rnReq(i).fire & ( 0 to i ).map{ j => ~io.rnReq(i).bits.is_privil_dpt }.foldLeft(false.B)(_|_)  //~io.rnReq(i).bits.is_privil_dpt

    rnRspFifo.io.enq(i).bits.aluIsa    := io.rnReq(i).bits.aluIsa
    rnRspFifo.io.enq(i).bits.bruIsa    := io.rnReq(i).bits.bruIsa
    rnRspFifo.io.enq(i).bits.lsuIsa    := io.rnReq(i).bits.lsuIsa
    rnRspFifo.io.enq(i).bits.csrIsa    := io.rnReq(i).bits.csrIsa
    rnRspFifo.io.enq(i).bits.mulIsa    := io.rnReq(i).bits.mulIsa
    rnRspFifo.io.enq(i).bits.vecIsa  := io.rnReq(i).bits.vecIsa
    rnRspFifo.io.enq(i).bits.privil_isa := 0.U.asTypeOf( new Privil_isa )
    rnRspFifo.io.enq(i).bits.fpuIsa    := io.rnReq(i).bits.fpuIsa
    rnRspFifo.io.enq(i).bits.param      := io.rnReq(i).bits.param
    rnRspFifo.io.enq(i).bits.phy        := reg_phy(i)

            when( io.rnReq(i).bits.fpuIsa.is_fpu ) {
              when(~io.rnReq(i).bits.fpuIsa.hasTwoRs) { rnRspFifo.io.enq(i).bits.phy.rs2 := 0.U }
              when(~io.rnReq(i).bits.fpuIsa.hasThreeRs) { rnRspFifo.io.enq(i).bits.phy.rs3 := 0.U }      
            }

    io.xRename(i).req.valid    := io.rnReq(i).fire & io.rnReq(i).bits.isXwb
    io.xRename(i).req.bits.rd0 := io.rnReq(i).bits.param.raw.rd0

    io.fRename(i).req.valid    := io.rnReq(i).fire & io.rnReq(i).bits.isFwb
    io.fRename(i).req.bits.rd0 := io.rnReq(i).bits.param.raw.rd0

    for ( j <- 0 until rnChn ) { assert( PopCount( Seq(io.vRename(j).req.fire, io.fRename(j).req.fire, io.xRename(j).req.fire)) <= 1.U, "Assert Failed, rename should be one-hot" ) }

    reg_phy(i).rd0 := 
      Mux1H(
        Seq(io.xRename(i).req.fire -> io.xRename(i).rsp.rd0) ++
        ( if(fpuNum > 0) { Seq(io.fRename(i).req.fire -> io.fRename(i).rsp.rd0) }                              else { Seq() } ) ++
        ( if(hasVector)  { Seq(io.rnReq(i).fire & io.rnReq(i).bits.is_vwb -> io.rnReq(i).bits.param.raw.rd0) } else { Seq() } ) //vector dont need to rename
      )
  }
}




trait WriteBackLookup {  this: RenameBase =>
  for ( i <- 0 until rnChn ) {
    io.xLookup(i).req.vm0 := DontCare
    io.xLookup(i).req.rs1 := io.rnReq(i).bits.param.raw.rs1
    io.xLookup(i).req.rs2 := io.rnReq(i).bits.param.raw.rs2
    io.xLookup(i).req.rs3 := 0.U

    if( fpuNum > 0 ) {
      io.fLookup(i).req.vm0 := DontCare
      io.fLookup(i).req.rs1 := io.rnReq(i).bits.param.raw.rs1
      io.fLookup(i).req.rs2 := io.rnReq(i).bits.param.raw.rs2
      io.fLookup(i).req.rs3 := io.rnReq(i).bits.param.raw.rs3      
    } else {
      io.fLookup(i).req.vm0 := DontCare
      io.fLookup(i).req.rs1 := 0.U
      io.fLookup(i).req.rs2 := 0.U
      io.fLookup(i).req.rs3 := 0.U
    }

    reg_phy(i).vm0 :=
      (if( hasVector ) { io.rnReq(i).bits.param.raw.vm0 } else { 0.U })

    reg_phy(i).rs1 :=
      Mux1H( Seq(
        io.rnReq(i).bits.isRS1 -> io.xLookup(i).rsp.rs1,
        io.rnReq(i).bits.isFS1 -> io.fLookup(i).rsp.rs1,
        io.rnReq(i).bits.isVS1 -> io.rnReq(i).bits.param.raw.rs1, //dont lookup
      ))

    reg_phy(i).rs2 :=
      Mux1H(Seq(
        io.rnReq(i).bits.isRS2 -> io.xLookup(i).rsp.rs2,
        io.rnReq(i).bits.isFS2 -> io.fLookup(i).rsp.rs2,
        io.rnReq(i).bits.isVS2 -> io.rnReq(i).bits.param.raw.rs2, //dont lookup
      ))

    reg_phy(i).rs3 :=
      Mux1H(Seq(
        io.rnReq(i).bits.isFS3 -> io.fLookup(i).rsp.rs3,
        io.rnReq(i).bits.isVS3 -> io.rnReq(i).bits.param.raw.rs3, //dont lookup
      ))

  }
}



trait LoadRob{ this: RenameBase =>
  reOrder_fifo_i.io.flush := false.B
  for ( i <- 0 until rnChn ){
    reOrder_fifo_i.io.enq(i).valid := io.rnReq(i).fire

    reOrder_fifo_i.io.enq(i).bits.pc             := io.rnReq(i).bits.param.pc
    reOrder_fifo_i.io.enq(i).bits.rd0_raw        := io.rnReq(i).bits.param.raw.rd0
    reOrder_fifo_i.io.enq(i).bits.rd0_phy        := reg_phy(i).rd0
    reOrder_fifo_i.io.enq(i).bits.is_branch      := io.rnReq(i).bits.bruIsa.is_branch
    reOrder_fifo_i.io.enq(i).bits.is_jalr        := io.rnReq(i).bits.bruIsa.jalr
    reOrder_fifo_i.io.enq(i).bits.is_lu          := io.rnReq(i).bits.lsuIsa.is_lu
    reOrder_fifo_i.io.enq(i).bits.is_su          := io.rnReq(i).bits.lsuIsa.is_su
    reOrder_fifo_i.io.enq(i).bits.is_amo         := io.rnReq(i).bits.lsuIsa.is_amo
    reOrder_fifo_i.io.enq(i).bits.is_fence       := io.rnReq(i).bits.lsuIsa.fence
    reOrder_fifo_i.io.enq(i).bits.is_fence_i     := io.rnReq(i).bits.lsuIsa.fence_i
    reOrder_fifo_i.io.enq(i).bits.is_sfence_vma  := io.rnReq(i).bits.lsuIsa.sfence_vma
    reOrder_fifo_i.io.enq(i).bits.is_wfi         := io.rnReq(i).bits.aluIsa.wfi
    reOrder_fifo_i.io.enq(i).bits.is_csr         := io.rnReq(i).bits.csrIsa.is_csr
    reOrder_fifo_i.io.enq(i).bits.is_fcsr        := io.rnReq(i).bits.fpuIsa.is_fpu
    reOrder_fifo_i.io.enq(i).bits.is_fpu         := io.rnReq(i).bits.fpuIsa.is_fpu | io.rnReq(i).bits.lsuIsa.is_fpu
    reOrder_fifo_i.io.enq(i).bits.privil         := io.rnReq(i).bits.privil_isa
    reOrder_fifo_i.io.enq(i).bits.is_illeage     := io.rnReq(i).bits.is_illeage
    reOrder_fifo_i.io.enq(i).bits.is_rvc         := io.rnReq(i).bits.param.is_rvc

    reOrder_fifo_i.io.enq(i).bits.isXwb := io.rnReq(i).bits.isXwb
    reOrder_fifo_i.io.enq(i).bits.isFwb := io.rnReq(i).bits.isFwb
    reOrder_fifo_i.io.enq(i).bits.isVwb := io.rnReq(i).bits.isVwb
      
    reOrder_fifo_i.io.enq(i).bits.isVector := io.rnReq(i).bits.vecIsa.isVector | io.rnReq(i).bits.lsuIsa.is_vls
    reOrder_fifo_i.io.enq(i).bits.isVLoad  := io.rnReq(i).bits.lsuIsa.isVLoad
    reOrder_fifo_i.io.enq(i).bits.isVStore := io.rnReq(i).bits.lsuIsa.isVStore
    reOrder_fifo_i.io.enq(i).bits.isFoF    := io.rnReq(i).bits.lsuIsa.vleNff
    reOrder_fifo_i.io.enq(i).bits.vlCnt    := io.rnReq(i).bits.vAttach.vlCnt


  }

}


trait CSRMalloc{ this: RenameBase =>
  for ( i <- 0 until rnChn ) {
    io.xpuCsrMolloc(i).bits := 
      Mux( io.rnReq(i).bits.csrIsa.is_csr, io.rnReq(i).bits.param.imm, 0.U)

    io.xpuCsrMolloc(i).valid :=
      io.rnReq(i).fire & io.rnReq(i).bits.csrIsa.is_csr

    io.fpuCsrMolloc(i).bits := DontCare

    io.fpuCsrMolloc(i).valid :=
      io.rnReq(i).fire & io.rnReq(i).bits.fpuIsa.is_fpu     
  }

}


trait RenameFeatureCheck { this: RenameBase =>
  if ( fpuNum > 0 ) {
  } else {
    for ( i <- 0 until rnChn ) {
      when( io.rnReq(i).fire ) {
        assert(io.rnReq(i).bits.fpuIsa.is_fpu === false.B)
        assert(io.rnReq(i).bits.isFwb === false.B)
      }
    }
  }
  if( mulNum > 0 ) {

  } else {
    for ( i <- 0 until rnChn ) {
      when( io.rnReq(i).fire ) {
        assert(io.rnReq(i).bits.mulIsa.is_mulDiv === false.B, "Assert Failed at Rename, MulDiv is not supported in this Version!")
      }
    }    
  }

}








class Rename()(implicit p: Parameters) extends RenameBase
with RenameMalloc
with WriteBackLookup
with CSRMalloc
with LoadRob
with RenameFeatureCheck {

  rnRspFifo.io.flush := false.B


  for (i <- 0 until rnChn ) yield {
    io.rnReq(i).ready := (
      for ( j <- 0 to i by 1 ) yield {
        io.xRename(j).req.ready &
        io.fRename(j).req.ready &
        reOrder_fifo_i.io.enq(j).ready &
        (io.xpuCsrMolloc(j).ready | ~io.rnReq(j).bits.csrIsa.is_csr) & 
        (io.fpuCsrMolloc(j).ready | ~io.rnReq(j).bits.fpuIsa.is_fpu) & 
        rnRspFifo.io.enq(j).ready
      }
    ).reduce(_&_)
  }
}

