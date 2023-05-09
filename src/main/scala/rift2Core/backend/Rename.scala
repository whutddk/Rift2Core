
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

  def Pkg_Rename_Bundle( instr: Info_instruction, rename: Reg_PHY): Dpt_info = {
    val res = Wire(new Dpt_info)

    res.alu_isa    := instr.alu_isa
    res.bru_isa    := instr.bru_isa
    res.lsu_isa    := instr.lsu_isa
    res.csr_isa    := instr.csr_isa
    res.mul_isa    := instr.mul_isa
    res.vectorIsa  := instr.vectorIsa
    res.privil_isa := 0.U.asTypeOf( new Privil_isa )
    res.fpu_isa    := instr.fpu_isa
    res.param      := instr.param
    res.phy        := rename

                    when( instr.fpu_isa.is_fpu ) {
                      when(~instr.fpu_isa.hasTwoRs) { res.phy.rs2 := 0.U }
                      when(~instr.fpu_isa.hasThreeRs) { res.phy.rs3 := 0.U }      
                    }

    return res
  }


  def Pkg_rod_i(instr:Info_instruction, rename: Reg_PHY): Info_reorder_i = {
    val res = Wire(new Info_reorder_i)

      res.pc             := instr.param.pc
      res.rd0_raw        := instr.param.raw.rd0
      res.rd0_phy        := rename.rd0
      res.is_branch      := instr.bru_isa.is_branch
      res.is_jalr        := instr.bru_isa.jalr
      res.is_lu          := instr.lsu_isa.is_lu
      res.is_su          := instr.lsu_isa.is_su
      res.is_amo         := instr.lsu_isa.is_amo
      res.is_fence       := instr.lsu_isa.fence
      res.is_fence_i     := instr.lsu_isa.fence_i
      res.is_sfence_vma  := instr.lsu_isa.sfence_vma
      res.is_wfi         := instr.alu_isa.wfi
      res.is_csr         := instr.csr_isa.is_csr
      res.is_fcsr        := instr.fpu_isa.is_fpu
      res.is_fpu         := instr.fpu_isa.is_fpu | instr.lsu_isa.is_fpu
      res.privil         := instr.privil_isa
      res.is_illeage     := instr.is_illeage
      res.is_rvc         := instr.param.is_rvc

      res.isXcmm := instr.is_iwb
      res.isFcmm := instr.is_fwb
      res.isVcmm := instr.isVwb

      res.isVector := instr.vectorIsa.isVector | instr.lsu_isa.is_vls

      res.isLast   := (if(hasVector){ instr.vAttach.get.isLast } else { true.B })
      

    return res
  }
}

trait RenameMalloc { this: RenameBase =>
  for ( i <- 0 until rnChn ) {

    rnRspFifo.io.enq(i).valid :=
      io.rnReq(i).fire & ( 0 to i ).map{ j => ~io.rnReq(i).bits.is_privil_dpt }.foldLeft(false.B)(_|_)  //~io.rnReq(i).bits.is_privil_dpt

    rnRspFifo.io.enq(i).bits  := Pkg_Rename_Bundle(io.rnReq(i).bits, reg_phy(i))
            

    io.xRename(i).req.valid    := io.rnReq(i).fire & io.rnReq(i).bits.is_iwb
    io.xRename(i).req.bits.rd0 := io.rnReq(i).bits.param.raw.rd0

    io.fRename(i).req.valid    := io.rnReq(i).fire & io.rnReq(i).bits.is_fwb
    io.fRename(i).req.bits.rd0 := io.rnReq(i).bits.param.raw.rd0

    for ( j <- 0 until rnChn ) { assert( PopCount( Seq(io.vRename(j).req.fire, io.fRename(j).req.fire, io.xRename(j).req.fire)) <= 1.U, "Assert Failed, rename should be one-hot" ) }

    reg_phy(i).rd0 := 
      Mux1H(
        Seq(io.xRename(i).req.fire -> io.xRename(i).rsp.rd0) ++
        (if(fpuNum > 0) { Seq(io.fRename(i).req.fire -> io.fRename(i).rsp.rd0) } else { Seq() } )
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
      (if( hasVector ) { io.vLookup(i).rsp.vm0 } else { 0.U })

    reg_phy(i).rs1 :=
      MuxCase( io.xLookup(i).rsp.rs1, Seq() ++
        ( if( fpuNum > 0 ) { Seq(io.rnReq(i).bits.fpu_isa.is_fop -> io.fLookup(i).rsp.rs1) } else Seq() ) ++ 
      )

    reg_phy(i).rs2 :=
      MuxCase( io.xLookup(i).rsp.rs2, Seq() ++ 
        ( if( fpuNum > 0 ) { Seq((io.rnReq(i).bits.fpu_isa.is_fop  | io.rnReq(i).bits.lsu_isa.isFStore) -> io.fLookup(i).rsp.rs2) } else {Seq()} )
      )

    reg_phy(i).rs3 :=
      MuxCase(0.U, Seq() ++ 
        (if( fpuNum > 0 ) { Seq( io.rnReq(i).bits.fpu_isa.is_fop                                    -> io.fLookup(i).rsp.rs3)} else {Seq()} )
      )

  }
}



trait LoadRob{ this: RenameBase =>
  reOrder_fifo_i.io.flush := false.B
  for ( i <- 0 until rnChn ){
    reOrder_fifo_i.io.enq(i).valid := io.rnReq(i).fire
    reOrder_fifo_i.io.enq(i).bits  := Pkg_rod_i(io.rnReq(i).bits, reg_phy(i))    
  }

}


trait CSRMalloc{ this: RenameBase =>
  for ( i <- 0 until rnChn ) {
    io.xpuCsrMolloc(i).bits := 
      Mux( io.rnReq(i).bits.csr_isa.is_csr, io.rnReq(i).bits.param.imm, 0.U)

    io.xpuCsrMolloc(i).valid :=
      io.rnReq(i).fire & io.rnReq(i).bits.csr_isa.is_csr

    io.fpuCsrMolloc(i).bits := DontCare

    io.fpuCsrMolloc(i).valid :=
      io.rnReq(i).fire & io.rnReq(i).bits.fpu_isa.is_fpu     
  }

}


















trait RenameFeatureCheck { this: RenameBase =>
  if ( fpuNum > 0 ) {
  } else {
    for ( i <- 0 until rnChn ) {
      when( io.rnReq(i).fire ) {
        assert(io.rnReq(i).bits.fpu_isa.is_fpu === false.B)
        assert(io.rnReq(i).bits.is_fwb === false.B)
      }
    }
  }
  if( mulNum > 0 ) {

  } else {
    for ( i <- 0 until rnChn ) {
      when( io.rnReq(i).fire ) {
        assert(io.rnReq(i).bits.mul_isa.is_mulDiv === false.B, "Assert Failed at Rename, MulDiv is not supported in this Version!")
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
        (io.xpuCsrMolloc(j).ready | ~io.rnReq(j).bits.csr_isa.is_csr) & 
        (io.fpuCsrMolloc(j).ready | ~io.rnReq(j).bits.fpu_isa.is_fpu) & 
        rnRspFifo.io.enq(j).ready
      }
    ).reduce(_&_)
  }
}

