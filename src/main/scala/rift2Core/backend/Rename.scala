
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
import chipsalliance.rocketchip.config.Parameters


class Rename()(implicit p: Parameters) extends RiftModule {
  val io = IO(new Bundle{
    val rnReq = Vec(rnChn, Flipped(new DecoupledIO(new IF4_Bundle)))
    val rnRsp = Vec(rnChn, (new DecoupledIO(new Dpt_info)))

    val xLookup = Vec( rnChn, new Lookup_Bundle )
    val fLookup = Vec( rnChn, new Lookup_Bundle )
    val xRename = Vec( rnChn, new Rename_Bundle )
    val fRename = Vec( rnChn, new Rename_Bundle )

    val rod_i = Vec(cm_chn,new DecoupledIO(new Info_reorder_i))
 
  })

  val rnRspReport = Module( new RePort( new Dpt_info, port = rnChn) )
  val rnRspFifo = Module(new MultiPortFifo(new Dpt_info, aw = (if(!isMinArea) 4 else 1 ), rnChn, rnChn))
  rnRspFifo.io.enq <> rnRspReport.io.deq
  rnRspFifo.io.deq <> io.rnRsp

  val reOrder_fifo_i = {
    val mdl = Module(new MultiPortFifo(new Info_reorder_i, aw = (if(!isMinArea) 4 else 1 ), rnChn, cm_chn))
    mdl.io.deq <> io.rod_i
    mdl
  }

  val reg_phy = Wire(Vec(rnChn, new Reg_PHY ) )

  for ( i <- 0 until rnChn ) yield {

    rnRspReport.io.enq(i).valid := io.rnReq(i).fire & ~io.rnReq(i).bits.is_privil_dpt
    rnRspReport.io.enq(i).bits  := Pkg_Rename_Info(io.rnReq(i).bits, reg_phy(i))
     
    io.xRename(i).req.valid    := io.rnReq(i).fire & io.rnReq(i).bits.is_iwb
    io.xRename(i).req.bits.rd0 := io.rnReq(i).bits.param.raw.rd0

    io.xLookup(i).req.rs1 := io.rnReq(i).bits.param.raw.rs1
    io.xLookup(i).req.rs2 := io.rnReq(i).bits.param.raw.rs2
    io.xLookup(i).req.rs3 := 0.U

    io.fRename(i).req.valid    := io.rnReq(i).fire & io.rnReq(i).bits.is_fwb
    io.fRename(i).req.bits.rd0 := io.rnReq(i).bits.param.raw.rd0

    io.fLookup(i).req.rs1 := io.rnReq(i).bits.param.raw.rs1
    io.fLookup(i).req.rs2 := io.rnReq(i).bits.param.raw.rs2
    io.fLookup(i).req.rs3 := io.rnReq(i).bits.param.raw.rs3

    
    reg_phy(i).rs1 := Mux(io.rnReq(i).bits.fpu_isa.is_fop, io.fLookup(i).rsp.rs1, io.xLookup(i).rsp.rs1)
    reg_phy(i).rs2 := Mux(io.rnReq(i).bits.fpu_isa.is_fop | io.rnReq(i).bits.lsu_isa.is_fst, io.fLookup(i).rsp.rs2, io.xLookup(i).rsp.rs2)
    reg_phy(i).rs3 := Mux(io.rnReq(i).bits.fpu_isa.is_fop, io.fLookup(i).rsp.rs3, (regNum-1).U)
    reg_phy(i).rd0 := 
      Mux1H(Seq(
        io.fRename(i).req.fire -> io.fRename(i).rsp.rd0,
        io.xRename(i).req.fire -> io.xRename(i).rsp.rd0,
      ))

    for ( j <- 0 until rnChn ) { assert( PopCount( Seq(io.fRename(j).req.fire, io.xRename(j).req.fire)) <= 1.U, "Assert Failed, rename should be one-hot" ) }

    reOrder_fifo_i.io.enq(i).valid := io.rnReq(i).fire
    reOrder_fifo_i.io.enq(i).bits  := Pkg_rod_i(io.rnReq(i).bits, reg_phy(i))

    io.rnReq(i).ready := (
      for ( j <- 0 to i by 1 ) yield {
        io.xRename(j).req.ready & io.fRename(j).req.ready & reOrder_fifo_i.io.enq(i).ready &
        rnRspFifo.io.enq(j).ready
      }
    ).reduce(_&_)

     

  }


  def Pkg_Rename_Info( instr: Info_instruction, rename: Reg_PHY): Dpt_info = {
    val res = Wire(new Dpt_info)

    res.alu_isa    := instr.alu_isa
    res.bru_isa    := instr.bru_isa
    res.lsu_isa    := instr.lsu_isa
    res.csr_isa    := instr.csr_isa
    res.mul_isa    := instr.mul_isa
    res.privil_isa := 0.U.asTypeOf( new Privil_isa )
    res.fpu_isa    := instr.fpu_isa
    res.param      := instr.param
    res.phy        := rename

    when( instr.fpu_isa.is_fpu ) {
      when(~instr.fpu_isa.hasTwoRs) { res.phy.rs2 := (regNum-1).U }
      when(~instr.fpu_isa.hasThreeRs) { res.phy.rs3 := (regNum-1).U }      
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

      res.is_xcmm := instr.is_iwb
      res.is_fcmm := instr.is_fwb

    return res
  }

  rnRspFifo.io.flush := false.B
  reOrder_fifo_i.io.flush := false.B
  
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


