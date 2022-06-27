
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
import base._
import rift2Core.define._
import rift._
import chipsalliance.rocketchip.config.Parameters


class Dispatch()(implicit p: Parameters) extends RiftModule {
  val io = IO(new Bundle{
    val bd_dpt = Vec(rn_chn, Flipped(new DecoupledIO(new IF4_Bundle())))

    // val dpt_rename = Vec( rn_chn, new dpt_rename_info(64))
    val dpt_Xlookup = Vec( rn_chn, new dpt_lookup_info )
    val dpt_Flookup = Vec( rn_chn, new dpt_lookup_info )
    val dpt_Xrename = Vec( rn_chn, new dpt_rename_info )
    val dpt_Frename = Vec( rn_chn, new dpt_rename_info )


    val ooo_dpt_iss = Vec(opChn/2, new DecoupledIO(new Dpt_info))
    val ito_dpt_iss = Vec(opChn/2, new DecoupledIO(new Dpt_info))

    val rod_i = Vec(cm_chn,new DecoupledIO(new Info_reorder_i))
 
  })


  val ooo_dpt_rePort =  Module(new RePort( new Dpt_info, port = rn_chn))

  val ooo_dpt_fifo = {
    val mdl = Module(new ZipQueue( new Dpt_info, (if(!isMinArea) 4 else 3), in = rn_chn, out = opChn/2, zip = opChn/2) )
    mdl.io.enq <> ooo_dpt_rePort.io.deq
    mdl.io.deq <> io.ooo_dpt_iss
    mdl
  }

  val ito_dpt_rePort =  Module(new RePort( new Dpt_info, port = rn_chn))
  val ito_dpt_fifo = {
    val mdl = Module(new ZipQueue( new Dpt_info, (if(!isMinArea) 4 else 3), in = rn_chn, out = opChn/2, zip = opChn/2) )
    mdl.io.enq <> ito_dpt_rePort.io.deq
    mdl.io.deq <> io.ito_dpt_iss
    mdl
  }



  val reOrder_fifo_i = {
    val mdl = Module(new MultiPortFifo(new Info_reorder_i, aw = (if(!isMinArea) 4 else 2), rn_chn, cm_chn))
    mdl.io.deq <> io.rod_i
    mdl
  }

  val reg_phy = Wire(Vec(rn_chn, new Reg_PHY ) )

  for ( i <- 0 until rn_chn ) yield {
    ooo_dpt_rePort.io.enq(i).valid := io.bd_dpt(i).fire & io.bd_dpt(i).bits.is_ooo_dpt
    ito_dpt_rePort.io.enq(i).valid := io.bd_dpt(i).fire & io.bd_dpt(i).bits.is_ito_dpt

    ooo_dpt_rePort.io.enq(i).bits := Mux( ooo_dpt_rePort.io.enq(i).valid, Pkg_ooo_dpt(io.bd_dpt(i).bits, reg_phy(i)), 0.U.asTypeOf(new Dpt_info) )
    ito_dpt_rePort.io.enq(i).bits := Mux( ito_dpt_rePort.io.enq(i).valid, Pkg_ito_dpt(io.bd_dpt(i).bits, reg_phy(i)), 0.U.asTypeOf(new Dpt_info) )

    io.dpt_Xrename(i).req.valid := io.bd_dpt(i).fire & io.bd_dpt(i).bits.is_iwb
    io.dpt_Xrename(i).req.bits.rd0 := Mux( io.dpt_Xrename(i).req.valid, io.bd_dpt(i).bits.param.raw.rd0, 0.U )

    io.dpt_Xlookup(i).req.rs1 := io.bd_dpt(i).bits.param.raw.rs1
    io.dpt_Xlookup(i).req.rs2 := io.bd_dpt(i).bits.param.raw.rs2
    io.dpt_Xlookup(i).req.rs3 := 0.U

  
    io.dpt_Frename(i).req.valid := io.bd_dpt(i).fire & io.bd_dpt(i).bits.is_fwb
    io.dpt_Frename(i).req.bits.rd0 :=  Mux( io.dpt_Frename(i).req.valid, io.bd_dpt(i).bits.param.raw.rd0, 0.U )

    io.dpt_Flookup(i).req.rs1 := io.bd_dpt(i).bits.param.raw.rs1
    io.dpt_Flookup(i).req.rs2 := io.bd_dpt(i).bits.param.raw.rs2
    io.dpt_Flookup(i).req.rs3 := io.bd_dpt(i).bits.param.raw.rs3

    
    reg_phy(i).rs1 := Mux(io.bd_dpt(i).bits.fpu_isa.is_fop, io.dpt_Flookup(i).rsp.rs1, io.dpt_Xlookup(i).rsp.rs1)
    reg_phy(i).rs2 := Mux(io.bd_dpt(i).bits.fpu_isa.is_fop | io.bd_dpt(i).bits.lsu_isa.is_fst, io.dpt_Flookup(i).rsp.rs2, io.dpt_Xlookup(i).rsp.rs2)
    reg_phy(i).rs3 := Mux(io.bd_dpt(i).bits.fpu_isa.is_fop, io.dpt_Flookup(i).rsp.rs3, (regNum-1).U)
    reg_phy(i).rd0 := 
      Mux1H(Seq(
        io.dpt_Frename(i).req.fire -> io.dpt_Frename(i).rsp.rd0,
        io.dpt_Xrename(i).req.fire -> io.dpt_Xrename(i).rsp.rd0,
      ))


    for ( i <- 0 until rn_chn ) {
      assert( PopCount( Seq(io.dpt_Frename(i).req.fire, io.dpt_Xrename(i).req.fire)) <= 1.U, "Assert Failed, rename should be one-hot" )      
    }



    reOrder_fifo_i.io.enq(i).valid := io.bd_dpt(i).fire
    reOrder_fifo_i.io.enq(i).bits  := Mux( reOrder_fifo_i.io.enq(i).valid, Pkg_rod_i(io.bd_dpt(i).bits, reg_phy(i)), DontCare )

    io.bd_dpt(i).ready := (
      for ( j <- 0 to i by 1 ) yield {
        ooo_dpt_rePort.io.enq(j).ready & io.dpt_Xrename(j).req.ready & io.dpt_Frename(j).req.ready & reOrder_fifo_i.io.enq(i).ready &
        ito_dpt_rePort.io.enq(j).ready
      }      
    ).reduce(_&_)

     

  }


  def Pkg_ooo_dpt( instr: Info_instruction, rename: Reg_PHY): Dpt_info = {
    val res = Wire(new Dpt_info)

    res.alu_isa    := instr.alu_isa
    res.bru_isa    := 0.U.asTypeOf( new Bru_isa )
    res.lsu_isa    := 0.U.asTypeOf( new Lsu_isa )
    res.csr_isa    := 0.U.asTypeOf( new Csr_isa )
    res.mul_isa    := instr.mul_isa
    res.privil_isa := 0.U.asTypeOf( new Privil_isa )
    res.fpu_isa    := 0.U.asTypeOf( new Fpu_isa )
    res.param      := instr.param
    res.phy        := rename
    return res
  }

  def Pkg_ito_dpt( instr:Info_instruction, rename: Reg_PHY): Dpt_info = {
    val res = Wire(new Dpt_info)

    res.alu_isa    := 0.U.asTypeOf( new Alu_isa )
    res.bru_isa    := instr.bru_isa
    res.lsu_isa    := instr.lsu_isa
    res.csr_isa    := instr.csr_isa
    res.mul_isa    := 0.U.asTypeOf( new Mul_isa )
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


  reOrder_fifo_i.io.flush := false.B
  
  if ( hasFpu ) {
  } else {
    assert( io.bd_dpt.forall{ x:DecoupledIO[IF4_Bundle] => x.bits.fpu_isa.is_fpu === false.B} )
    assert( io.bd_dpt.forall{ x:DecoupledIO[IF4_Bundle] => x.bits.is_fwb === false.B} )
  }
}


