
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
import rift2Core.frontend._



class Dispatch(rn_chn: Int = 2, cmm_chn: Int = 2) extends Module {
  val io = IO(new Bundle{
    val bd_dpt = Vec(rn_chn, Flipped(new DecoupledIO(new Info_bd_dpt())))

    // val dpt_rename = Vec( rn_chn, new dpt_rename_info(64))
    val dpt_Xlookup = Vec( rn_chn, new dpt_lookup_info(64) )
    val dpt_Flookup = Vec( rn_chn, new dpt_lookup_info(64) )
    val dpt_Xrename = Vec( rn_chn, new dpt_rename_info(64) )
    val dpt_Frename = Vec( rn_chn, new dpt_rename_info(64) )


    val ooo_dpt_iss = Vec(2, new DecoupledIO(new Dpt_info))
    val bru_dpt_iss = new DecoupledIO(new Dpt_info)
    val csr_dpt_iss = new DecoupledIO(new Dpt_info)
    val lsu_dpt_iss = new DecoupledIO(new Dpt_info)
    val fpu_dpt_iss = new DecoupledIO(new Dpt_info)

    val rod_i = Vec(cmm_chn,new DecoupledIO(new Info_reorder_i))
 
  })


  val ooo_dpt_rePort =  Module(new RePort( new Dpt_info, port = rn_chn))

  val ooo_dpt_fifo = {
    val mdl = Module(new ZipQueue( new Dpt_info, 4, in = rn_chn, out = 2 ))
    mdl.io.enq <> ooo_dpt_rePort.io.deq
    mdl.io.deq <> io.ooo_dpt_iss
    mdl
  }

  val bru_dpt_rePort = Module(new RePort( new Dpt_info, port = rn_chn))
  val bru_dpt_fifo = {
    val mdl = Module(new MultiPortFifo( new Dpt_info, aw = 4, in = rn_chn, out = 1 ))
    mdl.io.enq <> bru_dpt_rePort.io.deq
    mdl.io.deq(0) <> io.bru_dpt_iss
    mdl
  }

  val csr_dpt_rePort = Module(new RePort( new Dpt_info, port = rn_chn))
  val csr_dpt_fifo = {
    val mdl = Module(new MultiPortFifo( new Dpt_info, aw = 4, in = rn_chn, out = 1 ))
    mdl.io.enq <> csr_dpt_rePort.io.deq
    mdl.io.deq(0) <> io.csr_dpt_iss
    mdl
  }

  val lsu_dpt_rePort = Module(new RePort( new Dpt_info, port = rn_chn))
  val lsu_dpt_fifo = {
    val mdl = Module(new MultiPortFifo( new Dpt_info, aw = 4, in = rn_chn, out = 1 ))
    mdl.io.enq <> lsu_dpt_rePort.io.deq
    mdl.io.deq(0) <> io.lsu_dpt_iss
    mdl
  }

  val fpu_dpt_rePort = Module(new RePort( new Dpt_info, port = rn_chn))
  val fpu_dpt_fifo = {
    val mdl = Module(new MultiPortFifo( new Dpt_info, aw = 4, in = rn_chn, out = 1 ))
    mdl.io.enq <> fpu_dpt_rePort.io.deq
    mdl.io.deq(0) <> io.fpu_dpt_iss
    mdl
  }

  val reOrder_fifo_i = {
    val mdl = Module(new MultiPortFifo(new Info_reorder_i, 4, rn_chn, cmm_chn))
    mdl.io.deq <> io.rod_i
    mdl
  }

  val reg_phy = Wire(Vec(rn_chn, new Reg_phy(64)) )

  for ( i <- 0 until rn_chn ) yield {
    ooo_dpt_rePort.io.enq(i).valid := io.bd_dpt(i).fire & io.bd_dpt(i).bits.info.is_ooo_dpt
    bru_dpt_rePort.io.enq(i).valid := io.bd_dpt(i).fire & io.bd_dpt(i).bits.info.bru_isa.is_bru
    csr_dpt_rePort.io.enq(i).valid := io.bd_dpt(i).fire & io.bd_dpt(i).bits.info.csr_isa.is_csr
    lsu_dpt_rePort.io.enq(i).valid := io.bd_dpt(i).fire & io.bd_dpt(i).bits.info.lsu_isa.is_lsu
    fpu_dpt_rePort.io.enq(i).valid := io.bd_dpt(i).fire & io.bd_dpt(i).bits.info.fpu_isa.is_fpu

    ooo_dpt_rePort.io.enq(i).bits := Mux( ooo_dpt_rePort.io.enq(i).valid, Pkg_ooo_dpt(io.bd_dpt(i).bits.info, reg_phy(i)), 0.U.asTypeOf(new Dpt_info) )
    bru_dpt_rePort.io.enq(i).bits := Mux( bru_dpt_rePort.io.enq(i).valid, Pkg_bru_dpt(io.bd_dpt(i).bits.info, reg_phy(i)), 0.U.asTypeOf(new Dpt_info) )
    csr_dpt_rePort.io.enq(i).bits := Mux( csr_dpt_rePort.io.enq(i).valid, Pkg_csr_dpt(io.bd_dpt(i).bits.info, reg_phy(i)), 0.U.asTypeOf(new Dpt_info) )
    lsu_dpt_rePort.io.enq(i).bits := Mux( lsu_dpt_rePort.io.enq(i).valid, Pkg_lsu_dpt(io.bd_dpt(i).bits.info, reg_phy(i)), 0.U.asTypeOf(new Dpt_info) )
    fpu_dpt_rePort.io.enq(i).bits := Mux( fpu_dpt_rePort.io.enq(i).valid, Pkg_fpu_dpt(io.bd_dpt(i).bits.info, reg_phy(i)), 0.U.asTypeOf(new Dpt_info) )

    io.dpt_Xrename(i).req.valid := io.bd_dpt(i).fire & io.bd_dpt(i).bits.info.is_iwb
    io.dpt_Xrename(i).req.bits.rd0 := Mux( io.dpt_Xrename(i).req.valid, io.bd_dpt(i).bits.info.param.raw.rd0, 0.U )

    io.dpt_Xlookup(i).req.rs1 := io.bd_dpt(i).bits.info.param.raw.rs1
    io.dpt_Xlookup(i).req.rs2 := io.bd_dpt(i).bits.info.param.raw.rs2
    io.dpt_Xlookup(i).req.rs3 := 0.U

  
    io.dpt_Frename(i).req.valid := io.bd_dpt(i).fire & io.bd_dpt(i).bits.info.is_fwb
    io.dpt_Frename(i).req.bits.rd0 :=  Mux( io.dpt_Frename(i).req.valid, io.bd_dpt(i).bits.info.param.raw.rd0, 0.U )

    io.dpt_Flookup(i).req.rs1 := io.bd_dpt(i).bits.info.param.raw.rs1
    io.dpt_Flookup(i).req.rs2 := io.bd_dpt(i).bits.info.param.raw.rs2
    io.dpt_Flookup(i).req.rs3 := io.bd_dpt(i).bits.info.param.raw.rs3

    
    reg_phy(i).rs1 := Mux(io.bd_dpt(i).bits.info.fpu_isa.is_fop, io.dpt_Flookup(i).rsp.rs1, io.dpt_Xlookup(i).rsp.rs1)
    reg_phy(i).rs2 := Mux(io.bd_dpt(i).bits.info.fpu_isa.is_fop | io.bd_dpt(i).bits.info.lsu_isa.is_fst, io.dpt_Flookup(i).rsp.rs2, io.dpt_Xlookup(i).rsp.rs2)
    reg_phy(i).rs3 := Mux(io.bd_dpt(i).bits.info.fpu_isa.is_fop, io.dpt_Flookup(i).rsp.rs3, 63.U)
    reg_phy(i).rd0 := 
      Mux1H(Seq(
        io.dpt_Frename(i).req.fire -> io.dpt_Frename(i).rsp.rd0,
        io.dpt_Xrename(i).req.fire -> io.dpt_Xrename(i).rsp.rd0,
      ))


    reOrder_fifo_i.io.enq(i).valid := io.bd_dpt(i).fire
    reOrder_fifo_i.io.enq(i).bits  := Mux( reOrder_fifo_i.io.enq(i).valid, Pkg_rod_i(io.bd_dpt(i).bits.info, reg_phy(i)), DontCare )

    io.bd_dpt(i).ready := (
      for ( j <- 0 to i by 1 ) yield {
        ooo_dpt_rePort.io.enq(j).ready & io.dpt_Xrename(j).req.ready & io.dpt_Frename(j).req.ready & reOrder_fifo_i.io.enq(i).ready &
        bru_dpt_rePort.io.enq(j).ready &
        csr_dpt_rePort.io.enq(j).ready &
        lsu_dpt_rePort.io.enq(j).ready &
        fpu_dpt_rePort.io.enq(j).ready
      }      
    ).reduce(_&_)

     

  }


  def Pkg_ooo_dpt( instr: Info_instruction, rename: Reg_phy): Dpt_info = {
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

  def Pkg_bru_dpt( instr:Info_instruction, rename: Reg_phy): Dpt_info = {
    val res = Wire(new Dpt_info)

    res.alu_isa    := 0.U.asTypeOf( new Alu_isa )
    res.bru_isa    := instr.bru_isa
    res.lsu_isa    := 0.U.asTypeOf( new Lsu_isa )
    res.csr_isa    := 0.U.asTypeOf( new Csr_isa )
    res.mul_isa    := 0.U.asTypeOf( new Mul_isa )
    res.privil_isa := 0.U.asTypeOf( new Privil_isa )
    res.fpu_isa    := 0.U.asTypeOf( new Fpu_isa )
    res.param      := instr.param
    res.phy        := rename
    return res
  }

  def Pkg_csr_dpt( instr:Info_instruction, rename: Reg_phy): Dpt_info = {
    val res = Wire(new Dpt_info)

    res.alu_isa    := 0.U.asTypeOf( new Alu_isa )
    res.bru_isa    := 0.U.asTypeOf( new Bru_isa )
    res.lsu_isa    := 0.U.asTypeOf( new Lsu_isa )
    res.csr_isa    := instr.csr_isa
    res.mul_isa    := 0.U.asTypeOf( new Mul_isa )
    res.privil_isa := 0.U.asTypeOf( new Privil_isa )
    res.fpu_isa    := 0.U.asTypeOf( new Fpu_isa )
    res.param      := instr.param
    res.phy        := rename
    return res
  }

  def Pkg_lsu_dpt( instr:Info_instruction, rename: Reg_phy): Dpt_info = {
    val res = Wire(new Dpt_info)

    res.alu_isa    := 0.U.asTypeOf( new Alu_isa )
    res.bru_isa    := 0.U.asTypeOf( new Bru_isa )
    res.lsu_isa    := instr.lsu_isa
    res.csr_isa    := 0.U.asTypeOf( new Csr_isa )
    res.mul_isa    := 0.U.asTypeOf( new Mul_isa )
    res.privil_isa := 0.U.asTypeOf( new Privil_isa )
    res.fpu_isa    := 0.U.asTypeOf( new Fpu_isa )
    res.param      := instr.param
    res.phy        := rename
    return res
  }

  def Pkg_fpu_dpt( instr:Info_instruction, rename: Reg_phy): Dpt_info = {
    val res = Wire(new Dpt_info)

    res.alu_isa    := 0.U.asTypeOf( new Alu_isa )
    res.bru_isa    := 0.U.asTypeOf( new Bru_isa )
    res.lsu_isa    := 0.U.asTypeOf( new Lsu_isa )
    res.csr_isa    := 0.U.asTypeOf( new Csr_isa )
    res.mul_isa    := 0.U.asTypeOf( new Mul_isa )
    res.privil_isa := 0.U.asTypeOf( new Privil_isa )
    res.fpu_isa    := instr.fpu_isa
    res.param      := instr.param


    res.phy        := rename

    when(~instr.fpu_isa.hasTwoRs) { res.phy.rs2 := 63.U }
    when(~instr.fpu_isa.hasThreeRs) { res.phy.rs3 := 63.U }

    return res
  }

  def Pkg_rod_i(instr:Info_instruction, rename: Reg_phy): Info_reorder_i = {
    val res = Wire(new Info_reorder_i)

      res.pc             := instr.param.pc
      res.rd0_raw        := instr.param.raw.rd0
      res.rd0_phy        := rename.rd0
      res.is_branch      := instr.bru_isa.is_branch
      res.is_lu          := instr.lsu_isa.is_lu
      res.is_su          := instr.lsu_isa.is_su
      res.is_amo         := instr.lsu_isa.is_amo
      res.is_fence       := instr.lsu_isa.fence
      res.is_fence_i     := instr.lsu_isa.fence_i
      res.is_sfence_vma  := instr.lsu_isa.sfence_vma
      res.is_wfi         := instr.alu_isa.wfi
      res.is_csr         := instr.csr_isa.is_csr
      res.is_fpu         := instr.fpu_isa.is_fpu | instr.lsu_isa.is_fpu
      res.privil         := instr.privil_isa
      res.is_illeage     := instr.is_illeage

      res.is_xcmm := instr.is_iwb
      res.is_fcmm := instr.is_fwb

    return res
  }



  bru_dpt_fifo.io.flush := false.B
  csr_dpt_fifo.io.flush := false.B
  lsu_dpt_fifo.io.flush := false.B
  fpu_dpt_fifo.io.flush := false.B
  reOrder_fifo_i.io.flush := false.B
  
}


