
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

    val dpt_rename = Vec( rn_chn, Flipped(new dpt_rename_info(dp)) )

    val ooo_dpt_iss = Vec(4, new DecoupledIO(new Dpt_info))
    val ito_dpt_iss = new DecoupledIO(new Dpt_info)

    val rod_i = Vec(rn_chn,new DecoupledIO(new Info_reorder_i))
 
  })



  val ooo_dpt_rePort = Module(new RePort( new Dpt_info, port = rn_chn))

  val ooo_dpt_fifo = {
    val mdl = Module(new ZipQueue( new Dpt_info, 4, in = rn_chn, out = 4 ))
    mdl.io.enq <> ooo_dpt_rePort.io.deq
    mdl.io.deq <> io.ooo_dpt_iss
    mdl
  }

  val ito_dpt_rePort = Module(new RePort( new Dpt_info, port = rn_chn))

  val ito_dpt_fifo = {
    val mdl = Module(new MultiPortFifo( new Dpt_info, aw = 4, in = rn_chn, out = 1 ))
    mdl.io.enq <> ito_dpt_rePort.io.deq
    mdl.io.deq <> io.ito_dpt_iss
    mdl
  }

  val reOrder_fifo_i = {
    val mdl = Module(new MultiPortFifo(new Info_reorder_i, 4, rn_chn, cmm_chn))
    mdl.io.deq <> io.rod_i
    mdl
  }

  for ( i <- 0 until rn_chn ) yield {
    ooo_dpt_rePort.io.enq(i).valid := io.bd_dpt(i).fire & io.bd_dpt(i).bits.info.is_ooo_dpt
    ito_dpt_rePort.io.enq(i).valid := io.bd_dpt(i).fire & io.bd_dpt(i).bits.info.is_ito_dpt

    ooo_dpt_rePort.io.enq(i).bits := Mux( ooo_dpt_rePort.io.enq(i).valid, Pkg_ooo_dpt(io.bd_dpt(i).bits.info, io.dpt_rename(i).rsp), DontCare )
    dpt_rePort.io.enq(i).bits := Mux( ito_dpt_rePort.io.enq(i).valid, Pkg_ito_dpt(io.bd_dpt(i).bits.info, io.dpt_rename(i).rsp), DontCare )

    io.dpt_rename(i).req.valid := io.bd_dpt(i).fire
    io.dpt_rename(i).req.bits := Mux( io.dpt_rename(i).req.valid, io.bd_dpt(i).bits.info.param.raw )


    reOrder_fifo_i.io.enq(i).valid := io.bd_dpt(i).fire
    reOrder_fifo_i.io.enq(i).bits  := Mux( reOrder_fifo_i.io.enq(i).valid, Pkg_rod_i(io.bd_dpt(i).bits.info, io.dpt_rename(i).rsp), DontCare )

    io.bd_dpt(i).ready := ooo_dpt_rePort.io.enq(i).ready & ito_dpt_rePort.io.enq(i).ready & io.dpt_rename(i).req.ready

  }


  def Pkg_ooo_dpt( instr: Info_instruction, rename: Reg_phy): Dpt_info = {
    val res = Wire(new Dpt_info)

    res.alu_isa    := instr.alu_isa
    res.bru_isa    := 0.U.asTypeOf( new Bru_isa )
    res.lsu_isa    := instr.lsu_isa
    res.csr_isa    := 0.U.asTypeOf( new Csr_isa )
    res.mul_isa    := instr.mul_isa
    res.privil_isa := 0.U.asTypeOf( new Privil_isa )
    res.fpu_isa    := 0.U.asTypeOf( new Fpu_isa )
    res.param      := instr.param
    res.phy        := rename
    return res
  }

  def Pkg_ito_dpt( instr:Info_instruction, rename: Reg_phy): Dpt_info = {
    val res = Wire(new Dpt_info)

    res.alu_isa    := 0.U.asTypeOf( new Alu_isa )
    res.bru_isa    := instr.bru_isa
    res.lsu_isa    := 0.U.asTypeOf( new Lsu_isa )
    res.csr_isa    := instr.csr_isa
    res.mul_isa    := 0.U.asTypeOf( new Mul_isa )
    res.privil_isa := 0.U.asTypeOf( new Privil_isa )
    res.fpu_isa    := 0.U.asTypeOf( new Fpu_isa )
    res.param      := instr.param
    res.phy        := rename
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
      res.privil         := instr.privil_isa
      res.is_illeage     := instr.is_illeage

    return reorder_i_info
  }



  ooo_dpt_fifo.io.flush := io.flush
  ito_dpt_fifo.io.flush := io.flush
  reOrder_fifo_i.io.flush := io.flush
  
}


