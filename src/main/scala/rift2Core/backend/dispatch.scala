/*
* @Author: Ruige Lee
* @Date:   2021-03-23 09:31:02
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-03-24 12:02:49
*/

/*
  Copyright (c) 2020 - 2021 Ruige Lee <wut.ruigeli@gmail.com>

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


class Rename(ptr: Vec[UInt], log: Vec[UInt] ) {

  
  def is_free_1st(): Bool = return log.exists( (x:UInt) => (x === 0.U) )
  def is_free_2nd(): Bool = return (log.count( (x:UInt) => (x === 0.U)) >= 2.U)

  def malloc_1st(): UInt = return log.indexWhere( (x:UInt) => (x === 0.U) )
  def malloc_2nd(): UInt = return log.lastIndexWhere( (x:UInt) => (x === 0.U) )

  def lookup( rs: UInt ): UInt = return ptr(rs)

}

trait ReOrder {

  def rod_mux_i(bd_dpt_info: Info_bd_dpt, rd0_phy: UInt): Info_reorder_i = {
    val reorder_i_info = Wire(new Info_reorder_i)
    reorder_i_info.pc             := bd_dpt_info.info.param.pc
    reorder_i_info.rd0_raw        := bd_dpt_info.info.param.rd0_raw
    reorder_i_info.rd0_phy        := rd0_phy
    reorder_i_info.is_branch      := bd_dpt_info.info.bru_isa.is_branch
    reorder_i_info.is_lu          := bd_dpt_info.info.lsu_isa.is_lu
    reorder_i_info.is_su          := bd_dpt_info.info.lsu_isa.is_su
    reorder_i_info.is_amo         := bd_dpt_info.info.lsu_isa.is_amo
    reorder_i_info.is_fence       := bd_dpt_info.info.lsu_isa.fence
    reorder_i_info.is_fence_i     := bd_dpt_info.info.lsu_isa.fence_i
    reorder_i_info.is_csr         := bd_dpt_info.info.csr_isa.is_csr
    reorder_i_info.privil         := bd_dpt_info.info.privil_isa
    reorder_i_info.is_illeage     := bd_dpt_info.info.is_illeage

    return reorder_i_info
  }


  def rod_mux_f(bd_dpt_info: Info_bd_dpt, rd0_phy: UInt): Info_reorder_f = {
    val reorder_f_info = Wire(new Info_reorder_f)
    reorder_f_info.pc             := bd_dpt_info.info.param.pc
    reorder_f_info.rd0_phy        := rd0_phy
    reorder_f_info.is_lu          := bd_dpt_info.info.lsu_isa.is_lu
    reorder_f_info.is_su          := bd_dpt_info.info.lsu_isa.is_su
    return reorder_f_info
  }

}

trait Dpt{

  def dpt_mux_alu( bd_dpt_info: Info_bd_dpt, rd0_phy: UInt, rs1_phy: UInt, rs2_phy: UInt ): Alu_dpt_info = {
    val alu_dpt_info = Wire(new Alu_dpt_info)
    alu_dpt_info.isa        := bd_dpt_info.info.alu_isa
    alu_dpt_info.param      := bd_dpt_info.info.param
    alu_dpt_info.phy.rd0 := rd0_phy
    alu_dpt_info.phy.rs1 := rs1_phy
    alu_dpt_info.phy.rs2 := rs2_phy
    alu_dpt_info.phy.rs3 := DontCare
    return alu_dpt_info
  }

  def dpt_mux_bru( bd_dpt_info: Info_bd_dpt, rd0_phy: UInt, rs1_phy: UInt, rs2_phy: UInt ): Bru_dpt_info = {
    val bru_dpt_info = Wire(new Bru_dpt_info)
    bru_dpt_info.isa        := bd_dpt_info.info.bru_isa
    bru_dpt_info.param      := bd_dpt_info.info.param
    bru_dpt_info.phy.rd0 := rd0_phy
    bru_dpt_info.phy.rs1 := rs1_phy
    bru_dpt_info.phy.rs2 := rs2_phy
    bru_dpt_info.phy.rs3 := DontCare
    return bru_dpt_info
  }

  def dpt_mux_lsu( bd_dpt_info: Info_bd_dpt, rd0_phy: UInt, rs1_phy: UInt, rs2_phy: UInt ): Lsu_dpt_info = {
    val lsu_dpt_info = Wire(new Lsu_dpt_info)
    lsu_dpt_info.isa        := bd_dpt_info.info.lsu_isa
    lsu_dpt_info.param      := bd_dpt_info.info.param
    lsu_dpt_info.phy.rd0 := rd0_phy
    lsu_dpt_info.phy.rs1 := rs1_phy
    lsu_dpt_info.phy.rs2 := rs2_phy
    lsu_dpt_info.phy.rs3 := DontCare
    return lsu_dpt_info
  }

  def dpt_mux_csr( bd_dpt_info: Info_bd_dpt, rd0_phy: UInt, rs1_phy: UInt ): Csr_dpt_info = {
    val csr_dpt_info = Wire(new Csr_dpt_info)
    csr_dpt_info.isa        := bd_dpt_info.info.csr_isa
    csr_dpt_info.param      := bd_dpt_info.info.param
    csr_dpt_info.phy.rd0 := rd0_phy
    csr_dpt_info.phy.rs1 := rs1_phy
    csr_dpt_info.phy.rs2 := DontCare
    csr_dpt_info.phy.rs3 := DontCare
    return csr_dpt_info
  }

  def dpt_mux_mul( bd_dpt_info: Info_bd_dpt, rd0_phy: UInt, rs1_phy: UInt, rs2_phy: UInt ): Mul_dpt_info = {
    val mul_dpt_info = Wire(new Mul_dpt_info)
    mul_dpt_info.isa        := bd_dpt_info.info.mul_isa
    mul_dpt_info.param      := bd_dpt_info.info.param
    mul_dpt_info.phy.rd0 := rd0_phy
    mul_dpt_info.phy.rs1 := rs1_phy
    mul_dpt_info.phy.rs2 := rs2_phy
    mul_dpt_info.phy.rs3 := DontCare
    return mul_dpt_info
  }

  def dpt_mux_fpu( bd_dpt_info: Info_bd_dpt, rd0_phy: UInt, rs1_phy: UInt, rs2_phy: UInt, rs3_phy: UInt ): Fpu_dpt_info = {
    val fpu_dpt_info = Wire(new Fpu_dpt_info)
    fpu_dpt_info.isa        := bd_dpt_info.info.fpu_isa
    fpu_dpt_info.param      := bd_dpt_info.info.param
    fpu_dpt_info.phy.rd0 := rd0_phy
    fpu_dpt_info.phy.rs1 := rs1_phy
    fpu_dpt_info.phy.rs2 := rs2_phy
    fpu_dpt_info.phy.rs3 := rs3_phy
    return fpu_dpt_info
  }


}





class Dispatch_ss extends Module with Superscalar with ReOrder with Dpt{
  val io = IO(new Bundle{
    val bd_dpt = Vec(2, Flipped(new DecoupledIO(new Info_bd_dpt())))

    val alu_dpt_iss = new DecoupledIO(new Alu_dpt_info())
    val bru_dpt_iss = new DecoupledIO(new Bru_dpt_info())
    val lsu_dpt_iss = new DecoupledIO(new Lsu_dpt_info())
    val csr_dpt_iss = new DecoupledIO(new Csr_dpt_info())
    val mul_dpt_iss = new DecoupledIO(new Mul_dpt_info())

    val rod_i = Vec(2,new DecoupledIO(new Info_reorder_i))

    val rn_ptr_i = Vec(32, Input(UInt(6.W)))
    val log_i = Vec(64, Input(UInt(2.W)))
    val rn_op_i = Vec(2, ValidIO( new Info_rename_op ))

    val flush = Input(Bool())

  })

  val rename_i = new Rename(io.rn_ptr_i, io.log_i)

  val alu_dpt_iss_fifo = Module(new Queue(new Alu_dpt_info, 4))
  val bru_dpt_iss_fifo = Module(new Queue(new Bru_dpt_info, 4))
  val lsu_dpt_iss_fifo = Module(new Queue(new Lsu_dpt_info, 4))
  val csr_dpt_iss_fifo = Module(new Queue(new Csr_dpt_info, 4))
  val mul_dpt_iss_fifo = Module(new Queue(new Mul_dpt_info, 4))

  val reOrder_fifo_i = Module(new MultiPortFifo(new Info_reorder_i, 4, 2, 2))

  val is_alu_dpt_1st = Wire(Bool())
  val is_bru_dpt_1st = Wire(Bool())
  val is_lsu_dpt_1st = Wire(Bool())
  val is_csr_dpt_1st = Wire(Bool())
  val is_mul_dpt_1st = Wire(Bool())
  val is_privil_dpt_1st = Wire(Bool())
  val is_dpt_1st = Wire(Bool())

  val is_alu_dpt_2nd    = Wire(Bool())
  val is_bru_dpt_2nd    = Wire(Bool())
  val is_lsu_dpt_2nd    = Wire(Bool())
  val is_csr_dpt_2nd    = Wire(Bool())
  val is_mul_dpt_2nd    = Wire(Bool())
  val is_privil_dpt_2nd = Wire(Bool())
  val is_dpt_2nd = Wire(Bool())




  alu_dpt_iss_fifo.reset := io.flush | reset.asBool
  bru_dpt_iss_fifo.reset := io.flush | reset.asBool
  lsu_dpt_iss_fifo.reset := io.flush | reset.asBool
  csr_dpt_iss_fifo.reset := io.flush | reset.asBool
  mul_dpt_iss_fifo.reset := io.flush | reset.asBool

  reOrder_fifo_i.io.flush := io.flush


  io.alu_dpt_iss <> alu_dpt_iss_fifo.io.deq
  io.bru_dpt_iss <> bru_dpt_iss_fifo.io.deq
  io.lsu_dpt_iss <> lsu_dpt_iss_fifo.io.deq
  io.csr_dpt_iss <> csr_dpt_iss_fifo.io.deq
  io.mul_dpt_iss <> mul_dpt_iss_fifo.io.deq

  io.rod_i <> reOrder_fifo_i.io.deq

  def is_iwb(i: Int) = true.B

  def bd_dpt_info(i: Int) = io.bd_dpt(i).bits
  def instruction_info(i: Int) = bd_dpt_info(i).info


  def rd0_raw(i: Int) = instruction_info(i).param.rd0_raw
  def rs1_raw(i: Int) = instruction_info(i).param.rs1_raw	
  def rs2_raw(i: Int) = instruction_info(i).param.rs2_raw
  def rs3_raw(i: Int) = instruction_info(i).param.rs3_raw



  val rd0_phy_1st = Wire(UInt(6.W))
  val rs1_phy_1st = Wire(UInt(6.W))
  val rs2_phy_1st = Wire(UInt(6.W))
  val rd0_phy_2nd = Wire(UInt(6.W))
  val rs1_phy_2nd = Wire(UInt(6.W))
  val rs2_phy_2nd = Wire(UInt(6.W))

  rd0_phy_1st := rename_i.malloc_1st()
  rd0_phy_2nd := rename_i.malloc_2nd()
  
  rs1_phy_1st := rename_i.lookup(rs1_raw(0))
  rs2_phy_1st := rename_i.lookup(rs2_raw(0))
  
  rs1_phy_2nd := Mux( ( rd0_raw(0) === rs1_raw(1) ), rd0_phy_1st, rename_i.lookup(rs1_raw(1)) )
  rs2_phy_2nd := Mux( ( rd0_raw(0) === rs2_raw(1) ), rd0_phy_1st, rename_i.lookup(rs2_raw(1)) )

  
  
  io.rn_op_i(0).valid := is_iwb(0) & ( is_alu_dpt_1st | is_bru_dpt_1st | is_lsu_dpt_1st | is_csr_dpt_1st | is_mul_dpt_1st  )
  io.rn_op_i(1).valid := is_iwb(1) & ( is_alu_dpt_2nd | is_bru_dpt_2nd | is_lsu_dpt_2nd | is_csr_dpt_2nd | is_mul_dpt_2nd  )

  io.rn_op_i(0).bits.raw := rd0_raw(0)
  io.rn_op_i(1).bits.raw := rd0_raw(1)

  io.rn_op_i(0).bits.phy := rd0_phy_1st
  io.rn_op_i(1).bits.phy := rd0_phy_2nd

  //   io.rn_op_i(i)(j) := 
  //     ((i.U === rd0_raw(0)) & ( j.U === rd0_idx_1st ) & is_iwb(0) & ( is_alu_dpt_1st | is_bru_dpt_1st | is_lsu_dpt_1st | is_csr_dpt_1st | is_mul_dpt_1st  ) ) | 
  //     ((i.U === rd0_raw(1)) & ( j.U === rd0_idx_2nd ) & is_iwb(1) & ( is_alu_dpt_2nd | is_bru_dpt_2nd | is_lsu_dpt_2nd | is_csr_dpt_2nd | is_mul_dpt_2nd  ) )

  // }






  
  
  def is_rod_ready(i: Int) = reOrder_fifo_i.io.enq(i).ready //Mux(is_iwb(i), reOrder_fifo_i.io.enq(i).ready, reOrder_fifo_f.io.enq(i).ready)



  is_alu_dpt_1st    := 
        (io.bd_dpt(0).valid & instruction_info(0).alu_isa.is_alu & rename_i.is_free_1st() & is_rod_ready(0) & alu_dpt_iss_fifo.io.enq.ready)

  is_bru_dpt_1st    := 
        (io.bd_dpt(0).valid & instruction_info(0).bru_isa.is_bru & rename_i.is_free_1st() & is_rod_ready(0) & bru_dpt_iss_fifo.io.enq.ready)

  is_lsu_dpt_1st    := 
        (io.bd_dpt(0).valid & instruction_info(0).lsu_isa.is_lsu & rename_i.is_free_1st() & is_rod_ready(0) & lsu_dpt_iss_fifo.io.enq.ready)

  is_csr_dpt_1st    := 
        (io.bd_dpt(0).valid & instruction_info(0).csr_isa.is_csr & rename_i.is_free_1st() & is_rod_ready(0) & csr_dpt_iss_fifo.io.enq.ready)

  is_mul_dpt_1st    := 
        (io.bd_dpt(0).valid & instruction_info(0).mul_isa.is_mul & rename_i.is_free_1st() & is_rod_ready(0) & mul_dpt_iss_fifo.io.enq.ready)

  is_privil_dpt_1st := 
        (io.bd_dpt(0).valid & (instruction_info(0).privil_isa.is_privil | io.bd_dpt(0).bits.info.is_illeage ) & is_rod_ready(0))
  
  is_dpt_1st        := 
        (is_alu_dpt_1st | is_bru_dpt_1st | is_lsu_dpt_1st | is_csr_dpt_1st | is_mul_dpt_1st | is_privil_dpt_1st) //| is_fpu_dpt_1st




  is_alu_dpt_2nd    := 
        (~is_alu_dpt_1st & is_dpt_1st) &
        (io.bd_dpt(1).valid & instruction_info(1).alu_isa.is_alu & rename_i.is_free_2nd() & is_rod_ready(1) & alu_dpt_iss_fifo.io.enq.ready )
  
  is_bru_dpt_2nd    := 
        (~is_bru_dpt_1st & is_dpt_1st) &
        (io.bd_dpt(1).valid & instruction_info(1).bru_isa.is_bru & rename_i.is_free_2nd() & is_rod_ready(1) & bru_dpt_iss_fifo.io.enq.ready)

  is_lsu_dpt_2nd    := 
        (~is_lsu_dpt_1st & is_dpt_1st) &
        (io.bd_dpt(1).valid & instruction_info(1).lsu_isa.is_lsu & rename_i.is_free_2nd() & is_rod_ready(1) & lsu_dpt_iss_fifo.io.enq.ready)


  is_csr_dpt_2nd    := 
        (~is_csr_dpt_1st & is_dpt_1st) &
        (io.bd_dpt(1).valid & instruction_info(1).csr_isa.is_csr & rename_i.is_free_2nd() & is_rod_ready(1) & csr_dpt_iss_fifo.io.enq.ready)


  is_mul_dpt_2nd    := 
        (~is_mul_dpt_1st & is_dpt_1st) &
        (io.bd_dpt(1).valid & instruction_info(1).mul_isa.is_mul & rename_i.is_free_2nd() & is_rod_ready(1) & mul_dpt_iss_fifo.io.enq.ready)


  is_privil_dpt_2nd := 
        (is_dpt_1st) &
        (io.bd_dpt(1).valid & (instruction_info(1).privil_isa.is_privil | io.bd_dpt(1).bits.info.is_illeage) & is_rod_ready(1))


  is_dpt_2nd        := is_alu_dpt_2nd | is_bru_dpt_2nd | is_lsu_dpt_2nd | is_csr_dpt_2nd | is_mul_dpt_2nd | is_privil_dpt_2nd //| is_fpu_dpt_2nd






  alu_dpt_iss_fifo.io.enq.bits := Mux(
                    is_alu_dpt_1st,
                    dpt_mux_alu(bd_dpt_info(0), rd0_phy_1st, rs1_phy_1st, rs2_phy_1st),
                    dpt_mux_alu(bd_dpt_info(1), rd0_phy_2nd, rs1_phy_2nd, rs2_phy_2nd)
                    )
  bru_dpt_iss_fifo.io.enq.bits := Mux(
                    is_bru_dpt_1st,
                    dpt_mux_bru(bd_dpt_info(0), rd0_phy_1st, rs1_phy_1st, rs2_phy_1st),
                    dpt_mux_bru(bd_dpt_info(1), rd0_phy_2nd, rs1_phy_2nd, rs2_phy_2nd)
                  )
  lsu_dpt_iss_fifo.io.enq.bits := Mux(
                    is_lsu_dpt_1st,
                    dpt_mux_lsu(bd_dpt_info(0), rd0_phy_1st, rs1_phy_1st, rs2_phy_1st),
                    dpt_mux_lsu(bd_dpt_info(1), rd0_phy_2nd, rs1_phy_2nd, rs2_phy_2nd)
                  )
  csr_dpt_iss_fifo.io.enq.bits := Mux(
                    is_csr_dpt_1st,
                    dpt_mux_csr(bd_dpt_info(0), rd0_phy_1st, rs1_phy_1st),
                    dpt_mux_csr(bd_dpt_info(1), rd0_phy_2nd, rs1_phy_2nd)
                  )
  mul_dpt_iss_fifo.io.enq.bits := Mux(
                    is_mul_dpt_1st,
                    dpt_mux_mul(bd_dpt_info(0), rd0_phy_1st, rs1_phy_1st, rs2_phy_1st),
                    dpt_mux_mul(bd_dpt_info(1), rd0_phy_2nd, rs1_phy_2nd, rs2_phy_2nd)
                  )

  // fpu_dpt_iss_fifo.io.enq(0).bits := dpt_mux_fpu(id_dpt_info(0), rd0_idx_1st, rs1_idx_1st, rs2_idx_1st, rs3_idx_1st)



  reOrder_fifo_i.io.enq(0).bits := rod_mux_i(bd_dpt_info(0), rd0_phy_1st)
  reOrder_fifo_i.io.enq(1).bits := rod_mux_i(bd_dpt_info(1), rd0_phy_2nd)

  override def is_1st_solo = false.B
  override def is_2nd_solo = false.B


  io.bd_dpt(0).ready := is_dpt_1st
  io.bd_dpt(1).ready := is_dpt_2nd



  alu_dpt_iss_fifo.io.enq.valid := is_alu_dpt_1st | is_alu_dpt_2nd
  bru_dpt_iss_fifo.io.enq.valid := is_bru_dpt_1st | is_bru_dpt_2nd
  lsu_dpt_iss_fifo.io.enq.valid := is_lsu_dpt_1st | is_lsu_dpt_2nd
  csr_dpt_iss_fifo.io.enq.valid := is_csr_dpt_1st | is_csr_dpt_2nd
  mul_dpt_iss_fifo.io.enq.valid := is_mul_dpt_1st | is_mul_dpt_2nd
  // fpu_dpt_iss_fifo.io.enq(0).valid := is_fpu_dpt_1st


  reOrder_fifo_i.io.enq(0).valid := is_iwb(0) & is_dpt_1st
  reOrder_fifo_i.io.enq(1).valid := is_iwb(1) & is_dpt_2nd



}


