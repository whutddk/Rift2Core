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

package rift2Core.privilege


import chisel3._
import chisel3.util._

import chisel3.util.random._

import rift._

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

class Info_pte_sv39 extends Bundle {
  val value = UInt(64.W)

  def V = value(0).asBool
  def R = value(1).asBool
  def W = value(2).asBool
  def X = value(3).asBool
  def U = value(4).asBool
  def G = value(5).asBool
  def A = value(6).asBool
  def D = value(7).asBool
  def rsw = value(9,8)
  def ppn = MixedVecInit( Seq( value(18,10), value(27,19), value(53,28) ) )

  val is_4K_page = Bool()
  val is_giga_page = Bool()
  val is_mega_page = Bool()
}







class Info_mmu_req extends Bundle {
  val vaddr = UInt(64.W)
  val is_X = Bool()
  val is_W = Bool()
  val is_R = Bool()
}


class Info_mmu_rsp extends Bundle {
  val paddr  = UInt(64.W)
  val is_page_fault = Bool()
  val is_pmp_fault = Bool()
}


class Info_cmm_mmu extends Bundle {
  val satp = UInt(64.W)

	val pmpcfg = Vec(16, UInt(64.W))
  val pmpaddr = Vec(64, UInt(64.W))

  val priv_lvl = UInt(2.W)

  val mstatus = UInt(64.W)
  val sstatus = UInt(64.W)

}






/**
  * Top layer of Memory management unit
  * 
  * including instruction-translation-lookaside-buffer with physical-memory-protection
  * 
  * including data-translation-lookaside-buffer with physical-memory-protection
  * 
  * including page-table-walker with physical-memory-protection
  *
  */
class MMU(edge: TLEdgeOut)(implicit p: Parameters) extends RiftModule {
  val io = IO(new Bundle{
    val if_mmu = Flipped(ValidIO(new Info_mmu_req))
    val mmu_if = ValidIO(new Info_mmu_rsp)

    val lsu_mmu = Flipped(ValidIO(new Info_mmu_req))
    val mmu_lsu = ValidIO(new Info_mmu_rsp)

    val cmm_mmu = Input( new Info_cmm_mmu )


    val ptw_get    = new DecoupledIO(new TLBundleA(edge.bundle))
    val ptw_access = Flipped(new DecoupledIO(new TLBundleD(edge.bundle)))


    val flush = Input(Bool())
  })

  val itlb = Module( new TLB(32) )
  val dtlb = Module( new TLB(32) )
  val ptw  = Module( new PTW(edge) )

  val pmpcfg_vec = VecInit(
     io.cmm_mmu.pmpcfg(0)(7,0).asTypeOf(new Info_pmpcfg),   io.cmm_mmu.pmpcfg(0)(15,8).asTypeOf(new Info_pmpcfg),
     io.cmm_mmu.pmpcfg(0)(23,16).asTypeOf(new Info_pmpcfg), io.cmm_mmu.pmpcfg(0)(31,24).asTypeOf(new Info_pmpcfg),
     io.cmm_mmu.pmpcfg(0)(39,32).asTypeOf(new Info_pmpcfg), io.cmm_mmu.pmpcfg(0)(47,40).asTypeOf(new Info_pmpcfg),
     io.cmm_mmu.pmpcfg(0)(55,48).asTypeOf(new Info_pmpcfg), io.cmm_mmu.pmpcfg(0)(63,56).asTypeOf(new Info_pmpcfg)
  )

  val pmp_addr_vec = VecInit( Seq(0.U(64.W)) ++ (for ( i <- 0 until 8 ) yield io.cmm_mmu.pmpaddr(i)) )

  itlb.io.vaddr.valid := io.if_mmu.valid
  itlb.io.vaddr.bits  := io.if_mmu.bits.vaddr
  itlb.io.asid_i  := io.cmm_mmu.satp(59,44)

  dtlb.io.vaddr.valid := io.lsu_mmu.valid
  dtlb.io.vaddr.bits  := io.lsu_mmu.bits.vaddr
  dtlb.io.asid_i  := io.cmm_mmu.satp(59,44)

  ptw.io.satp_ppn := io.cmm_mmu.satp(43,0)

  val is_mmu_bypass_if = io.cmm_mmu.satp(63,60) === 0.U | io.cmm_mmu.priv_lvl === "b11".U

  val is_mmu_bypass_ls = (io.cmm_mmu.satp(63,60) === 0.U | io.cmm_mmu.priv_lvl === "b11".U) & 
                        ~(io.cmm_mmu.mstatus(17) === 1.U & io.cmm_mmu.priv_lvl === "b11".U)

  val ptw_req_no_dnxt = Wire(UInt(2.W))
  val ptw_req_no_qout = RegNext(ptw_req_no_dnxt, 0.U(2.W))

  ptw_req_no_dnxt := {
    val is_iptw = (io.if_mmu.valid  & ~is_mmu_bypass_if  & ~itlb.io.pte_o.valid)
    val is_dptw = (io.lsu_mmu.valid & ~is_mmu_bypass_ls & ~dtlb.io.pte_o.valid)
    
    MuxCase( ptw_req_no_qout, Array(
      (ptw_req_no_qout === 0.U & is_iptw) -> 1.U,
      (ptw_req_no_qout === 0.U & is_dptw) -> 2.U,
      (ptw.io.ptw_o.valid)       -> 0.U
    ))
  }

  val is_ptw_pmp_fault = RegEnable(
    PMP( pmp_addr_vec, pmpcfg_vec, ptw.io.ptw_get.bits.address, io.cmm_mmu.priv_lvl , Cat(false.B, false.B, true.B)),
    ptw.io.ptw_get.valid
    )


  val i_paddr = {
    val ipte = Mux( itlb.io.pte_o.valid, itlb.io.pte_o.bits, ptw.io.ptw_o.bits )
    val pa_ppn_2 = ipte.ppn(2)
    val pa_ppn_1 = Mux( (ipte.is_giga_page ), io.if_mmu.bits.vaddr(29,21), ipte.ppn(1) )
    val pa_ppn_0 = Mux( (ipte.is_giga_page | ipte.is_mega_page), io.if_mmu.bits.vaddr(20,12), ipte.ppn(0) )
    val pa_pgoff = io.if_mmu.bits.vaddr(11,0)
    Cat( pa_ppn_2, pa_ppn_1, pa_ppn_0, pa_pgoff )
  }

  val d_paddr = {
    val dpte = Mux( dtlb.io.pte_o.valid, dtlb.io.pte_o.bits, ptw.io.ptw_o.bits )
    val pa_ppn_2 = dpte.ppn(2)
    val pa_ppn_1 = Mux( (dpte.is_giga_page ), io.lsu_mmu.bits.vaddr(29,21), dpte.ppn(1) )
    val pa_ppn_0 = Mux( (dpte.is_giga_page | dpte.is_mega_page), io.lsu_mmu.bits.vaddr(20,12), dpte.ppn(0) )
    val pa_pgoff = io.lsu_mmu.bits.vaddr(11,0)
    Cat( pa_ppn_2, pa_ppn_1, pa_ppn_0, pa_pgoff )
  }


  io.mmu_if.valid := Mux( is_mmu_bypass_if, io.if_mmu.valid, (itlb.io.pte_o.valid | (ptw.io.ptw_o.valid & ptw_req_no_qout === 1.U ) ) )
  io.mmu_if.bits.paddr := Mux( is_mmu_bypass_if, io.if_mmu.bits.vaddr, i_paddr )
  
  io.mmu_if.bits.is_page_fault := {
    val ipte = Mux( itlb.io.pte_o.valid, itlb.io.pte_o.bits, ptw.io.ptw_o.bits )
    ~is_mmu_bypass_if & is_chk_page_fault( ipte, io.if_mmu.bits.vaddr, io.cmm_mmu.priv_lvl, "b100".U)
  }
  
  io.mmu_if.bits.is_pmp_fault := 
    PMP( pmp_addr_vec, pmpcfg_vec, io.mmu_if.bits.paddr, io.cmm_mmu.priv_lvl , Cat(io.if_mmu.bits.is_X, io.if_mmu.bits.is_W, io.if_mmu.bits.is_R)) | 
    (is_ptw_pmp_fault & ptw_req_no_qout === 1.U)

  io.mmu_lsu.valid := Mux( is_mmu_bypass_ls, io.lsu_mmu.valid, (dtlb.io.pte_o.valid | (ptw.io.ptw_o.valid & ptw_req_no_qout === 2.U )) )
  io.mmu_lsu.bits.paddr := Mux( is_mmu_bypass_ls, io.lsu_mmu.bits.vaddr, d_paddr )

  io.mmu_lsu.bits.is_page_fault := {
    val dpte = Mux( dtlb.io.pte_o.valid, dtlb.io.pte_o.bits, ptw.io.ptw_o.bits )
    ~is_mmu_bypass_ls & is_chk_page_fault( dpte, io.lsu_mmu.bits.vaddr, io.cmm_mmu.priv_lvl, Cat(io.if_mmu.bits.is_X, io.lsu_mmu.bits.is_W, io.lsu_mmu.bits.is_R ))
  }

  io.mmu_lsu.bits.is_pmp_fault := 
    PMP( pmp_addr_vec, pmpcfg_vec, io.mmu_lsu.bits.paddr, io.cmm_mmu.priv_lvl , Cat(io.if_mmu.bits.is_X, io.if_mmu.bits.is_W, io.if_mmu.bits.is_R) ) | 
    (is_ptw_pmp_fault & ptw_req_no_qout === 2.U)









  itlb.io.tlb_renew.bits := ptw.io.ptw_o.bits
  dtlb.io.tlb_renew.bits := ptw.io.ptw_o.bits

  itlb.io.tlb_renew.valid := (ptw.io.ptw_o.valid & ptw_req_no_qout === 1.U ) & ~ptw.io.is_ptw_fail
  dtlb.io.tlb_renew.valid := (ptw.io.ptw_o.valid & ptw_req_no_qout === 2.U ) & ~ptw.io.is_ptw_fail





  ptw.io.vaddr.valid := ptw_req_no_dnxt === 1.U | ptw_req_no_dnxt === 2.U
  ptw.io.vaddr.bits := 
    MuxCase( DontCare, Array(
      (ptw_req_no_dnxt === 0.U) -> DontCare,
      (ptw_req_no_dnxt === 1.U) -> io.if_mmu.bits.vaddr,
      (ptw_req_no_dnxt === 2.U) -> io.lsu_mmu.bits.vaddr
    ))

  


  io.ptw_get <> ptw.io.ptw_get
  ptw.io.ptw_access <> io.ptw_access


  def is_chk_page_fault(
    pte: Info_pte_sv39,
    chk_vaddr: UInt,
    chk_priv: UInt,
    chk_type: UInt): Bool = {

      val is_vaddr_illegal = chk_vaddr(63,39) =/= Fill(25,chk_vaddr(38))
      val is_U_access_ilegal =
        (chk_priv === "b00".U & pte.U === false.B) |
        (chk_priv === "b01".U & pte.U === true.B & (io.cmm_mmu.sstatus(18) === false.B) | chk_type(2) === true.B )

      val is_A_illegal = pte.A === false.B
      val is_D_illegal = pte.D === false.B & chk_type(1) === true.B
      val is_MXR_illegal = io.cmm_mmu.mstatus(19) === false.B & pte.R === false.B

      return is_vaddr_illegal | is_U_access_ilegal | is_A_illegal | is_D_illegal | is_MXR_illegal
    }


  itlb.io.flush := io.flush
  dtlb.io.flush := io.flush

}
