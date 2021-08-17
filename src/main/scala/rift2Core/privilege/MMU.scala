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




trait Info_access_lvl extends Bundle {
  val is_X = Bool()
  val is_W = Bool()
  val is_R = Bool()
}


class Info_mmu_req extends Bundle with Info_access_lvl{
  val vaddr = UInt(64.W)
}


class Info_mmu_rsp extends Bundle {
  val paddr  = UInt(64.W)
  val is_paging_fault = Bool()
  val is_access_fault = Bool()

  def is_fault = is_access_fault | is_paging_fault
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
    val if_mmu = Flipped(DecoupledIO(new Info_mmu_req))
    val mmu_if = DecoupledIO(new Info_mmu_rsp)
    val if_flush = Input(Bool())

    val lsu_mmu = Flipped(DecoupledIO(new Info_mmu_req))
    val mmu_lsu = DecoupledIO(new Info_mmu_rsp)
    val lsu_flush = Input(Bool())


    val cmm_mmu = Input( new Info_cmm_mmu )


    val ptw_get    = new DecoupledIO(new TLBundleA(edge.bundle))
    val ptw_access = Flipped(new DecoupledIO(new TLBundleD(edge.bundle)))


    val flush = Input(Bool())
  })

  val itlb = Module( new TLB(32) )
  val dtlb = Module( new TLB(32) )
  val ptw  = Module( new PTW(edge) )

  val is_bypass_if = io.cmm_mmu.satp(63,60) === 0.U | io.cmm_mmu.priv_lvl === "b11".U
  val is_bypass_ls = (io.cmm_mmu.satp(63,60) === 0.U | io.cmm_mmu.priv_lvl === "b11".U) & 
                        ~(io.cmm_mmu.mstatus(17) === 1.U & io.cmm_mmu.priv_lvl === "b11".U)

  val ptw_arb = Module(new Arbiter(new Info_mmu_req, 2))

  val kill_iptw = RegInit(false.B)
  val kill_dptw = RegInit(false.B)

  when( io.if_flush & io.if_mmu.valid & ~io.mmu_if.fire ) {
    kill_iptw := true.B
  } .elsewhen ( io.mmu_if.fire ) {
    kill_iptw := false.B
  }


  when( io.lsu_flush & io.lsu_mmu.valid & ~io.mmu_lsu.fire ) {
    kill_dptw := true.B
  } .elsewhen ( io.mmu_lsu.fire ) {
    kill_dptw := false.B
  }

  // val immu_rsp_fifo = Module(new Queue(new Info_mmu_rsp, 4, false, true) )
  // val dmmu_rsp_fifo = Module(new Queue(new Info_mmu_rsp, 4, false, true) )

  // io.mmu_if  <> immu_rsp_fifo.io.deq
  // io.mmu_lsu <> dmmu_rsp_fifo.io.deq

  itlb.io.req.valid := io.if_mmu.valid
  itlb.io.req.bits  := io.if_mmu.bits
  itlb.io.asid_i  := io.cmm_mmu.satp(59,44)
  io.if_mmu.ready :=
    io.mmu_if.fire & ~io.if_flush & ~kill_iptw



  dtlb.io.req.valid := io.lsu_mmu.valid
  dtlb.io.req.bits  := io.lsu_mmu.bits
  dtlb.io.asid_i  := io.cmm_mmu.satp(59,44)
  io.lsu_mmu.ready :=
    io.mmu_lsu.fire & ~io.lsu_flush & ~kill_dptw



  ptw.io.cmm_mmu := io.cmm_mmu


  ptw_arb.io.in(0).valid := io.if_mmu.valid & ~itlb.io.is_hit & ~is_bypass_if
  ptw_arb.io.in(0).bits  := io.if_mmu.bits

  ptw_arb.io.in(1).valid := io.lsu_mmu.valid & ~dtlb.io.is_hit & ~is_bypass_ls
  ptw_arb.io.in(1).bits  := io.lsu_mmu.bits

  ptw_arb.io.out <> ptw.io.ptw_i






  {
    val pte = Mux( itlb.io.is_hit, itlb.io.pte_o, ptw.io.ptw_o.bits.pte )
    val vaddr = io.if_mmu.bits.vaddr

    val ipaddr = Mux( is_bypass_if, vaddr, v2paddr( vaddr, pte ) )
      
    io.mmu_if.bits.paddr := ipaddr

    io.mmu_if.bits.is_access_fault :=
      PMP( io.cmm_mmu, ipaddr, Cat( io.if_mmu.bits.is_X, io.if_mmu.bits.is_W, io.if_mmu.bits.is_R) ) | 
      (ptw.io.ptw_o.bits.is_access_fault & ptw.io.ptw_o.bits.is_X & ptw.io.ptw_o.valid) |
      ipaddr(63,32) =/= (0.U)

    io.mmu_if.bits.is_paging_fault := 
      ~is_bypass_if &
      (
        is_chk_page_fault( pte, io.if_mmu.bits.vaddr, io.cmm_mmu.priv_lvl, "b100".U) |
        (ptw.io.ptw_o.bits.is_ptw_fail & ptw.io.ptw_o.bits.is_X & ptw.io.ptw_o.valid)
      )

    io.mmu_if.valid :=
      ( ~io.if_flush & ~kill_iptw ) & (
        (io.if_mmu.valid & is_bypass_if) |
        (io.if_mmu.valid & itlb.io.is_hit) |
        (io.if_mmu.valid & (ptw.io.ptw_o.bits.is_X & ptw.io.ptw_o.valid))        
      )



    assert( ~((ptw.io.ptw_o.bits.is_X & ptw.io.ptw_o.valid) & itlb.io.is_hit)  )
  }


  {
    val pte = Mux( dtlb.io.is_hit, dtlb.io.pte_o, ptw.io.ptw_o.bits.pte )
    val vaddr = io.lsu_mmu.bits.vaddr

    val dpaddr = Mux( is_bypass_ls, vaddr, v2paddr( vaddr, pte ) )
  
    io.mmu_lsu.bits.paddr := dpaddr

    io.mmu_lsu.bits.is_access_fault :=
      PMP( io.cmm_mmu, dpaddr, Cat(io.lsu_mmu.bits.is_X, io.lsu_mmu.bits.is_W, io.lsu_mmu.bits.is_R) ) | 
      (ptw.io.ptw_o.bits.is_access_fault & (~ptw.io.ptw_o.bits.is_X & ptw.io.ptw_o.valid)) |
      dpaddr(63,32) =/= (0.U)

    io.mmu_lsu.bits.is_paging_fault :=
      ~is_bypass_ls & (
        is_chk_page_fault( pte, io.lsu_mmu.bits.vaddr, io.cmm_mmu.priv_lvl, Cat(io.lsu_mmu.bits.is_X, io.lsu_mmu.bits.is_W, io.lsu_mmu.bits.is_R )) |
        ptw.io.ptw_o.bits.is_ptw_fail & ~ptw.io.ptw_o.bits.is_X & ptw.io.ptw_o.valid
      )

    io.mmu_lsu.valid :=
      ( ~io.lsu_flush & ~kill_dptw ) & (
        (io.lsu_mmu.valid & is_bypass_ls) |
        (io.lsu_mmu.valid & dtlb.io.is_hit) |
        (io.lsu_mmu.valid & (~ptw.io.ptw_o.bits.is_X & ptw.io.ptw_o.valid))        
      )





    assert( ~((~ptw.io.ptw_o.bits.is_X & ptw.io.ptw_o.valid) & dtlb.io.is_hit)  )
  }

  ptw.io.ptw_o.ready := 
    Mux( ptw.io.ptw_o.bits.is_X,
    io.mmu_if.ready  | kill_iptw | io.if_flush,
    io.mmu_lsu.ready | kill_dptw | io.lsu_flush )


  // when( ptw.io.ptw_o.fire ) {
  //   when( ptw.io.ptw_o.bits.is_X ) {
  //     assert ( immu_rsp_fifo.io.enq.ready === true.B )          
  //     assert ( immu_rsp_fifo.io.enq.ready === true.B )          
  //   }

  // }

  
  





  itlb.io.tlb_renew.bits := ptw.io.ptw_o.bits.pte
  dtlb.io.tlb_renew.bits := ptw.io.ptw_o.bits.pte

  itlb.io.tlb_renew.valid := ptw.io.ptw_o.fire &  ptw.io.ptw_o.bits.is_X & ~ptw.io.ptw_o.bits.is_ptw_fail
  dtlb.io.tlb_renew.valid := ptw.io.ptw_o.fire & ~ptw.io.ptw_o.bits.is_X & ~ptw.io.ptw_o.bits.is_ptw_fail




  io.ptw_get <> ptw.io.ptw_get
  ptw.io.ptw_access <> io.ptw_access

  itlb.io.flush := io.flush
  dtlb.io.flush := io.flush



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


  def v2paddr( vaddr: UInt, pte: Info_pte_sv39 ): UInt = {
    val paddr = Wire( UInt(64.W) )

    val pa_ppn_2 = pte.ppn(2)
    val pa_ppn_1 = Mux( (pte.is_giga_page ), vaddr(29,21), pte.ppn(1) )
    val pa_ppn_0 = Mux( (pte.is_giga_page | pte.is_mega_page), vaddr(20,12), pte.ppn(0) )
    val pa_pgoff = vaddr(11,0)

    paddr := Cat( pa_ppn_2, pa_ppn_1, pa_ppn_0, pa_pgoff )
    return paddr
  }    



}
