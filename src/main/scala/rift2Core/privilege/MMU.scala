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

import tilelink._

class Info_pte_sv39 extends Bundle {
  val V = Bool()
  val R = Bool()
  val W = Bool()
  val X = Bool()
  val U = Bool()
  val G = Bool()
  val A = Bool()
  val D = Bool()
  val rsw = UInt(2.W)
  val ppn = MixedVec( Seq( UInt(9.W), UInt(9.W), UInt(26.W) ) )
  val reserved = UInt(10.W)

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


class Info_csr_mmu extends Bundle {
  val satp = UInt(64.W)

	val pmpcfg = Vec(16, UInt(64.W))
  val pmpaddr = Vec(64, UInt(64.W))

  val priv_lvl = UInt(2.W)

  val mstatus = UInt(64.W)

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
class MMU extends Module {
  val io = IO(new Bundle{
    val if_mmu = ValidIO(UInt(64.W))
    val mmu_if = ValidIO(new Info_mmu_rsp)

    val iss_mmu = ValidIO(new Info_mmu_req)
    val mmu_iss = ValidIO(new Info_mmu_rsp)

    val csr_mmu = Input( new Info_csr_mmu )

    val mmu_chn_a = new DecoupledIO(new TLchannel_a(64, 32))
    val mmu_chn_d = Flipped(new DecoupledIO(new TLchannel_d(64)))


    val flush = Input(Bool())
  })

  val itlb = Module( new TLB(32) )
  val dtlb = Module( new TLB(32) )
  val ptw  = Module( new PTW )

  val pmpcfg_vec = VecInit(
     io.csr_mmu.pmpcfg(0)(7,0),   io.csr_mmu.pmpcfg(0)(15,8),
     io.csr_mmu.pmpcfg(0)(23,16), io.csr_mmu.pmpcfg(0)(31,24),
     io.csr_mmu.pmpcfg(0)(39,32), io.csr_mmu.pmpcfg(0)(47,40),
     io.csr_mmu.pmpcfg(0)(55,48), io.csr_mmu.pmpcfg(0)(63,56)
  )

  val pmp_addr_vec = VecInit( 0.U(64.W), for ( i <- 0 unitl 8 ) yield io.csr_mmu.pmpaddr(i) )

  itlb.io.vaddr  := io.if_mmu.bits
  itlb.io.asid_i := io.csr_mmu.satp(59,44)
  dtlb.io.vaddr  := io.iss_mmu.bits
  dtlb.io.asid_i := io.csr_mmu.satp(59,44)

  val is_mmu_bypass_if = io.csr_mmu.satp(63,60) === 0.U | io.csr_mmu.priv_lvl === "b11".U

  val is_mmu_bypass_ls = (io.csr_mmu.satp(63,60) === 0.U | io.csr_mmu.priv_lvl === "b11".U) & 
                        ~(io.csr_mmu.mstatus(17) === 1.U & io.csr_mmu.priv_lvl === "b11".U)

  val req_no = RegInit(0.U(2.W))

  when( req_no === 0.U & io.if_mmu.valid & ~is_mmu_bypass_if & ~itlb.io.hit ){ req_no := 1.U }
  .elsewhen( req_no === 0.U & io.iss_mmu.valid & ~is_mmu_bypass_iss & ~dtlb.io.hit ){ req_no := 2.U }
  .elsewhen( ptw.io.ptw_tlb.valid ){ req_no := 0.U }

  val ipte = Mux( itlb.io.is_hit, itlb.io.pte_o, ptw.io.ptw_tlb.bits.pte )

  val ptw_rtn = 

  io.mmu_if.valid := Mux( is_mmu_bypass_if, io.if_mmu.valid, Mux( itlb.io.is_hit, true.B, Mux(  ) ) )
  io.mmu_if.bits.paddr := Mux( is_mmu_bypass_if, io.if_mmu.bits.vaddr, Mux( itlb.io.is_hit, ) )    
  io.mmu_if.bits.is_page_fault := Mux( is_mmu_bypass_if, false.B, Mux( itlb.io.is_hit,)  
  io.mmu_if.bits.is_pmp_fault := 
    PMP( pmp_addr_vec, pmpcfg_vec, io.mmu_if.bits.paddr, io.csr_mmu.priv_lvl , Cat(io.if_mmu.bits.is_X, io.if_mmu.bits.is_W, io.if_mmu.bits.is_R)) | 


  io.mmu_iss.valid := Mux( is_mmu_bypass_iss, io.iss_mmu.valid, Mux( dtlb.io.is_hit, true.B, Mux() ))
  io.mmu_iss.bits.paddr := Mux( is_mmu_bypass_iss, io.iss_mmu.bits.vaddr,  Mux( dtlb.io.is_hit, ) )    
  io.mmu_iss.bits.is_page_fault := Mux( is_mmu_bypass_iss, false.B,  Mux( dtlb.io.is_hit, ) )  
  io.mmu_iss.bits.is_pmp_fault := 
    PMP( pmp_addr_vec, pmpcfg_vec, io.mmu_iss.bits.paddr, io.csr_mmu.priv_lvl , Cat(io.if_mmu.bits.is_X, io.if_mmu.bits.is_W, io.if_mmu.bits.is_R) ) | 




    itlb.io.pte_o  = Output( new Info_pte_sv39 )
    dtlb.io.pte_o  = Output( new Info_pte_sv39 )

    itlb.io.ptw_tlb = Flipped(ValidIO(new Info_ptw_tlb))
    dtlb.io.ptw_tlb = Flipped(ValidIO(new Info_ptw_tlb))

    itlb.io.is_hit = Output(Bool())
    dtlb.io.is_hit = Output(Bool())

    itlb.io.flush = Input(Bool())
    dtlb.io.flush = Input(Bool())



    ptw.io.vaddr = Flipped(ValidIO(UInt(64.W)))
    ptw.io.ptw_tlb = ValidIO(new Info_ptw_tlb)
    ptw.io.satp_ppn = Input(UInt(44.W))
    ptw.io.ptw_chn_a = new DecoupledIO(new TLchannel_a(64, 32))
    ptw.io.ptw_chn_d = Flipped(new DecoupledIO(new TLchannel_d(64)))
}
