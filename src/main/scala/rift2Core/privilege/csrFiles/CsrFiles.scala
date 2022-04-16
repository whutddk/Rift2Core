
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

package rift2Core.privilege.csrFiles


import chisel3._
import chisel3.util._
import rift2Core.define._
import rift2Core.privilege._

// abstract class  BaseCsrFiles extends BaseCommit

class FCSRBundle extends Bundle{
  val frm      = UInt(3.W)
  val fflags   = UInt(5.W)
}

/**
  * Machine Status Registers
  * @param SD (63) whether either fs or xs is dirty
  * @param MBE (37) endian of M-mode, when 0, is little-endian, hard-wire to 0.U(64.bits)
  * @param SBE (36) endian of S-mode, when 0, is little-endian, hard-wire to 0.U(64.bits)
  * @param SXL (35,34) XLEN of S-mode, hard-wire to 2.U(64.bits)
  * @param UXL (33,32) XLEN of U-mode, hard-wire to 2.U(64.bits)
  * @param TSR (22) trap sret, when 1, sret in s-mode cause illegal instruction exception
  * @param TW (21) Time out wait for WFI, when 1 wfi cause illegal instruction exception in U\S-MODE
  * @param TVM (20) Trap Virtual Memory, when 1, access satp or Sfence.vma will cause illegal instruction exception
  * @param MXR (19) Make executable Readable. When 0, only loads form pages marked readable
  * @param SUM (18) permit Supervisor User Memory access. When 0, S-mode accesses to page.U === 1 will fault.
  * @param MPRV (17) Memory Privilege; When 1, load store using trans&protect in M-mode; When MRET or SRet to S\U-mode, set to 0.
  * @param XS (16,15) aditional user-mode extension and associated state
  * @param FS (14,13) float point statuw=s
  * @param MPP (12,11) Previous Mode is U-mode or S-mode or M-mode? When MRet, privilege mode update to MPP, MPP set to "U"
  * @param SPP (8) Previous Mode is U-mode or S-mode? When SRet, privilege mode update to SPP, SPP set to "U"
  * @param MPIE (7)  When MRet, MPIE set to 1
  * @param UBE (6) endian of U-mode, when 0, is little-endian, hard-wire to 0.U(64.bits)
  * @param SPIE (5) When SRet, SPIE set to 1
  * @param MIE (3) M-mode Interrupt Enable; When MRet, update to MPIE
  * @param SIE (1) S-mode Interrupt Enable; When SRet, update to SPIE
  */
class MStatusBundle extends Bundle{
  val sd = UInt(1.W)
  val reserved0 = UInt(25.W)
  val mbe = UInt(1.W)
  val sbe = UInt(1.W)
  val sxl = UInt(2.W)
  val uxl = UInt(2.W)
  val reserved1 = UInt(9.W)
  val tsr = UInt(1.W)
  val tw = UInt(1.W)
  val tvm = UInt(1.W)
  val mxr = UInt(1.W)
  val sum = UInt(1.W)
  val mprv = UInt(1.W)
  val xs = UInt(2.W)
  val fs = UInt(2.W)
  val mpp = UInt(2.W)
  val reserved2 = UInt(2.W)
  val spp = UInt(1.W)
  val mpie = UInt(1.W)
  val ube = UInt(1.W)
  val spie = UInt(1.W)
  val reserved3 = UInt(1.W)
  val mie = UInt(1.W)
  val reserved4 = UInt(1.W)
  val sie = UInt(1.W)
  val reserved5 = UInt(1.W)
}

class MSIntBundle extends Bundle{
  val reserved0 = UInt(4.W)
  val mei       = UInt(1.W)
  val reserved1 = UInt(1.W)
  val sei       = UInt(1.W)
  val reserved2 = UInt(1.W)
  val mti       = UInt(1.W)
  val reserved3 = UInt(1.W)
  val sti       = UInt(1.W)
  val reserved4 = UInt(1.W)
  val msi       = UInt(1.W)
  val reserved5 = UInt(1.W)
  val ssi       = UInt(1.W)
  val reserved6 = UInt(1.W)
}

class TVecBundle extends Bundle{
  val base = UInt(62.W)
  val mode = UInt(2.W)
}

class CounterenBundle extends Bundle{
  val hpm = UInt(32.W)
  def ir = hpm(2)
  def tm = hpm(1)
  def cy = hpm(0)
}

class CauseBundle extends Bundle{
  val Interrupt = UInt(1.W)
  val exception_code = UInt(63.W)
}

class PmpcfgBundle extends Bundle{
  val L = UInt(1.W)
  val reserved0 = UInt(2.W)
  val A = UInt(2.W)
  val X = UInt(1.W)
  val W = UInt(1.W)
  val R = UInt(1.W)
}

class SatpBundle extends Bundle{
  val mode = UInt(1.W)
  val asid = UInt(9.W)
  val ppn = UInt(22.W)
}

class DcsrBundle extends Bundle{
  val xdebugver = UInt(4.W)
  val reserved0 = UInt(12.W)
  val ebreakm   = UInt(1.W)
  val reserved1 = UInt(1.W)
  val ebreaks   = UInt(1.W)
  val ebreaku   = UInt(1.W)
  val stepie    = UInt(1.W)
  val stopcount = UInt(1.W)
  val stoptime  = UInt(1.W)
  val cause     = UInt(3.W)
  val reserved2 = UInt(1.W)
  val mprven    = UInt(1.W)
  val nmip      = UInt(1.W)
  val step      = UInt(1.W)
  val prv       = UInt(2.W)
}

class CSR_Bundle extends Bundle {

  // val ustatus  = UInt(64.W)
  // val uie      = UInt(64.W)
  // val utvec    = UInt(64.W)
  // val uscratch = UInt(64.W)
  // val uepc     = UInt(64.W)
  // val ucause   = UInt(64.W)
  // val utval    = UInt(64.W)
  // val uip      = UInt(64.W)
  // val fflags   = UInt(64.W)
  // val frm      = UInt(64.W)
  val fcsr        = new FCSRBundle
  val cycle       = UInt(64.W)
  val time        = UInt(64.W)
  val instret     = UInt(64.W)
  // val sstatus     = UInt(64.W)
  // val sedeleg  = UInt(64.W)
  // val sideleg  = UInt(64.W)
  // val sie         = new MSIntBundle
  val stvec       = new TVecBundle
  val scounteren  = new CounterenBundle
  val sscratch    = UInt(64.W)
  val sepc        = UInt(64.W)
  val scause      = new CauseBundle
  val stval       = UInt(64.W)
  val sip         = new MSIntBundle
  val satp        = new SatpBundle
  // val hstatus     = UInt(64.W)
  // val hedeleg     = UInt(64.W)
  // val hideleg     = UInt(64.W)
  // val hie         = UInt(64.W)
  // val hcounteren  = UInt(64.W)
  // val hgeie       = UInt(64.W)
  // val htval       = UInt(64.W)
  // val hip         = UInt(64.W)
  // val hvip        = UInt(64.W)
  // val htinst      = UInt(64.W)
  // val hgeip       = UInt(64.W)
  // val hgatp       = UInt(64.W)
  // val htimedelta  = UInt(64.W)
  // val vsstatus    = UInt(64.W)
  // val vsie        = UInt(64.W)
  // val vstvec      = UInt(64.W)
  // val vsscratch   = UInt(64.W)
  // val vsepc       = UInt(64.W)
  // val vscause     = UInt(64.W)
  // val vstval      = UInt(64.W)
  // val vsip        = UInt(64.W)
  // val vsatp       = UInt(64.W)
  val mvendorid   = UInt(64.W)
  val marchid     = UInt(64.W)
  val mimpid      = UInt(64.W)
  val mhartid     = UInt(64.W)
  val mstatus     = new MStatusBundle
  val misa        = UInt(64.W)
  val medeleg     = UInt(64.W)
  val mideleg     = UInt(64.W)
  val mie         = new MSIntBundle
  val mtvec       = new TVecBundle
  val mcounteren  = new CounterenBundle
  val mscratch    = UInt(64.W)
  val mepc        = UInt(64.W)
  val mcause      = new CauseBundle
  val mtval       = UInt(64.W)
  val mip         = new MSIntBundle
  val mtinst      = UInt(64.W)
  val mtval2      = UInt(64.W)
  val mcycle      = UInt(64.W)
  val minstret    = UInt(64.W)
  val mcountinhibit = UInt(64.W)
  val tselect     = UInt(64.W)
  val tdata1      = UInt(64.W)
  val tdata2      = UInt(64.W)
  val tdata3      = UInt(64.W)
  val dcsr        = UInt(64.W)
  val dpc         = new DcsrBundle
  val dscratch0   = UInt(64.W)
  val dscratch1   = UInt(64.W)
  val dscratch2   = UInt(64.W)


  val pmpcfg  = Vec( 16, Vec(8, new PmpcfgBundle) )
  val pmpaddr = Vec( 64, UInt(64.W))


  val hpmcounter  = Vec( 32, UInt(64.W))
  val mhpmcounter = Vec( 32, UInt(64.W))
  val mhpmevent   = Vec( 32, UInt(64.W))


  def sstatus = mstatus & Cat( "b1".U, 0.U(29.W), "b11".U, 0.U(12.W), "b11011110000101100010".U )
  def sie = (mie.asUInt & Cat()).asTypeOf(new MSIntBundle)
  def sip = (mip.asUInt & Cat()).asTypeOf(new MSIntBundle)
}

// trait CsrFiles { this: BaseCsrFiles => 




// }

abstract class CsrFiles extends CsrFiles_H{




  def csr_read_res(addr: UInt) = {
    val pmpcfg_arr = {
      val addr_chk = for ( i <- 0 until 16 ) yield { addr === ("h3A0".U + i.U) }
      val reg_sel  = for ( i <- 0 until 16 ) yield { Seq(pmpcfg(i)) ++ pmpcfg_dnxt(i) }
      addr_chk zip reg_sel
    }

    val pmpaddr_arr = {
      val addr_chk = for ( i <- 0 until 64 ) yield { addr === ("h3B0".U + i.U) }
      val reg_sel  = for ( i <- 0 until 64 ) yield { Seq(pmpaddr(i)) ++ pmpaddr_dnxt(i) }
      addr_chk zip reg_sel
    }

    val hpmcounter_arr = {
      val addr_chk = for ( i <- 3 until 32 ) yield { addr === ("hC00".U + i.U) }
      val reg_sel  = for ( i <- 3 until 32 ) yield { Seq(hpmcounter(i)) ++ hpmcounter_dnxt(i) }
      addr_chk zip reg_sel
    }

    val mhpmcounter_arr = {
      val addr_chk = for ( i <- 3 until 32 ) yield { addr === ("hB00".U + i.U) }
      val reg_sel  = for ( i <- 3 until 32 ) yield { Seq(mhpmcounter(i)) ++ mhpmcounter_dnxt(i) }
      addr_chk zip reg_sel      
    }

    val mhpmevent_arr = {
      val addr_chk = for ( i <- 3 until 32 ) yield { addr === ("h320".U + i.U) }
      val reg_sel  = for ( i <- 3 until 32 ) yield { Seq(mhpmevent(i)) ++ mhpmevent_dnxt(i) }
      addr_chk zip reg_sel      
    }

    val normal_arr = Array(
          // ( addr === "h000".U ) -> ustatus,
          // ( addr === "h004".U ) -> uie,
          // ( addr === "h005".U ) -> utvec,
          // ( addr === "h040".U ) -> uscratch,
          // ( addr === "h041".U ) -> uepc,
          // ( addr === "h042".U ) -> ucause,
          // ( addr === "h043".U ) -> utval,
          // ( addr === "h044".U ) -> uip,
          ( addr === "h001".U ) -> ( Seq(fcsr(4,0) ++ fcsr_dnxt.map(_(4,0))), 
          ( addr === "h002".U ) -> ( Seq(fcsr(7,5) ++ fcsr_dnxt.map(_(7,5))),
          ( addr === "h003".U ) -> ( Seq(fcsr      ++ fcsr_dnxt),
          ( addr === "hC00".U ) -> ( Seq(cycle     ++ cycle_dnxt),
          ( addr === "hC01".U ) -> ( Seq(time      ++ time_dnxt),
          ( addr === "hC02".U ) -> ( Seq(instret   ++ instret_dnxt),
          ( addr === "h100".U ) -> ( Seq(sstatus   ++ sstatus_dnxt),
          // ( addr === "h102".U ) -> sedeleg,
          // ( addr === "h103".U ) -> sideleg,
          ( addr === "h104".U ) -> sie,
          ( addr === "h105".U ) -> stvec,
          ( addr === "h106".U ) -> scounteren,
          ( addr === "h140".U ) -> sscratch,
          ( addr === "h141".U ) -> sepc,
          ( addr === "h142".U ) -> scause,
          ( addr === "h143".U ) -> stval,
          ( addr === "h144".U ) -> sip,
          ( addr === "h180".U ) -> satp,
          // ( addr === "h600".U ) -> hstatus,
          // ( addr === "h602".U ) -> hedeleg,
          // ( addr === "h603".U ) -> hideleg,
          // ( addr === "h604".U ) -> hie,
          // ( addr === "h606".U ) -> hcounteren,
          // ( addr === "h607".U ) -> hgeie,
          // ( addr === "h643".U ) -> htval,
          // ( addr === "h644".U ) -> hip,
          // ( addr === "h645".U ) -> hvip,
          // ( addr === "h64A".U ) -> htinst,
          // ( addr === "hE12".U ) -> hgeip,
          // ( addr === "h680".U ) -> hgatp,
          // ( addr === "h605".U ) -> htimedelta,
          // ( addr === "h200".U ) -> vsstatus,
          // ( addr === "h204".U ) -> vsie,
          // ( addr === "h205".U ) -> vstvec,
          // ( addr === "h240".U ) -> vsscratch,
          // ( addr === "h241".U ) -> vsepc,
          // ( addr === "h242".U ) -> vscause,
          // ( addr === "h243".U ) -> vstval,
          // ( addr === "h244".U ) -> vsip,
          // ( addr === "h280".U ) -> vsatp,
          ( addr === "hF11".U ) -> mvendorid,
          ( addr === "hF12".U ) -> marchid,
          ( addr === "hF13".U ) -> mimpid,
          ( addr === "hF14".U ) -> mhartid,
          ( addr === "h300".U ) -> mstatus,
          ( addr === "h301".U ) -> misa,
          ( addr === "h302".U ) -> medeleg,
          ( addr === "h303".U ) -> mideleg,
          ( addr === "h304".U ) -> mie,
          ( addr === "h305".U ) -> mtvec,
          ( addr === "h306".U ) -> mcounteren,
          ( addr === "h340".U ) -> mscratch,
          ( addr === "h341".U ) -> mepc,
          ( addr === "h342".U ) -> mcause,
          ( addr === "h343".U ) -> mtval,
          ( addr === "h344".U ) -> mip,
          ( addr === "h34A".U ) -> mtinst,
          ( addr === "h34B".U ) -> mtval2,

          ( addr === "hB00".U ) -> mcycle,
          ( addr === "hB02".U ) -> minstret,
          ( addr === "h320".U ) -> mcountinhibit,
          ( addr === "h7A0".U ) -> tselect,
          ( addr === "h7A1".U ) -> tdata1,
          ( addr === "h7A2".U ) -> tdata2,
          ( addr === "h7A3".U ) -> tdata3,
          ( addr === "h7B0".U ) -> dcsr,
          ( addr === "h7B1".U ) -> dpc,
          ( addr === "h7B2".U ) -> dscratch0,
          ( addr === "h7B3".U ) -> dscratch1,
          ( addr === "h7B4".U ) -> dscratch2,
        )

    Mux1H(pmpcfg_arr ++ pmpaddr_arr ++ hpmcounter_arr ++ mhpmcounter_arr ++ mhpmevent_arr ++ normal_arr )
  }

  def csr_read_prilvl(addr: UInt) = {
    val pmpcfg_arr = {
      val addr_chk = for ( i <- 0 until 16 ) yield { addr === ("h3A0".U + i.U) }
      val reg_sel  = for ( i <- 0 until 16 ) yield { priv_lvl_qout >= "b11".U }
      addr_chk zip reg_sel
    }

    val pmpaddr_arr = {
      val addr_chk = for ( i <- 0 until 64 ) yield { addr === ("h3B0".U + i.U) }
      val reg_sel  = for ( i <- 0 until 64 ) yield { priv_lvl_qout >= "b11".U }
      addr_chk zip reg_sel
    }

    val hpmcounter_arr = {
      val addr_chk = for ( i <- 3 until 32 ) yield { addr === ("hC00".U + i.U) }
      val reg_sel  = for ( i <- 3 until 32 ) yield { false.B }
      addr_chk zip reg_sel
    }

    val mhpmcounter_arr = {
      val addr_chk = for ( i <- 3 until 32 ) yield { addr === ("hB00".U + i.U) }
      val reg_sel  = for ( i <- 3 until 32 ) yield { false.B }
      addr_chk zip reg_sel      
    }

    val mhpmevent_arr = {
      val addr_chk = for ( i <- 3 until 32 ) yield { addr === ("h320".U + i.U) }
      val reg_sel  = for ( i <- 3 until 32 ) yield { false.B }
      addr_chk zip reg_sel      
    }

    val normal_arr = Array(
          // ( addr === "h000".U ) -> ustatus,
          // ( addr === "h004".U ) -> uie,
          // ( addr === "h005".U ) -> utvec,
          // ( addr === "h040".U ) -> uscratch,
          // ( addr === "h041".U ) -> uepc,
          // ( addr === "h042".U ) -> ucause,
          // ( addr === "h043".U ) -> utval,
          // ( addr === "h044".U ) -> uip,
          ( addr === "h001".U ) -> (priv_lvl_qout >= "b00".U),
          ( addr === "h002".U ) -> (priv_lvl_qout >= "b00".U),
          ( addr === "h003".U ) -> (priv_lvl_qout >= "b00".U),
          ( addr === "hC00".U ) -> (priv_lvl_qout >= "b00".U),
          ( addr === "hC01".U ) -> (priv_lvl_qout >= "b00".U),
          ( addr === "hC02".U ) -> (priv_lvl_qout >= "b00".U),
          ( addr === "h100".U ) -> (priv_lvl_qout >= "b00".U),
          // ( addr === "h102".U ) -> sedeleg,
          // ( addr === "h103".U ) -> sideleg,
          ( addr === "h104".U ) -> (priv_lvl_qout >= "b01".U),
          ( addr === "h105".U ) -> (priv_lvl_qout >= "b01".U),
          ( addr === "h106".U ) -> (priv_lvl_qout >= "b01".U),
          ( addr === "h140".U ) -> (priv_lvl_qout >= "b01".U),
          ( addr === "h141".U ) -> (priv_lvl_qout >= "b01".U),
          ( addr === "h142".U ) -> (priv_lvl_qout >= "b01".U),
          ( addr === "h143".U ) -> (priv_lvl_qout >= "b01".U),
          ( addr === "h144".U ) -> (priv_lvl_qout >= "b01".U),
          ( addr === "h180".U ) -> ((priv_lvl_qout === "b11".U) | (priv_lvl_qout === "b01".U & mstatus(20) === 0.U)), //TVM IN S-MODE
          ( addr === "h600".U ) -> false.B,
          ( addr === "h602".U ) -> false.B,
          ( addr === "h603".U ) -> false.B,
          ( addr === "h604".U ) -> false.B,
          ( addr === "h606".U ) -> false.B,
          ( addr === "h607".U ) -> false.B,
          ( addr === "h643".U ) -> false.B,
          ( addr === "h644".U ) -> false.B,
          ( addr === "h645".U ) -> false.B,
          ( addr === "h64A".U ) -> false.B,
          ( addr === "hE12".U ) -> false.B,
          ( addr === "h680".U ) -> false.B,
          ( addr === "h605".U ) -> false.B,
          ( addr === "h200".U ) -> false.B,
          ( addr === "h204".U ) -> false.B,
          ( addr === "h205".U ) -> false.B,
          ( addr === "h240".U ) -> false.B,
          ( addr === "h241".U ) -> false.B,
          ( addr === "h242".U ) -> false.B,
          ( addr === "h243".U ) -> false.B,
          ( addr === "h244".U ) -> false.B,
          ( addr === "h280".U ) -> false.B,
          ( addr === "hF11".U ) -> (priv_lvl_qout >= "b11".U),
          ( addr === "hF12".U ) -> (priv_lvl_qout >= "b11".U),
          ( addr === "hF13".U ) -> (priv_lvl_qout >= "b11".U),
          ( addr === "hF14".U ) -> (priv_lvl_qout >= "b11".U),
          ( addr === "h300".U ) -> (priv_lvl_qout >= "b11".U),
          ( addr === "h301".U ) -> (priv_lvl_qout >= "b11".U),
          ( addr === "h302".U ) -> (priv_lvl_qout >= "b11".U),
          ( addr === "h303".U ) -> (priv_lvl_qout >= "b11".U),
          ( addr === "h304".U ) -> (priv_lvl_qout >= "b11".U),
          ( addr === "h305".U ) -> (priv_lvl_qout >= "b11".U),
          ( addr === "h306".U ) -> (priv_lvl_qout >= "b11".U),
          ( addr === "h340".U ) -> (priv_lvl_qout >= "b11".U),
          ( addr === "h341".U ) -> (priv_lvl_qout >= "b11".U),
          ( addr === "h342".U ) -> (priv_lvl_qout >= "b11".U),
          ( addr === "h343".U ) -> (priv_lvl_qout >= "b11".U),
          ( addr === "h344".U ) -> (priv_lvl_qout >= "b11".U),
          ( addr === "h34A".U ) -> (priv_lvl_qout >= "b11".U),
          ( addr === "h34B".U ) -> (priv_lvl_qout >= "b11".U),

          ( addr === "hB00".U ) -> (priv_lvl_qout >= "b11".U),
          ( addr === "hB02".U ) -> (priv_lvl_qout >= "b11".U),
          ( addr === "h320".U ) -> (priv_lvl_qout >= "b11".U),
          ( addr === "h7A0".U ) -> (priv_lvl_qout >= "b11".U),
          ( addr === "h7A1".U ) -> (priv_lvl_qout >= "b11".U),
          ( addr === "h7A2".U ) -> (priv_lvl_qout >= "b11".U),
          ( addr === "h7A3".U ) -> (priv_lvl_qout >= "b11".U),
          ( addr === "h7B0".U ) -> is_inDebugMode,
          ( addr === "h7B1".U ) -> is_inDebugMode,
          ( addr === "h7B2".U ) -> is_inDebugMode,
          ( addr === "h7B3".U ) -> is_inDebugMode
        )

    val res = Mux1H(pmpcfg_arr ++ pmpaddr_arr ++ hpmcounter_arr ++ mhpmcounter_arr ++ mhpmevent_arr ++ normal_arr )
    ~res
  }


  def csr_write_denied(addr: UInt) = {
    val pmpcfg_arr = {
      val addr_chk = for ( i <- 0 until 16 ) yield { addr === ("h3A0".U + i.U) }
      val reg_sel  = for ( i <- 0 until 16 ) yield { true.B }
      addr_chk zip reg_sel
    }

    val pmpaddr_arr = {
      val addr_chk = for ( i <- 0 until 64 ) yield { addr === ("h3B0".U + i.U) }
      val reg_sel  = for ( i <- 0 until 64 ) yield { true.B}
      addr_chk zip reg_sel
    }

    val hpmcounter_arr = {
      val addr_chk = for ( i <- 3 until 32 ) yield { addr === ("hC00".U + i.U) }
      val reg_sel  = for ( i <- 3 until 32 ) yield { false.B }
      addr_chk zip reg_sel
    }

    val mhpmcounter_arr = {
      val addr_chk = for ( i <- 3 until 32 ) yield { addr === ("hB00".U + i.U) }
      val reg_sel  = for ( i <- 3 until 32 ) yield { false.B }
      addr_chk zip reg_sel      
    }

    val mhpmevent_arr = {
      val addr_chk = for ( i <- 3 until 32 ) yield { addr === ("h320".U + i.U) }
      val reg_sel  = for ( i <- 3 until 32 ) yield { false.B }
      addr_chk zip reg_sel      
    }

    val normal_arr = Array(
          // ( addr === "h000".U ) -> ustatus,
          // ( addr === "h004".U ) -> uie,
          // ( addr === "h005".U ) -> utvec,
          // ( addr === "h040".U ) -> uscratch,
          // ( addr === "h041".U ) -> uepc,
          // ( addr === "h042".U ) -> ucause,
          // ( addr === "h043".U ) -> utval,
          // ( addr === "h044".U ) -> uip,
          ( addr === "h001".U ) -> true.B,
          ( addr === "h002".U ) -> true.B,
          ( addr === "h003".U ) -> true.B,
          ( addr === "hC00".U ) -> false.B,
          ( addr === "hC01".U ) -> false.B,
          ( addr === "hC02".U ) -> false.B,
          ( addr === "h100".U ) -> true.B,
          // ( addr === "h102".U ) -> sedeleg,
          // ( addr === "h103".U ) -> sideleg,
          ( addr === "h104".U ) -> true.B,
          ( addr === "h105".U ) -> true.B,
          ( addr === "h106".U ) -> true.B,
          ( addr === "h140".U ) -> true.B,
          ( addr === "h141".U ) -> true.B,
          ( addr === "h142".U ) -> true.B,
          ( addr === "h143".U ) -> true.B,
          ( addr === "h144".U ) -> true.B,
          ( addr === "h180".U ) -> true.B,
          ( addr === "h600".U ) -> false.B,
          ( addr === "h602".U ) -> false.B,
          ( addr === "h603".U ) -> false.B,
          ( addr === "h604".U ) -> false.B,
          ( addr === "h606".U ) -> false.B,
          ( addr === "h607".U ) -> false.B,
          ( addr === "h643".U ) -> false.B,
          ( addr === "h644".U ) -> false.B,
          ( addr === "h645".U ) -> false.B,
          ( addr === "h64A".U ) -> false.B,
          ( addr === "hE12".U ) -> false.B,
          ( addr === "h680".U ) -> false.B,
          ( addr === "h605".U ) -> false.B,
          ( addr === "h200".U ) -> false.B,
          ( addr === "h204".U ) -> false.B,
          ( addr === "h205".U ) -> false.B,
          ( addr === "h240".U ) -> false.B,
          ( addr === "h241".U ) -> false.B,
          ( addr === "h242".U ) -> false.B,
          ( addr === "h243".U ) -> false.B,
          ( addr === "h244".U ) -> false.B,
          ( addr === "h280".U ) -> false.B,
          ( addr === "hF11".U ) -> true.B,
          ( addr === "hF12".U ) -> true.B,
          ( addr === "hF13".U ) -> true.B,
          ( addr === "hF14".U ) -> true.B,
          ( addr === "h300".U ) -> true.B,
          ( addr === "h301".U ) -> true.B,
          ( addr === "h302".U ) -> true.B,
          ( addr === "h303".U ) -> true.B,
          ( addr === "h304".U ) -> true.B,
          ( addr === "h305".U ) -> true.B,
          ( addr === "h306".U ) -> true.B,
          ( addr === "h340".U ) -> true.B,
          ( addr === "h341".U ) -> true.B,
          ( addr === "h342".U ) -> true.B,
          ( addr === "h343".U ) -> true.B,
          ( addr === "h344".U ) -> true.B,
          ( addr === "h34A".U ) -> true.B,
          ( addr === "h34B".U ) -> true.B,

          ( addr === "hB00".U ) -> true.B,
          ( addr === "hB02".U ) -> true.B,
          ( addr === "h320".U ) -> true.B,
          ( addr === "h7A0".U ) -> true.B,
          ( addr === "h7A1".U ) -> true.B,
          ( addr === "h7A2".U ) -> true.B,
          ( addr === "h7A3".U ) -> true.B,
          ( addr === "h7B0".U ) -> is_inDebugMode,
          ( addr === "h7B1".U ) -> is_inDebugMode,
          ( addr === "h7B2".U ) -> is_inDebugMode,
          ( addr === "h7B3".U ) -> is_inDebugMode
        )

    val res = Mux1H(pmpcfg_arr ++ pmpaddr_arr ++ hpmcounter_arr ++ mhpmcounter_arr ++ mhpmevent_arr ++ normal_arr )
    ~res
  }

} 
