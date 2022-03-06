
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



abstract class CsrFiles extends CsrFiles_H{




  def csr_read_res(addr: UInt) = {
    val pmpcfg_arr = {
      val addr_chk = for ( i <- 0 until 16 ) yield { addr === ("h3A0".U + i.U) }
      val reg_sel  = for ( i <- 0 until 16 ) yield { pmpcfg(i) }
      addr_chk zip reg_sel
    }

    val pmpaddr_arr = {
      val addr_chk = for ( i <- 0 until 64 ) yield { addr === ("h3B0".U + i.U) }
      val reg_sel  = for ( i <- 0 until 64 ) yield { pmpaddr(i) }
      addr_chk zip reg_sel
    }

    val hpmcounter_arr = {
      val addr_chk = for ( i <- 3 until 32 ) yield { addr === ("hC00".U + i.U) }
      val reg_sel  = for ( i <- 3 until 32 ) yield { hpmcounter(i) }
      addr_chk zip reg_sel
    }

    val mhpmcounter_arr = {
      val addr_chk = for ( i <- 3 until 32 ) yield { addr === ("hB00".U + i.U) }
      val reg_sel  = for ( i <- 3 until 32 ) yield { mhpmcounter(i) }
      addr_chk zip reg_sel      
    }

    val mhpmevent_arr = {
      val addr_chk = for ( i <- 3 until 32 ) yield { addr === ("h320".U + i.U) }
      val reg_sel  = for ( i <- 3 until 32 ) yield { mhpmevent(i) }
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
          ( addr === "h001".U ) -> fcsr(4,0),
          ( addr === "h002".U ) -> fcsr(7,5),
          ( addr === "h003".U ) -> fcsr,
          ( addr === "hC00".U ) -> cycle,
          ( addr === "hC01".U ) -> time,
          ( addr === "hC02".U ) -> instret,
          ( addr === "h100".U ) -> sstatus,
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
          ( addr === "h600".U ) -> hstatus,
          ( addr === "h602".U ) -> hedeleg,
          ( addr === "h603".U ) -> hideleg,
          ( addr === "h604".U ) -> hie,
          ( addr === "h606".U ) -> hcounteren,
          ( addr === "h607".U ) -> hgeie,
          ( addr === "h643".U ) -> htval,
          ( addr === "h644".U ) -> hip,
          ( addr === "h645".U ) -> hvip,
          ( addr === "h64A".U ) -> htinst,
          ( addr === "hE12".U ) -> hgeip,
          ( addr === "h680".U ) -> hgatp,
          ( addr === "h605".U ) -> htimedelta,
          ( addr === "h200".U ) -> vsstatus,
          ( addr === "h204".U ) -> vsie,
          ( addr === "h205".U ) -> vstvec,
          ( addr === "h240".U ) -> vsscratch,
          ( addr === "h241".U ) -> vsepc,
          ( addr === "h242".U ) -> vscause,
          ( addr === "h243".U ) -> vstval,
          ( addr === "h244".U ) -> vsip,
          ( addr === "h280".U ) -> vsatp,
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
          ( addr === "h7B3".U ) -> dscratch1
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
          ( addr === "h7B0".U ) -> false.B,
          ( addr === "h7B1".U ) -> false.B,
          ( addr === "h7B2".U ) -> false.B,
          ( addr === "h7B3".U ) -> false.B
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
          ( addr === "h7B0".U ) -> false.B,
          ( addr === "h7B1".U ) -> false.B,
          ( addr === "h7B2".U ) -> false.B,
          ( addr === "h7B3".U ) -> false.B
        )

    val res = Mux1H(pmpcfg_arr ++ pmpaddr_arr ++ hpmcounter_arr ++ mhpmcounter_arr ++ mhpmevent_arr ++ normal_arr )
    ~res
  }

} 
