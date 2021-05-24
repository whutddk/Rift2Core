
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

package rift2Core.privilege.csrFiles


import chisel3._
import chisel3.util._
import rift2Core.define._
import rift2Core.privilege._



abstract class CsrFiles extends CsrFiles_H{




  def csr_read(addr: UInt) = {
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
          ( addr === "h001".U ) -> fflags,
          ( addr === "h002".U ) -> frm,
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


} 
