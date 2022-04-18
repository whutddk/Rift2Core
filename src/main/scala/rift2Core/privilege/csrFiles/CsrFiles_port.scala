

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


class Exe_Port extends Bundle {
  val addr = UInt(12.W)
  val dat_i = UInt(64.W)
  val op_rw = Bool()
  val op_rs = Bool()
  val op_rc = Bool()

  def csr_read_prilvl(priv_lvl: UInt, DMode: Bool) = {
    val pmpcfg_arr = {
      val addr_chk = for ( i <- 0 until 16 ) yield { addr === ("h3A0".U + i.U) }
      val reg_sel  = for ( i <- 0 until 16 ) yield { priv_lvl >= "b11".U }
      addr_chk zip reg_sel
    }

    val pmpaddr_arr = {
      val addr_chk = for ( i <- 0 until 64 ) yield { addr === ("h3B0".U + i.U) }
      val reg_sel  = for ( i <- 0 until 64 ) yield { priv_lvl >= "b11".U }
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
          ( addr === "h001".U ) -> (priv_lvl >= "b00".U),
          ( addr === "h002".U ) -> (priv_lvl >= "b00".U),
          ( addr === "h003".U ) -> (priv_lvl >= "b00".U),
          ( addr === "hC00".U ) -> (priv_lvl >= "b00".U),
          ( addr === "hC01".U ) -> (priv_lvl >= "b00".U),
          ( addr === "hC02".U ) -> (priv_lvl >= "b00".U),
          ( addr === "h100".U ) -> (priv_lvl >= "b00".U),
          // ( addr === "h102".U ) -> sedeleg,
          // ( addr === "h103".U ) -> sideleg,
          ( addr === "h104".U ) -> (priv_lvl >= "b01".U),
          ( addr === "h105".U ) -> (priv_lvl >= "b01".U),
          ( addr === "h106".U ) -> (priv_lvl >= "b01".U),
          ( addr === "h140".U ) -> (priv_lvl >= "b01".U),
          ( addr === "h141".U ) -> (priv_lvl >= "b01".U),
          ( addr === "h142".U ) -> (priv_lvl >= "b01".U),
          ( addr === "h143".U ) -> (priv_lvl >= "b01".U),
          ( addr === "h144".U ) -> (priv_lvl >= "b01".U),
          ( addr === "h180".U ) -> ((priv_lvl === "b11".U) | (priv_lvl === "b01".U & mstatus(20) === 0.U)), //TVM IN S-MODE
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
          ( addr === "hF11".U ) -> (priv_lvl >= "b11".U),
          ( addr === "hF12".U ) -> (priv_lvl >= "b11".U),
          ( addr === "hF13".U ) -> (priv_lvl >= "b11".U),
          ( addr === "hF14".U ) -> (priv_lvl >= "b11".U),
          ( addr === "h300".U ) -> (priv_lvl >= "b11".U),
          ( addr === "h301".U ) -> (priv_lvl >= "b11".U),
          ( addr === "h302".U ) -> (priv_lvl >= "b11".U),
          ( addr === "h303".U ) -> (priv_lvl >= "b11".U),
          ( addr === "h304".U ) -> (priv_lvl >= "b11".U),
          ( addr === "h305".U ) -> (priv_lvl >= "b11".U),
          ( addr === "h306".U ) -> (priv_lvl >= "b11".U),
          ( addr === "h340".U ) -> (priv_lvl >= "b11".U),
          ( addr === "h341".U ) -> (priv_lvl >= "b11".U),
          ( addr === "h342".U ) -> (priv_lvl >= "b11".U),
          ( addr === "h343".U ) -> (priv_lvl >= "b11".U),
          ( addr === "h344".U ) -> (priv_lvl >= "b11".U),
          ( addr === "h34A".U ) -> (priv_lvl >= "b11".U),
          ( addr === "h34B".U ) -> (priv_lvl >= "b11".U),

          ( addr === "hB00".U ) -> (priv_lvl >= "b11".U),
          ( addr === "hB02".U ) -> (priv_lvl >= "b11".U),
          ( addr === "h320".U ) -> (priv_lvl >= "b11".U),
          ( addr === "h7A0".U ) -> (priv_lvl >= "b11".U),
          ( addr === "h7A1".U ) -> (priv_lvl >= "b11".U),
          ( addr === "h7A2".U ) -> (priv_lvl >= "b11".U),
          ( addr === "h7A3".U ) -> (priv_lvl >= "b11".U),
          ( addr === "h7B0".U ) -> DMode,
          ( addr === "h7B1".U ) -> DMode,
          ( addr === "h7B2".U ) -> DMode,
          ( addr === "h7B3".U ) -> DMode
        )

    val res = Mux1H(pmpcfg_arr ++ pmpaddr_arr ++ hpmcounter_arr ++ mhpmcounter_arr ++ mhpmevent_arr ++ normal_arr )
    ~res
  }


  def csr_write_denied(DMode: Bool) = {
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
          ( addr === "h7B0".U ) -> DMode,
          ( addr === "h7B1".U ) -> DMode,
          ( addr === "h7B2".U ) -> DMode,
          ( addr === "h7B3".U ) -> DMode
        )

    val res = Mux1H(pmpcfg_arr ++ pmpaddr_arr ++ hpmcounter_arr ++ mhpmcounter_arr ++ mhpmevent_arr ++ normal_arr )
    ~res
  }

def csr_read_res(csrfiles: CSR_Bundle) = {
    val pmpcfg_arr = {
      val addr_chk = for ( i <- 0 until 16 ) yield { addr === ("h3A0".U + i.U) }
      val reg_sel  = for ( i <- 0 until 16 ) yield {csrfiles.pmpcfg(i) }
      addr_chk zip reg_sel
    }

    val pmpaddr_arr = {
      val addr_chk = for ( i <- 0 until 64 ) yield { addr === ("h3B0".U + i.U) }
      val reg_sel  = for ( i <- 0 until 64 ) yield { csrfiles.pmpaddr(i)}
      addr_chk zip reg_sel
    }

    val hpmcounter_arr = {
      val addr_chk = for ( i <- 3 until 32 ) yield { addr === ("hC00".U + i.U) }
      val reg_sel  = for ( i <- 3 until 32 ) yield { csrfiles.hpmcounter(i) }
      addr_chk zip reg_sel
    }

    val mhpmcounter_arr = {
      val addr_chk = for ( i <- 3 until 32 ) yield { addr === ("hB00".U + i.U) }
      val reg_sel  = for ( i <- 3 until 32 ) yield { csrfiles.mhpmcounter(i) }
      addr_chk zip reg_sel      
    }

    val mhpmevent_arr = {
      val addr_chk = for ( i <- 3 until 32 ) yield { addr === ("h320".U + i.U) }
      val reg_sel  = for ( i <- 3 until 32 ) yield { csrfiles.mhpmevent(i) }
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
          ( addr === "h001".U ) -> csrfiles.fcsr(4,0), 
          ( addr === "h002".U ) -> csrfiles.fcsr(7,5),
          ( addr === "h003".U ) -> csrfiles.fcsr,
          ( addr === "hC00".U ) -> csrfiles.cycle,
          ( addr === "hC01".U ) -> csrfiles.time,
          ( addr === "hC02".U ) -> csrfiles.instret,
          ( addr === "h100".U ) -> csrfiles.sstatus,
          // ( addr === "h102".U ) -> sedeleg,
          // ( addr === "h103".U ) -> sideleg,
          ( addr === "h104".U ) -> csrfiles.sie,
          ( addr === "h105".U ) -> csrfiles.stvec,
          ( addr === "h106".U ) -> csrfiles.scounteren,
          ( addr === "h140".U ) -> csrfiles.sscratch,
          ( addr === "h141".U ) -> csrfiles.sepc,
          ( addr === "h142".U ) -> csrfiles.scause,
          ( addr === "h143".U ) -> csrfiles.stval,
          ( addr === "h144".U ) -> csrfiles.sip,
          ( addr === "h180".U ) -> csrfiles.satp,
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
          ( addr === "hF11".U ) -> csrfiles.mvendorid,
          ( addr === "hF12".U ) -> csrfiles.marchid,
          ( addr === "hF13".U ) -> csrfiles.mimpid,
          ( addr === "hF14".U ) -> csrfiles.mhartid,
          ( addr === "h300".U ) -> csrfiles.mstatus,
          ( addr === "h301".U ) -> csrfiles.misa,
          ( addr === "h302".U ) -> csrfiles.medeleg,
          ( addr === "h303".U ) -> csrfiles.mideleg,
          ( addr === "h304".U ) -> csrfiles.mie,
          ( addr === "h305".U ) -> csrfiles.mtvec,
          ( addr === "h306".U ) -> csrfiles.mcounteren,
          ( addr === "h340".U ) -> csrfiles.mscratch,
          ( addr === "h341".U ) -> csrfiles.mepc,
          ( addr === "h342".U ) -> csrfiles.mcause,
          ( addr === "h343".U ) -> csrfiles.mtval,
          ( addr === "h344".U ) -> csrfiles.mip,
          ( addr === "h34A".U ) -> csrfiles.mtinst,
          ( addr === "h34B".U ) -> csrfiles.mtval2,

          ( addr === "hB00".U ) -> csrfiles.mcycle,
          ( addr === "hB02".U ) -> csrfiles.minstret,
          ( addr === "h320".U ) -> csrfiles.mcountinhibit,
          ( addr === "h7A0".U ) -> csrfiles.tselect,
          ( addr === "h7A1".U ) -> csrfiles.tdata1,
          ( addr === "h7A2".U ) -> csrfiles.tdata2,
          ( addr === "h7A3".U ) -> csrfiles.tdata3,
          ( addr === "h7B0".U ) -> csrfiles.dcsr,
          ( addr === "h7B1".U ) -> csrfiles.dpc,
          ( addr === "h7B2".U ) -> csrfiles.dscratch0,
          ( addr === "h7B3".U ) -> csrfiles.dscratch1,
          ( addr === "h7B4".U ) -> csrfiles.dscratch2,
        )

    Mux1H(pmpcfg_arr ++ pmpaddr_arr ++ hpmcounter_arr ++ mhpmcounter_arr ++ mhpmevent_arr ++ normal_arr )
  }



}


object Reg_Exe_Port {

  def apply( csr_reg: UInt, addr: UInt, ep: Exe_Port ): (Bool, UInt) = {
    val enable = (ep.addr === addr) & (ep.op_rw | ep.op_rs | ep.op_rc)
    val dnxt = Mux1H(Seq(
        ep.op_rw -> ( ep.dat_i),
        ep.op_rs -> (csr_reg | ep.dat_i),
        ep.op_rc -> (csr_reg & ~ep.dat_i),
      ))
    return (enable, dnxt)
  }
}

// object Reg_Exe_Port_EN {
//   def apply( addr: UInt, ep: Exe_Port ): Bool = {
//     val enable = (ep.addr === addr) & (ep.op_rw | ep.op_rs | ep.op_rc)
//     return enable
//   }
// }

// object Reg_Exe_Port_Dnxt {
//   def apply( csr_reg: UInt, ep: Exe_Port ): UInt = {
//     val dnxt = Mux1H(Seq(
//         ep.op_rw -> ( ep.dat_i),
//         ep.op_rs -> (csr_reg | ep.dat_i),
//         ep.op_rc -> (csr_reg & ~ep.dat_i),
//       ))
//     return dnxt
//   }
// }

// object Csr_WR {
//   def apply( out: UInt, csr_reg: UInt, addr: UInt, ep: Exe_Port ) = {
//     val (enable, dnxt) = Reg_Exe_Port ( csr_reg, addr, ep )
//     when( enable ) { out := dnxt }
//   }
// }

// abstract class CsrFiles_port(cm: Int) extends Module{
//   val exe_port = Wire(new Exe_Port)
//   val exe_fport = Wire(new Exe_Port)
//   val is_trap = Wire(Bool())
//   // val is_exception = Wire(Bool())
//   val is_mRet = Wire(Bool())
//   val is_sRet = Wire(Bool())
//   val is_dRet = Wire(Bool())
//   // val is_uRet: Bool
//   // lazy val is_xRet = is_mRet | is_sRet //| is_uRet

//   val commit_pc = Wire(UInt(64.W))
//   // val cmmnxt_pc = Wire(UInt(64.W))
//   val ill_instr = Wire(UInt(64.W))
//   val ill_ivaddr = Wire(UInt(64.W))
//   val ill_dvaddr = Wire(UInt(64.W))

//   val is_csrw_illegal = Wire(Vec(cm,Bool())
//   val is_fcsrw_illegal = Wire(Vec(cm,Bool()))
//   val is_csrr_illegal = Wire(Vec(cm,Bool())

//   val is_instr_misAlign = WireDefault(false.B)
//   val is_instr_access_fault      = Wire(Bool())
//   val is_instr_illeage          = Wire(Bool())
//   val is_breakPoint             = Wire(Bool())
//   val is_load_misAlign          = Wire(Bool())
//   val is_load_access_fault       = Wire(Bool())
//   val is_storeAMO_misAlign      = Wire(Bool())
//   val is_storeAMO_access_fault   = Wire(Bool())
//   val is_ecall_U                  = Wire(Bool())
//   val is_ecall_S                  = Wire(Bool())
//   val is_ecall_M                  = Wire(Bool())
//   val is_instr_paging_fault      = Wire(Bool())
//   val is_load_paging_fault         = Wire(Bool())
//   val is_storeAMO_paging_fault   = Wire(Bool())



//   val retired_cnt = Wire( UInt(2.W))

//   val clint_sw_m = Wire(Bool())
//   val clint_sw_s = Wire(Bool())
//   val clint_tm_m = Wire(Bool())
//   val clint_tm_s = Wire(Bool())
//   val clint_ex_m = Wire(Bool())
//   val clint_ex_s = Wire(Bool())

//   val rtc_clock = Wire(Bool())


//   // val ustatus = Wire(UInt(64.W))
//   // val uie = Wire(UInt(64.W))
//   // val utvec = Wire(UInt(64.W))
//   // val uscratch = Wire(UInt(64.W))
//   // val uepc = Wire(UInt(64.W))
//   // val ucause = Wire(UInt(64.W))
//   // val utval = Wire(UInt(64.W))
//   // val uip = Wire(UInt(64.W))
//   // val fflags = Wire(UInt(64.W))
//   // val frm = Wire(UInt(64.W))
//   val fcsr = Wire(UInt(64.W))
//   val cycle = Wire(UInt(64.W))
//   val time = Wire(UInt(64.W))
//   val instret = Wire(UInt(64.W))
//   val sstatus = Wire(UInt(64.W))
//   // val sedeleg = Wire(UInt(64.W))
//   // val sideleg = Wire(UInt(64.W))
//   val sie = Wire(UInt(64.W))
//   val stvec = Wire(UInt(64.W))
//   val scounteren = Wire(UInt(64.W))
//   val sscratch = Wire(UInt(64.W))
//   val sepc = Wire(UInt(64.W))
//   val scause = Wire(UInt(64.W))
//   val stval = Wire(UInt(64.W))
//   val sip = Wire(UInt(64.W))
//   val satp = Wire(UInt(64.W))
//   val hstatus = Wire(UInt(64.W))
//   val hedeleg = Wire(UInt(64.W))
//   val hideleg = Wire(UInt(64.W))
//   val hie = Wire(UInt(64.W))
//   val hcounteren = Wire(UInt(64.W))
//   val hgeie = Wire(UInt(64.W))
//   val htval = Wire(UInt(64.W))
//   val hip = Wire(UInt(64.W))
//   val hvip = Wire(UInt(64.W))
//   val htinst = Wire(UInt(64.W))
//   val hgeip = Wire(UInt(64.W))
//   val hgatp = Wire(UInt(64.W))
//   val htimedelta = Wire(UInt(64.W))
//   val vsstatus = Wire(UInt(64.W))
//   val vsie = Wire(UInt(64.W))
//   val vstvec = Wire(UInt(64.W))
//   val vsscratch = Wire(UInt(64.W))
//   val vsepc = Wire(UInt(64.W))
//   val vscause = Wire(UInt(64.W))
//   val vstval = Wire(UInt(64.W))
//   val vsip = Wire(UInt(64.W))
//   val vsatp = Wire(UInt(64.W))
//   val mvendorid = Wire(UInt(64.W))
//   val marchid = Wire(UInt(64.W))
//   val mimpid = Wire(UInt(64.W))
//   val mhartid = Wire(UInt(64.W))
//   val mstatus = Wire(UInt(64.W))
//   val misa = Wire(UInt(64.W))
//   val medeleg = Wire(UInt(64.W))
//   val mideleg = Wire(UInt(64.W))
//   val mie = Wire(UInt(64.W))
//   val mtvec = Wire(UInt(64.W))
//   val mcounteren = Wire(UInt(64.W))
//   val mscratch = Wire(UInt(64.W))
//   val mepc = Wire(UInt(64.W))
//   val mcause = Wire(UInt(64.W))
//   val mtval = Wire(UInt(64.W))
//   val mip = Wire(UInt(64.W))
//   val mtinst = Wire(UInt(64.W))
//   val mtval2 = Wire(UInt(64.W))
//   val mcycle = Wire(UInt(64.W))
//   val minstret = Wire(UInt(64.W))
//   val mcountinhibit = Wire(UInt(64.W))
//   val tselect = Wire(UInt(64.W))
//   val tdata1 = Wire(UInt(64.W))
//   val tdata2 = Wire(UInt(64.W))
//   val tdata3 = Wire(UInt(64.W))
//   val dcsr = Wire(UInt(64.W))
//   val dpc = Wire(UInt(64.W))
//   val dscratch0 = Wire(UInt(64.W))
//   val dscratch1 = Wire(UInt(64.W))
//   val dscratch2 = Wire(UInt(64.W))


//   val pmpcfg  = Wire(Vec( 16, UInt(64.W)) )
//   val pmpaddr = Wire(Vec( 64, UInt(64.W)) )


//   val hpmcounter = WireDefault(VecInit( Seq.fill(32)(0.U(64.W)) ))
//   val mhpmcounter = WireDefault(VecInit( Seq.fill(32)(0.U(64.W)) ))
//   val mhpmevent = WireDefault(VecInit( Seq.fill(32)(0.U(64.W)) ))



//   val is_fpu_state_change = Wire(Bool())

//   val is_step_int_block = Wire(Bool())


//   val priv_lvl_dnxt = Wire(UInt(2.W))
//   val priv_lvl_enable = Wire(Bool())
//   val priv_lvl_qout = RegEnable(priv_lvl_dnxt, "b11".U(2.W), priv_lvl_enable)


//   val priv_lvl_if = priv_lvl_qout
//   val priv_lvl_ls = Mux( mstatus(17), mstatus(12,11), priv_lvl_qout )
// }

