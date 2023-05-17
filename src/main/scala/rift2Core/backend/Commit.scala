
/*
  Copyright (c) 2020 - 2023 Wuhan University of Technology <295054118@whut.edu.cn>

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

import rift2Core.define._
import rift2Core.backend._
import rift2Core.privilege._
import debug._
import chisel3.experimental._
import base._
import rift2Chip._
import org.chipsalliance.cde.config._
import rift2Core.diff._

class ExInt_Bundle extends Bundle {
  val is_single_step = Bool()
  val is_trigger = Bool()
  val hartHaltReq = Bool()

  val emu_reset = Bool()
	val msi = Bool()
	val ssi = Bool()
	val mti = Bool()
	val sti = Bool()
	val mei = Bool()
	val sei = Bool()  
}

abstract class BaseCommit()(implicit p: Parameters) extends RiftModule {

  class CommitIO extends Bundle{
    // val cm_op = Vec(cmChn, new Info_commit_op(32, maxRegNum))

    val xCommit = Vec(cmChn, new Info_commit_op(32, xRegNum))
    val fCommit = Vec(cmChn, new Info_commit_op(32, fRegNum))
    val vCommit = Vec(cmChn, new Vector_Commit_Bundle)
              // val csrCmm = Vec(cmChn, new SeqReg_Commit_Bundle(cRegNum))
              // val csrOp = Input(Vec(cmChn, new Exe_Port))
    val rod = Vec(cmChn, Flipped(new DecoupledIO( new Info_reorder_i ) ))
    val cmm_lsu = Output(new Info_cmm_lsu)
    val lsu_cmm = Input( new Info_lsu_cmm )
    val bctq = Flipped(Decoupled(new Branch_CTarget_Bundle))
    val jctq = Flipped(Decoupled(new Jump_CTarget_Bundle))
    val cmmRedirect = new ValidIO(new Commit_Redirect_Bundle)
    val if_cmm = Input( new Info_if_cmm )
    val ifence = Output(Bool())
    val cmm_mmu = Output( new Info_cmm_mmu )
    val dm = Flipped(new Info_DM_cmm)
    val rtc_clock = Input(Bool())
    val aclint = Input(new AClint_Bundle)
    val plic   = Input(new Plic_Bundle)
    val csrfiles = Output(new CSR_Bundle)
    val diff_commit = Output(new Info_cmm_diff)
    val diff_csr = Output(new Info_csr_reg)

    val xpuCsrCommit    = Flipped(Decoupled(new Exe_Port))
    val fpuCsrCommit    = Flipped(Decoupled(new Exe_Port))
    val vpuCsrCommit    = Flipped(Decoupled(new Exe_Port))
  }

  val io: CommitIO = IO(new CommitIO)


  val csrfiles = Reg(new CSR_Bundle); io.csrfiles := csrfiles
  val commit_state = Wire( Vec( cmChn, UInt(2.W)) )
  val commit_state_is_comfirm     = for ( i <- 0 until cmChn ) yield commit_state(i) === 3.U
  val commit_state_is_misPredict  = for ( i <- 0 until cmChn ) yield commit_state(i) === 2.U
  val commit_state_is_abort       = for ( i <- 0 until cmChn ) yield commit_state(i) === 1.U
  val commit_state_is_idle        = for ( i <- 0 until cmChn ) yield commit_state(i) === 0.U


  val cmm_state = Wire( Vec(cmChn, new CMMState_Bundle ) )
  val csr_state = Wire( Vec(cmChn, new CSR_Bundle ) )

  val is_retired = ( 0 until cmChn ).map{ i => {commit_state_is_comfirm(i) | commit_state_is_misPredict(i) } }

}

trait CommitBranch{ this: BaseCommit =>
  // val emptyBCTQ = {
  //   val res = Wire(Vec(cmChn, Flipped(DecoupledIO( new Branch_CTarget_Bundle ) )))
  //   res := 0.U.asTypeOf(Vec(cmChn, Flipped(DecoupledIO( new Branch_CTarget_Bundle ) )))
  //   res(0) <> io.bctq
  //   res
  // }

  // val emptyJCTQ = {
  //   val res = Wire(Vec(cmChn, Flipped(DecoupledIO( new Jump_CTarget_Bundle ) )))
  //   res := 0.U.asTypeOf(Vec(cmChn, Flipped(DecoupledIO( new Jump_CTarget_Bundle ) )))
  //   res(0) <> io.jctq
  //   res
  // }

  // val bctq = ReDirect( emptyBCTQ, VecInit( io.rod.map{_.bits.is_branch} ) )
  // val jctq = ReDirect( emptyJCTQ, VecInit( io.rod.map{_.bits.is_jalr} ) )

  // ( 0 until cmChn ).map{ i =>
  //   bctq(i).ready := is_retired(i) & io.rod(i).bits.is_branch
  //   assert( bctq(i).fire === (is_retired(i) & io.rod(i).bits.is_branch) )
  //   assert( bctq.map{_.fire}.reduce(_|_) === io.bctq.fire )
  // }

  // ( 0 until cmChn ).map{ i =>
  //   jctq(i).ready := is_retired(i) & io.rod(i).bits.is_jalr
  //   assert( jctq(i).fire === (is_retired(i) & io.rod(i).bits.is_jalr) )
  //   assert( jctq.map{_.fire}.reduce(_|_) === io.jctq.fire )
  // }

  io.bctq.ready := ( 0 until cmChn ).map{ i => is_retired(i) & io.rod(i).bits.is_branch }.foldLeft(false.B)(_|_)
  assert( ~(io.bctq.ready & ~io.bctq.valid) )
  assert( PopCount( ( 0 until cmChn ).map{ i => is_retired(i) & io.rod(i).bits.is_branch } ) <= 1.U )

  io.jctq.ready := ( 0 until cmChn ).map{ i => is_retired(i) & io.rod(i).bits.is_jalr }.foldLeft(false.B)(_|_)
  assert( ~(io.jctq.ready & ~io.jctq.valid) )
  assert( PopCount( ( 0 until cmChn ).map{ i => is_retired(i) & io.rod(i).bits.is_jalr } ) <= 1.U )
}

trait CommitDebug{ this: BaseCommit =>
  val is_single_step = RegInit(false.B)
  val is_trigger = false.B
  val emu_reset = RegInit( false.B )

  io.dm.hartIsInReset := emu_reset

  when( io.dm.hartResetReq ){
    emu_reset := true.B
  }.elsewhen( emu_reset ){
    emu_reset := false.B
  }



  when ((is_retired(0) | commit_state_is_abort(0)) & cmm_state(0).is_step){
    is_single_step := true.B
  }.elsewhen( csrfiles.DMode ){
    is_single_step := false.B
  }
}



/** commit
  * @note for every commit-chn, it can be:
  * comfirm: commit at this tick
  * abort: cancel and flush at this tick
  * cancel: the perivious chn abort
  * idle: empty line or waitting to check whether is comfirm or abort
  */
abstract class CommitState(implicit p: Parameters) extends BaseCommit
with UpdateCsrFilesFun
with CommitBranch
with CommitDebug
{


  val abort_chn = Wire(UInt(log2Ceil(cmChn).W)); abort_chn := DontCare
  ( 1 until cmChn ).map{ i => assert( commit_state(i) <= commit_state(i-1) ) }
  
  for ( i <- 0 until cmChn ) {
    when( (~io.rod(i).valid)  ) {
      commit_state(i) := 0.U //IDLE
    }
    .otherwise {
      when( cmm_state(i).is_xRet | cmm_state(i).isException | cmm_state(i).isEbreakDM | cmm_state(i).isInterrupt | cmm_state(i).isNomaskInterrupt | cmm_state(i).is_fence_i | cmm_state(i).is_sfence_vma ) {
        commit_state(i) := 1.U //abort
        for ( j <- 0 until i ) { when( ~commit_state_is_comfirm(j) ) {commit_state(i) := 0.U}} //override to idle }
        abort_chn := i.U
      }
      .elsewhen( cmm_state(i).is_wb ) {
        when( io.rod(i).bits.is_branch ) {
          when( ( 0 until i ).map{ j => io.rod(j).bits.is_branch}.foldLeft(false.B)(_|_) ){//PopCount( (0 to i).map{ j => io.rod(j).bits.is_branch } ) > 1.U
            commit_state(i) := 0.U
          } .otherwise{
            commit_state(i) := Mux( io.bctq.bits.isMisPredict, 2.U, 3.U ) //mis-predict )
          }
        }
        .elsewhen( io.rod(i).bits.is_jalr ) {
          when( ( 0 until i ).map{ j => io.rod(j).bits.is_jalr}.foldLeft(false.B)(_|_) ){
            commit_state(i) := 0.U
          } .otherwise{
            commit_state(i) := Mux( io.jctq.bits.isMisPredict, 2.U, 3.U ) //mis-predict )
          }
        }
        .elsewhen( io.rod(i).bits.is_csr ){
          commit_state(i) :=  Mux( ( 0 until i ).map{ j => io.rod(j).bits.is_csr }.foldLeft(false.B)(_|_), 0.U, 3.U )
        }
        .elsewhen( io.rod(i).bits.is_fcsr ){
          commit_state(i) :=  Mux( ( 0 until i ).map{ j => io.rod(j).bits.is_fcsr }.foldLeft(false.B)(_|_), 0.U, 3.U )
        }
        .otherwise {
          commit_state(i) := 3.U //confirm
        }
        for ( j <- 0 until i ) { when( ~commit_state_is_comfirm(j) ) {commit_state(i) := 0.U} } //override to idle }
      }
      .otherwise {
        commit_state(i) := 0.U //idle
      }
    }    
  }

  //assert
  ( 1 until cmChn ).map{ i =>  assert( ~((is_retired(i) | commit_state_is_abort(i)) & ~(is_retired(i-1) | commit_state_is_abort(i-1)) ) )}  //the following chn will not retired or abort if the perivious not



}




trait CommitComb{ this: CommitState =>
  ( 0 until cmChn ).map{ i => {
    cmm_state(i).rod      := io.rod(i).bits

    cmm_state(i).lsu_cmm := io.lsu_cmm
    cmm_state(i).csrExe  := 
      Mux1H(Seq(
        io.rod(i).bits.is_csr  -> io.xpuCsrCommit.bits.asUInt,
        io.rod(i).bits.is_fcsr -> io.fpuCsrCommit.bits.asUInt,
      )).asTypeOf(new Exe_Port)

    cmm_state(i).is_wb   :=
      Mux1H(Seq(
        io.rod(i).bits.isXwb -> io.xCommit(i).is_writeback,
        io.rod(i).bits.isFwb -> io.fCommit(i).is_writeback,
        io.rod(i).bits.isVwb -> io.vCommit(i).isWroteback,
      ))
      
    cmm_state(i).ill_ivaddr               := io.if_cmm.ill_vaddr
    cmm_state(i).ill_dvaddr               := io.lsu_cmm.trap_addr
    // cmm_state(i).is_csrr_illegal          := cmm_state(i).csrfiles.csr_read_prilvl(io.xpuCsrCommit.bits.addr)// & io.csr_addr.valid

    cmm_state(i).exint.is_single_step := is_single_step
    cmm_state(i).exint.is_trigger := is_trigger
    cmm_state(i).exint.emu_reset  := emu_reset
    cmm_state(i).exint.hartHaltReq := io.dm.hartHaltReq
	  cmm_state(i).exint.msi := io.aclint.msi
	  cmm_state(i).exint.ssi := io.aclint.ssi
	  cmm_state(i).exint.mti := io.aclint.mti
	  cmm_state(i).exint.sti := io.aclint.sti
	  cmm_state(i).exint.mei := io.plic.mei
	  cmm_state(i).exint.sei := io.plic.sei 


    cmm_state(i).isVException := io.rod(i).bits.isVwb & io.vCommit(i).isException
    cmm_state(i).excepitonIdx := io.vCommit(i).excepitonIdx
  }}

}


trait CommitRegFiles { this: CommitState =>

  for ( i <- 0 until cmChn ) yield {
    io.xCommit(i).phy := io.rod(i).bits.rd0_phy
    io.xCommit(i).raw := io.rod(i).bits.rd0_raw

    io.fCommit(i).phy := io.rod(i).bits.rd0_phy
    io.fCommit(i).raw := io.rod(i).bits.rd0_raw

    io.vCommit(i).phy := io.rod(i).bits.rd0_raw
  }


  ( 0 until cmChn ).map{ i =>
    io.xCommit(i).is_comfirm      := commit_state_is_comfirm(i)
    io.xCommit(i).is_MisPredict   := commit_state_is_misPredict(i)
    io.xCommit(i).is_abort        := commit_state_is_abort(i)

    io.fCommit(i).is_comfirm      := commit_state_is_comfirm(i)
    io.fCommit(i).is_MisPredict   := false.B
    io.fCommit(i).is_abort        := commit_state_is_abort(i) | commit_state_is_misPredict(i)

    io.vCommit(i).isComfirm := io.rod(i).bits.isVwb & commit_state_is_comfirm(i)
    io.vCommit(i).isAbort   := commit_state_is_misPredict(i) | commit_state_is_abort(i)
  }

}

trait CommitCsrFiles { this: CommitState =>
  io.xpuCsrCommit.ready := ( 0 until cmChn ).map{ i => commit_state_is_comfirm(i) & io.rod(i).bits.is_csr }.foldLeft(false.B)(_|_)
  assert( ~(io.xpuCsrCommit.ready & ~io.xpuCsrCommit.valid) )

  io.fpuCsrCommit.ready := ( 0 until cmChn ).map{ i => commit_state_is_comfirm(i) & io.rod(i).bits.is_fcsr}.foldLeft(false.B)(_|_)
  assert( ~(io.fpuCsrCommit.ready & ~io.fpuCsrCommit.valid) )
        // io.csrCmm(i).isComfirm := commit_state_is_comfirm(i)
        // io.csrCmm(i).isAbort   := commit_state_is_abort(i) | commit_state_is_misPredict(i)
        // io.csrCmm(i).idx       := io.rod(i).bits.csrw( log2Ceil(cRegNum)-1,  0 )
        // io.csrCmm(i).addr      := io.rod(i).bits.csrw( log2Ceil(cRegNum)+11, log2Ceil(cRegNum)+0 )


  csrfiles.mcycle := csrfiles.mcycle + 1.U //may be overriden
  val rtc = ShiftRegisters( io.rtc_clock, 4, false.B, true.B ); when(rtc(3) ^ rtc(2)) { csrfiles.time := csrfiles.time + 1.U }
  
  for( i <- 0 until cmChn ) { when(is_retired(i) | commit_state_is_abort(i)) { csrfiles := csr_state(i) } }
  when( reset.asBool ) { resetToDefault(csrfiles) }

  ( 0 until cmChn ).map{ i => {
    if ( i == 0 ) { cmm_state(i).csrfiles := csrfiles } else { cmm_state(i).csrfiles := csr_state(i-1) }
    csr_state(i) := update_csrfiles(in = cmm_state(i))
  }}

  def resetToDefault(csrfiles: CSR_Bundle) = {

    csrfiles.priv_lvl      := "b11".U
    csrfiles.DMode         := false.B
    csrfiles.fcsr          := 0.U.asTypeOf(new FCSRBundle)
    csrfiles.cycle         := 0.U
    csrfiles.time          := 0.U
    csrfiles.instret       := 0.U
    csrfiles.stvec         := 0.U.asTypeOf(new TVecBundle)
    csrfiles.scounteren    := 0.U.asTypeOf(new CounterenBundle)
    csrfiles.sscratch      := 0.U.asTypeOf(UInt(64.W))
    csrfiles.sepc          := 0.U.asTypeOf(UInt(64.W))
    csrfiles.scause        := 0.U.asTypeOf(new CauseBundle)
    csrfiles.stval         := 0.U.asTypeOf(UInt(64.W))
    // csrfiles.sip           := 0.U.asTypeOf(new MSIntBundle)
    csrfiles.satp          := 0.U.asTypeOf(new SatpBundle)
    csrfiles.mvendorid     := 0.U
    csrfiles.marchid       := 0.U
    csrfiles.mimpid        := 0.U
    csrfiles.mhartid       := 0.U

    csrfiles.mstatus.mbe  := 0.U(1.W)
    csrfiles.mstatus.sbe  := 0.U(1.W)
    csrfiles.mstatus.sxl  := 2.U(2.W)
    csrfiles.mstatus.uxl  := 2.U(2.W)
    csrfiles.mstatus.tsr  := 0.U(1.W)
    csrfiles.mstatus.tw   := 0.U(1.W)
    csrfiles.mstatus.tvm  := 0.U(1.W)
    csrfiles.mstatus.mxr  := 0.U(1.W)
    csrfiles.mstatus.sum  := 0.U(1.W)
    csrfiles.mstatus.mprv := 0.U(1.W)
    csrfiles.mstatus.xs   := 0.U(2.W)
    csrfiles.mstatus.fs   := 0.U(2.W)
    csrfiles.mstatus.mpp  := "b11".U(2.W)
    csrfiles.mstatus.spp  := 0.U(1.W)
    csrfiles.mstatus.mpie := 0.U(1.W)
    csrfiles.mstatus.ube  := 0.U(1.W)
    csrfiles.mstatus.spie := 0.U(1.W)
    csrfiles.mstatus.mie  := 0.U(1.W)
    csrfiles.mstatus.sie  := 0.U(1.W)
    csrfiles.mstatus.sd   := 0.U(1.W)

    csrfiles.misa          := Cat(2.U(2.W), 0.U(36.W), "b00000101000001000100101101".U(26.W))
    csrfiles.medeleg       := 0.U
    csrfiles.mideleg       := 0.U
    csrfiles.mie           := 0.U.asTypeOf(new MSIntBundle)
    csrfiles.mtvec         := 0.U.asTypeOf(new TVecBundle)
    csrfiles.mcounteren    := 0.U.asTypeOf(new CounterenBundle)
    csrfiles.mscratch      := 0.U
    csrfiles.mepc          := 0.U
    csrfiles.mcause        := 0.U.asTypeOf(new CauseBundle)
    csrfiles.mtval         := 0.U
    csrfiles.mip           := 0.U.asTypeOf(new MSIntBundle)
    csrfiles.mtinst        := 0.U
    csrfiles.mtval2        := 0.U
    csrfiles.mcycle        := 0.U
    csrfiles.minstret      := 0.U
    csrfiles.mcountinhibit := 0.U
    csrfiles.tselect       := 0.U
    csrfiles.tdata1        := 0.U
    csrfiles.tdata2        := 0.U
    csrfiles.tdata3        := 0.U
              
    csrfiles.dcsr.xdebugver := 4.U(4.W)
    csrfiles.dcsr.ebreakm   := 0.U(1.W)
    csrfiles.dcsr.ebreaks   := 0.U(1.W)
    csrfiles.dcsr.ebreaku   := 0.U(1.W)
    csrfiles.dcsr.stepie    := 0.U(1.W)
    csrfiles.dcsr.stopcount := 0.U(1.W)
    csrfiles.dcsr.stoptime  := 0.U(1.W)
    csrfiles.dcsr.cause     := 0.U(3.W)
    csrfiles.dcsr.mprven    := 0.U(1.W)
    csrfiles.dcsr.nmip      := 0.U(1.W)
    csrfiles.dcsr.step      := 0.U(1.W)
    csrfiles.dcsr.prv       := 3.U(2.W)

    csrfiles.dpc           := 0.U
    csrfiles.dscratch0     := 0.U
    csrfiles.dscratch1     := 0.U
    csrfiles.dscratch2     := 0.U
    csrfiles.pmpcfg        := (if (pmpNum==0) { VecInit( Seq.fill(1)(VecInit( Seq.fill(8)(0.U.asTypeOf( new PmpcfgBundle) ))))} else {VecInit( Seq.fill(pmpNum)(VecInit( Seq.fill(8)(0.U.asTypeOf( new PmpcfgBundle) ))))})
    csrfiles.pmpaddr       := (if (pmpNum==0) { VecInit( Seq.fill(8)(0.U(64.W)) ) }                             else {VecInit( Seq.fill(8*pmpNum)(0.U(64.W)) )})
    csrfiles.hpmcounter    := VecInit( Seq.fill(32)(0.U(64.W)) )
    csrfiles.mhpmcounter   := VecInit( Seq.fill(32)(0.U(64.W)) )
    csrfiles.mhpmevent     := VecInit( Seq.fill(32)(0.U(64.W)) )    
  }
}

trait CommitIFRedirect { this: CommitState =>

  io.cmmRedirect.valid := false.B
  io.cmmRedirect.bits.pc := 0.U
  for ( i <- 0 until cmChn ) yield {
    when(io.rod(i).bits.is_branch & io.bctq.bits.isMisPredict & is_retired(i)) {
      io.cmmRedirect.valid := true.B
      io.cmmRedirect.bits.pc := io.bctq.bits.finalTarget
    }
    
    when(io.rod(i).bits.is_jalr   & io.jctq.bits.isMisPredict & is_retired(i)) {
      io.cmmRedirect.valid := true.B
      io.cmmRedirect.bits.pc := io.jctq.bits.finalTarget
    }

    when( commit_state_is_abort(i) ) {
      when( cmm_state(i).csrfiles.DMode ) {
        when( cmm_state(i).is_fence_i | cmm_state(i).is_sfence_vma ) {
          io.cmmRedirect.valid := true.B
          io.cmmRedirect.bits.pc := (extVaddr(io.rod(i).bits.pc, vlen) + 4.U)
        }
        when( cmm_state(i).isInterrupt | cmm_state(i).isNomaskInterrupt ) {
          assert(false.B, "Assert Failed, All interrupts (including NMI) are masked in Dmode! Page-39")
        }
        when( cmm_state(i).isException ) {
          when( cmm_state(i).is_ebreak_exc ) {
            io.cmmRedirect.valid := true.B
            io.cmmRedirect.bits.pc := "h00000800".U //for ebreak, jump to rom to halt again
          } .otherwise {
            io.cmmRedirect.valid := true.B
            io.cmmRedirect.bits.pc := "h00000808".U //for other exception, jump to exception flag
          }
        }
        when( cmm_state(i).is_dRet ) {
          io.cmmRedirect.valid := true.B
          io.cmmRedirect.bits.pc := cmm_state(i).csrfiles.dpc
        } 
      } .otherwise {


        when(cmm_state(i).isNomaskInterrupt){
          io.cmmRedirect.valid := true.B
          io.cmmRedirect.bits.pc := Mux1H(Seq(
              cmm_state(i).exint.emu_reset              -> "h80000000".U,
              cmm_state(i).isDebugInterrupt             -> "h00000800".U,
            ))
        }
        .elsewhen(cmm_state(i).isEbreakDM) {
          io.cmmRedirect.valid := true.B
          io.cmmRedirect.bits.pc := "h00000800".U
        }
        .elsewhen( cmm_state(i).is_mRet ) {
          io.cmmRedirect.valid := true.B
          io.cmmRedirect.bits.pc := cmm_state(i).csrfiles.mepc
        } 
        .elsewhen( cmm_state(i).is_sRet ) {
          io.cmmRedirect.valid := true.B
          io.cmmRedirect.bits.pc := cmm_state(i).csrfiles.sepc
        } 
        .elsewhen( cmm_state(i).isException ) { //exception has higher priority than interrupt
          io.cmmRedirect.valid := true.B
          io.cmmRedirect.bits.pc := Mux1H(Seq(
              (update_priv_lvl(cmm_state(i)) === "b11".U) -> cmm_state(i).csrfiles.mtvec.asUInt,
              (update_priv_lvl(cmm_state(i)) === "b01".U) -> cmm_state(i).csrfiles.stvec.asUInt,
            ))
        }
        .elsewhen( cmm_state(i).isInterrupt ) {
          io.cmmRedirect.valid := true.B
          io.cmmRedirect.bits.pc := Mux1H(Seq(
              (update_priv_lvl(cmm_state(i)) === "b11".U) -> cmm_state(i).csrfiles.mtvec.asUInt,
              (update_priv_lvl(cmm_state(i)) === "b01".U) -> cmm_state(i).csrfiles.stvec.asUInt,
            ))
        }

        when( cmm_state(i).is_fence_i | cmm_state(i).is_sfence_vma ) {
          io.cmmRedirect.valid := true.B
          io.cmmRedirect.bits.pc := (extVaddr(io.rod(i).bits.pc, vlen) + 4.U)
        }

        // assert( PopCount(Seq( cmm_state(i).is_xRet, (cmm_state(i).isInterrupt | cmm_state(i).isException), cmm_state(i).is_fence_i, cmm_state(i).is_sfence_vma)) <= 1.U, "Assert Failed, only one situation will be trigger ecah time!" )
      }
    }
  }
}

trait CommitDiff { this: CommitState =>
  ( 0 until cmChn ).map{i =>
    io.diff_commit.pc(i) := extVaddr(io.rod(i).bits.pc, vlen)
    io.diff_commit.comfirm(i) := (commit_state_is_comfirm(i) | commit_state_is_misPredict(i))
    io.diff_commit.abort(i) := commit_state_is_abort(i)
  } 

  io.diff_commit.priv_lvl := csrfiles.priv_lvl
  io.diff_commit.is_ecall_M := ( 0 until cmChn ).map{ i => { commit_state_is_abort(i) & cmm_state(i).is_ecall_M }}.reduce(_|_)
  io.diff_commit.is_ecall_S := ( 0 until cmChn ).map{ i => { commit_state_is_abort(i) & cmm_state(i).is_ecall_S }}.reduce(_|_)
  io.diff_commit.is_ecall_U := ( 0 until cmChn ).map{ i => { commit_state_is_abort(i) & cmm_state(i).is_ecall_U }}.reduce(_|_)

	io.diff_csr.mstatus   := csrfiles.mstatus.asUInt
	io.diff_csr.mtvec     := csrfiles.mtvec.asUInt
	io.diff_csr.mscratch  := csrfiles.mscratch
	io.diff_csr.mepc      := csrfiles.mepc
	io.diff_csr.mcause    := csrfiles.mcause.asUInt
	io.diff_csr.mtval     := csrfiles.mtval
  io.diff_csr.mvendorid := csrfiles.mvendorid
  io.diff_csr.marchid   := csrfiles.marchid
  io.diff_csr.mimpid    := csrfiles.mimpid
  io.diff_csr.mhartid   := csrfiles.mhartid
  io.diff_csr.misa      := csrfiles.misa
  io.diff_csr.mie       := csrfiles.mie.asUInt
  io.diff_csr.mip       := csrfiles.mip.asUInt
  io.diff_csr.medeleg   := csrfiles.medeleg
  io.diff_csr.mideleg   := csrfiles.mideleg
  // io.diff_csr.mcounteren           = mcounteren
  // io.diff_csr.mcountinhibit        = mcountinhibit
  // io.diff_csr.tselect              = tselect
  // io.diff_csr.tdata1[MAX_TRIGGERS] = tdata1
  // io.diff_csr.tdata2[MAX_TRIGGERS] = tdata2
  // io.diff_csr.tdata3[MAX_TRIGGERS] = tdata3
  // io.diff_csr.mhpmevent[32]        = mhpmevent
  if ( pmpNum == 0 ) {
    io.diff_csr.pmpcfg(0) := 0.U
    for( i <- 0 until 8 ) { io.diff_csr.pmpaddr(i)  := 0.U   }
  } else {
    for ( i <- 0 until pmpNum ) io.diff_csr.pmpcfg(i) := csrfiles.pmpcfg(i).asUInt
    for ( i <- 0 until 8*pmpNum )  io.diff_csr.pmpaddr(i)  := csrfiles.pmpaddr(i)  
  }



  io.diff_csr.stvec    := csrfiles.stvec.asUInt
  io.diff_csr.sscratch := csrfiles.sscratch
  io.diff_csr.sepc     := csrfiles.sepc
  io.diff_csr.scause   := csrfiles.scause.asUInt
  io.diff_csr.stval    := csrfiles.stval.asUInt
  io.diff_csr.satp     := csrfiles.satp.asUInt
  // io.diff_csr.scounteren := scounteren
  // io.diff_csr.dcsr       := dcsr
  // io.diff_csr.dpc        := dpc
  // io.diff_csr.dscratch   := dscratch
  io.diff_csr.fflags  := csrfiles.fcsr.fflags
  io.diff_csr.frm     := csrfiles.fcsr.frm

  io.diff_csr.mcycle := csrfiles.mcycle
  io.diff_csr.minstret := csrfiles.minstret
  io.diff_csr.mhpmcounter := (if ( hpmNum == 4 ) {csrfiles.mhpmcounter} else { 0.U.asTypeOf(Vec( 32, UInt(64.W))) })
}


trait CommitInfoMMU{ this: CommitState =>
    io.cmm_mmu.satp := csrfiles.satp.asUInt
    if( pmpNum == 0 ) { io.cmm_mmu.pmpcfg  := DontCare } else { for ( i <- 0 until pmpNum )   io.cmm_mmu.pmpcfg(i)  := csrfiles.pmpcfg(i).asUInt }
    if( pmpNum == 0 ) { io.cmm_mmu.pmpaddr := DontCare } else { for ( i <- 0 until 8*pmpNum ) io.cmm_mmu.pmpaddr(i) := csrfiles.pmpaddr(i)       }
    io.cmm_mmu.priv_lvl_if   := csrfiles.priv_lvl
    io.cmm_mmu.priv_lvl_ls   := Mux( csrfiles.mstatus.mprv.asBool, csrfiles.mstatus.mpp, csrfiles.priv_lvl )
    io.cmm_mmu.mstatus    := csrfiles.mstatus.asUInt
    io.cmm_mmu.sstatus    := csrfiles.sstatus.asUInt
    io.cmm_mmu.sfence_vma := ( 0 until cmChn ).map{ i => 
      commit_state_is_abort(i) & cmm_state(i).is_sfence_vma 
    }.reduce(_|_)
}

trait CommitInfoLsu{ this: CommitState =>
  io.cmm_lsu.is_amo_pending := {
    io.rod(0).valid & io.rod(0).bits.is_amo & ~io.xCommit(0).is_writeback //only pending amo in rod0 is send out
  }
  println("Warning, amo_pending can only emmit at chn0")

  ( 0 until cmChn ).map{ i =>
    io.cmm_lsu.is_store_commit(i) := io.rod(i).bits.is_su & commit_state_is_comfirm(i)
  }

  io.cmm_lsu.isVstorePending := {
    io.rod(0).valid & io.rod(0).bits.is_su & io.rod(0).bits.isVector & ~io.vCommit(0).isWroteBack //only pending amo in rod0 is send out
  }
  println("Warning, vstore_pending can only emmit at chn0")
}



/**
  * @note new feature
  * 1. abort can only emmit at chn0 -> abort can emmit at any chn
  * 2. branch/jalr can resolve at any chn but only one of every
  * 3. branch misPredict will redirect at cmm
  */
class Commit()(implicit p: Parameters) extends CommitState
with CommitComb
with CommitCsrFiles
with CommitRegFiles
with CommitIFRedirect
with CommitInfoMMU
with CommitInfoLsu
with CommitDiff{

  ( 0 until cmChn ).map{ i =>
    io.rod(i).ready := (is_retired(i) | commit_state_is_abort(i))
  }

  io.ifence := ( 0 until cmChn ).map{ i => 
    commit_state_is_abort(i) & cmm_state(i).is_fence_i
  }.reduce(_|_)

}




@chiselName
class CMMState_Bundle(implicit p: Parameters) extends RiftBundle{
  val rod = new Info_reorder_i
  val csrfiles = new CSR_Bundle
  val lsu_cmm = new Info_lsu_cmm

  val csrExe = new Exe_Port
  val is_wb = Bool()
  val ill_ivaddr = UInt(64.W)
	val ill_dvaddr = UInt(64.W)

  val exint = new ExInt_Bundle

  val isVException = Bool()
  val excepitonIdx = UInt((log2Ceil(vParams.vlen/8).W))

  def is_load_accessFault: Bool = {
    val is_load_accessFault = 
      lsu_cmm.is_access_fault & (
        (rod.is_lu & is_wb) | (rod.isVLoad & isVException & ~(rod.isFoF & excepitonIdx =/= 0.U) )
      )

    return is_load_accessFault
  }

  def is_store_accessFault: Bool = {
    val is_store_accessFault =
      lsu_cmm.is_access_fault & (
        ( ( rod.is_su | rod.is_amo ) & is_wb ) | (rod.isVStore & isVException)
      )

    return is_store_accessFault
  }

  def is_load_pagingFault: Bool = {
    val is_load_pagingFault =
      lsu_cmm.is_paging_fault & (
        ( rod.is_lu & is_wb ) | (rod.isVLoad & isVException & ~(rod.isFoF & excepitonIdx =/= 0.U) )
      )

    return is_load_pagingFault
  }

  def is_store_pagingFault: Bool = {
    val is_store_pagingFault =
      lsu_cmm.is_paging_fault & (
        (( rod.is_su | rod.is_amo ) & is_wb) | (rod.isVStore & isVException)
      )

    return is_store_pagingFault
  }

  def is_load_misAlign: Bool = {
    val is_load_misAlign =
      lsu_cmm.is_misAlign & (
        ( rod.is_lu & is_wb ) | (rod.isVLoad & isVException & ~(rod.isFoF & excepitonIdx =/= 0.U) )
      )

    return is_load_misAlign
  }

  def is_instr_misAlign: Bool = {
    return false.B
  }

  def is_store_misAlign: Bool = {
    val is_store_misAlign =
      lsu_cmm.is_misAlign & (
        ( (rod.is_su | rod.is_amo) & is_wb ) | (rod.isVStore & isVException)
      )

    return is_store_misAlign
  }

  def is_ecall: Bool = {
    val is_ecall = rod.privil.ecall
    return is_ecall
  }

  def is_ecall_U: Bool = {
    val is_ecall_U = is_ecall & csrfiles.priv_lvl === "b00".U
    return is_ecall_U    
  }

  def is_ecall_S: Bool = {
    val is_ecall_S = is_ecall & csrfiles.priv_lvl === "b01".U
    return is_ecall_S    
  }

  def is_ecall_M: Bool = {
    val is_ecall_M = is_ecall & csrfiles.priv_lvl === "b11".U
    return is_ecall_M    
  }

  def is_ebreak_exc: Bool = {
    val is_ebreak_exc = rod.privil.ebreak & ~is_ebreak_breakpointn
    return is_ebreak_exc
  }

  def is_instr_access_fault: Bool = {
    val is_instr_access_fault = rod.privil.is_access_fault
    return is_instr_access_fault
  }

  def is_instr_paging_fault: Bool = {
    val is_instr_paging_fault = rod.privil.is_paging_fault
    return is_instr_paging_fault
  }

  def is_csrr_illegal: Bool = {
    val is_csrr_illegal = csrfiles.isViolateCSRR(csrExe.addr)
    return is_csrr_illegal
  }

  def is_csrw_illegal: Bool = {
    val is_csrw_illegal =
      ( csrExe.op_rc | csrExe.op_rs | csrExe.op_rw ) &
      csrfiles.isViolateCSRW(csrExe.addr)
    return is_csrw_illegal
  }

      

  def is_illeage: Bool = {
    val is_csr_illegal = 
      (is_csrr_illegal  & rod.is_csr &  is_wb) |
      (is_csrw_illegal  & rod.is_csr &  is_wb)

    val is_ill_sfence = is_wb & rod.is_sfence_vma & ( (csrfiles.mstatus.tvm & csrfiles.priv_lvl === "b01".U) | csrfiles.priv_lvl === "b00".U)
    val is_ill_wfi  = is_wb & rod.is_wfi        & (  csrfiles.mstatus.tw & csrfiles.priv_lvl < "b11".U )

    val is_ill_mRet = rod.privil.mret & csrfiles.priv_lvl =/= "b11".U
    val is_ill_sRet = rod.privil.sret & ( csrfiles.priv_lvl === "b00".U | ( csrfiles.priv_lvl === "b01".U & csrfiles.mstatus.tsr) )
    val is_ill_dRet = rod.privil.dret & ~csrfiles.DMode
    val is_ill_fpus =
      (is_wb &
      ( rod.is_fpu | // fpu and fload fstore
        (rod.is_csr & (csrExe.addr === "h001".U | csrExe.addr === "h002".U | csrExe.addr === "h003".U))
      ) & csrfiles.mstatus.fs === 0.U)

    val is_illeage = rod.is_illeage | is_csr_illegal | is_ill_sfence | is_ill_wfi | is_ill_mRet | is_ill_sRet | is_ill_dRet | is_ill_fpus
    return is_illeage.asBool
  }
     
  def is_mRet: Bool = {
    val is_mRet = rod.privil.mret & csrfiles.priv_lvl === "b11".U
    return is_mRet
  }

  def is_sRet: Bool = {
    val is_sRet = rod.privil.sret & ( csrfiles.priv_lvl === "b11".U | ( csrfiles.priv_lvl === "b01".U & ~csrfiles.mstatus.tsr.asBool) )
    return is_sRet
  }

  def is_dRet: Bool = {
    val is_dRet = rod.privil.dret & csrfiles.DMode
    return is_dRet
  }

  def is_fence_i: Bool = {
    val is_fence_i = rod.is_fence_i & is_wb
    return is_fence_i
  }

  def is_sfence_vma: Bool = {
    val is_sfence_vma = rod.is_sfence_vma & is_wb & ( (~csrfiles.mstatus.tvm.asBool & csrfiles.priv_lvl === "b01".U) | csrfiles.priv_lvl === "b11".U)
    return is_sfence_vma
  }


  def isException: Bool = {
    val isException = 
      is_ecall                 |
      is_ebreak_exc            |
      is_instr_access_fault    |
      is_instr_paging_fault    |
      is_illeage               |
      is_load_accessFault      |
      is_store_accessFault     |
      is_load_misAlign         |
      is_store_misAlign        |
      is_load_pagingFault      |
      is_store_pagingFault
    return isException
  }

  def isInterrupt: Bool = {
    val isInterrupt = (((csrfiles.is_m_interrupt | csrfiles.is_s_interrupt) & ~is_step_int_block) ) & ~csrfiles.DMode
    return isInterrupt
  }

  def is_xRet: Bool = {
    val is_xRet = is_mRet | is_sRet | is_dRet
    return is_xRet
  }

  def is_fpu_state_change: Bool = {
    val is_fpu_state_change =
      (~isInterrupt & ~isException & ~isNomaskInterrupt & ~isEbreakDM) &
      (
        (rod.is_fpu) |
        (rod.is_csr & (csrExe.addr === "h001".U | csrExe.addr === "h002".U | csrExe.addr === "h003".U))
      )
    return is_fpu_state_change
  }


  def is_ebreak_breakpointn: Bool = {
    val is_ebreak_breakpointn = ~csrfiles.DMode & 
      Mux1H(Seq(
        ( csrfiles.priv_lvl === "b11".U) -> csrfiles.dcsr.ebreakm,
        ( csrfiles.priv_lvl === "b01".U) -> csrfiles.dcsr.ebreaks,
        ( csrfiles.priv_lvl === "b00".U) -> csrfiles.dcsr.ebreaku,
      ))
    return is_ebreak_breakpointn.asBool
  }

  def is_step_int_block: Bool = {
    val is_step_int_block = ~csrfiles.dcsr.stepie & csrfiles.DMode
    return is_step_int_block.asBool
  }

  def is_step: Bool = {
    val is_step = csrfiles.dcsr.step & ~csrfiles.DMode
    return is_step.asBool
  }

  def commit_pc: UInt = {
    val commit_pc = extVaddr(rod.pc, vlen)
    return commit_pc
  } 
  
  def isEbreakDM: Bool = {
    val isEbreakDM = rod.privil.ebreak & is_ebreak_breakpointn
    return isEbreakDM
  }

  def isDebugInterrupt: Bool = {
    val isDebugInterrupt = 
      exint.is_single_step | exint.is_trigger | exint.hartHaltReq 

    return isDebugInterrupt
  }

  def isNomaskInterrupt: Bool = {
    val isNomaskInterrupt =  ~csrfiles.DMode & (isDebugInterrupt | exint.emu_reset)
    return isNomaskInterrupt
  }

}

