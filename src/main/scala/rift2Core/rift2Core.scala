
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


package rift2Core

import chisel3._
import chisel3.util._
import rift2Chip._
import rift2Core.frontend._
import rift2Core.backend._
import rift2Core.diff._
import rift2Core.privilege._
import debug._

import rift2Core.define._

import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import chisel3.util.experimental.{FlattenInstance, InlineInstance}


class Rift2Core(isFlatten: Boolean = false)(implicit p: Parameters) extends LazyModule with HasRiftParameters {
  val prefetcherClientParameters = TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = "prefetch",
      sourceId = IdRange(0, 1),
    ))
  )

  val dcacheClientParameters = TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = "dcache",
      sourceId = IdRange(0, dcacheParams.bk),
      supportsProbe = if (hasL2) { TransferSizes((l1DW)/8) } else { TransferSizes.none }
    ))
  )

  val systemClientParameters = TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = "system",
      sourceId = IdRange(0, 1),
    ))
  )

  val periphClientParameters = TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = "periph",
      sourceId = IdRange(0, 1),
    ))
  )

  val icacheClientParameters = TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = "icache",
      sourceId = IdRange(0, 1),
    ))
  )

  val mmuClientParameters = TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = "mmu",
      sourceId = IdRange(0, 2),
    ))
  )

  val icacheClientNode = TLClientNode(Seq(icacheClientParameters))
  val dcacheClientNode = TLClientNode(Seq(dcacheClientParameters))
  val systemClientNode = TLClientNode(Seq(systemClientParameters))
  val periphClientNode = TLClientNode(Seq(periphClientParameters))
  val    mmuClientNode = TLClientNode(Seq(   mmuClientParameters))
  val prefetchClinetNode = TLClientNode(Seq( prefetcherClientParameters))
  

  lazy val module: Rift2CoreImp = if(isFlatten) {new Rift2CoreImp(this) with FlattenInstance} else {new Rift2CoreImp(this)}
}
 
abstract class Rift2CoreImpBase(outer: Rift2Core, isFlatten: Boolean = false) extends LazyModuleImp(outer) with HasRiftParameters { 
  class Rift2CoreIO extends Bundle{
    val dm        = if (hasDebugger) {Some(Flipped(new Info_DM_cmm))} else {None}
    val rtc_clock = Input(Bool())
    val aclint = Input(new AClint_Bundle)
    val plic = Input(new Plic_Bundle)    
  }
  
  val io: Rift2CoreIO = IO(new Rift2CoreIO)

  val ( icache_bus, icache_edge ) = outer.icacheClientNode.out.head
  val ( dcache_bus, dcache_edge ) = outer.dcacheClientNode.out.head
  val ( system_bus, system_edge ) = outer.systemClientNode.out.head
  val ( periph_bus, periph_edge ) = outer.periphClientNode.out.head
  val (    mmu_bus,    mmu_edge ) = outer.mmuClientNode.out.head
  val ( prefetch_bus, prefetch_edge) = outer.prefetchClinetNode.out.head


  val if1 = (if (hasuBTB) {Module(new IF1Predict)} else {Module(new IF1NPredict)})
  val if2 = Module(new IF2(icache_edge))
  val if3 = Module(new IF3)
  val if4 = Module(new IF4)
  val preRename_stage = if(hasVector) { Module(new VecPreRename) } else { Module(new FakeVecPreRename) }
  val rnm_stage = Module(new Rename)
  val preIssue_stage = if(hasVector) { Module(new VecPreIssue) } else { Module(new FakeVecPreIssue) }
  val iss_stage = Module(new Issue)
  val exe_stage = Module(new Execute( (Seq( dcache_edge, system_edge, periph_edge ) ) ) )
  val iwb_stage = Module(new WriteBack)
  val cmm_stage = Module(new Commit)
  val i_mmu = Module(new MMU(edge = mmu_edge ))
  val diff = Module(new diff)




}

trait Rift2CoreImpFrontend{ this: Rift2CoreImpBase =>

  if1.io.if4Redirect := if4.io.if4Redirect

  if1.io.pc_gen <> if2.io.if2_req
  if2.io.if2_resp <> if3.io.if3_req
  if3.io.if3_resp <> if4.io.if4_req

  if3.io.btbResp  <> if4.io.btbResp
  if3.io.bimResp  <> if4.io.bimResp
  if3.io.tageResp <> if4.io.tageResp
  
  if3.io.if4_update_ghist := if4.io.if4_update_ghist
  if3.io.if4Redirect := if4.io.if4Redirect
}

trait Rift2CoreImpBackend{ this: Rift2CoreImpBase =>
  
  if4.io.if4_resp <> preRename_stage.io.enq
  preRename_stage.io.deq <> rnm_stage.io.rnReq

  preRename_stage.io.vpuCsrMolloc <> iwb_stage.io.vpuCsrMolloc

  rnm_stage.io.rnRsp <> preIssue_stage.io.enq
  
  preIssue_stage.io.deq <> iss_stage.io.dptReq

  preIssue_stage.io.molloc <> iwb_stage.io.vMolloc
  preIssue_stage.io.readOp <> iwb_stage.io.vReadOp


  iss_stage.io.alu_iss_exe <> exe_stage.io.alu_iss_exe
  iss_stage.io.bru_iss_exe <> exe_stage.io.bru_iss_exe
  iss_stage.io.lsu_iss_exe <> exe_stage.io.lsu_iss_exe
  iss_stage.io.csr_iss_exe <> exe_stage.io.csr_iss_exe
  iss_stage.io.mul_iss_exe <> exe_stage.io.mul_iss_exe
  iss_stage.io.fpu_iss_exe <> exe_stage.io.fpu_iss_exe

  iwb_stage.io.xLookup <> rnm_stage.io.xLookup
  iwb_stage.io.fLookup <> rnm_stage.io.fLookup

  iwb_stage.io.xRename <> rnm_stage.io.xRename
  iwb_stage.io.fRename <> rnm_stage.io.fRename


  iss_stage.io.irgLog := iwb_stage.io.irgLog
  iss_stage.io.frgLog := iwb_stage.io.frgLog

  iwb_stage.io.irgReq := iss_stage.io.irgReq
  iwb_stage.io.frgReq := iss_stage.io.frgReq

  iss_stage.io.irgRsp := iwb_stage.io.irgRsp
  iss_stage.io.frgRsp := iwb_stage.io.frgRsp

  iwb_stage.io.alu_iWriteBack <> exe_stage.io.alu_exe_iwb
  iwb_stage.io.bru_iWriteBack <> exe_stage.io.bru_exe_iwb
  iwb_stage.io.csr_iWriteBack <> exe_stage.io.csr_exe_iwb
  iwb_stage.io.mul_iWriteBack <> exe_stage.io.mul_exe_iwb

  iwb_stage.io.mem_iWriteBack <> exe_stage.io.lsu_exe_iwb
  iwb_stage.io.mem_fWriteBack <> exe_stage.io.lsu_exe_fwb
  iwb_stage.io.mem_vWriteBack <> exe_stage.io.lsu_exe_vwb

  iwb_stage.io.fpu_iWriteBack <> exe_stage.io.fpu_exe_iwb
  iwb_stage.io.fpu_fWriteBack <> exe_stage.io.fpu_exe_fwb

  iwb_stage.io.xpuCsrWriteBack := exe_stage.io.xpuCsrWriteBack
  iwb_stage.io.fpuCsrWriteBack := exe_stage.io.fpuCsrWriteBack
  iwb_stage.io.vpuCsrWriteBack.valid := false.B
  iwb_stage.io.vpuCsrWriteBack.bits  := DontCare

  iwb_stage.io.xpuCsrMolloc    <> rnm_stage.io.xpuCsrMolloc
  iwb_stage.io.fpuCsrMolloc    <> rnm_stage.io.fpuCsrMolloc

  preRename_stage.io.csrfiles := cmm_stage.io.csrfiles
  preIssue_stage.io.csrfiles := cmm_stage.io.csrfiles
  iss_stage.io.csrfiles := cmm_stage.io.csrfiles

}

trait Rift2CoreImpPredict{ this: Rift2CoreImpBase =>
  if1.io.jcmm_update := exe_stage.io.jcmm_update
  if1.io.bcmm_update := exe_stage.io.bcmm_update
  if3.io.jcmm_update := exe_stage.io.jcmm_update
  if3.io.bcmm_update := exe_stage.io.bcmm_update
  if4.io.jcmm_update := exe_stage.io.jcmm_update

  exe_stage.io.bftq <> if4.io.bftq
  exe_stage.io.jftq <> if4.io.jftq
}

trait Rift2CoreImpBus{ this: Rift2CoreImpBase =>

  if2.io.icache_access.bits := icache_bus.d.bits
  if2.io.icache_access.valid := icache_bus.d.valid
  icache_bus.d.ready := if2.io.icache_access.ready

  icache_bus.a.valid := if2.io.icache_get.valid
  icache_bus.a.bits := if2.io.icache_get.bits
  if2.io.icache_get.ready := icache_bus.a.ready

  if( hasL2 ) {
    exe_stage.io.missUnit_dcache_grant.get.bits  := dcache_bus.d.bits
    exe_stage.io.missUnit_dcache_grant.get.valid := dcache_bus.d.valid & ( dcache_bus.d.bits.opcode === TLMessages.Grant | dcache_bus.d.bits.opcode === TLMessages.GrantData )

    exe_stage.io.writeBackUnit_dcache_grant.get.bits  := dcache_bus.d.bits
    exe_stage.io.writeBackUnit_dcache_grant.get.valid := dcache_bus.d.valid & ( dcache_bus.d.bits.opcode === TLMessages.ReleaseAck )

    dcache_bus.d.ready := 
      Mux1H(Seq(
        ( dcache_bus.d.bits.opcode === TLMessages.Grant || dcache_bus.d.bits.opcode === TLMessages.GrantData ) -> exe_stage.io.missUnit_dcache_grant.get.ready,
        ( dcache_bus.d.bits.opcode === TLMessages.ReleaseAck )                                                 -> exe_stage.io.writeBackUnit_dcache_grant.get.ready
      ))

    dcache_bus.a.valid := exe_stage.io.missUnit_dcache_acquire.get.valid
    dcache_bus.a.bits  := exe_stage.io.missUnit_dcache_acquire.get.bits
    exe_stage.io.missUnit_dcache_acquire.get.ready := dcache_bus.a.ready

    exe_stage.io.probeUnit_dcache_probe.get.valid := dcache_bus.b.valid
    exe_stage.io.probeUnit_dcache_probe.get.bits  := dcache_bus.b.bits
    dcache_bus.b.ready := exe_stage.io.probeUnit_dcache_probe.get.ready
    
    dcache_bus.c.valid := exe_stage.io.writeBackUnit_dcache_release.get.valid
    dcache_bus.c.bits  := exe_stage.io.writeBackUnit_dcache_release.get.bits
    exe_stage.io.writeBackUnit_dcache_release.get.ready := dcache_bus.c.ready
    
    dcache_bus.e.valid := exe_stage.io.missUnit_dcache_grantAck.get.valid
    dcache_bus.e.bits  := exe_stage.io.missUnit_dcache_grantAck.get.bits
    exe_stage.io.missUnit_dcache_grantAck.get.ready := dcache_bus.e.ready
  } else {
    dcache_bus.a.valid := exe_stage.io.dcache_getPut.get.valid
    dcache_bus.a.bits  := exe_stage.io.dcache_getPut.get.bits
    exe_stage.io.dcache_getPut.get.ready := dcache_bus.a.ready

    exe_stage.io.dcache_access.get.bits  := dcache_bus.d.bits
    exe_stage.io.dcache_access.get.valid := dcache_bus.d.valid
    dcache_bus.d.ready := exe_stage.io.dcache_access.get.ready

  }
  
  exe_stage.io.system_access.bits  := system_bus.d.bits
  exe_stage.io.system_access.valid := system_bus.d.valid
  system_bus.d.ready := exe_stage.io.system_access.ready
  system_bus.a.valid := exe_stage.io.system_getPut.valid
  system_bus.a.bits  := exe_stage.io.system_getPut.bits
  exe_stage.io.system_getPut.ready := system_bus.a.ready

  exe_stage.io.periph_access.bits  := periph_bus.d.bits
  exe_stage.io.periph_access.valid := periph_bus.d.valid
  periph_bus.d.ready := exe_stage.io.periph_access.ready
  periph_bus.a.valid := exe_stage.io.periph_getPut.valid
  periph_bus.a.bits  := exe_stage.io.periph_getPut.bits
  exe_stage.io.periph_getPut.ready := periph_bus.a.ready

  mmu_bus.a.valid := i_mmu.io.ptw_get.valid
  mmu_bus.a.bits := i_mmu.io.ptw_get.bits
  i_mmu.io.ptw_get.ready := mmu_bus.a.ready
  mmu_bus.d.ready := i_mmu.io.ptw_access.ready
  i_mmu.io.ptw_access.bits  := mmu_bus.d.bits
  i_mmu.io.ptw_access.valid := mmu_bus.d.valid


  val prefetcher = Module(new PreFetcher(prefetch_edge))
  prefetcher.io.stqReq          := exe_stage.io.preFetch
  prefetcher.io.icacheRefillReq := if2.io.preFetch

  prefetch_bus.a.valid := prefetcher.io.intent.valid
  prefetch_bus.a.bits := prefetcher.io.intent.bits
  prefetcher.io.intent.ready := prefetch_bus.a.ready
  prefetch_bus.d.ready := prefetcher.io.hintAck.ready
  prefetcher.io.hintAck.bits  := prefetch_bus.d.bits
  prefetcher.io.hintAck.valid := prefetch_bus.d.valid
}

trait Rift2CoreImpMMU{ this: Rift2CoreImpBase =>
  i_mmu.io.if_mmu <> if2.io.if_mmu
  i_mmu.io.mmu_if <> if2.io.mmu_if
  i_mmu.io.lsu_mmu <> exe_stage.io.lsu_mmu
  i_mmu.io.mmu_lsu <> exe_stage.io.mmu_lsu
  i_mmu.io.cmm_mmu <> cmm_stage.io.cmm_mmu
}

trait Rift2CoreImpCommit{ this: Rift2CoreImpBase =>
  iwb_stage.io.xCommit <> cmm_stage.io.xCommit
  iwb_stage.io.fCommit <> cmm_stage.io.fCommit
  iwb_stage.io.vCommit <> cmm_stage.io.vCommit

  cmm_stage.io.rod <> rnm_stage.io.rod_i

  cmm_stage.io.cmm_lsu <> exe_stage.io.cmm_lsu
  cmm_stage.io.lsu_cmm <> exe_stage.io.lsu_cmm
  preIssue_stage.io.isPndVStore := cmm_stage.io.isPndVStore

  cmm_stage.io.bctq <> exe_stage.io.bctq
  cmm_stage.io.jctq <> exe_stage.io.jctq
  cmm_stage.io.cmmRedirect <> if1.io.cmmRedirect
  cmm_stage.io.if_cmm := if2.io.if_cmm
  cmm_stage.io.aclint := io.aclint
  cmm_stage.io.plic := io.plic
  if2.io.ifence := cmm_stage.io.ifence

  iwb_stage.io.xpuCsrCommit <> cmm_stage.io.xpuCsrCommit
  iwb_stage.io.fpuCsrCommit <> cmm_stage.io.fpuCsrCommit
  iwb_stage.io.vpuCsrCommit <> cmm_stage.io.vpuCsrCommit

  cmm_stage.io.rtc_clock := io.rtc_clock
  if (hasDebugger) {cmm_stage.io.dm <> io.dm.get}
  else {
    cmm_stage.io.dm.hartResetReq := false.B
    cmm_stage.io.dm.hartHaltReq := false.B
  }

  i_mmu.io.if_flush := if2.io.flush
  i_mmu.io.lsu_flush := exe_stage.io.flush

  if2.io.flush := cmm_stage.io.cmmRedirect.fire | if4.io.if4Redirect.valid
  if3.io.flush := cmm_stage.io.cmmRedirect.fire
  if4.io.flush := cmm_stage.io.cmmRedirect.fire

  preRename_stage.io.flush := cmm_stage.io.cmmRedirect.fire
  rnm_stage.reset := cmm_stage.io.cmmRedirect.fire | reset.asBool
  preIssue_stage.io.flush := cmm_stage.io.cmmRedirect.fire
  iss_stage.io.flush := cmm_stage.io.cmmRedirect.fire
  exe_stage.io.flush := cmm_stage.io.cmmRedirect.fire

  diff.io.diffXReg := iwb_stage.io.diffXReg
  diff.io.diffFReg := iwb_stage.io.diffFReg
  diff.io.commit   := cmm_stage.io.diff_commit
  diff.io.csr      := cmm_stage.io.diff_csr


}


class Rift2CoreImp(outer: Rift2Core, isFlatten: Boolean = false)(implicit p: Parameters) extends Rift2CoreImpBase(outer, isFlatten)
with Rift2CoreImpFrontend
with Rift2CoreImpBackend
with Rift2CoreImpPredict
with Rift2CoreImpBus
with Rift2CoreImpMMU
with Rift2CoreImpCommit{
  
}








