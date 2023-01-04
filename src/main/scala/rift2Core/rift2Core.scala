
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

import chipsalliance.rocketchip.config._
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
  

  lazy val module = if(isFlatten) {new Rift2CoreImp(this) with FlattenInstance} else {new Rift2CoreImp(this)}
}
 
class Rift2CoreImp(outer: Rift2Core, isFlatten: Boolean = false) extends LazyModuleImp(outer) with HasRiftParameters { 
  val io = IO(new Bundle{
    val dm        = if (hasDebugger) {Some(Flipped(new Info_DM_cmm))} else {None}
    val rtc_clock = Input(Bool())
    val aclint = Input(new AClint_Bundle)
    val plic = Input(new Plic_Bundle)
  })

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


  if1.io.if4Redirect := if4.io.if4Redirect

  if1.io.pc_gen <> if2.io.if2_req
  if2.io.if2_resp <> if3.io.if3_req
  if3.io.if3_resp <> if4.io.if4_req


  


  if3.io.btbResp  <> if4.io.btbResp
  if3.io.bimResp  <> if4.io.bimResp
  if3.io.tageResp <> if4.io.tageResp
  

  if3.io.if4_update_ghist := if4.io.if4_update_ghist
  if3.io.if4Redirect := if4.io.if4Redirect


  val rnm_stage = {
    val mdl = Module(new Rename)
    mdl.io.rnReq <> if4.io.if4_resp
    mdl
  }



  val iss_stage = {
    val mdl = Module(new Issue)
    mdl.io.dptReq <> rnm_stage.io.rnRsp
    mdl
  }


  val exe_stage = {
    val mdl = Module(new Execute( (Seq( dcache_edge, system_edge, periph_edge ) ) ) )
    iss_stage.io.alu_iss_exe <> mdl.io.alu_iss_exe
    iss_stage.io.bru_iss_exe <> mdl.io.bru_iss_exe
    iss_stage.io.lsu_iss_exe <> mdl.io.lsu_iss_exe
    iss_stage.io.csr_iss_exe <> mdl.io.csr_iss_exe
    iss_stage.io.mul_iss_exe <> mdl.io.mul_iss_exe
    iss_stage.io.fpu_iss_exe <> mdl.io.fpu_iss_exe
    mdl
  }


  val iwb_stage = { 
    val mdl = Module(new WriteBack)
    mdl.io.xLookup <> rnm_stage.io.xLookup
    mdl.io.fLookup <> rnm_stage.io.fLookup
    mdl.io.xRename <> rnm_stage.io.xRename
    mdl.io.fRename <> rnm_stage.io.fRename

    iss_stage.io.irgLog := mdl.io.irgLog
    iss_stage.io.frgLog := mdl.io.frgLog
    mdl.io.irgReq := iss_stage.io.irgReq
    mdl.io.frgReq := iss_stage.io.frgReq
    iss_stage.io.irgRsp := mdl.io.irgRsp
    iss_stage.io.frgRsp := mdl.io.frgRsp


    mdl.io.alu_iWriteBack <> exe_stage.io.alu_exe_iwb
    mdl.io.bru_iWriteBack <> exe_stage.io.bru_exe_iwb
    mdl.io.csr_iWriteBack <> exe_stage.io.csr_exe_iwb
    mdl.io.mem_iWriteBack <> exe_stage.io.lsu_exe_iwb
    mdl.io.mem_fWriteBack <> exe_stage.io.lsu_exe_fwb
    mdl.io.mul_iWriteBack <> exe_stage.io.mul_exe_iwb
    mdl.io.fpu_iWriteBack <> exe_stage.io.fpu_exe_iwb
    mdl.io.fpu_fWriteBack <> exe_stage.io.fpu_exe_fwb

    mdl.io.cWriteBack <> exe_stage.io.cWriteBack
    mdl.io.csrIsReady <> iss_stage.io.csrIsReady

    mdl.io.cLookup <> rnm_stage.io.cLookup
    mdl.io.cRename <> rnm_stage.io.cRename


    mdl
  }




  val cmm_stage = Module(new Commit)

  val i_mmu = {
    val mdl = Module(new MMU(edge = mmu_edge ))
    mdl.io.if_mmu <> if2.io.if_mmu
    mdl.io.mmu_if <> if2.io.mmu_if
    mdl.io.lsu_mmu <> exe_stage.io.lsu_mmu
    mdl.io.mmu_lsu <> exe_stage.io.mmu_lsu
    mdl.io.cmm_mmu <> cmm_stage.io.cmm_mmu
    mdl
  }


    if1.io.jcmm_update := exe_stage.io.jcmm_update
    if1.io.bcmm_update := exe_stage.io.bcmm_update
    if3.io.jcmm_update := exe_stage.io.jcmm_update
    if3.io.bcmm_update := exe_stage.io.bcmm_update
    if4.io.jcmm_update := exe_stage.io.jcmm_update

  


    exe_stage.io.bftq <> if4.io.bftq
    exe_stage.io.jftq <> if4.io.jftq


  

  i_mmu.io.if_flush := if2.io.flush
  i_mmu.io.lsu_flush := exe_stage.io.flush

  if2.io.flush := cmm_stage.io.cmmRedirect.fire | if4.io.if4Redirect.valid
  if3.io.flush := cmm_stage.io.cmmRedirect.fire
  if4.io.flush := cmm_stage.io.cmmRedirect.fire

  rnm_stage.reset := cmm_stage.io.cmmRedirect.fire | reset.asBool
  iss_stage.io.flush := cmm_stage.io.cmmRedirect.fire
  exe_stage.io.flush := cmm_stage.io.cmmRedirect.fire



  cmm_stage.io.cm_op <> iwb_stage.io.commit
  cmm_stage.io.rod <> rnm_stage.io.rod_i
  cmm_stage.io.cmm_lsu <> exe_stage.io.cmm_lsu
  cmm_stage.io.lsu_cmm <> exe_stage.io.lsu_cmm
  cmm_stage.io.csr_addr <> exe_stage.io.csr_addr
  cmm_stage.io.csr_data <> exe_stage.io.csr_data
  cmm_stage.io.csr_cmm_op <> exe_stage.io.csr_cmm_op
  cmm_stage.io.fcsr <> exe_stage.io.fcsr
  cmm_stage.io.fcsr_cmm_op <> exe_stage.io.fcsr_cmm_op
  cmm_stage.io.bctq <> exe_stage.io.bctq
  cmm_stage.io.jctq <> exe_stage.io.jctq
  cmm_stage.io.cmmRedirect <> if1.io.cmmRedirect
  cmm_stage.io.if_cmm := if2.io.if_cmm
  cmm_stage.io.aclint := io.aclint
  cmm_stage.io.plic := io.plic
  if2.io.ifence := cmm_stage.io.ifence


  iss_stage.io.csrfiles := cmm_stage.io.csrfiles
  iwb_stage.io.cCommit <> cmm_stage.io.csrCmm
  cmm_stage.io.csrOp := iwb_stage.io.csrOp


  cmm_stage.io.rtc_clock := io.rtc_clock
  if (hasDebugger) {cmm_stage.io.dm <> io.dm.get}
  else {
    cmm_stage.io.dm.hartResetReq := false.B
    cmm_stage.io.dm.hartHaltReq := false.B
  }


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
    // val dcache_getPut = if ( hasL2 ) { None } else { Some(        new DecoupledIO(new TLBundleA(dEdge(0).bundle)) ) }
    // val dcache_access = if ( hasL2 ) { None } else { Some(Flipped(new DecoupledIO(new TLBundleD(dEdge(0).bundle)))) }

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




  val diff = {
    val mdl = Module(new diff)
    mdl.io.diffXReg := iwb_stage.io.diffXReg
    mdl.io.diffFReg := iwb_stage.io.diffFReg
    mdl.io.commit   := cmm_stage.io.diff_commit
    mdl.io.csr      := cmm_stage.io.diff_csr
    mdl
  }


  rnm_stage.io.vLookup.map{x=>x.rsp := DontCare}
  iss_stage.io.vrgRsp.map{ x => {x.valid := false.B; x.bits := DontCare} }
  iss_stage.io.vrgLog      := DontCare
  rnm_stage.io.vRename.map{x=>x.req.ready := true.B; x.rsp := DontCare}


}





