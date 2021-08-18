/*
* @Author: Ruige Lee
* @Date:   2021-03-18 16:11:48
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-03-23 19:17:01
*/


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


package rift2Core

import chisel3._
import chisel3.util._
import rift2Core.frontend._
import rift2Core.backend._
import rift2Core.diff._
import rift2Core.privilege._
import axi._

import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._




class Rift2Core()(implicit p: Parameters) extends LazyModule{
  val dcacheClientParameters = TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = "dcache",
      sourceId = IdRange(0, 1),
      supportsProbe = TransferSizes(32)
    ))
  )

  val icacheClientParameters = TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = "icache",
      sourceId = IdRange(0, 1),
      supportsProbe = TransferSizes(32)
    ))
  )

  val mmuClientParameters = TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = "mmu",
      sourceId = IdRange(0, 1),
      // supportsGet = TransferSizes(32)
    ))
  )

  val icacheClientNode = TLClientNode(Seq(icacheClientParameters))
  val dcacheClientNode = TLClientNode(Seq(dcacheClientParameters))
  val    mmuClientNode = TLClientNode(Seq(   mmuClientParameters))

  lazy val module = new Rift2CoreImp(this)
}
 
class Rift2CoreImp(outer: Rift2Core) extends LazyModuleImp(outer) {
  val io = IO(new Bundle{

    val sys_chn_ar = new DecoupledIO(new AXI_chn_a( 32, 1, 1 ))
    val sys_chn_r = Flipped( new DecoupledIO(new AXI_chn_r( 64, 1, 1)) )
    val sys_chn_aw = new DecoupledIO(new AXI_chn_a( 32, 1, 1 ))
    val sys_chn_w = new DecoupledIO(new AXI_chn_w( 64, 1 )) 
    val sys_chn_b = Flipped( new DecoupledIO(new AXI_chn_b( 1, 1 )))

    val rtc_clock = Input(Bool())
  })

  val ( icache_bus, icache_edge ) = outer.icacheClientNode.out.head
  val ( dcache_bus, dcache_edge ) = outer.dcacheClientNode.out.head
  val (    mmu_bus,    mmu_edge ) = outer.mmuClientNode.out.head

  val diff = Module(new diff)



  val pc_stage = Module(new Pc_gen)
  val if_stage = Module(new Ifetch(icache_edge))
  val pd_stage = Module(new Predecode_ss)
  val bd_stage = Module(new BP_ID_ss)


  val dpt_stage = Module(new Dispatch_ss)
  val iss_stage = Module(new Issue)
  val exe_stage = Module(new Execute(dcache_edge))
  val iwb_stage = Module(new WriteBack)
  val cmm_stage = Module(new Commit)

  val i_regfiles = Module(new Regfiles)

  val i_mmu = Module(new MMU(edge = mmu_edge))

  i_mmu.io.if_mmu <> if_stage.io.if_mmu
  i_mmu.io.mmu_if <> if_stage.io.mmu_if
  i_mmu.io.lsu_mmu <> exe_stage.io.lsu_mmu
  i_mmu.io.mmu_lsu <> exe_stage.io.mmu_lsu

  i_mmu.io.cmm_mmu <> cmm_stage.io.cmm_mmu


  i_mmu.io.flush := false.B




  pc_stage.io.bd_pc <> bd_stage.io.bd_pc
  

  pc_stage.io.pc_pd <> pd_stage.io.pc_pd	//valid when flush for new pc 

  
  pc_stage.io.pc_if <> if_stage.io.pc_if
  if_stage.io.if_iq <> pd_stage.io.if_pd
  pd_stage.io.pd_bd <> bd_stage.io.pd_bd
  bd_stage.io.bd_dpt <> dpt_stage.io.bd_dpt


  dpt_stage.io.alu_dpt_iss <> iss_stage.io.alu_dpt_iss
  dpt_stage.io.bru_dpt_iss <> iss_stage.io.bru_dpt_iss
  dpt_stage.io.lsu_dpt_iss <> iss_stage.io.lsu_dpt_iss
  dpt_stage.io.csr_dpt_iss <> iss_stage.io.csr_dpt_iss
  dpt_stage.io.mul_dpt_iss <> iss_stage.io.mul_dpt_iss

  iss_stage.io.alu_iss_exe <> exe_stage.io.alu_iss_exe
  iss_stage.io.bru_iss_exe <> exe_stage.io.bru_iss_exe
  iss_stage.io.lsu_iss_exe <> exe_stage.io.lsu_iss_exe
  iss_stage.io.csr_iss_exe <> exe_stage.io.csr_iss_exe
  iss_stage.io.mul_iss_exe <> exe_stage.io.mul_iss_exe

  exe_stage.io.alu_exe_iwb <>	iwb_stage.io.exe_iwb(0)
  exe_stage.io.bru_exe_iwb <>	iwb_stage.io.exe_iwb(1)
  exe_stage.io.csr_exe_iwb <>	iwb_stage.io.exe_iwb(2)	
  exe_stage.io.lsu_exe_iwb <>	iwb_stage.io.exe_iwb(3)	
  exe_stage.io.mul_exe_iwb <>	iwb_stage.io.exe_iwb(4)



  

  bd_stage.io.bru_pd_b <> exe_stage.io.bru_pd_b
  pc_stage.io.bru_pd_j <> exe_stage.io.bru_pd_j

  

  i_mmu.io.if_flush := if_stage.io.flush
  i_mmu.io.lsu_flush := exe_stage.io.flush
  if_stage.io.flush  := cmm_stage.io.is_commit_abort(0) | bd_stage.io.bd_pc.valid | exe_stage.io.bru_pd_j.valid
  // pd_stage.io.flush  := exe_stage.io.icache_fence_req
  bd_stage.io.flush  := cmm_stage.io.is_commit_abort(0) | exe_stage.io.bru_pd_j.valid 
  // id_stage.io.flush  := cmm_stage.io.is_commit_abort(0) 
  dpt_stage.io.flush := cmm_stage.io.is_commit_abort(0)
  iss_stage.io.flush := cmm_stage.io.is_commit_abort(0)
  exe_stage.io.flush := cmm_stage.io.is_commit_abort(0)
  i_regfiles.io.flush := cmm_stage.io.is_commit_abort(0)

  cmm_stage.io.is_misPredict := bd_stage.io.is_misPredict_taken



  cmm_stage.io.rod_i <> dpt_stage.io.rod_i
  cmm_stage.io.cmm_lsu <> exe_stage.io.cmm_lsu
  cmm_stage.io.lsu_cmm <> exe_stage.io.lsu_cmm
  cmm_stage.io.cmm_bru_ilp <> exe_stage.io.cmm_bru_ilp
  cmm_stage.io.csr_addr <> exe_stage.io.csr_addr
  cmm_stage.io.csr_data <> exe_stage.io.csr_data
  cmm_stage.io.csr_cmm_op <> exe_stage.io.csr_cmm_op

  cmm_stage.io.cmm_pc <> pc_stage.io.cmm_pc


  i_regfiles.io.wb_op <> iwb_stage.io.wb_op
  i_regfiles.io.rn_op <> dpt_stage.io.rn_op_i
  i_regfiles.io.cm_op <> cmm_stage.io.cm_op
  i_regfiles.io.files <> iss_stage.io.files
  i_regfiles.io.log <> cmm_stage.io.log
  i_regfiles.io.log <> iss_stage.io.log
  i_regfiles.io.log <> dpt_stage.io.log_i
  i_regfiles.io.rn_ptr <> dpt_stage.io.rn_ptr_i

  cmm_stage.io.rtc_clock := io.rtc_clock
  



  if_stage.io.missUnit_icache_grant.bits := icache_bus.d.bits
  if_stage.io.missUnit_icache_grant.valid := icache_bus.d.valid & ( icache_bus.d.bits.opcode === TLMessages.Grant | icache_bus.d.bits.opcode === TLMessages.GrantData )

  if_stage.io.writeBackUnit_icache_grant.bits := icache_bus.d.bits
  if_stage.io.writeBackUnit_icache_grant.valid := icache_bus.d.valid & ( icache_bus.d.bits.opcode === TLMessages.ReleaseAck )

  icache_bus.d.ready := 
    Mux1H(Seq(
      ( icache_bus.d.bits.opcode === TLMessages.Grant || icache_bus.d.bits.opcode === TLMessages.GrantData ) -> if_stage.io.missUnit_icache_grant.ready,
      ( icache_bus.d.bits.opcode === TLMessages.ReleaseAck ) -> if_stage.io.writeBackUnit_icache_grant.ready
    ))

  icache_bus.a.valid := if_stage.io.missUnit_icache_acquire.valid
  icache_bus.a.bits := if_stage.io.missUnit_icache_acquire.bits
  if_stage.io.missUnit_icache_acquire.ready := icache_bus.a.ready

  if_stage.io.probeUnit_icache_probe.valid := icache_bus.b.valid
  if_stage.io.probeUnit_icache_probe.bits := icache_bus.b.bits
  icache_bus.b.ready := if_stage.io.probeUnit_icache_probe.ready
  
  icache_bus.c.valid := if_stage.io.writeBackUnit_icache_release.valid
  icache_bus.c.bits := if_stage.io.writeBackUnit_icache_release.bits
  if_stage.io.writeBackUnit_icache_release.ready := icache_bus.c.ready
  
  icache_bus.e.valid := if_stage.io.missUnit_icache_grantAck.valid
  icache_bus.e.bits := if_stage.io.missUnit_icache_grantAck.bits
  if_stage.io.missUnit_icache_grantAck.ready := icache_bus.e.ready 








  exe_stage.io.missUnit_dcache_grant.bits := dcache_bus.d.bits
  exe_stage.io.missUnit_dcache_grant.valid := dcache_bus.d.valid & ( dcache_bus.d.bits.opcode === TLMessages.Grant | dcache_bus.d.bits.opcode === TLMessages.GrantData )

  exe_stage.io.writeBackUnit_dcache_grant.bits := dcache_bus.d.bits
  exe_stage.io.writeBackUnit_dcache_grant.valid := dcache_bus.d.valid & ( dcache_bus.d.bits.opcode === TLMessages.ReleaseAck )

  dcache_bus.d.ready := 
    Mux1H(Seq(
      ( dcache_bus.d.bits.opcode === TLMessages.Grant || dcache_bus.d.bits.opcode === TLMessages.GrantData ) -> exe_stage.io.missUnit_dcache_grant.ready,
      ( dcache_bus.d.bits.opcode === TLMessages.ReleaseAck ) -> exe_stage.io.writeBackUnit_dcache_grant.ready
    ))

  dcache_bus.a.valid := exe_stage.io.missUnit_dcache_acquire.valid
  dcache_bus.a.bits := exe_stage.io.missUnit_dcache_acquire.bits
  exe_stage.io.missUnit_dcache_acquire.ready := dcache_bus.a.ready

  exe_stage.io.probeUnit_dcache_probe.valid := dcache_bus.b.valid
  exe_stage.io.probeUnit_dcache_probe.bits := dcache_bus.b.bits
  dcache_bus.b.ready := exe_stage.io.probeUnit_dcache_probe.ready
  
  dcache_bus.c.valid := exe_stage.io.writeBackUnit_dcache_release.valid
  dcache_bus.c.bits := exe_stage.io.writeBackUnit_dcache_release.bits
  exe_stage.io.writeBackUnit_dcache_release.ready := dcache_bus.c.ready
  
  dcache_bus.e.valid := exe_stage.io.missUnit_dcache_grantAck.valid
  dcache_bus.e.bits := exe_stage.io.missUnit_dcache_grantAck.bits
  exe_stage.io.missUnit_dcache_grantAck.ready := dcache_bus.e.ready 

  mmu_bus.a.valid := i_mmu.io.ptw_get.valid
  mmu_bus.a.bits := i_mmu.io.ptw_get.bits
  i_mmu.io.ptw_get.ready := mmu_bus.a.ready

  mmu_bus.d.ready := i_mmu.io.ptw_access.ready
  i_mmu.io.ptw_access.bits := mmu_bus.d.bits
  i_mmu.io.ptw_access.valid := mmu_bus.d.valid


  exe_stage.io.sys_chn_ar <> io.sys_chn_ar
  exe_stage.io.sys_chn_r  <> io.sys_chn_r
  exe_stage.io.sys_chn_aw <> io.sys_chn_aw
  exe_stage.io.sys_chn_w  <> io.sys_chn_w
  exe_stage.io.sys_chn_b  <> io.sys_chn_b














  diff.io.register := i_regfiles.io.diff_register
  diff.io.commit   := cmm_stage.io.diff_commit


}




