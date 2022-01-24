
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
  val dcacheClientNode = 
    for ( i <- 0 until 8 ) yield {
      TLClientNode(Seq(dcacheClientParameters))
    }
  val systemClientNode = TLClientNode(Seq(systemClientParameters))
  val periphClientNode = TLClientNode(Seq(periphClientParameters))
  val    mmuClientNode = TLClientNode(Seq(   mmuClientParameters))

  lazy val module = new Rift2CoreImp(this)
}
 
class Rift2CoreImp(outer: Rift2Core) extends LazyModuleImp(outer) {
  val io = IO(new Bundle{

    val rtc_clock = Input(Bool())
  })

  val ( icache_bus, icache_edge ) = outer.icacheClientNode.out.head
  val  dcache_bus  = for ( i <- 0 until 8 ) yield { outer.dcacheClientNode(i).out.head._1}
  val  dcache_edge = for ( i <- 0 until 8 ) yield { outer.dcacheClientNode(i).out.head._2}
  val ( system_bus, system_edge ) = outer.systemClientNode.out.head
  val ( periph_bus, periph_edge ) = outer.periphClientNode.out.head
  val (    mmu_bus,    mmu_edge ) = outer.mmuClientNode.out.head

  val diff = Module(new diff)



  val pc_stage = Module(new Pc_gen)
  val if_stage = Module(new Ifetch(icache_edge))
  val pd_stage = Module(new Predecode_ss)
  val bd_stage = Module(new BP_ID_ss)


  val dpt_stage = {
    val mdl = Module(new Dispatch)
    mdl.io.bd_dpt <> bd_stage.io.bd_dpt
    mdl
  }

  val iss_stage = {
    val mdl = Module(new Issue)
    mdl.io.ooo_dpt_iss <> dpt_stage.io.ooo_dpt_iss
    mdl.io.ito_dpt_iss <> dpt_stage.io.ito_dpt_iss
    mdl
  }


  val exe_stage = Module(new Execute( ((for ( i <- 0 until 8 ) yield dcache_edge(i) ) ++ Seq( system_edge, periph_edge ) ) ) )






//     val commit = Vec(cmm_chn, Flipped(Valid(new Info_commit_op(dp))))
//   })

  val iwb_stage = { 
    val mdl = Module(new WriteBack)
    mdl.io.dpt_rename <> dpt_stage.io.dpt_rename
    mdl.io.ooo_readOp <> iss_stage.io.ooo_readOp
    mdl.io.ito_readOp <> iss_stage.io.ito_readOp
    mdl.io.fpu_readOp.reg.valid := false.B
    mdl.io.fpu_readOp.reg.bits := DontCare

    mdl.io.alu_iWriteBack <> exe_stage.io.alu_exe_iwb
    mdl.io.bru_iWriteBack <> exe_stage.io.bru_exe_iwb
    mdl.io.csr_iWriteBack <> exe_stage.io.csr_exe_iwb
    mdl.io.mem_iWriteBack <> exe_stage.io.lsu_exe_wb
    mdl.io.mul_iWriteBack <> exe_stage.io.mul_exe_iwb
    mdl.io.fpu_iWriteBack.valid := false.B
    mdl.io.fpu_iWriteBack.bits := DontCare


    mdl
  }



  val cmm_stage = Module(new Commit)

  // val i_regfiles = Module(new Regfiles)

  val i_mmu = Module(new MMU(edge = mmu_edge))

  i_mmu.io.if_mmu <> if_stage.io.if_mmu
  i_mmu.io.mmu_if <> if_stage.io.mmu_if
  i_mmu.io.lsu_mmu <> exe_stage.io.lsu_mmu
  i_mmu.io.mmu_lsu <> exe_stage.io.mmu_lsu

  i_mmu.io.cmm_mmu <> cmm_stage.io.cmm_mmu




  pc_stage.io.bd_pc <> bd_stage.io.bd_pc
  

  pc_stage.io.pc_pd <> pd_stage.io.pc_pd	//valid when flush for new pc 

  
  pc_stage.io.pc_if <> if_stage.io.pc_if
  if_stage.io.if_iq <> pd_stage.io.if_pd
  pd_stage.io.if_cmm_shadow := if_stage.io.if_cmm
  pd_stage.io.pd_bd <> bd_stage.io.pd_bd



  iss_stage.io.alu_iss_exe <> exe_stage.io.alu_iss_exe
  iss_stage.io.bru_iss_exe <> exe_stage.io.bru_iss_exe
  iss_stage.io.lsu_iss_exe <> exe_stage.io.lsu_iss_exe
  iss_stage.io.csr_iss_exe <> exe_stage.io.csr_iss_exe
  iss_stage.io.mul_iss_exe <> exe_stage.io.mul_iss_exe





  

  bd_stage.io.bru_pd_b <> exe_stage.io.bru_pd_b
  pc_stage.io.bru_pd_j <> exe_stage.io.bru_pd_j

  

  i_mmu.io.if_flush := if_stage.io.flush
  i_mmu.io.lsu_flush := exe_stage.io.flush
  if_stage.io.flush  := cmm_stage.io.is_commit_abort(0) | bd_stage.io.bd_pc.valid | exe_stage.io.bru_pd_j.valid
  // pd_stage.io.flush  := exe_stage.io.icache_fence_req
  bd_stage.io.flush  := cmm_stage.io.is_commit_abort(0) | exe_stage.io.bru_pd_j.valid 
  // id_stage.io.flush  := cmm_stage.io.is_commit_abort(0) 
  dpt_stage.reset := cmm_stage.io.is_commit_abort(0) | reset.asBool
  iss_stage.reset := cmm_stage.io.is_commit_abort(0) | reset.asBool
  exe_stage.io.flush := cmm_stage.io.is_commit_abort(0)

  cmm_stage.io.is_misPredict := bd_stage.io.is_misPredict_taken


  cmm_stage.io.cm_op <> iwb_stage.io.commit
  cmm_stage.io.rod_i <> dpt_stage.io.rod_i
  cmm_stage.io.cmm_lsu <> exe_stage.io.cmm_lsu
  cmm_stage.io.lsu_cmm <> exe_stage.io.lsu_cmm
  cmm_stage.io.cmm_bru_ilp <> exe_stage.io.cmm_bru_ilp
  cmm_stage.io.csr_addr <> exe_stage.io.csr_addr
  cmm_stage.io.csr_data <> exe_stage.io.csr_data
  cmm_stage.io.csr_cmm_op <> exe_stage.io.csr_cmm_op

  cmm_stage.io.cmm_pc <> pc_stage.io.cmm_pc
  cmm_stage.io.if_cmm := if_stage.io.if_cmm
  if_stage.io.ifence := cmm_stage.io.ifence


  cmm_stage.io.rtc_clock := io.rtc_clock
  



  if_stage.io.icache_access.bits := icache_bus.d.bits
  if_stage.io.icache_access.valid := icache_bus.d.valid
  icache_bus.d.ready := if_stage.io.icache_access.ready

  icache_bus.a.valid := if_stage.io.icache_get.valid
  icache_bus.a.bits := if_stage.io.icache_get.bits
  if_stage.io.icache_get.ready := icache_bus.a.ready




  for ( i <- 0 until 8 ) yield {
    exe_stage.io.missUnit_dcache_grant(i).bits  := dcache_bus(i).d.bits
    exe_stage.io.missUnit_dcache_grant(i).valid := dcache_bus(i).d.valid & ( dcache_bus(i).d.bits.opcode === TLMessages.Grant | dcache_bus(i).d.bits.opcode === TLMessages.GrantData )

    exe_stage.io.writeBackUnit_dcache_grant(i).bits  := dcache_bus(i).d.bits
    exe_stage.io.writeBackUnit_dcache_grant(i).valid := dcache_bus(i).d.valid & ( dcache_bus(i).d.bits.opcode === TLMessages.ReleaseAck )

    dcache_bus(i).d.ready := 
      Mux1H(Seq(
        ( dcache_bus(i).d.bits.opcode === TLMessages.Grant || dcache_bus(i).d.bits.opcode === TLMessages.GrantData ) -> exe_stage.io.missUnit_dcache_grant(i).ready,
        ( dcache_bus(i).d.bits.opcode === TLMessages.ReleaseAck )                                                    -> exe_stage.io.writeBackUnit_dcache_grant(i).ready
      ))

    dcache_bus(i).a.valid := exe_stage.io.missUnit_dcache_acquire(i).valid
    dcache_bus(i).a.bits  := exe_stage.io.missUnit_dcache_acquire(i).bits
    exe_stage.io.missUnit_dcache_acquire(i).ready := dcache_bus(i).a.ready

    exe_stage.io.probeUnit_dcache_probe(i).valid := dcache_bus(i).b.valid
    exe_stage.io.probeUnit_dcache_probe(i).bits  := dcache_bus(i).b.bits
    dcache_bus(i).b.ready := exe_stage.io.probeUnit_dcache_probe(i).ready
    
    dcache_bus(i).c.valid := exe_stage.io.writeBackUnit_dcache_release(i).valid
    dcache_bus(i).c.bits  := exe_stage.io.writeBackUnit_dcache_release(i).bits
    exe_stage.io.writeBackUnit_dcache_release(i).ready := dcache_bus(i).c.ready
    
    dcache_bus(i).e.valid := exe_stage.io.missUnit_dcache_grantAck(i).valid
    dcache_bus(i).e.bits  := exe_stage.io.missUnit_dcache_grantAck(i).bits
    exe_stage.io.missUnit_dcache_grantAck(i).ready := dcache_bus(i).e.ready     
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
  i_mmu.io.ptw_access.bits := mmu_bus.d.bits
  i_mmu.io.ptw_access.valid := mmu_bus.d.valid







  diff.io.register := iwb_stage.io.diff_register
  diff.io.commit   := cmm_stage.io.diff_commit
  diff.io.csr      := cmm_stage.io.diff_csr


}




