



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

package rift2Core.backend


import chisel3._
import chisel3.util._
import rift2Core.define._
import rift2Core.backend._
import rift2Core.backend.fpu._
import rift2Core.backend.lsu._
import rift2Core.privilege._
import rift2Chip._
import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._


class Execute(edge: Seq[TLEdgeOut])(implicit p: Parameters) extends RiftModule {


  val io = IO(new Bundle{
    val alu_iss_exe = Vec(aluNum, Flipped(new DecoupledIO(new Alu_iss_info)))
    val alu_exe_iwb = Vec(aluNum, new DecoupledIO(new WriteBack_info(dw=64)))
    val bru_iss_exe = Flipped(new DecoupledIO(new Bru_iss_info))
    val bru_exe_iwb = new DecoupledIO(new WriteBack_info(dw=64))
    val csr_iss_exe = Flipped(new DecoupledIO(new Csr_iss_info))
    val csr_exe_iwb = new DecoupledIO(new WriteBack_info(dw=64))
    val lsu_iss_exe = Flipped(new DecoupledIO(new Lsu_iss_info))
    val lsu_exe_iwb = new DecoupledIO(new WriteBack_info(dw=64))
    val lsu_exe_fwb = new DecoupledIO(new WriteBack_info(dw=65))
    val mul_iss_exe = Vec(mulNum max 1, Flipped(new DecoupledIO(new Mul_iss_info)))
    val mul_exe_iwb = Vec(mulNum max 1, new DecoupledIO(new WriteBack_info(dw=64)))
    val fpu_iss_exe = Vec(fpuNum max 1, Flipped(new DecoupledIO(new Fpu_iss_info)))
    val fpu_exe_iwb = Vec(fpuNum max 1, new DecoupledIO(new WriteBack_info(dw=64)))
    val fpu_exe_fwb = Vec(fpuNum max 1, new DecoupledIO(new WriteBack_info(dw=65)))
    val fcsr = Input(UInt(24.W))
    val fcsr_cmm_op = Vec(cm_chn, DecoupledIO( new Exe_Port ))

    val bftq = Flipped(Decoupled(new Branch_FTarget_Bundle))
    val jftq = Flipped(Decoupled(new Jump_FTarget_Bundle))

    val bctq = Decoupled(new Branch_CTarget_Bundle)
    val jctq = Decoupled(new Jump_CTarget_Bundle)

    val bcmm_update = Valid(new Branch_CTarget_Bundle)
    val jcmm_update = Valid(new Jump_CTarget_Bundle)

    val csr_addr = ValidIO(UInt(12.W))
    val csr_data = Flipped(ValidIO(UInt(64.W)))
    val csr_cmm_op = DecoupledIO( new Exe_Port ) 

    val lsu_mmu = DecoupledIO(new Info_mmu_req)
    val mmu_lsu = Flipped(DecoupledIO(new Info_mmu_rsp))
    val cmm_lsu = Input(new Info_cmm_lsu)
    val lsu_cmm = Output( new Info_lsu_cmm )

    val missUnit_dcache_acquire  = if( hasL2 ) Some(new DecoupledIO(new TLBundleA(edge(0).bundle)))               else {None}
    val missUnit_dcache_grant    = if( hasL2 ) Some(Flipped(new DecoupledIO(new TLBundleD(edge(0).bundle))))      else {None}
    val missUnit_dcache_grantAck = if( hasL2 ) Some(DecoupledIO(new TLBundleE(edge(0).bundle)))                   else {None}
    val probeUnit_dcache_probe   = if( hasL2 )     Some(Flipped(new DecoupledIO(new TLBundleB(edge(0).bundle))))  else {None}
    val writeBackUnit_dcache_release = if( hasL2 ) Some(new DecoupledIO(new TLBundleC(edge(0).bundle))         )  else {None}
    val writeBackUnit_dcache_grant   = if( hasL2 ) Some(Flipped(new DecoupledIO(new TLBundleD(edge(0).bundle))))  else {None}

    val dcache_getPut = if ( hasL2 ) { None } else { Some(        new DecoupledIO(new TLBundleA(edge(0).bundle)) ) }
    val dcache_access = if ( hasL2 ) { None } else { Some(Flipped(new DecoupledIO(new TLBundleD(edge(0).bundle)))) }

    val system_getPut = new DecoupledIO(new TLBundleA(edge(1).bundle))
    val system_access = Flipped(new DecoupledIO(new TLBundleD(edge(1).bundle)))
    val periph_getPut = new DecoupledIO(new TLBundleA(edge(2).bundle))
    val periph_access = Flipped(new DecoupledIO(new TLBundleD(edge(2).bundle)))

    val preFetch = ValidIO( new PreFetch_Req_Bundle )

    val flush = Input(Bool())


  })

  val alu = for( i <- 0 until aluNum ) yield Module(new Alu)
  val bru = Module(new Bru)
  val lsu = {
    val mdl = Module(new Lsu((edge)))

    mdl.io.lsu_iss_exe <> io.lsu_iss_exe
    mdl.io.lsu_exe_iwb <> io.lsu_exe_iwb
    mdl.io.lsu_exe_fwb <> io.lsu_exe_fwb

    mdl.io.lsu_mmu <> io.lsu_mmu
    mdl.io.mmu_lsu <> io.mmu_lsu

    mdl.io.cmm_lsu <> io.cmm_lsu
    mdl.io.lsu_cmm <> io.lsu_cmm

    if ( hasL2 ) {
    io.missUnit_dcache_acquire.get.valid := mdl.io.missUnit_dcache_acquire.get.valid
    io.missUnit_dcache_acquire.get.bits  := mdl.io.missUnit_dcache_acquire.get.bits
    mdl.io.missUnit_dcache_acquire.get.ready := io.missUnit_dcache_acquire.get.ready

    mdl.io.missUnit_dcache_grant.get.valid := io.missUnit_dcache_grant.get.valid
    mdl.io.missUnit_dcache_grant.get.bits  := io.missUnit_dcache_grant.get.bits
    io.missUnit_dcache_grant.get.ready := mdl.io.missUnit_dcache_grant.get.ready

    io.missUnit_dcache_grantAck.get.valid := mdl.io.missUnit_dcache_grantAck.get.valid
    io.missUnit_dcache_grantAck.get.bits := mdl.io.missUnit_dcache_grantAck.get.bits
    mdl.io.missUnit_dcache_grantAck.get.ready := io.missUnit_dcache_grantAck.get.ready

    mdl.io.probeUnit_dcache_probe.get.valid := io.probeUnit_dcache_probe.get.valid
    mdl.io.probeUnit_dcache_probe.get.bits := io.probeUnit_dcache_probe.get.bits
    io.probeUnit_dcache_probe.get.ready := mdl.io.probeUnit_dcache_probe.get.ready

    io.writeBackUnit_dcache_release.get.valid := mdl.io.writeBackUnit_dcache_release.get.valid
    io.writeBackUnit_dcache_release.get.bits := mdl.io.writeBackUnit_dcache_release.get.bits
    mdl.io.writeBackUnit_dcache_release.get.ready := io.writeBackUnit_dcache_release.get.ready

    mdl.io.writeBackUnit_dcache_grant.get.valid := io.writeBackUnit_dcache_grant.get.valid
    mdl.io.writeBackUnit_dcache_grant.get.bits := io.writeBackUnit_dcache_grant.get.bits
    io.writeBackUnit_dcache_grant.get.ready := mdl.io.writeBackUnit_dcache_grant.get.ready      
    } else {
      io.dcache_getPut.get.valid := mdl.io.dcache_getPut.get.valid
      io.dcache_getPut.get.bits  := mdl.io.dcache_getPut.get.bits
      mdl.io.dcache_getPut.get.ready := io.dcache_getPut.get.ready

      mdl.io.dcache_access.get.valid := io.dcache_access.get.valid
      mdl.io.dcache_access.get.bits  := io.dcache_access.get.bits
      io.dcache_access.get.ready := mdl.io.dcache_access.get.ready
    }
   


    io.system_getPut.valid := mdl.io.system_getPut.valid
    io.system_getPut.bits := mdl.io.system_getPut.bits
    mdl.io.system_getPut.ready := io.system_getPut.ready
    mdl.io.system_access.valid := io.system_access.valid
    mdl.io.system_access.bits := io.system_access.bits
    io.system_access.ready := mdl.io.system_access.ready

    io.periph_getPut.valid := mdl.io.periph_getPut.valid
    io.periph_getPut.bits := mdl.io.periph_getPut.bits
    mdl.io.periph_getPut.ready := io.periph_getPut.ready
    mdl.io.periph_access.valid := io.periph_access.valid
    mdl.io.periph_access.bits := io.periph_access.bits
    io.periph_access.ready := mdl.io.periph_access.ready

    io.preFetch := mdl.io.preFetch

    mdl.io.flush := io.flush
    mdl

  }
  val csr = Module(new Csr)
  val mulDiv = (if( mulNum > 0 ) { for ( i <- 0 until mulNum ) yield Module(new MulDiv) } else { Seq(Module(new FakeMulDiv)) })

  val fpu = if( fpuNum > 0 ) { for( i <- 0 until fpuNum ) yield Module(new FAlu()) } else { Seq(Module(new FakeFAlu())) }


  for( i <- 0 until (fpuNum max 1) ) {
    fpu(i).io.fpu_iss_exe <> io.fpu_iss_exe(i)
    fpu(i).io.fpu_exe_iwb <> io.fpu_exe_iwb(i)
    fpu(i).io.fpu_exe_fwb <> io.fpu_exe_fwb(i)
    fpu(i).io.flush := io.flush
    fpu(i).io.fcsr  := io.fcsr
    fpu(i).io.fcsr_cmm_op <> io.fcsr_cmm_op; require( fpuNum <= 1 )
  }

  for( i <- 0 until aluNum ) {
    alu(i).io.alu_iss_exe <> io.alu_iss_exe(i)
    alu(i).io.alu_exe_iwb <> io.alu_exe_iwb(i)
    alu(i).io.flush <> io.flush    
  }


  bru.io.bru_iss_exe <> io.bru_iss_exe
  bru.io.bru_exe_iwb <> io.bru_exe_iwb
  bru.io.flush <> io.flush

  bru.io.bftq <> io.bftq
  bru.io.jftq <> io.jftq

  io.bctq <> bru.io.bctq
  io.jctq <> bru.io.jctq

  io.bcmm_update := bru.io.bcmm_update
  io.jcmm_update := bru.io.jcmm_update

  csr.io.csr_iss_exe <> io.csr_iss_exe
  csr.io.csr_exe_iwb <> io.csr_exe_iwb
  csr.io.csr_addr <> io.csr_addr
  csr.io.csr_data <> io.csr_data
  csr.io.csr_cmm_op <> io.csr_cmm_op
  csr.io.flush <> io.flush

  for( i <- 0 until (mulNum max 1) ) {
    mulDiv(i).io.mul_iss_exe <> io.mul_iss_exe(i)
    mulDiv(i).io.mul_exe_iwb <> io.mul_exe_iwb(i)
    mulDiv(i).io.flush := io.flush    
  }




}




