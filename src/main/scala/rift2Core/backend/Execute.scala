



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
import rift2Core.backend.memory._
import rift2Core.privilege.csrFiles._
import rift2Core.privilege._
import rift._
import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

class Execute(edge: Seq[TLEdgeOut])(implicit p: Parameters) extends RiftModule {
  val io = IO(new Bundle{
    val alu_iss_exe = Flipped(new DecoupledIO(new Alu_iss_info))
    val alu_exe_iwb = new DecoupledIO(new WriteBack_info(64))
    val bru_iss_exe = Flipped(new DecoupledIO(new Bru_iss_info))
    val bru_exe_iwb = new DecoupledIO(new WriteBack_info(64))
    val csr_iss_exe = Flipped(new DecoupledIO(new Csr_iss_info))
    val csr_exe_iwb = new DecoupledIO(new WriteBack_info(64))
    val lsu_iss_exe = Flipped(new DecoupledIO(new Lsu_iss_info))
    val lsu_exe_iwb = new DecoupledIO(new WriteBack_info(64))
    val lsu_exe_fwb = new DecoupledIO(new WriteBack_info(64))
    val mul_iss_exe = Flipped(new DecoupledIO(new Mul_iss_info))
    val mul_exe_iwb = new DecoupledIO(new WriteBack_info(64))
    val fpu_iss_exe = Flipped(new DecoupledIO(new Fpu_iss_info))
    val fpu_exe_iwb = new DecoupledIO(new WriteBack_info(64))
    val fpu_exe_fwb = new DecoupledIO(new WriteBack_info(64))


    val cmm_bru_ilp = Input(Bool())
    val bru_pd_b = new ValidIO( Bool() )
    val bru_pd_j = new ValidIO( UInt(64.W) )

    val csr_addr = ValidIO(UInt(12.W))
    val csr_data = Flipped(ValidIO(UInt(64.W)))
    val csr_cmm_op = DecoupledIO( new Exe_Port ) 

    val lsu_mmu = DecoupledIO(new Info_mmu_req)
    val mmu_lsu = Flipped(DecoupledIO(new Info_mmu_rsp))
    val cmm_lsu = Input(new Info_cmm_lsu)
    val lsu_cmm = Output( new Info_lsu_cmm )

    val missUnit_dcache_acquire = 
      MixedVec(  for ( i <- 0 until 8 ) yield  DecoupledIO(new TLBundleA(edge(i).bundle))) 
    val missUnit_dcache_grant = 
      MixedVec(  for ( i <- 0 until 8 ) yield  Flipped(DecoupledIO(new TLBundleD(edge(i).bundle))))
    val missUnit_dcache_grantAck  = 
      MixedVec(  for ( i <- 0 until 8 ) yield  DecoupledIO(new TLBundleE(edge(i).bundle)))
    val probeUnit_dcache_probe = 
      MixedVec(  for ( i <- 0 until 8 ) yield  Flipped(DecoupledIO(new TLBundleB(edge(i).bundle))))
    val writeBackUnit_dcache_release =
      MixedVec(  for ( i <- 0 until 8 ) yield  DecoupledIO(new TLBundleC(edge(i).bundle)))
    val writeBackUnit_dcache_grant   =
      MixedVec(  for ( i <- 0 until 8 ) yield  Flipped(DecoupledIO(new TLBundleD(edge(i).bundle))))

    val system_getPut = new DecoupledIO(new TLBundleA(edge(8).bundle))
    val system_access = Flipped(new DecoupledIO(new TLBundleD(edge(8).bundle)))
    val periph_getPut = new DecoupledIO(new TLBundleA(edge(9).bundle))
    val periph_access = Flipped(new DecoupledIO(new TLBundleD(edge(9).bundle)))

    val flush = Input(Bool())


  })

  val alu = Module(new Alu)
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
    for ( i <- 0 until 8 ) yield {
      io.missUnit_dcache_acquire(i).valid := mdl.io.missUnit_dcache_acquire(i).valid
      io.missUnit_dcache_acquire(i).bits := mdl.io.missUnit_dcache_acquire(i).bits
      mdl.io.missUnit_dcache_acquire(i).ready := io.missUnit_dcache_acquire(i).ready

      mdl.io.missUnit_dcache_grant(i).valid := io.missUnit_dcache_grant(i).valid
      mdl.io.missUnit_dcache_grant(i).bits  := io.missUnit_dcache_grant(i).bits
      io.missUnit_dcache_grant(i).ready := mdl.io.missUnit_dcache_grant(i).ready

      io.missUnit_dcache_grantAck(i).valid := mdl.io.missUnit_dcache_grantAck(i).valid
      io.missUnit_dcache_grantAck(i).bits := mdl.io.missUnit_dcache_grantAck(i).bits
      mdl.io.missUnit_dcache_grantAck(i).ready := io.missUnit_dcache_grantAck(i).ready

      mdl.io.probeUnit_dcache_probe(i).valid := io.probeUnit_dcache_probe(i).valid
      mdl.io.probeUnit_dcache_probe(i).bits := io.probeUnit_dcache_probe(i).bits
      io.probeUnit_dcache_probe(i).ready := mdl.io.probeUnit_dcache_probe(i).ready

      io.writeBackUnit_dcache_release(i).valid := mdl.io.writeBackUnit_dcache_release(i).valid
      io.writeBackUnit_dcache_release(i).bits := mdl.io.writeBackUnit_dcache_release(i).bits
      mdl.io.writeBackUnit_dcache_release(i).ready := io.writeBackUnit_dcache_release(i).ready

      mdl.io.writeBackUnit_dcache_grant(i).valid := io.writeBackUnit_dcache_grant(i).valid
      mdl.io.writeBackUnit_dcache_grant(i).bits := io.writeBackUnit_dcache_grant(i).bits
      io.writeBackUnit_dcache_grant(i).ready := mdl.io.writeBackUnit_dcache_grant(i).ready
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



    mdl.io.flush := io.flush
    mdl

  }
  val csr = Module(new Csr)
  val mul = Module(new Mul)

  val fpu = {
    val mdl = Module(new FAlu)

    mdl.io.fpu_iss_exe <> io.fpu_iss_exe
    mdl.io.fpu_exe_iwb <> io.fpu_exe_iwb
    mdl.io.fpu_exe_fwb <> io.fpu_exe_fwb
    mdl.io.flush := io.flush

    mdl
  }





















  alu.io.alu_iss_exe <> io.alu_iss_exe
  alu.io.alu_exe_iwb <> io.alu_exe_iwb
  alu.io.flush <> io.flush

  bru.io.bru_iss_exe <> io.bru_iss_exe
  bru.io.bru_exe_iwb <> io.bru_exe_iwb
  bru.io.cmm_bru_ilp <> io.cmm_bru_ilp
  bru.io.bru_pd_b <> io.bru_pd_b
  bru.io.bru_pd_j <> io.bru_pd_j
  bru.io.flush <> io.flush

  csr.io.csr_iss_exe <> io.csr_iss_exe
  csr.io.csr_exe_iwb <> io.csr_exe_iwb
  csr.io.csr_addr <> io.csr_addr
  csr.io.csr_data <> io.csr_data
  csr.io.csr_cmm_op <> io.csr_cmm_op
  csr.io.flush <> io.flush

  mul.io.mul_iss_exe <> io.mul_iss_exe
  mul.io.mul_exe_iwb <> io.mul_exe_iwb
  mul.io.flush <> io.flush



}




