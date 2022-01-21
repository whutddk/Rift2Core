



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

class Execute(edge: Vec[TLEdgeOut])(implicit p: Parameters) extends RiftModule {
  val io = IO(new Bundle{
    val alu_iss_exe = Flipped(new DecoupledIO(new Alu_iss_info))
    val alu_exe_iwb = new DecoupledIO(new WriteBack_info(64))
    val bru_iss_exe = Flipped(new DecoupledIO(new Bru_iss_info))
    val bru_exe_iwb = new DecoupledIO(new WriteBack_info(64))
    val csr_iss_exe = Flipped(new DecoupledIO(new Csr_iss_info))
    val csr_exe_iwb = new DecoupledIO(new WriteBack_info(64))
    val lsu_iss_exe = Flipped(new DecoupledIO(new Lsu_iss_info))
    val lsu_exe_wb = new DecoupledIO(new WriteBack_info(64))
    val mul_iss_exe = Flipped(new DecoupledIO(new Mul_iss_info))
    val mul_exe_iwb = new DecoupledIO(new WriteBack_info(64))

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
      for ( i <- 0 until 8 ) yield Decoupled(new TLBundleA(edge(i).bundle))
    val missUnit_dcache_grant = 
      for ( i <- 0 until 8 ) yield Flipped(DecoupledIO(new TLBundleD(edge(i).bundle)))
    val missUnit_dcache_grantAck  = 
      for ( i <- 0 until 8 ) yield Decoupled(new TLBundleE(edge(i).bundle))
    val probeUnit_dcache_probe = 
      for ( i <- 0 until 8 ) yield Flipped(DecoupledIO(new TLBundleB(edge(i).bundle)))
    val writeBackUnit_dcache_release =
      for ( i <- 0 until 8 ) yield DecoupledIO(new TLBundleC(edge(i).bundle))
    val writeBackUnit_dcache_grant   =
      for ( i <- 0 until 8 ) yield Flipped(DecoupledIO(new TLBundleD(edge(i).bundle)))

    val system_getPut = new DecoupledIO(new TLBundleA(edge(8).bundle))
    val system_access = Flipped(new DecoupledIO(new TLBundleD(edge(8).bundle)))
    val periph_getPut = new DecoupledIO(new TLBundleA(edge(9+1).bundle))
    val periph_access = Flipped(new DecoupledIO(new TLBundleD(edge(9+1).bundle)))

    val flush = Input(Bool())


  })

  val alu = Module(new Alu)
  val bru = Module(new Bru)
  val lsu = {
    val mdl = Module(new Lsu((edge)))

    mdl.io.lsu_iss_exe <> io.lsu_iss_exe
    mdl.io.lsu_exe_wb <> io.lsu_exe_wb

    mdl.io.lsu_mmu <> io.lsu_mmu
    mdl.io.mmu_lsu <> io.mmu_lsu

    mdl.io.cmm_lsu <> io.cmm_lsu
    mdl.io.lsu_cmm <> io.lsu_cmm
    for ( i <- 0 until 8 ) yield {
      mdl.io.missUnit_dcache_acquire(i) <> io.missUnit_dcache_acquire(i)
      mdl.io.missUnit_dcache_grant(i) <> io.missUnit_dcache_grant(i)
      mdl.io.missUnit_dcache_grantAck(i) <> mdl.io.missUnit_dcache_grantAck(i)
      mdl.io.probeUnit_dcache_probe(i) <> io.probeUnit_dcache_probe(i)
      mdl.io.writeBackUnit_dcache_release(i) <> io.writeBackUnit_dcache_release(i)
      mdl.io.writeBackUnit_dcache_grant(i) <> io.writeBackUnit_dcache_grant(i)
    }

    mdl.io.system_getPut <> io.system_getPut
    mdl.io.system_access <> io.system_access
    mdl.io.periph_getPut <> io.periph_getPut
    mdl.io.periph_access <> io.periph_access

    mdl.io.flush := io.flush
    mdl

  }
  val csr = Module(new Csr)
  val mul = Module(new Mul)























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




