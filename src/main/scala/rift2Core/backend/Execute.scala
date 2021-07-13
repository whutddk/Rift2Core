


/*
* @Author: Ruige Lee
* @Date:   2021-03-29 14:34:17
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-03-29 14:34:23
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

package rift2Core.backend


import chisel3._
import chisel3.util._
import rift2Core.define._
import rift2Core.backend._
import rift2Core.privilege.csrFiles._
import tilelink._
import axi._
import rift._
import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

class Execute(tlc_edge: TLEdgeOut)(implicit p: Parameters) extends RiftModule {
  val io = IO(new Bundle{
    val alu_iss_exe = Flipped(new DecoupledIO(new Alu_iss_info))
    val alu_exe_iwb = new DecoupledIO(new Exe_iwb_info)
    val bru_iss_exe = Flipped(new DecoupledIO(new Bru_iss_info))
    val bru_exe_iwb = new DecoupledIO(new Exe_iwb_info)
    val csr_iss_exe = Flipped(new DecoupledIO(new Csr_iss_info))
    val csr_exe_iwb = new DecoupledIO(new Exe_iwb_info)
    val lsu_iss_exe = Flipped(new DecoupledIO(new Lsu_iss_info))
    val lsu_exe_iwb = new DecoupledIO(new Exe_iwb_info)
    val mul_iss_exe = Flipped(new DecoupledIO(new Mul_iss_info))
    val mul_exe_iwb = new DecoupledIO(new Exe_iwb_info)

    val cmm_bru_ilp = Input(Bool())
    val bru_pd_b = new ValidIO( Bool() )
    val bru_pd_j = new ValidIO( UInt(64.W) )

    val csr_addr = Output(UInt(12.W))
    val csr_data = Input(UInt(64.W))
    val csr_cmm_op = DecoupledIO( new Exe_Port ) 



    val cmm_lsu = Input(new Info_cmm_lsu)
    val lsu_cmm = Output( new Info_lsu_cmm )

    val icache_fence_req = Output(Bool())

    val missUnit_dcache_acquire = Decoupled(new TLBundleA(tlc_edge.bundle))
    val missUnit_dcache_grant   = Flipped(DecoupledIO(new TLBundleD(tlc_edge.bundle)))
    val missUnit_dcache_grantAck  = Decoupled(new TLBundleE(tlc_edge.bundle))
    val probeUnit_dcache_probe = Flipped(DecoupledIO(new TLBundleB(tlc_edge.bundle)))
    val writeBackUnit_dcache_release = DecoupledIO(new TLBundleC(tlc_edge.bundle))
    val writeBackUnit_dcache_grant   = Flipped(DecoupledIO(new TLBundleD(tlc_edge.bundle)))

    val sys_chn_ar = new DecoupledIO(new AXI_chn_a( 32, 1, 1 ))
    val sys_chn_r = Flipped( new DecoupledIO(new AXI_chn_r( 64, 1, 1)) )
    val sys_chn_aw = new DecoupledIO(new AXI_chn_a( 32, 1, 1 ))
    val sys_chn_w = new DecoupledIO(new AXI_chn_w( 64, 1 )) 
    val sys_chn_b = Flipped( new DecoupledIO(new AXI_chn_b( 1, 1 )))

    val flush = Input(Bool())


  })

  val alu = Module(new Alu)
  val bru = Module(new Bru)
  val lsu = Module(new Lsu((tlc_edge)))
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


  lsu.io.lsu_iss_exe <> io.lsu_iss_exe
  lsu.io.lsu_exe_iwb <> io.lsu_exe_iwb

  io.missUnit_dcache_acquire      <> lsu.io.missUnit_dcache_acquire
  lsu.io.missUnit_dcache_grant <> io.missUnit_dcache_grant
  io.missUnit_dcache_grantAck     <> lsu.io.missUnit_dcache_grantAck
  lsu.io.probeUnit_dcache_probe <> io.probeUnit_dcache_probe
  io.writeBackUnit_dcache_release <> lsu.io.writeBackUnit_dcache_release
  lsu.io.writeBackUnit_dcache_grant <> io.writeBackUnit_dcache_grant

  lsu.io.sys_chn_ar <> io.sys_chn_ar
  lsu.io.sys_chn_r  <> io.sys_chn_r
  lsu.io.sys_chn_aw <> io.sys_chn_aw
  lsu.io.sys_chn_w  <> io.sys_chn_w
  lsu.io.sys_chn_b  <> io.sys_chn_b




  lsu.io.cmm_lsu <> io.cmm_lsu
  lsu.io.lsu_cmm <> io.lsu_cmm

  lsu.io.icache_fence_req <> io.icache_fence_req

  lsu.io.flush <> io.flush

  mul.io.mul_iss_exe <> io.mul_iss_exe
  mul.io.mul_exe_iwb <> io.mul_exe_iwb
  mul.io.flush <> io.flush



}




