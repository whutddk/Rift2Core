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
import rift2Core.cache._
import tilelink._
import axi._


class Rift2Core extends Module {
  val io = IO(new Bundle{
    val il1_chn_a = new DecoupledIO(new TLchannel_a(128, 32))
    val il1_chn_d = Flipped(new DecoupledIO( new TLchannel_d(128) ))

    val dl1_chn_a = new DecoupledIO(new TLchannel_a(128, 32))
    val dl1_chn_d = Flipped(new DecoupledIO( new TLchannel_d(128) ))

    val l2c_fence_req = Output(Bool())
    val l3c_fence_req = Output(Bool())

    val sys_chn_ar = new DecoupledIO(new AXI_chn_a( 32, 1, 1 ))
    val sys_chn_r = Flipped( new DecoupledIO(new AXI_chn_r( 64, 1, 1)) )
    val sys_chn_aw = new DecoupledIO(new AXI_chn_a( 32, 1, 1 ))
    val sys_chn_w = new DecoupledIO(new AXI_chn_w( 64, 1 )) 
    val sys_chn_b = Flipped( new DecoupledIO(new AXI_chn_b( 1, 1 )))

    val rtc_clock = Input(Bool())
  })

  lazy val pc_stage = Module(new Pc_gen)
  lazy val if_stage = Module(new Ifetch)
  lazy val pd_stage = Module(new Predecode_ss)
  lazy val bd_stage = Module(new BP_ID_ss)
  // lazy val id_stage = Module(new BranchPredict_ss)

  lazy val dpt_stage = Module(new Dispatch_ss)
  lazy val iss_stage = Module(new Issue)
  lazy val exe_stage = Module(new Execute)
  lazy val iwb_stage = Module(new WriteBack)
  lazy val cmm_stage = Module(new Commit)

  lazy val i_regfiles = Module(new Regfiles)

///////////////////////////////////////////////////////////////////////////







  io.l2c_fence_req := exe_stage.io.l2c_fence_req
  io.l3c_fence_req := exe_stage.io.l3c_fence_req



////////////////////////////////////////////////////////////////////////////




  if_stage.io.il1_chn_a <> io.il1_chn_a
  if_stage.io.il1_chn_d <> io.il1_chn_d

  exe_stage.io.dl1_chn_a <> io.dl1_chn_a
  exe_stage.io.dl1_chn_d <> io.dl1_chn_d
  exe_stage.io.sys_chn_ar <> io.sys_chn_ar
  exe_stage.io.sys_chn_r  <> io.sys_chn_r
  exe_stage.io.sys_chn_aw <> io.sys_chn_aw
  exe_stage.io.sys_chn_w  <> io.sys_chn_w
  exe_stage.io.sys_chn_b  <> io.sys_chn_b

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


  if_stage.io.is_il1_fence_req := exe_stage.io.il1_fence_req

  

  bd_stage.io.bru_pd_b <> exe_stage.io.bru_pd_b
  pc_stage.io.bru_pd_j <> exe_stage.io.bru_pd_j

  


  if_stage.io.flush  := cmm_stage.io.is_commit_abort(0) | cmm_stage.io.is_commit_abort(1) | bd_stage.io.bd_pc.valid | exe_stage.io.bru_pd_j.valid
  pd_stage.io.flush  := exe_stage.io.il1_fence_req
  bd_stage.io.flush  := cmm_stage.io.is_commit_abort(0) | cmm_stage.io.is_commit_abort(1) | exe_stage.io.bru_pd_j.valid | exe_stage.io.il1_fence_req
  // id_stage.io.flush  := cmm_stage.io.is_commit_abort(0) | cmm_stage.io.is_commit_abort(1)
  dpt_stage.io.flush := cmm_stage.io.is_commit_abort(0) | cmm_stage.io.is_commit_abort(1)
  iss_stage.io.flush := cmm_stage.io.is_commit_abort(0) | cmm_stage.io.is_commit_abort(1)
  exe_stage.io.flush := cmm_stage.io.is_commit_abort(0) | cmm_stage.io.is_commit_abort(1)
  i_regfiles.io.flush := cmm_stage.io.is_commit_abort(0) | cmm_stage.io.is_commit_abort(1)

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
  
}




