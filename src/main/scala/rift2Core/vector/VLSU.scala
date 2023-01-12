

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

package rift2Core.backend.lsu

import chisel3._
import chisel3.util._

import rift2Core.define._
import rift2Chip._

import rift2Core.privilege._
import rift2Core.backend._

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.tilelink._



abstract class VLSUBase (val edge: Seq[TLEdgeOut])(implicit p: Parameters) extends RiftModule {
  require( hasVector == true, "Vector is not supported in your configuration!" )
  val io = IO(new Bundle{
    val lsu_iss_exe = Flipped(new DecoupledIO(new Lsu_iss_info))
    val lsu_exe_iwb = new DecoupledIO(new WriteBack_info(dw=64))
    val lsu_exe_fwb = new DecoupledIO(new WriteBack_info(dw=65))
    val lsu_exe_vwb = new DecoupledIO(new )

    val cmm_lsu = Input(new Info_cmm_lsu)
    val lsu_cmm = Output( new Info_lsu_cmm )

    val lsu_mmu = DecoupledIO(new Info_mmu_req)
    val mmu_lsu = Flipped(DecoupledIO(new Info_mmu_rsp))


    val missUnit_dcache_acquire      = if( hasL2 ) Some(new DecoupledIO(new TLBundleA(edge(0).bundle)))          else {None}
    val missUnit_dcache_grant        = if( hasL2 ) Some(Flipped(new DecoupledIO(new TLBundleD(edge(0).bundle)))) else {None}
    val missUnit_dcache_grantAck     = if( hasL2 ) Some(DecoupledIO(new TLBundleE(edge(0).bundle)))              else {None}
    val probeUnit_dcache_probe       = if( hasL2 ) Some(Flipped(new DecoupledIO(new TLBundleB(edge(0).bundle)))) else {None}
    val writeBackUnit_dcache_release = if( hasL2 ) Some(new DecoupledIO(new TLBundleC(edge(0).bundle))         ) else {None}
    val writeBackUnit_dcache_grant   = if( hasL2 ) Some(Flipped(new DecoupledIO(new TLBundleD(edge(0).bundle)))) else {None}

    val dcache_getPut = if ( hasL2 ) { None } else { Some(        new DecoupledIO(new TLBundleA(edge(0).bundle)) ) }
    val dcache_access = if ( hasL2 ) { None } else { Some(Flipped(new DecoupledIO(new TLBundleD(edge(0).bundle)))) }


    val system_getPut = new DecoupledIO(new TLBundleA(edge(1).bundle))
    val system_access = Flipped(new DecoupledIO(new TLBundleD(edge(1).bundle)))

    val periph_getPut = new DecoupledIO(new TLBundleA(edge(2).bundle))
    val periph_access = Flipped(new DecoupledIO(new TLBundleD(edge(2).bundle)))

    val preFetch = ValidIO( new PreFetch_Req_Bundle )

    val flush = Input(Bool())
  })


  val lsu = Module(new Lsu((edge)))


  io.cmm_lsu <> lsu.io.cmm_lsu
  io.lsu_cmm <> lsu.io.lsu_cmm
  io.lsu_mmu <> lsu.io.lsu_mmu
  io.mmu_lsu <> lsu.io.mmu_lsu

  if( hasL2 ) { io.missUnit_dcache_acquire.get      <> lsu.io.missUnit_dcache_acquire.get }
  if( hasL2 ) { io.missUnit_dcache_grant.get        <> lsu.io.missUnit_dcache_grant.get }
  if( hasL2 ) { io.missUnit_dcache_grantAck.get     <> lsu.io.missUnit_dcache_grantAck.get }
  if( hasL2 ) { io.probeUnit_dcache_probe.get       <> lsu.io.probeUnit_dcache_probe.get }
  if( hasL2 ) { io.writeBackUnit_dcache_release.get <> lsu.io.writeBackUnit_dcache_release.get }
  if( hasL2 ) { io.writeBackUnit_dcache_grant.get   <> lsu.io.writeBackUnit_dcache_grant.get }

  if( !hasL2 ) { io.dcache_getPut.get <> lsu.io.dcache_getPut.get }
  if( !hasL2 ) { io.dcache_access.get <> lsu.io.dcache_access.get }

  io.system_getPut <> lsu.io.system_getPut
  io.system_access <> lsu.io.system_access
  io.periph_getPut <> lsu.io.periph_getPut
  io.periph_access <> lsu.io.periph_access

  io.preFetch <> lsu.io.preFetch
  lsu.io.flush := io.flush



    val lsu_iss_exe = Flipped(new DecoupledIO(new Lsu_iss_info))
    val lsu_exe_iwb = new DecoupledIO(new WriteBack_info(dw=64))
    val lsu_exe_fwb = new DecoupledIO(new WriteBack_info(dw=65))


}

trait VLSUScoreboard{ this: => VLSUBase
  val VReqBuf = Vec ( Vec (), Lsu_iss_info)
  val VRspBuf = Vec ( Vec (), Lsu_iss_info)
}



  val        = Bool()
  val       = Bool()
  val vluxei    = Bool()
  val vloxei    = Bool()
  val     = Bool()

  val vlNreN    = Bool()

  val        = Bool()

  val       = Bool()
  val vsuxei    = Bool()
  val vsoxei    = Bool()
  val vsNr      = Bool()
}

trait Rsp{
  when (vle vse vlse vsse ){
    
  }
  when(vlm vsm ) {

  }
  when(vleff){
    
  }
}