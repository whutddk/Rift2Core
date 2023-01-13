

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






}


class VLSU_Entry_Bundle extends RiftBundle{

  val info        = Vec( vlen/8, new Lsu_iss_info)
  val isRequested = Vec( vlen/8, Bool() )
  val isWroteBack = Vec( vlen/8, Bool() )
  val isException = Vec( vlen/8, Bool() )
  val res         = Vec( vlen/8, UInt(8.W) )          

  def isMasked    = Vec( vlen/8, Bool() )
  def vstart      = UInt((log2Ceil(vlen/8)).W)
  def rd0         = UInt((log2Ceil(vRegNum)).W)
  def nextOp      = 

}

trait VLSUScoreboard{ this: => VLSUBase
  def entry = 16

  val sbBuf = Reg( Vec( entry, new VLSU_Entry_Bundle) )


  val sbISSPtr = RegInit( 0.U((log2Ceil(entry)+1).W) )
  val sbEXEPtr = RegInit( 0.U((log2Ceil(entry)+1).W) )
  val sbVWBPtr = RegInit( 0.U((log2Ceil(entry)+1).W) )

  val isSBEmpty = sbISSPtr === sbEXEPtr
  val isSBFull  = (sbIWBPtr.extract(log2Ceil(entry)) =/= sbISSPtr.extract(log2Ceil(entry))) & (sbIWBPtr(log2Ceil(entry)-1,0) === sbISSPtr.extract(log2Ceil(entry)-1,0))

}

trait VLSUEnq{ this: => VLSUBase
  when( isSBEmpty & ~io.lsu_iss_exe.bits.isVpu ){ //bypass normal lsu request
    lsu.io.lsu_iss_exe <> io.lsu_iss_exe
  } .otherwise{
    io.lsu_iss_exe.ready := ~isSBFull

    when( io.lsu_iss_exe.fire ){
      sbBuf(sbISSPtr).info := Pkg_VLSUSB_Info(io.lsu_iss_exe.bits)
      sbBuf(sbISSPtr).isRequested := 0.U.asTypeof(Vec( vlen/8, Bool() ))
      sbBuf(sbISSPtr).isWroteBack := 0.U.asTypeof(Vec( vlen/8, Bool() ))
      sbBuf(sbISSPtr).isException := 0.U.asTypeof(Vec( vlen/8, Bool() ))
      sbISSPtr := sbISSPtr + 1.U
    }

    when( ~isSBEmpty ){
      def exeIdx 
      lsu.io.lsu_iss_exe.valid := true.B
      lsu.io.lsu_iss_exe.bits := sbBuf(sbEXEPtr).info(exeIdx)

      when( lsu.io.lsu_iss_exe.fire ){
        assert( sbBuf(sbEXEPtr).isRequested(exeIdx) === false.B )
        sbBuf(sbEXEPtr).isRequested(exeIdx) := true.B
        when( PopCount( sbBuf(sbEXEPtr).isRequested.map{~_} ) === 1.U ){

          sbEXEPtr := sbEXEPtr + 1.U
        }
      }
    } .otherwise{
      lsu.io.lsu_iss_exe.valid := false.B
      lsu.io.lsu_iss_exe.bits  := 0.U.asTypeof(new Lsu_iss_info)
    }

  }
}

trait VLSUDeq{ this: => VLSUBase
  io.lsu_exe_fwb <> lsu.io.lsu_exe_fwb

  val bufIdx = lsu.io.lsu_exe_iwb.bits.bufIdx
  val eleIdx = lsu.io.lsu_exe_iwb.bits.eleIdx

  when( bufIdx === entry ){
    io.lsu_exe_iwb <> lsu.io.lsu_exe_iwb
  } .otherwise{
    lsu.io.lsu_exe_iwb.ready := true.B

    when( lsu.io.lsu_exe_iwb.fire ){
      sbBuf(bufIdx).res(eleIdx)         := Pkg_VLSUSB_RES()
      sbBuf(bufIdx).isWroteBack(eleIdx) := true.B
      sbBuf(bufIdx).isException(eleIdx) :=   

      when( PopCount(sbBuf(bufIdx).isWroteBack.map{~_}) === 1.U ){
        io.lsu_exe_vwb.valid := true.B
        io.lsu_exe_vwb.bits  := Pkg_VLSUWB_info()
      }
    }

  }

  when(io.lsu_exe_vwb.fire){
    sbVWBPtr := sbVWBPtr + 1.U
  }


}

class Vector_Request_Bundle extends RiftBundle{
  val res         = Vec( vlen/8, UInt(8.W) )
  val op          = Vec( vlen/8, new Lsu_iss_info)
  val isRequested = Vec( vlen/8, Bool() )
  val isMasked    = Vec( vlen/8, Bool() )
  val vstart      = UInt((log2Ceil(vlen/8)).W)
  val rd0         = UInt((log2Ceil(vRegNum)).W)
}



class Vector_WriteBack_Bundle extends RiftBundle{
  val res         = Vec( vlen/8, UInt(8.W) )
  val isWroteBack = Vec( vlen/8, Bool() )
  val isMasked    = Vec( vlen/8, Bool() )
  val isInvalid   = Vec( vlen/8, Bool() )
  val vstart      = UInt((log2Ceil(8*vlen/8)).W)
  val rd0         = UInt((log2Ceil(vRegNum)).W)
}







trait Rsp{
  when (vle vse vlse vsse ){
    
  }
  when(vlm vsm ) {

  }
  when(vleff){
    
  }
  when( vlxei vsxei){

  }
  when(vlr vsr )
}