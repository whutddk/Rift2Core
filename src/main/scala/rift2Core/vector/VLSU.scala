

/*
  Copyright (c) 2020 - 2023 Wuhan University of Technology <295054118@whut.edu.cn>

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

import org.chipsalliance.cde.config._
import freechips.rocketchip.tilelink._
import chisel3.experimental.dataview._

class VLSU_Entry_Bundle(implicit p: Parameters) extends RiftBundle{

  val originInfo     = new Lsu_iss_info
  val splitInfo      = Vec( vParams.vlen/8, new Lsu_iss_info)
  val isRequested = Vec( vParams.vlen/8, Bool() )
  val isWroteBack = Vec( vParams.vlen/8, Bool() )
  val isException = Vec( vParams.vlen/8, Bool() )
  val exceptionInfo = UInt(64.W)
  val res         = UInt(vParams.vlen.W)
  val isExeReady  = Bool()

}


class Vector_WriteBack_Bundle(implicit p: Parameters) extends RiftBundle{
  val res         = Vec( vParams.vlen/8, UInt(8.W) )
  val isWroteBack = Vec( vParams.vlen/8, Bool() )
  val isMasked    = Vec( vParams.vlen/8, Bool() )
  val isInvalid   = Vec( vParams.vlen/8, Bool() )
  val vstart      = UInt((log2Ceil(8*vParams.vlen/8)).W)
  val rd0         = UInt((log2Ceil(vRegNum)).W)
}

abstract class VLSUBase (val edge: Seq[TLEdgeOut])(implicit p: Parameters) extends RiftModule{

  class VLSUIO extends Bundle{
    val lsu_iss_exe = Flipped(new DecoupledIO(new Lsu_iss_info))
    val lsu_exe_iwb = new DecoupledIO(new WriteBack_info(dw=64))
    val lsu_exe_fwb = new DecoupledIO(new WriteBack_info(dw=65))
    val lsu_exe_vwb = new DecoupledIO(new WriteBack_info(dw=vParams.vlen))

    val lsu_cWriteBack = Valid(new SeqReg_WriteBack_Bundle(64, cRegNum))

    val cmm_lsu = Input(new Info_cmm_lsu)
    // val lsu_cmm = Output( new Info_lsu_cmm )

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
  }
  
  val io: VLSUIO = IO(new VLSUIO)


  val lsu = Module(new Lsu((edge)))


  io.cmm_lsu <> lsu.io.cmm_lsu
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




abstract class VLSUScoreboard(edge: Seq[TLEdgeOut])(implicit p: Parameters) extends VLSUBase(edge){


  val sbBuf = Reg( Vec( vParams.lsuEntry, new VLSU_Entry_Bundle) )


  val sbISSPtr = RegInit( 0.U((log2Ceil(vParams.lsuEntry)+1).W) )
  val sbEXEPtr = RegInit( 0.U((log2Ceil(vParams.lsuEntry)+1).W) )
  val sbVWBPtr = RegInit( 0.U((log2Ceil(vParams.lsuEntry)+1).W) )

  val isSBEmpty = sbISSPtr === sbEXEPtr
  val isSBFull  =
    (sbVWBPtr.extract(log2Ceil(vParams.lsuEntry)) =/= sbISSPtr.extract(log2Ceil(vParams.lsuEntry))) &
    (sbVWBPtr(log2Ceil(vParams.lsuEntry)-1,0)     === sbISSPtr(log2Ceil(vParams.lsuEntry)-1,0))

  def Pkg_VlsuScoreBoard_update(ori: Lsu_iss_info, bufIdx: UInt): VLSU_Entry_Bundle = {
    require(hasVector)


    val sbInfo = Wire( new VLSU_Entry_Bundle )

    val mask8  = Wire( UInt((vParams.vlen/8 ).W))
    val mask16 = Wire( UInt((vParams.vlen/16).W))
    val mask32 = Wire( UInt((vParams.vlen/32).W))
    val mask64 = Wire( UInt((vParams.vlen/64).W))

    val bodyTotal = Wire( Vec( log2Ceil(vParams.vlmax), Bool()) )
    val body8  = Wire( UInt((vParams.vlen/8 ).W))
    val body16 = Wire( UInt((vParams.vlen/16).W))
    val body32 = Wire( UInt((vParams.vlen/32).W))
    val body64 = Wire( UInt((vParams.vlen/64).W))

    val preStartTotal = Wire( Vec( log2Ceil(vParams.vlmax), Bool()) )
    val preStart8  = Wire( UInt((vParams.vlen/8 ).W))
    val preStart16 = Wire( UInt((vParams.vlen/16).W))
    val preStart32 = Wire( UInt((vParams.vlen/32).W))
    val preStart64 = Wire( UInt((vParams.vlen/64).W))

    val tailTotal = Wire( Vec( log2Ceil(vParams.vlmax), Bool()) )
    val tail8  = Wire( UInt((vParams.vlen/8 ).W))
    val tail16 = Wire( UInt((vParams.vlen/16).W))
    val tail32 = Wire( UInt((vParams.vlen/32).W))
    val tail64 = Wire( UInt((vParams.vlen/64).W))

    val resInit8  = Wire( UInt((vParams.vlen).W) )
    val resInit16 = Wire( UInt((vParams.vlen).W) )
    val resInit32 = Wire( UInt((vParams.vlen).W) )
    val resInit64 = Wire( UInt((vParams.vlen).W) )

    mask8  := ori.param.dat.op0(vParams.vlen/8-1,  0) | Fill( vParams.vlen/8,  ori.vAttach.get.vm )
    mask16 := ori.param.dat.op0(vParams.vlen/16-1, 0) | Fill( vParams.vlen/16, ori.vAttach.get.vm )
    mask32 := ori.param.dat.op0(vParams.vlen/32-1, 0) | Fill( vParams.vlen/32, ori.vAttach.get.vm )
    mask64 := ori.param.dat.op0(vParams.vlen/64-1, 0) | Fill( vParams.vlen/64, ori.vAttach.get.vm )
    

    for( i <- 0 until log2Ceil(vParams.vlmax) ) {
      preStartTotal(i) :=  i.U  < ori.vAttach.get.vstart
      bodyTotal(i)     := (i.U >= ori.vAttach.get.vstart && i.U <= ori.vAttach.get.vl)
      tailTotal(i)     :=                                   i.U  > ori.vAttach.get.vl
    }

    preStart8  := preStartTotal.asUInt >> (ori.vAttach.get.group << log2Ceil(vParams.vlen/8))
    preStart16 := preStartTotal.asUInt >> (ori.vAttach.get.group << log2Ceil(vParams.vlen/16))
    preStart32 := preStartTotal.asUInt >> (ori.vAttach.get.group << log2Ceil(vParams.vlen/32))
    preStart64 := preStartTotal.asUInt >> (ori.vAttach.get.group << log2Ceil(vParams.vlen/64))

    body8  := bodyTotal.asUInt >> (ori.vAttach.get.group << log2Ceil(vParams.vlen/8))
    body16 := bodyTotal.asUInt >> (ori.vAttach.get.group << log2Ceil(vParams.vlen/16))
    body32 := bodyTotal.asUInt >> (ori.vAttach.get.group << log2Ceil(vParams.vlen/32))
    body64 := bodyTotal.asUInt >> (ori.vAttach.get.group << log2Ceil(vParams.vlen/64))

    tail8  := tailTotal.asUInt >> (ori.vAttach.get.group << log2Ceil(vParams.vlen/8))
    tail16 := tailTotal.asUInt >> (ori.vAttach.get.group << log2Ceil(vParams.vlen/16))
    tail32 := tailTotal.asUInt >> (ori.vAttach.get.group << log2Ceil(vParams.vlen/32))
    tail64 := tailTotal.asUInt >> (ori.vAttach.get.group << log2Ceil(vParams.vlen/64))

    val eleValid =      
      Fill( vParams.vlen/8, 1.U) & ~(
        Mux1H(Seq(
          (ori.vAttach.get.vsew === "b000".U) -> (mask8 & body8),
          (ori.vAttach.get.vsew === "b001".U) -> (mask16 & body16),
          (ori.vAttach.get.vsew === "b010".U) -> (mask32 & body32),
          (ori.vAttach.get.vsew === "b011".U) -> (mask64 & body64),
        ))
      )

    sbInfo.isRequested := eleValid.asBools
    sbInfo.isWroteBack := eleValid.asBools

    sbInfo.isException := (0.U((vParams.vlen/8).W)).asBools



    resInit8 := Cat(
      ( 0 until vParams.vlen/8 ).map{ i =>
        Mux1H(Seq(
          preStart8(i) -> ori.param.dat.op3(8*i+7, 8*i),
          body8(i)     -> Mux( mask8(i), 0.U(8.W),          Mux( ori.vAttach.get.vma, Fill(8, 1.U(1.W)), ori.param.dat.op3(8*i+7, 8*i) ) ),
          tail8(i)     -> Mux( mask8(i), Fill(8, 1.U(1.W)), Mux( ori.vAttach.get.vta, Fill(8, 1.U(1.W)), ori.param.dat.op3(8*i+7, 8*i) ) ),
        ))
      }.reverse
    )

    resInit16 := Cat(
      ( 0 until vParams.vlen/16 ).map{ i =>
        Mux1H(Seq(
          preStart16(i) -> ori.param.dat.op3(16*i+15, 16*i),
          body16(i)     -> Mux( mask16(i), 0.U(16.W),          Mux( ori.vAttach.get.vma, Fill(16, 1.U(1.W)), ori.param.dat.op3(16*i+15, 16*i) ) ),
          tail16(i)     -> Mux( mask16(i), Fill(16, 1.U(1.W)), Mux( ori.vAttach.get.vta, Fill(16, 1.U(1.W)), ori.param.dat.op3(16*i+15, 16*i) ) ),
        ))         
      }.reverse
    )

    resInit32 := Cat(
      ( 0 until vParams.vlen/32 ).map{ i =>
        Mux1H(Seq(
          preStart32(i) -> ori.param.dat.op3(32*i+31, 32*i),
          body32(i)     -> Mux( mask32(i), 0.U(32.W),          Mux( ori.vAttach.get.vma, Fill(32, 1.U(1.W)), ori.param.dat.op3(32*i+31, 32*i) ) ),
          tail32(i)     -> Mux( mask32(i), Fill(32, 1.U(1.W)), Mux( ori.vAttach.get.vta, Fill(32, 1.U(1.W)), ori.param.dat.op3(32*i+31, 32*i) ) ),
        ))         
      }.reverse
    )

    resInit64 := Cat(
      ( 0 until vParams.vlen/64 ).map{ i =>
        Mux1H(Seq(
          preStart64(i) -> ori.param.dat.op3(64*i+63, 64*i),
          body64(i)     -> Mux( mask64(i), 0.U(64.W),          Mux( ori.vAttach.get.vma, Fill(64, 1.U(1.W)), ori.param.dat.op3(64*i+63, 64*i) ) ),
          tail64(i)     -> Mux( mask64(i), Fill(64, 1.U(1.W)), Mux( ori.vAttach.get.vta, Fill(64, 1.U(1.W)), ori.param.dat.op3(64*i+63, 64*i) ) ),
        ))         
      }.reverse
    )

    sbInfo.res := 
      Mux1H(Seq(
        (ori.vAttach.get.vsew === "b000".U) -> resInit8,
        (ori.vAttach.get.vsew === "b001".U) -> resInit16,
        (ori.vAttach.get.vsew === "b010".U) -> resInit32,
        (ori.vAttach.get.vsew === "b011".U) -> resInit64,
      ))


    sbInfo.originInfo := ori
    sbInfo.splitInfo := 0.U.asTypeOf(sbInfo.splitInfo)


    for( i <- 0 until vParams.vlen/8 ){
      when( ~sbInfo.isRequested(i) ){
        // require( vParams.lsuEntry <= maxRegNum )
        require( vParams.vlen/8   <= maxRegNum )
        val eleIdx = i.U

        sbInfo.splitInfo(i).vAttach.get.eleIdx := eleIdx
        sbInfo.splitInfo(i).vAttach.get.bufIdx := bufIdx
        sbInfo.splitInfo(i).param.dat.op0 := DontCare
        sbInfo.splitInfo(i).param.dat.op3 := DontCare

        when( ori.fun.vle | ori.fun.vlm | ori.fun.vleNff | ori.fun.vleNff ){
          println("Warning, vlm should be take care before load in vlsu")
          sbInfo.splitInfo(i).fun.lbu := ori.vAttach.get.vWidth === "b000".U
          sbInfo.splitInfo(i).fun.lhu := ori.vAttach.get.vWidth === "b001".U
          sbInfo.splitInfo(i).fun.lwu := ori.vAttach.get.vWidth === "b010".U
          sbInfo.splitInfo(i).fun.ld  := ori.vAttach.get.vWidth === "b011".U

          sbInfo.splitInfo(i).param.dat.op1 := ori.param.dat.op1(63,0) + adjAddr( i, ori.vAttach.get.nf, ori.vAttach.get.nfSel, ori.vAttach.get.vWidth )
          sbInfo.splitInfo(i).param.dat.op2 := DontCare

          sbInfo.isExeReady := true.B
        }.elsewhen( ori.fun.vse | ori.fun.vsm ){
          println("Warning, vsm should be take care before load in vlsu")
          sbInfo.splitInfo(i).fun.sb := ori.vAttach.get.vWidth === "b000".U
          sbInfo.splitInfo(i).fun.sh := ori.vAttach.get.vWidth === "b001".U
          sbInfo.splitInfo(i).fun.sw := ori.vAttach.get.vWidth === "b010".U
          sbInfo.splitInfo(i).fun.sd := ori.vAttach.get.vWidth === "b011".U

          sbInfo.splitInfo(i).param.dat.op1 := ori.param.dat.op1(63,0) + adjAddr( i, ori.vAttach.get.nf, ori.vAttach.get.nfSel, ori.vAttach.get.vWidth )
          sbInfo.splitInfo(i).param.dat.op2 := adjVSEle( ori.param.dat.op3, i, ori.vAttach.get.nf, ori.vAttach.get.nfSel, vsew = ori.vAttach.get.vWidth )

          sbInfo.isExeReady := false.B
        }.elsewhen( ori.fun.vlse ){
          sbInfo.splitInfo(i).fun.lbu := ori.vAttach.get.vWidth === "b000".U
          sbInfo.splitInfo(i).fun.lhu := ori.vAttach.get.vWidth === "b001".U
          sbInfo.splitInfo(i).fun.lwu := ori.vAttach.get.vWidth === "b010".U
          sbInfo.splitInfo(i).fun.ld  := ori.vAttach.get.vWidth === "b011".U
           
          sbInfo.splitInfo(i).param.dat.op1 := ori.param.dat.op1(63,0) + adjAddr( i, ori.vAttach.get.nf, ori.vAttach.get.nfSel, ori.vAttach.get.vWidth ) + ori.param.dat.op2(63,0) * i.U
          sbInfo.splitInfo(i).param.dat.op2 := DontCare   

          sbInfo.isExeReady := true.B           
        }.elsewhen( ori.fun.vsse ){
          sbInfo.splitInfo(i).fun.sb := ori.vAttach.get.vWidth === "b000".U
          sbInfo.splitInfo(i).fun.sh := ori.vAttach.get.vWidth === "b001".U
          sbInfo.splitInfo(i).fun.sw := ori.vAttach.get.vWidth === "b010".U
          sbInfo.splitInfo(i).fun.sd := ori.vAttach.get.vWidth === "b011".U
           
          sbInfo.splitInfo(i).param.dat.op1 := ori.param.dat.op1(63,0) + adjAddr( i, ori.vAttach.get.nf, ori.vAttach.get.nfSel, ori.vAttach.get.vWidth ) + ori.param.dat.op2(63,0) * i.U
          sbInfo.splitInfo(i).param.dat.op2 := adjVSEle( ori.param.dat.op3, i, ori.vAttach.get.nf, ori.vAttach.get.nfSel, vsew = ori.vAttach.get.vWidth )

          sbInfo.isExeReady := false.B
        }.elsewhen( ori.fun.vloxei ){
          sbInfo.splitInfo(i).fun.lbu := ori.vAttach.get.vWidth === "b000".U
          sbInfo.splitInfo(i).fun.lhu := ori.vAttach.get.vWidth === "b001".U
          sbInfo.splitInfo(i).fun.lwu := ori.vAttach.get.vWidth === "b010".U
          sbInfo.splitInfo(i).fun.ld  := ori.vAttach.get.vWidth === "b011".U
           
          sbInfo.splitInfo(i).param.dat.op1 := ori.param.dat.op1(63,0) + adjAddr( i, ori.vAttach.get.nf, ori.vAttach.get.nfSel, ori.vAttach.get.vWidth ) + adjVSEle( ori.param.dat.op2, i, "b000".U, 0.U, ori.vAttach.get.vsew ) 
          sbInfo.splitInfo(i).param.dat.op2 := DontCare

          sbInfo.isExeReady := true.B
        }.elsewhen( ori.fun.vsoxei ){
          sbInfo.splitInfo(i).fun.sb := ori.vAttach.get.vWidth === "b000".U
          sbInfo.splitInfo(i).fun.sh := ori.vAttach.get.vWidth === "b001".U
          sbInfo.splitInfo(i).fun.sw := ori.vAttach.get.vWidth === "b010".U
          sbInfo.splitInfo(i).fun.sd := ori.vAttach.get.vWidth === "b011".U
           
          sbInfo.splitInfo(i).param.dat.op1 := ori.param.dat.op1(63,0) + adjAddr( i, ori.vAttach.get.nf, ori.vAttach.get.nfSel, ori.vAttach.get.vWidth ) + adjVSEle( ori.param.dat.op2, i, "b000".U, 0.U, ori.vAttach.get.vsew ) 
          sbInfo.splitInfo(i).param.dat.op2 := adjVSEle( ori.param.dat.op3, i, ori.vAttach.get.nf, ori.vAttach.get.nfSel, ori.vAttach.get.vsew )

          sbInfo.isExeReady := false.B
        }.elsewhen( ori.fun.vlNreN ){

          sbInfo.splitInfo(i).fun.lbu := true.B//ori.param.vWidth === "b000".U
          // sbInfo.splitInfo(i).fun.lhu := ori.param.vWidth === "b001".U
          // sbInfo.splitInfo(i).fun.lwu := ori.param.vWidth === "b010".U
          // sbInfo.splitInfo(i).fun.ld := ori.param.vWidth === "b011".U

          sbInfo.splitInfo(i).param.dat.op1 := ori.param.dat.op1(63,0) + ((ori.vAttach.get.nf + 1.U) << log2Ceil(vParams.vlen/8)) + adjAddr( i, nf = "b000".U, 0.U, vWidth = "b000".U )
          sbInfo.splitInfo(i).param.dat.op2 := DontCare

          sbInfo.isExeReady := true.B
        }.elsewhen( ori.fun.vsNr ){

          sbInfo.splitInfo(i).fun.sb := true.B//ori.param.vWidth === "b000".U
          // sbInfo.splitInfo(i).fun.sh := ori.param.vWidth === "b001".U
          // sbInfo.splitInfo(i).fun.sw := ori.param.vWidth === "b010".U
          // sbInfo.splitInfo(i).fun.sd := ori.param.vWidth === "b011".U

          sbInfo.splitInfo(i).param.dat.op1 := ori.param.dat.op1(63,0) + ((ori.vAttach.get.nf + 1.U) << log2Ceil(vParams.vlen/8)) + adjAddr( i, nf = "b000".U, 0.U, vWidth = "b000".U )
          sbInfo.splitInfo(i).param.dat.op2 := adjVSEle( ori.param.dat.op3, i, nf = "b000".U, 0.U, vsew = "b000".U )
          sbInfo.isExeReady := false.B
        }.otherwise{
          assert(false.B, "Assert Failed, a none Vector LSU instr is push into the VScoreboard!")
        }

      }
    }


    return sbInfo
  }

  def Pkg_VlsuScoreBoard_update( ori: VLSU_Entry_Bundle, iwb: LSU_WriteBack_Bundle, cwb: SeqReg_WriteBack_Bundle ): VLSU_Entry_Bundle = {
    val sbInfo = WireDefault(ori)

    val eleIdx = iwb.vAttach.get.eleIdx

    val resMasked8  = Wire( UInt((vParams.vlen).W))
    val resMasked16 = Wire( UInt((vParams.vlen).W))
    val resMasked32 = Wire( UInt((vParams.vlen).W))
    val resMasked64 = Wire( UInt((vParams.vlen).W))

    resMasked8  := ( iwb.res & Fill(  8, 1.U(1.W) ) ) << (eleIdx << 3)
    resMasked16 := ( iwb.res & Fill( 16, 1.U(1.W) ) ) << (eleIdx << 4)
    resMasked32 := ( iwb.res & Fill( 32, 1.U(1.W) ) ) << (eleIdx << 5)
    resMasked64 := ( iwb.res & Fill( 64, 1.U(1.W) ) ) << (eleIdx << 6)
    
    sbInfo.isWroteBack(eleIdx) := true.B

    when( cwb.op_rw & ~sbInfo.isException.reduce(_|_) ){
      sbInfo.exceptionInfo := cwb.dati
      sbInfo.isException(eleIdx) := true.B
    }

    when( ori.originInfo.fun.vle | ori.originInfo.fun.vlm | ori.originInfo.fun.vleNff | ori.originInfo.fun.vlse ){
      sbInfo.res :=
        ori.res |
        Mux1H(Seq(
          (ori.originInfo.vAttach.get.vWidth === "b000".U) -> ( resMasked8  ), (ori.originInfo.vAttach.get.vWidth === "b001".U) -> ( resMasked16 ),
          (ori.originInfo.vAttach.get.vWidth === "b010".U) -> ( resMasked32 ), (ori.originInfo.vAttach.get.vWidth === "b011".U) -> ( resMasked64 ),          
        ))
    }.elsewhen( ori.originInfo.fun.vloxei ){
      sbInfo.res :=
        ori.res |
        Mux1H(Seq(
          (ori.originInfo.vAttach.get.vsew === "b000".U) -> ( resMasked8  ), (ori.originInfo.vAttach.get.vsew === "b001".U) -> ( resMasked16 ),
          (ori.originInfo.vAttach.get.vsew === "b010".U) -> ( resMasked32 ), (ori.originInfo.vAttach.get.vsew === "b011".U) -> ( resMasked64 ),          
        ))
    }.elsewhen( ori.originInfo.fun.vlNreN ){
      sbInfo.res :=
        ori.res | resMasked8
    }.elsewhen( ori.originInfo.fun.vse | ori.originInfo.fun.vsm | ori.originInfo.fun.vsse | ori.originInfo.fun.vsoxei | ori.originInfo.fun.vsNr ){
      ;
    }.otherwise{
      assert(false.B, "Assert Failed, a none Vector LSU instr is push into the VScoreboard!")
    }

    return sbInfo
  }

  def adjSeg( eleIdx: Int, nf: UInt, nfSel: UInt ): UInt = {
      nfSel +
      Mux1H(Seq(
        (nf === 0.U) -> (eleIdx*1).U,
        (nf === 1.U) -> (eleIdx*2).U,
        (nf === 2.U) -> (eleIdx*3).U,
        (nf === 3.U) -> (eleIdx*4).U,
        (nf === 4.U) -> (eleIdx*5).U,
        (nf === 5.U) -> (eleIdx*6).U,
        (nf === 6.U) -> (eleIdx*7).U,
        (nf === 7.U) -> (eleIdx*8).U,
      ))
  }

  def adjAddr( eleIdx: Int, nf: UInt, nfSel: UInt, vWidth: UInt ): UInt = {
    Mux1H(Seq(
      (vWidth === "b000".U) -> (adjSeg( eleIdx, nf, nfSel ) >> 0),
      (vWidth === "b001".U) -> (adjSeg( eleIdx, nf, nfSel ) >> 1),
      (vWidth === "b010".U) -> (adjSeg( eleIdx, nf, nfSel ) >> 2),
      (vWidth === "b011".U) -> (adjSeg( eleIdx, nf, nfSel ) >> 3),
    ))
  }

  def adjVSEle( op: UInt, eleIdx: Int, nf: UInt, nfSel: UInt, vsew: UInt ): UInt = {
    Mux1H(Seq(
      (vsew === "b000".U) -> (op >> (adjSeg( eleIdx, nf, nfSel ) >> 3)).apply(7,0),
      (vsew === "b001".U) -> (op >> (adjSeg( eleIdx, nf, nfSel ) >> 4)).apply(15,0),
      (vsew === "b010".U) -> (op >> (adjSeg( eleIdx, nf, nfSel ) >> 5)).apply(31,0),
      (vsew === "b011".U) -> (op >> (adjSeg( eleIdx, nf, nfSel ) >> 6)).apply(63,0),
    ))
  }

  def Pkg_VlsuScoreBoard_Rtn( buf: VLSU_Entry_Bundle )(implicit p: Parameters): (WriteBack_info, SeqReg_WriteBack_Bundle) = {
    val vwb = Wire(new WriteBack_info(dw = vParams.vlen))
    val cwb = Wire(new SeqReg_WriteBack_Bundle(64, cRegNum))

    vwb.rd0 := buf.originInfo.param.rd0
    vwb.res := buf.res

    cwb.addr  := "hFFF".U
    cwb.idx   := buf.originInfo.param.csrw
    cwb.op_rs := false.B
    cwb.op_rc := false.B

    when( buf.isException.forall((x:Bool) => (x === false.B)) ){ //vstart only
      require( (vlen + log2Ceil(vParams.vlmax)) + 4 <= 64)
      cwb.dati := 
        Cat( "b0000".U(4.W),
          (Cat( buf.originInfo.vAttach.get.vstart, 0.U(vlen.W) ) | 0.U(60.W))
        )
      cwb.op_rw := true.B
    }.elsewhen( (buf.isException(0) === true.B) || (buf.isException.reduce(_|_) & ~(buf.originInfo.fun.vleNff)) ){ //exception & vstart
      val vstart = Wire(UInt((log2Ceil(vParams.vlmax)).W))
      vstart := buf.originInfo.vAttach.get.vstart + OHToUInt(buf.isException.asUInt)
      assert( PopCount(buf.isException.asUInt) === 1.U )

      cwb.dati := 
        Cat( buf.exceptionInfo(63,61),
          (Cat( vstart, buf.exceptionInfo(vlen-1,0) ) | 0.U(61.W))
        )
      cwb.op_rw := true.B
    }.elsewhen( (buf.originInfo.fun.vleNff) & ~buf.isException(0) & buf.isException.reduce(_|_) ){ //vstart and vl
      assert( PopCount(buf.isException.asUInt) === 1.U )
      val vstart = Wire(UInt((log2Ceil(vParams.vlmax)).W))
      val vl     = Wire(UInt((log2Ceil(vParams.vlmax)).W))
      vstart := buf.originInfo.vAttach.get.vstart
      vl := buf.originInfo.vAttach.get.vstart + OHToUInt(buf.isException.asUInt)
      cwb.dati  := 
        Cat( "b0001".U(4.W),
          (Cat( vstart, ( vl | 0.U(vlen.W) ) ) | 0.U(60.W))
        )
      cwb.op_rw := true.B
    }

    return (vwb, cwb)
  }

}

trait VLSUEnq{ this: VLSUScoreboard => if(hasVector){
  
  when( isSBEmpty & ~io.lsu_iss_exe.bits.fun.isVector ){ //bypass normal lsu request
    lsu.io.lsu_iss_exe <> io.lsu_iss_exe
    lsu.io.lsu_iss_exe.bits.vAttach.get.bufIdx := (vParams.lsuEntry).U          //marks as bypass
  } .otherwise{
    io.lsu_iss_exe.ready := ~isSBFull

    when( io.lsu_iss_exe.fire ){
      sbBuf(sbISSPtr) := Pkg_VlsuScoreBoard_update(io.lsu_iss_exe.bits, bufIdx = sbISSPtr)
      sbISSPtr := sbISSPtr + 1.U
    }

    when( ~isSBEmpty ){
      def exeIdx = sbBuf(sbEXEPtr).isRequested.indexWhere((z:Bool) => (z === false.B)) //vstart

      when( sbBuf(sbEXEPtr).isException.exists((x:Bool) => (x === true.B)) ){ //bypass remained request if an exception occur
        
        lsu.io.lsu_iss_exe.valid := false.B
        lsu.io.lsu_iss_exe.bits  := DontCare

        for( i <- 0 until vParams.vlen/8 ){
          when( sbBuf(sbEXEPtr).isRequested(i) === false.B ){
            sbBuf(sbEXEPtr).isRequested(i) := true.B
            sbBuf(sbEXEPtr).isWroteBack(i) := true.B
          }
        }

        sbEXEPtr := sbEXEPtr + 1.U
      }.otherwise{
        when( sbBuf(sbEXEPtr).isExeReady ){ //is (vload) or (vstore & isCommit)
          lsu.io.lsu_iss_exe.valid := true.B
          lsu.io.lsu_iss_exe.bits := sbBuf(sbEXEPtr).splitInfo(exeIdx)

          when( lsu.io.lsu_iss_exe.fire ){
            assert( sbBuf(sbEXEPtr).isRequested(exeIdx) === false.B )
            sbBuf(sbEXEPtr).isRequested(exeIdx) := true.B
            when( PopCount( sbBuf(sbEXEPtr).isRequested.map{~_} ) === 1.U ){
              sbEXEPtr := sbEXEPtr + 1.U
            }
          }
        }.otherwise{ //vstore and not commit yet
          when( io.cmm_lsu.isVstorePending ){
            sbBuf(sbEXEPtr).isExeReady === true.B
          }

          lsu.io.lsu_iss_exe.valid := false.B
          lsu.io.lsu_iss_exe.bits  := 0.U.asTypeOf(new Lsu_iss_info)
        }
      }
    }
  }
}}



trait VLSUDeq{ this: VLSUScoreboard => if(hasVector){

  io.lsu_exe_fwb <> lsu.io.lsu_exe_fwb

  val bufIdx = lsu.io.lsu_exe_iwb.bits.vAttach.get.bufIdx

  when( bufIdx === vParams.lsuEntry.U ){ //bypass

    io.lsu_exe_iwb.valid := lsu.io.lsu_exe_iwb.valid
    io.lsu_exe_iwb.bits  := lsu.io.lsu_exe_iwb.bits.viewAsSupertype(new WriteBack_info(dw=64))
    lsu.io.lsu_exe_iwb.ready := io.lsu_exe_iwb.ready

    io.lsu_cWriteBack := lsu.io.lsu_cWriteBack
  } .otherwise{
    lsu.io.lsu_exe_iwb.ready := true.B

    when( lsu.io.lsu_exe_iwb.fire ){ //vector load return from lsu
      sbBuf(bufIdx) := Pkg_VlsuScoreBoard_update( sbBuf(bufIdx), iwb = lsu.io.lsu_exe_iwb.bits, cwb = lsu.io.lsu_cWriteBack.bits )
    }

    when( sbBuf(sbVWBPtr).isWroteBack.reduce(_&_) ){ //all load is written back
      val (vwb, cwb) = Pkg_VlsuScoreBoard_Rtn( buf = sbBuf(sbVWBPtr) )
      io.lsu_exe_vwb.valid := true.B
      io.lsu_exe_vwb.bits  := vwb

      io.lsu_cWriteBack.valid := true.B
      io.lsu_cWriteBack.bits  := cwb

      when(io.lsu_exe_vwb.fire){
        sbVWBPtr := sbVWBPtr + 1.U
      }
    }

  }

}}

trait VLSUBypass{ this: VLSUScoreboard => if(!hasVector){
  io.lsu_iss_exe <> lsu.io.lsu_iss_exe
  io.lsu_exe_iwb <> lsu.io.lsu_exe_iwb
  io.lsu_exe_fwb <> lsu.io.lsu_exe_fwb
  io.lsu_cWriteBack <> lsu.io.lsu_cWriteBack

  io.lsu_exe_vwb.valid := false.B
  io.lsu_exe_vwb.bits  := DontCare
 
}}




class VectorLSU (edge: Seq[TLEdgeOut])(implicit p: Parameters) extends VLSUScoreboard(edge) 
with VLSUEnq
with VLSUDeq
with VLSUBypass





