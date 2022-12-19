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

package rift2Chip

import chisel3._
import chisel3.util._


import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tilelink._
import chipsalliance.rocketchip.config.{Field, Parameters}

import rift2Core.define._

abstract class RiftModule(implicit val p: Parameters) extends Module with HasRiftParameters { def io: Record }
abstract class RiftBundle(implicit val p: Parameters) extends Bundle with HasRiftParameters


trait HasIcacheParameters extends HasRiftParameters {
  val icacheParams: IcacheParameters

  def dw = l1DW
  def bk = icacheParams.bk
  def cb = icacheParams.cb
  def cl = icacheParams.cl


  def addr_lsb = log2Ceil(dw/8)
  def line_w   = log2Ceil(cl)
  def cb_w = log2Ceil(cb)

  // require( (addr_lsb + line_w) == 12 )
  require( bk == 1 )
 
  def tag_w    = plen - addr_lsb - line_w
}

case class IcacheParameters(
  bk: Int = 1,
  cb: Int = 4,
  cl: Int = 128,
)

abstract class IcacheModule(implicit val p: Parameters) extends Module with HasIcacheParameters { def io: Record }
abstract class IcacheBundle(implicit val p: Parameters) extends Bundle with HasIcacheParameters

case class DcacheParameters(
  bk: Int = 8,
  cb: Int = 8,
  cl: Int = 128,
  sbEntry: Int = 16,
  stEntry: Int = 16,
)

trait HasDcacheParameters extends HasRiftParameters {
  val dcacheParams: DcacheParameters

  def dw = l1DW
  def bk = dcacheParams.bk
  def cb = dcacheParams.cb
  def cl = dcacheParams.cl
  def sbEntry = dcacheParams.sbEntry
  def stEntry = dcacheParams.stEntry

  def addr_lsb = log2Ceil(dw/8)
  def bk_w = log2Ceil(bk)
  def line_w   = log2Ceil(cl)
  def cb_w = log2Ceil(cb)


  def tag_w    = plen - addr_lsb - line_w - bk_w

  // require( (addr_lsb + line_w) == 12 )
  
}

abstract class DcacheModule(implicit val p: Parameters) extends Module with HasDcacheParameters { def io: Record }
abstract class DcacheBundle(implicit val p: Parameters) extends Bundle with HasDcacheParameters


case class VectorParameters(
  vlen: Int = 512,    //The number of bits in a single vector register
  elen: Int = 64,     //The maximum size in bits that can produce or consume 
  isEEW8: Boolean = true,
  isEEW16: Boolean = true,
  isEEW32: Boolean = true,
  isEEW64: Boolean = true,
  maxMUL : Int  = 8,

  opChn: Int = 4,
  wbChn: Int = 4,



){

  require( isPow2(elen) )
  require( isPow2(vlen) )
  require( elen >= 8 )
  require( vlen >= elen )
  require( vlen < 65536 )
  if( isEEW64 ) { elen >= 64 }
  if( isEEW32 ) { elen >= 32 }
  if( isEEW16 ) { elen >= 16 }
  if( isEEW8 )  { elen >= 8  }

  //requirement of V for application Processors
  require( vlen >= 128 )
  require(isEEW8  == true)
  require(isEEW16 == true)
  require(isEEW32 == true)
  require(isEEW64 == true)
  require( isPow2(maxMUL) )
  require(maxMUL >= 8)


  // val atw: Int = {
  //   if(isEEW8) {8}
  //   else if(isEEW16) {16}
  //   else if(isEEW32) {32}
  //   else if(isEEW64) {64}
  // }

  // val atNum: Int = vRegNum * (vlen / atw)
  // val minLMUL: float = atw / elen

}



// trait HasVectorParameters extends HasRiftParameters{
//   val vParams: VectorParameters

//   // def vlen = vectorParams.vlen
//   // def elen = vectorParams.elen
// }





case object RiftParamsKey extends Field[RiftSetting]




case class RiftSetting(

  hasL2: Boolean = true,
  hasDebugger: Boolean = true,
  hasPreFetch: Boolean = false,
  hasuBTB: Boolean = true,
  hasLRU: Boolean = false,
  hasVector: Boolean = true,


  isMinArea: Boolean = false,
  isLowPower: Boolean = false,

  ftChn: Int = 8, //fetch width
  rnChn: Int = 2,
  cmChn: Int = 2,
  opChn: Int = 4,
  wbChn: Int = 4,

  xRegNum: Int = 64,
  fRegNum: Int = 64,
  vRegNum: Int = 64,

  pmpNum: Int = 1,
  hpmNum: Int = 4,

  l1BeatBits: Int = 128,
  memBeatBits: Int = 128,

  vlen: Int = 39,
  plen: Int = 32,

  tlbEntry: Int = 16, 
  ifetchParameters: IFParameters = IFParameters(
    uBTB_entry = 16,
    uBTB_tag_w = 16,
    btb_cl = 4096,
    bim_cl = 4096,
    ras_dp = 256,
    tage_table = 6, 
  ),

  l1DW: Int = 256,

  dptEntry: Int = 16,

  aluNum: Int = 1,
  mulNum: Int = 1,
  fpuNum: Int = 0,

  vectorParameters: VectorParameters = VectorParameters()


  icacheParameters: IcacheParameters = IcacheParameters(
    bk = 1,
    cb = 4,
    cl = 256
  ),
  dcacheParameters: DcacheParameters = DcacheParameters(
    bk = 8,
    cb = 8,
    cl = 256,
    stEntry = 16,
    sbEntry = 16,
  ),
){
  require( icacheParameters.bk == 1 )
  require( log2Ceil( ifetchParameters.uBTB_entry ) <= ifetchParameters.uBTB_tag_w )
  require( vlen == 39 )
  require( plen >=32 && plen <= 56 )
  require( memBeatBits <= l1BeatBits )
  //require( opChn % 2 == 0 )
  require( xRegNum > 33 & fRegNum > 33 && vRegNum > 33)
  require( pmpNum >= 0 && pmpNum <= 8 )
  require( isPow2(dcacheParameters.stEntry) )
  require( isPow2(ftChn) )
  require( aluNum > 0 )
  require( dptEntry >= 1 )


  if(hasVector){
    // require( rnChn >= 2 )
    require( fpuNum > 0 )
  }
}

trait HasRiftParameters {
  implicit val p: Parameters

  val riftSetting = p(RiftParamsKey)

  val ifParams     = riftSetting.ifetchParameters
  val vParams      = riftSetting.vectorParameters
  val icacheParams = riftSetting.icacheParameters
  val dcacheParams = riftSetting.dcacheParameters

  def hasL2  = riftSetting.hasL2
  def hasDebugger = riftSetting.hasDebugger
  def hasPreFetch = riftSetting.hasPreFetch
  def hasuBTB  = riftSetting.hasuBTB
  def hasLRU  = riftSetting.hasLRU
  def hasVector = riftSetting.hasVector
  
  def ftChn = riftSetting.ftChn

  def cmChn = riftSetting.cmChn
  def rnChn = riftSetting.rnChn
  def opChn = riftSetting.opChn
  def wbChn = riftSetting.wbChn

  def xRegNum = riftSetting.xRegNum
  def fRegNum = riftSetting.fRegNum
  def vRegNum = riftSetting.vRegNum
  def maxRegNum = xRegNum max fRegNum max vRegNum

  def pmpNum = riftSetting.pmpNum
  def hpmNum = riftSetting.hpmNum

  def l1DW = riftSetting.l1DW
  def l1BeatBits = riftSetting.l1BeatBits
  def memBeatBits = riftSetting.memBeatBits

  def vlen = riftSetting.vlen
  def plen = riftSetting.plen

  def tlbEntry = riftSetting.tlbEntry

  def dptEntry = riftSetting.dptEntry

  def aluNum = riftSetting.aluNum
  def mulNum = riftSetting.mulNum
  def fpuNum = riftSetting.fpuNum


  def isMinArea = riftSetting.isMinArea
  def isLowPower = riftSetting.isLowPower
}


