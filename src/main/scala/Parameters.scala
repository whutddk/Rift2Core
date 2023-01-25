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
  vlen: Int = 128,
  elen: Int = 64,
)

trait HasVectorParameters extends HasRiftParameters{
  val vectorParams: VectorParameters

  // def vlen = vectorParams.vlen
  // def elen = vectorParams.elen
}





case object RiftParamsKey extends Field[RiftSetting]




case class RiftSetting(

  hasL2: Boolean = true,
  hasDebugger: Boolean = true,
  hasPreFetch: Boolean = false,
  hasuBTB: Boolean = true,
  hasLRU: Boolean = false,


  isMinArea: Boolean = false,
  isLowPower: Boolean = false,

  ftChn: Int = 8, //fetch width
  rnChn: Int = 2,
  cm_chn: Int = 2,
  opChn: Int = 4,
  wbChn: Int = 4,

  regNum: Int = 64,
  pmpNum: Int = 1,
  hpmNum: Int = 4,

  l1BeatBits: Int = 128,
  memBeatBits: Int = 128,

  vlen: Int = 39,
  plen: Int = 32,

  tlbEntry: Int = 16, 
  ifetchParameters: IFParameters = IFParameters(
    // GHR_length = 64,
    // UBTB_entry = 16,
    // fetch_w   = 64,

    // btb_tag_w = 8,
    // btb_cb  = 4,
  uBTB_entry = 16,
  uBTB_tag_w = 16,
  btb_cl = 4096,
  bim_cl = 4096,
  ras_dp = 256,
  tage_table = 6, 



    // tage_tag_w = 8,
  ),

  l1DW: Int = 256,

  dptEntry: Int = 16,

  aluNum: Int = 1,
  mulNum: Int = 1,
  fpuNum: Int = 0,


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
  require( regNum > 33 )
  require( pmpNum >= 0 && pmpNum <= 8 )
  require( isPow2(dcacheParameters.stEntry) )
  require( isPow2(ftChn) )
  require( aluNum > 0 )
  require( dptEntry >= 1 )

}

trait HasRiftParameters {
  implicit val p: Parameters

  val riftSetting = p(RiftParamsKey)

  val ifParams     = riftSetting.ifetchParameters
  val icacheParams = riftSetting.icacheParameters
  val dcacheParams = riftSetting.dcacheParameters

  def hasL2  = riftSetting.hasL2
  def hasDebugger = riftSetting.hasDebugger
  def hasPreFetch = riftSetting.hasPreFetch
  def hasuBTB  = riftSetting.hasuBTB
  def hasLRU  = riftSetting.hasLRU
  // def hasMulDiv = riftSetting.hasMulDiv
  
  def ftChn = riftSetting.ftChn

  def cm_chn = riftSetting.cm_chn
  def rnChn = riftSetting.rnChn
  def opChn = riftSetting.opChn
  def wbChn = riftSetting.wbChn

  def regNum = riftSetting.regNum
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


