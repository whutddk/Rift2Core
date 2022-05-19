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

package rift

import chisel3._
import chisel3.util._


import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tilelink._
import chipsalliance.rocketchip.config.{Field, Parameters}

import rift2Core.L1Cache._
import rift2Core.define._



case object RiftParamsKey extends Field[RiftSetting]




case class RiftSetting(
  rn_chn: Int = 2,
  cm_chn: Int = 2,
  ifetchParameters: IFParameters = IFParameters(
    // GHR_length = 64,
    // UBTB_entry = 16,
    // fetch_w   = 64,

    // btb_tag_w = 8,
    // btb_cb  = 4,
  uBTB_entry = 16,
  uBTB_tag_w = 8,
  btb_cl = 4096,
  bim_cl = 4096,
  ras_dp = 256,
  tage_table = 6, 



    // tage_tag_w = 8,
  ),

  icacheParameters: L1CacheParameters = IcacheParameters(
    dw = 256,
    bk = 1,
    cb = 4,
    cl = 128
  ),
  dcacheParameters: L1CacheParameters = DcacheParameters(
    dw = 256,
    bk = 8,
    cb = 8,
    cl = 128
  ),
){
  require( log2Ceil( ifetchParameters.uBTB_entry ) <= ifetchParameters.uBTB_tag_w )
}

trait HasRiftParameters {
  implicit val p: Parameters

  val riftSetting = p(RiftParamsKey)

  val ifParams     = riftSetting.ifetchParameters
  val icacheParams = riftSetting.icacheParameters
  val dcacheParams = riftSetting.dcacheParameters

  def cm_chn = riftSetting.cm_chn
  def rn_chn = riftSetting.rn_chn
}


