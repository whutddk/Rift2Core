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



case object CacheParamsKey extends Field[CacheSetting]

case class CacheSetting(
  icacheParameters: L1CacheParameters = IcacheParameters(
    dw = 128,
    bk = 2,
    cb = 4,
    cl = 128
  ),
  dcacheParameters: L1CacheParameters = DcacheParameters(
    dw = 64,
    bk = 4,
    cb = 8,
    nm = 8,
    cl = 128
  ),
){

}

trait HasCacheParameters {
  implicit val p: Parameters

  val cacheSetting = p(CacheParamsKey)

  val icacheParams = cacheSetting.icacheParameters
  val dcacheParams = cacheSetting.dcacheParameters

}

trait HasBackEndParameters {
  
}




trait HasRiftParameters extends HasCacheParameters with HasBackEndParameters
