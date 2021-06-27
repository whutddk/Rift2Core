/*
  Copyright (c) 2020 - 2021 Ruige Lee <m201772520@hust.edu.cn>

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

package rift2Core.L1Cache

import chisel3._
import chisel3.util._


import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tilelink._

import base._

import chipsalliance.rocketchip.config.{Field, Parameters}



case object CacheParamsKey extends Field[CacheParameters]

case class CacheParameters(
  // l1DcacheParameters: DcacheParameters = DcacheParameters(
  //   dw = 64,
  //   bk = 4,
  //   cb = 4,
  //   cl = 4
  // ),
){

}

trait HasCacheParameters {
  implicit val p: Parameters

  val cacheSetting = p(CacheParamsKey)

  // val dcacheParameters = cacheSetting.l1DcacheParameters

}

