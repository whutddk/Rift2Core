


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
import chipsalliance.rocketchip.config.Parameters

import base._

trait L1CacheParameters {
  def dw: Int
  def bk: Int
  def cb: Int
  def cl: Int
}

trait HasL1CacheParameters {
  val cacheParam: L1CacheParameters

  def dw = cacheParam.dw
  def bk = cacheParam.bk
  def cb = cacheParam.cb
  def cl = cacheParam.cl
}

abstract class L1CacheModule(implicit p: Parameters) extends MultiIOModule with HasL1CacheParameters
abstract class L1CacheBundle(implicit p: Parameters) extends Bundle with HasL1CacheParameters




