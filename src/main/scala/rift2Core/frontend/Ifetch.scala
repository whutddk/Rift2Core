/*
* @Author: Ruige Lee
* @Date:   2021-04-08 14:32:49
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-15 16:18:45
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


package rift2Core.frontend

import chisel3._
import chisel3.util._


import rift2Core.define._
import rift2Core.L1Cache._
import rift2Core.frontend._


import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

// class Info_if_cmm extends Bundle {
//   val is_access_fault = Bool()
//   val is_paging_fault = Bool()
//   val fault_vaddr = UInt(64.W)

// }


class Ifetch(edge: TLEdgeOut)(implicit p: Parameters) extends Icache(edge = edge){

}








