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

package rift2Core.frontend

import chisel3._
import chisel3.util._
import base._
import rift2Core.define._
import chisel3.experimental.dataview._
import chipsalliance.rocketchip.config.Parameters

class uBTBResp_Bundle extends Bundle {
  val isActive = Vec( 8, Bool() )
  val target = UInt(64.W)
}

// class uBTB()(implicit p: Parameters) extends IFetchModule {
//   val io = IO(new Bundle{
//     val req  = Input(new BTBReq_Bundle)
//     val Resp = Output( new uBTBResp_Bundle )
//   })


//   val buff = 
// }
