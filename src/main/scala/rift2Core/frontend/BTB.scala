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


class Info_BTB extends IFetchBundle {
  val targets = UInt(64.W)
  val tag     = UInt(8.W)
  val is_active = Vec(4, Bool()) 
  val is_jump   = Vec(4, Bool())
  val is_takens = Vec(4, Bool())
  val is_branch = Vec(4, Bool())
  val is_RVC    = Vec(4, Bool())

}


class BTB extends IFetchModule {

}



