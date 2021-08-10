


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


package rift2Core.privilege

import chisel3._
import chisel3.util._

import rift2Core.define._
import rift2Core.backend._
import rift2Core.privilege.csrFiles._


abstract class Privilege extends CsrFiles{

  priv_lvl_dnxt :=
    WireDefault(
      Mux1H( Seq(
        is_mRet -> mstatus(12,11),
        is_sRet -> mstatus(8),
        is_trap -> "b11".U
      ))
    )

  priv_lvl_enable := is_trap | is_mRet | is_sRet | is_trap

}


