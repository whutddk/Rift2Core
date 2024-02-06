/*
  Copyright (c) 2020 - 2024 Wuhan University of Technology <295054118@whut.edu.cn>

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

package rift2Core.backend


import chisel3._
import chisel3.util._
import rift2Core.define._

import rift2Chip._
import org.chipsalliance.cde.config._

class VPU_Param_Bundle(implicit p: Parameters) extends RD_PHY {
  val dat = new Operation_source(dw=64)
}

class VPU_Issue_Bundle()(implicit p: Parameters) extends RiftBundle{
  val fun = new VecIsa
  val param = new VPU_Param_Bundle
}



abstract class VPUBase()(implicit p: Parameters) extends RiftModule{
  class VPUIO extends Bundle{
    val vpu_iss_exe = Flipped(new DecoupledIO(new VPU_Issue_Bundle))

    val vpu_exe_iwb = Decoupled(new WriteBack_info(dw=64))
    val vpu_exe_fwb = Decoupled(new WriteBack_info(dw=65))
    val vpu_exe_vwb = Decoupled(new Vector_WriteBack_Bundle)

    // val vpuCsrWriteBack = Valid(new Exe_Port)

    val flush = Input(Bool())   
  }
}


trait VPUConfiguration{ this: VPUBase =>



}

