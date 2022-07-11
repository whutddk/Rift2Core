package test



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



import chisel3._
import rift2Chip._
import rift2Core._

import rift._
import rift2Core.define.{IFParameters}
import rift2Core.privilege._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.config._
import chisel3.stage._



class NormalCfg extends Config((site, here, up) => {
  case RiftParamsKey => RiftSetting()
})

class Rift2GoCfg extends Config((site, here, up) => {
  case RiftParamsKey => RiftSetting(
    hasFpu = false,
    hasDebugger = false,
    hasPreFetch = false,

    opChn = 4,
    wbChn = 2,

    regNum = 40,

    l1BeatBits = 64,
    memBeatBits = 64,

    tlbEntry = 2,

    ifetchParameters = IFParameters(
      uBTB_entry = 4,
      // uBTB_tag_w = 16,
      // btb_cl = 4096,
      // bim_cl = 4096,
      // ras_dp = 256,
      // tage_table = 6, 
    ),
    icacheParameters = IcacheParameters(
      cb = 2,
    ),
    dcacheParameters = DcacheParameters(
      bk = 1,
      cb = 2,
      sbEntry = 4,
      stEntry = 4,
    ),


    isMinArea = true,
    isLowPower = false,

  )
})


object testMain extends App {


  // val cfg = new NormalCfg
  val cfg = new Rift2GoCfg

  (new chisel3.stage.ChiselStage).execute(args, Seq(
      ChiselGeneratorAnnotation(() => {
    val soc = LazyModule(new Rift2Chip()(cfg))
    soc.module
  })
  ))
}

// import rift2Core.backend.fpu._

// object testModule extends App {
//   (new chisel3.stage.ChiselStage).execute(args, Seq(
//       ChiselGeneratorAnnotation(() => {
//     new Reservation_fpu()
//   })
//     ))
// }

import base._

object testModule extends App {
  (new chisel3.stage.ChiselStage).execute(args, Seq(
      ChiselGeneratorAnnotation(() => {
    new RePort( UInt(8.W), 3)
  })
    ))
}

