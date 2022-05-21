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
// import rift2Core.frontend._
// import rift2Core.backend._
// import rift2Core.cache._
import rift._
import rift2Core.privilege._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.config._
import chisel3.stage._



class miniCfg extends Config((site, here, up) => {
  case RiftParamsKey => RiftSetting()

  
})




object testMain extends App {

  // Driver.execute(args, () => new Rift2Chip )


  val cfg = new miniCfg

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

