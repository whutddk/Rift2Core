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

import freechips.rocketchip.diplomacy._
import freechips.rocketchip.config._
import chisel3.stage._

class Rift2GoCfg extends Rift2330
class NormalCfg  extends Rift2370



object testMain extends App {

  // val cfg = new NormalCfg
  // val cfg = new Rift2GoCfg
  // val cfg = new Rift2350
  val cfg = new Rift2370

  (new chisel3.stage.ChiselStage).execute( Array("--show-registrations", "--full-stacktrace", "--target-dir", "generated/Main") ++ args, Seq(
      ChiselGeneratorAnnotation(() => {
    val soc = LazyModule(new Rift2Chip()(cfg))
    soc.module
  })
  ))
}

object tapeMain extends App {

  val cfg = new Rift2330
  // val cfg = new Rift2330
  // val cfg = new Rift2350
  // val cfg = new Rift2370

  (new chisel3.stage.ChiselStage).execute( Array( "--target-dir", "generated/Main", "-E", "verilog", "--gen-mem-verilog", "true") ++ args, Seq(//, "--gen-mem-verilog", "true"
      ChiselGeneratorAnnotation(() => {
    val soc = LazyModule(new Rift2Link()(cfg))
    soc.module
  })
  ))

  (new chisel3.stage.ChiselStage).execute( Array( "--target-dir", "generated/TapeMain", "-E", "verilog", "--gen-mem-verilog", "true") ++ args, Seq(//, "--gen-mem-verilog", "true"
      ChiselGeneratorAnnotation(() => {
    val soc = LazyModule(new Rift2LinkA(isFlatten = false)(cfg))
    soc.module
  })
  ))

  (new chisel3.stage.ChiselStage).execute( Array( "--target-dir", "generated/TapeSim", "-E", "verilog") ++ args, Seq(
      ChiselGeneratorAnnotation(() => {
    val soc = LazyModule(new Rift2LinkB()(cfg))
    soc.module
  })
  ))
}


object testAll extends App {

  val config = Seq(
    (new Rift2300, "Rift2300" ),
    (new Rift2310, "Rift2310" ),
    (new Rift2320, "Rift2320" ),
    (new Rift2330, "Rift2330" ),
    (new Rift2340, "Rift2340" ),
    (new Rift2350, "Rift2350" ),
    (new Rift2360, "Rift2360" ),
    (new Rift2370, "Rift2370" ),
    (new Rift2380, "Rift2380" ),
    (new Rift2390, "Rift2390" ),
  )

  

  config.map{ cfg =>
    println("Compiling " + cfg._2)

    (new chisel3.stage.ChiselStage).execute( Array( "--target-dir", "generated/Release/"++cfg._2, "-E", "verilog" ) ++ args, Seq(
        ChiselGeneratorAnnotation(() => {
      val soc = LazyModule(new Rift2Chip(isFlatten = true)(cfg._1))
      soc.module
    })
    ))

    (new chisel3.stage.ChiselStage).execute( Array( "--target-dir", "generated/Debug/"++cfg._2, "-e", "verilog" ) ++ args, Seq(
        ChiselGeneratorAnnotation(() => {
      val soc = LazyModule(new Rift2Chip(isFlatten = false)(cfg._1))
      soc.module
    })
    ))
  }
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
import rift2Core.backend._

object testModule extends App {
  (new chisel3.stage.ChiselStage).execute(args, Seq(
      ChiselGeneratorAnnotation(() => {
    new Multiplier( Bool(), 32 )
  })
    ))
}

