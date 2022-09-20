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
    hasL2  = false,
    hasFpu = false,
    hasDebugger = true,
    hasPreFetch = false,
    hasuBTB = false,
    hasMulDiv = true,

    ftChn = 4,

    rn_chn = 1,
    opChn = 1,
    wbChn = 1,
    cm_chn = 1,

    pmpNum = 0,
    regNum = 34,
    hpmNum  = 0,

    l1BeatBits = 64,
    memBeatBits = 64,

    tlbEntry = 2,

    ifetchParameters = IFParameters(
      uBTB_entry = 4,
      // uBTB_tag_w = 16,
      btb_cl = 4,
      bim_cl = 8,
      ras_dp = 4,
      // tage_table = 6, 
    ),
    icacheParameters = IcacheParameters(
      dw = 128,
      bk = 1,
      cb = 2,
      cl = 2,
    ),
    dcacheParameters = DcacheParameters(
      dw = 128,
      bk = 1,
      cb = 2,
      cl = 2,
      sbEntry = 2,
      stEntry = 2,
      
    ),


    isMinArea = true,
    isLowPower = false,

  )
})

// class Rift2300 extends Config((site, here, up) => {
//   case RiftParamsKey => RiftSetting(

//     hasFpu = false,
//     hasDebugger = false,
//     hasPreFetch = false,

//     isMinArea = true,
//     isLowPower = false,

//     rn_chn = 1,
//     cm_chn = 1,
//     opChn = 2,
//     wbChn = 2,

//     regNum = 33,

//     l1BeatBits = 64,
//     memBeatBits = 64,


//     tlbEntry = 2, 
//     ifetchParameters = IFParameters(
//       uBTB_entry = 4,
//       btb_cl = 64,
//       bim_cl = 64,
//       ras_dp = 8,
//       tage_table = 1, 
//     ),

//     icacheParameters = IcacheParameters(
//       cb = 2,
//     ),
//     dcacheParameters = DcacheParameters(
//       bk = 1,
//       cb = 2,
//       sbEntry = 2,
//       stEntry = 2,
//     ),
//   )
// })

// class Rift2310 extends Config((site, here, up) => {
//   case RiftParamsKey => RiftSetting(
//     hasFpu = false,
//     hasDebugger = false,
//     hasPreFetch = false,

//     isMinArea = true,
//     isLowPower = false,

//     rn_chn = 1,
//     cm_chn = 1,
//     opChn = 2,
//     wbChn = 2,

//     regNum = 36,

//     l1BeatBits = 64,
//     memBeatBits = 64,


//     tlbEntry = 2, 
//     ifetchParameters = IFParameters(
//       uBTB_entry = 4,
//       btb_cl = 128,
//       bim_cl = 128,
//       ras_dp = 16,
//       tage_table = 1, 
//     ),

//     icacheParameters = IcacheParameters(
//       cb = 2,
//     ),
//     dcacheParameters = DcacheParameters(
//       bk = 1,
//       cb = 2,
//       sbEntry = 4,
//       stEntry = 4,
//     ),
//   )
// })

// class Rift2320 extends Config((site, here, up) => {
//   case RiftParamsKey => RiftSetting(
//     hasFpu = false,
//     hasDebugger = false,
//     hasPreFetch = false,

//     isMinArea = true,
//     isLowPower = false,

//     rn_chn = 1,
//     cm_chn = 1,
//     opChn = 2,
//     wbChn = 2,

//     regNum = 38,

//     l1BeatBits = 64,
//     memBeatBits = 64,


//     tlbEntry = 2, 
//     ifetchParameters = IFParameters(
//       uBTB_entry = 4,
//       btb_cl = 128,
//       bim_cl = 128,
//       ras_dp = 16,
//       tage_table = 1, 
//     ),

//     icacheParameters = IcacheParameters(
//       cb = 2,
//     ),
//     dcacheParameters = DcacheParameters(
//       bk = 1,
//       cb = 2,
//       sbEntry = 4,
//       stEntry = 4,
//     ),
//   )
// })

class Rift2330 extends Rift2GoCfg

class Rift2340 extends Config((site, here, up) => {
  case RiftParamsKey => RiftSetting(
    hasFpu = true,
    hasDebugger = true,
    hasPreFetch = false,

    opChn = 4,
    wbChn = 2,

    regNum = 48,

    l1BeatBits = 64,
    memBeatBits = 64,

    tlbEntry = 4,

    ifetchParameters = IFParameters(
      uBTB_entry = 8,
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
      sbEntry = 8,
      stEntry = 8,
    ),


    isMinArea = true,
    isLowPower = false,
  )
})

class Rift2350 extends Config((site, here, up) => {
  case RiftParamsKey => RiftSetting(
    hasFpu = false,
    hasDebugger = true,
    hasPreFetch = false,

    opChn = 4,
    wbChn = 4,

    regNum = 48,

    l1BeatBits = 128,
    memBeatBits = 128,

    tlbEntry = 8,

    ifetchParameters = IFParameters(
      uBTB_entry = 16,
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
      bk = 2,
      cb = 2,
      sbEntry = 8,
      stEntry = 8,
    ),


    isMinArea = true,
    isLowPower = false,
  )
})

class Rift2360 extends Config((site, here, up) => {
  case RiftParamsKey => RiftSetting(
    hasFpu = false,
    hasDebugger = true,
    hasPreFetch = false,

    opChn = 4,
    wbChn = 4,

    regNum = 54,

    l1BeatBits = 128,
    memBeatBits = 128,

    tlbEntry = 8,

    ifetchParameters = IFParameters(
      uBTB_entry = 16,
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
      bk = 2,
      cb = 4,
      sbEntry = 12,
      stEntry = 16,
    ),


    isMinArea = false,
    isLowPower = true,
  )
})

class Rift2370  extends NormalCfg

class Rift2380 extends Config((site, here, up) => {
  case RiftParamsKey => RiftSetting(
    hasFpu = true,
    hasDebugger = true,
    hasPreFetch = true,

    opChn = 6,
    wbChn = 4,

    regNum = 96,

    l1BeatBits = 128,
    memBeatBits = 128,

    tlbEntry = 24,

    ifetchParameters = IFParameters(
      uBTB_entry = 24,
      // uBTB_tag_w = 16,
      // btb_cl = 4096,
      // bim_cl = 4096,
      // ras_dp = 256,
      // tage_table = 6, 
    ),
    icacheParameters = IcacheParameters(
      cb = 8,
    ),
    dcacheParameters = DcacheParameters(
      bk = 4,
      cb = 8,
      sbEntry = 24,
      stEntry = 32,
    ),


    isMinArea = false,
    isLowPower = true,
  )
})

class Rift2390 extends Config((site, here, up) => {
  case RiftParamsKey => RiftSetting(
    hasFpu = true,
    hasDebugger = true,
    hasPreFetch = true,

    opChn = 8,
    wbChn = 4,

    regNum = 128,

    l1BeatBits = 128,
    memBeatBits = 128,

    tlbEntry = 32,

    ifetchParameters = IFParameters(
      uBTB_entry = 32,
      // uBTB_tag_w = 16,
      // btb_cl = 4096,
      // bim_cl = 4096,
      // ras_dp = 256,
      // tage_table = 6, 
    ),
    icacheParameters = IcacheParameters(
      cb = 8,
    ),
    dcacheParameters = DcacheParameters(
      bk = 8,
      cb = 8,
      sbEntry = 32,
      stEntry = 32,
    ),


    isMinArea = false,
    isLowPower = true,
  )
})


object testMain extends App {

  // val cfg = new NormalCfg
  val cfg = new Rift2GoCfg
  // val cfg = new Rift2350
  // val cfg = new Rift2370

  (new chisel3.stage.ChiselStage).execute( Array("--show-registrations", "--full-stacktrace", "--target-dir", "generated/Main") ++ args, Seq(
      ChiselGeneratorAnnotation(() => {
    val soc = LazyModule(new Rift2Chip()(cfg))
    soc.module
  })
  ))
}

object tapeMain extends App {

  val cfg = new Rift2GoCfg
  // val cfg = new Rift2330
  // val cfg = new Rift2350
  // val cfg = new Rift2370

  (new chisel3.stage.ChiselStage).execute( Array( "--target-dir", "generated/Main", "-E", "verilog") ++ args, Seq(//, "--gen-mem-verilog", "true"
      ChiselGeneratorAnnotation(() => {
    val soc = LazyModule(new Rift2Link()(cfg))
    soc.module
  })
  ))

  (new chisel3.stage.ChiselStage).execute( Array(  "--target-dir", "generated/TapeMain", "-E", "verilog") ++ args, Seq(//, "--gen-mem-verilog", "true"
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
    // (new Rift2300, "Rift2300" ),
    // (new Rift2310, "Rift2310" ),
    // (new Rift2320, "Rift2320" ),
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

