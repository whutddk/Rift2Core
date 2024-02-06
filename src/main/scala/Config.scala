package rift2Chip




import rift2Core.define.{IFParameters}
import org.chipsalliance.cde.config._

 


class Rift2300 extends Config((_, _, _) => {
  case RiftParamsKey => RiftSetting(
    hasL2  = false,
    hasDebugger = true,
    hasPreFetch = false,
    hasuBTB = false,



    ftChn = 4,

    rnChn = 1,
    opChn = 1,
    wbChn = 1,
    cmChn = 1,

    pmpNum = 0,
    xRegNum = 34,
    fRegNum = 34,
    vRegNum = 34,
    hpmNum  = 0,

    l1BeatBits = 64,
    memBeatBits = 64,

    tlbEntry = 2,


    l1DW = 128,

    ifetchParameters = IFParameters(
      uBTB_entry = 2,
      // uBTB_tag_w = 16,
      btb_cl = 0,
      bim_cl = 2,
      ras_dp = 0,
      // tage_table = 6, 
    ),
    icacheParameters = IcacheParameters(
      bk = 1,
      cb = 1,
      cl = 2,
    ),
    dcacheParameters = DcacheParameters(
      bk = 1,
      cb = 1,
      cl = 2,
      sbEntry = 1,
      stEntry = 2,
      
    ),

    dptEntry = 1,
    mulNum = 0,

    isMinArea = true,
    isLowPower = false,

  )
})



class Rift2310 extends Config((_, _, _) => {
  case RiftParamsKey => RiftSetting(
    hasL2  = true,
    hasDebugger = true,
    hasPreFetch = false,
    hasuBTB = false,



    ftChn = 4,

    rnChn = 1,
    opChn = 1,
    wbChn = 1,
    cmChn = 1,

    pmpNum = 0,
    xRegNum = 34,
    fRegNum = 34,
    vRegNum = 34,
    hpmNum  = 0,

    l1BeatBits = 64,
    memBeatBits = 64,

    tlbEntry = 2,


    l1DW = 128,

    ifetchParameters = IFParameters(
      uBTB_entry = 4,
      // uBTB_tag_w = 16,
      btb_cl = 4,
      bim_cl = 8,
      ras_dp = 4,
      // tage_table = 6, 
    ),
    icacheParameters = IcacheParameters(
      bk = 1,
      cb = 1,
      cl = 2,
    ),
    dcacheParameters = DcacheParameters(
      bk = 1,
      cb = 1,
      cl = 2,
      sbEntry = 2,
      stEntry = 2,
      
    ),

    dptEntry = 2,
    mulNum = 1,

    isMinArea = true,
    isLowPower = false,

  )
})

class Rift2320 extends Config((_, _, _) => {
  case RiftParamsKey => RiftSetting(
    hasL2  = true,
    hasDebugger = true,
    hasPreFetch = false,
    hasuBTB = false,

    ftChn = 4,

    rnChn = 1,
    opChn = 2,
    wbChn = 1,
    cmChn = 1,

    pmpNum = 0,
    xRegNum = 34,
    fRegNum = 34,
    vRegNum = 34,
    hpmNum  = 0,

    l1BeatBits = 64,
    memBeatBits = 64,

    tlbEntry = 2,


    l1DW = 128,

    ifetchParameters = IFParameters(
      uBTB_entry = 4,
      // uBTB_tag_w = 16,
      btb_cl = 4,
      bim_cl = 8,
      ras_dp = 4,
      // tage_table = 6, 
    ),
    icacheParameters = IcacheParameters(
      bk = 1,
      cb = 2,
      cl = 4,
    ),
    dcacheParameters = DcacheParameters(
      bk = 1,
      cb = 2,
      cl = 4,
      sbEntry = 2,
      stEntry = 2,
      
    ),

    dptEntry = 4,
    mulNum = 1,

    isMinArea = true,
    isLowPower = false,

  )
})

class Rift2330 extends Config((_, _, _) => {
  case RiftParamsKey => RiftSetting(
    hasL2  = true,
    hasDebugger = true,
    hasPreFetch = false,
    hasuBTB = false,

    ftChn = 4,

    rnChn = 2,
    opChn = 2,
    wbChn = 2,
    cmChn = 2,

    pmpNum = 0,
    hpmNum  = 0,

    xRegNum = 34,
    fRegNum = 34,
    vRegNum = 34,

    l1BeatBits = 64,
    memBeatBits = 64,

    tlbEntry = 2,

    ifetchParameters = IFParameters(
      uBTB_entry = 8,

      btb_cl = 4,
      bim_cl = 8,
      ras_dp = 4,
    ),

    l1DW = 128,

    icacheParameters = IcacheParameters(
      cb = 1,
      cl = 2,
    ),
    dcacheParameters = DcacheParameters(
      bk = 1,
      cb = 1,
      cl = 2,
      sbEntry = 2,
      stEntry = 2,
    ),

    dptEntry = 4,

    isMinArea = true,
    isLowPower = false,
  )
})

class Rift2330D extends Config((_, _, _) => {
  case RiftParamsKey => RiftSetting(
    hasDebugger = true,
    hasPreFetch = false,
    hasuBTB = false,
    // hasLRU = false,

    ftChn = 4,

    rnChn = 1,
    opChn = 1,
    wbChn = 1,
    cmChn = 1,

    xRegNum = 34,
    fRegNum = 34,
    vRegNum = 34,

    pmpNum = 0,
    hpmNum  = 0,

    l1BeatBits = 64,
    memBeatBits = 64,

    tlbEntry = 2,

    ifetchParameters = IFParameters(
      uBTB_entry = 8,

      btb_cl = 0,
      bim_cl = 2,
      ras_dp = 0,
    ),

    l1DW = 128,

    icacheParameters = IcacheParameters(
      cb = 1,
      cl = 2,
    ),
    dcacheParameters = DcacheParameters(
      bk = 1,
      cb = 1,
      cl = 2,
      sbEntry = 1,
      stEntry = 2,
    ),

    dptEntry = 1,
    mulNum = 0,

    isMinArea = true,
    isLowPower = false,
  )
})


class Rift2340 extends Config((_, _, _) => {
  case RiftParamsKey => RiftSetting(
    hasDebugger = true,
    hasPreFetch = false,

    opChn = 4,
    wbChn = 2,

    xRegNum = 48,
    fRegNum = 48,
    vRegNum = 48,

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

    dptEntry = 6,

    isMinArea = true,
    isLowPower = false,
  )
})

class Rift2350 extends Config((_, _, _) => {
  case RiftParamsKey => RiftSetting(
    hasDebugger = true,
    hasPreFetch = false,

    opChn = 4,
    wbChn = 4,

    xRegNum = 48,
    fRegNum = 48,
    vRegNum = 48,

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

    dptEntry = 8,

    isMinArea = true,
    isLowPower = false,
  )
})

class Rift2360 extends Config((_, _, _) => {
  case RiftParamsKey => RiftSetting(
    hasDebugger = true,
    hasPreFetch = false,

    opChn = 4,
    wbChn = 4,

    xRegNum = 54,
    fRegNum = 54,
    vRegNum = 54,

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

    dptEntry = 12,

    isMinArea = false,
    isLowPower = true,
  )
})

class Rift2370 extends Config((_, _, _) => {
  case RiftParamsKey => RiftSetting(
    // hasVector = true,

    // opChn = 2,
    // wbChn = 2,

    // pmpNum = 1,
    // hpmNum = 0,

    // icacheParameters = IcacheParameters(
    //   bk = 1,
    //   cb = 2,
    //   cl = 8
    // ),

    // dcacheParameters = DcacheParameters(
    //   bk = 2,
    //   cb = 2,
    //   cl = 8,
    //   stEntry = 16,
    //   sbEntry = 2,
    // ),

    // ifetchParameters = IFParameters(
    //   uBTB_entry = 4,
    //   btb_cl = 8,
    //   bim_cl = 8,
    //   ras_dp = 4,
    // ),

    // dptEntry = 4,

    // aluNum = 1,

    // xRegNum = 36,
    // fRegNum = 36,
    // tlbEntry = 4,
  )


})

class Rift2380 extends Config((_, _, _) => {
  case RiftParamsKey => RiftSetting(
    hasDebugger = true,
    hasPreFetch = false,

    opChn = 6,
    wbChn = 4,

    xRegNum = 96,
    fRegNum = 96,
    vRegNum = 96,

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

    dptEntry = 24,
    isMinArea = false,
    isLowPower = true,
  )
})

class Rift2390 extends Config((_, _, _) => {
  case RiftParamsKey => RiftSetting(
    hasDebugger = true,
    hasPreFetch = false,

    opChn = 8,
    wbChn = 4,

    xRegNum = 128,
    fRegNum = 128,
    vRegNum = 128,

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

    dptEntry = 32,

    isMinArea = false,
    isLowPower = true,
  )
})

