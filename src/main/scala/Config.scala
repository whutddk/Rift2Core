package rift2Chip

import chisel3._
import chisel3.util._

import rift2Core.define.{IFParameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.config._




class Rift2300 extends Config((site, here, up) => {
  case RiftParamsKey => RiftSetting(
    hasL2  = false,
    hasDebugger = true,
    hasPreFetch = false,
    hasuBTB = false,



    ftChn = 4,

    rnChn = 1,
    opChn = 1,
    wbChn = 1,
    cm_chn = 1,

    pmpNum = 0,
    regNum = 34,
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
    fpuNum = 0,
    mulNum = 0,

    isMinArea = true,
    isLowPower = false,

  )
})



class Rift2310 extends Config((site, here, up) => {
  case RiftParamsKey => RiftSetting(
    hasL2  = true,
    hasDebugger = true,
    hasPreFetch = false,
    hasuBTB = false,



    ftChn = 4,

    rnChn = 1,
    opChn = 1,
    wbChn = 1,
    cm_chn = 1,

    pmpNum = 0,
    regNum = 34,
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
    fpuNum = 0,
    mulNum = 1,

    isMinArea = true,
    isLowPower = false,

  )
})

class Rift2320 extends Config((site, here, up) => {
  case RiftParamsKey => RiftSetting(
    hasL2  = true,
    hasDebugger = true,
    hasPreFetch = false,
    hasuBTB = false,

    ftChn = 4,

    rnChn = 1,
    opChn = 2,
    wbChn = 1,
    cm_chn = 1,

    pmpNum = 0,
    regNum = 34,
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
    fpuNum = 0,
    mulNum = 1,

    isMinArea = true,
    isLowPower = false,

  )
})

class Rift2330 extends Config((site, here, up) => {
  case RiftParamsKey => RiftSetting(
    hasDebugger = true,
    hasPreFetch = false,
    hasuBTB = false,
    // hasLRU = false,

    ftChn = 4,

    opChn = 2,
    wbChn = 2,

      pmpNum = 0,
      hpmNum  = 0,

    regNum = 34,

    l1BeatBits = 64,
    memBeatBits = 64,

    tlbEntry = 2,

    ifetchParameters = IFParameters(
      uBTB_entry = 8,

      btb_cl = 4,
      bim_cl = 8,
      ras_dp = 4,
      // uBTB_tag_w = 16,
      // btb_cl = 4096,
      // bim_cl = 4096,
      // ras_dp = 256,
      // tage_table = 6, 
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
    fpuNum = 0,

    isMinArea = true,
    isLowPower = false,
  )
})

class Rift2330D extends Config((site, here, up) => {
  case RiftParamsKey => RiftSetting(
    hasDebugger = true,
    hasPreFetch = false,
    hasuBTB = false,
    // hasLRU = false,

    ftChn = 4,

    rnChn = 1,
    opChn = 1,
    wbChn = 1,
    cm_chn = 1,

    regNum = 34,

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
    fpuNum = 0,
    mulNum = 0,

    isMinArea = true,
    isLowPower = false,
  )
})


class Rift2340 extends Config((site, here, up) => {
  case RiftParamsKey => RiftSetting(
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

    dptEntry = 6,
    fpuNum = 1,

    isMinArea = true,
    isLowPower = false,
  )
})

class Rift2350 extends Config((site, here, up) => {
  case RiftParamsKey => RiftSetting(
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

    dptEntry = 8,
    fpuNum = 0,

    isMinArea = true,
    isLowPower = false,
  )
})

class Rift2360 extends Config((site, here, up) => {
  case RiftParamsKey => RiftSetting(
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

    dptEntry = 12,
    fpuNum = 0,

    isMinArea = false,
    isLowPower = true,
  )
})

class Rift2370 extends Config((site, here, up) => {
  case RiftParamsKey => RiftSetting(
    fpuNum = 1,
    aluNum = 2,
  )
})

class Rift2380 extends Config((site, here, up) => {
  case RiftParamsKey => RiftSetting(
    hasDebugger = true,
    hasPreFetch = false,

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

    dptEntry = 24,
    fpuNum = 1,
    isMinArea = false,
    isLowPower = true,
  )
})

class Rift2390 extends Config((site, here, up) => {
  case RiftParamsKey => RiftSetting(
    hasDebugger = true,
    hasPreFetch = false,

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

    dptEntry = 32,
    fpuNum = 1,

    isMinArea = false,
    isLowPower = true,
  )
})

