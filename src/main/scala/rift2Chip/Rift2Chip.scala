
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


package rift2Chip

import chisel3._
import chisel3.util._
import rift._
import rift2Core._
// import axi._
import debug._

import rift2Core.define._

import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba.axi4._
import sifive.blocks.inclusivecache._

import sifive.blocks.devices.chiplink._


/*

periph(64)        sba(64)      sys(64)      l1i(128*2)      l1d(128*2)     mmu(128*2) 
    \                 |            /             |               |              |
     \               |            /              \               |              /
   
   
                 /     |                                          |
               /      |                                           |
             /       |                                            |
           /         |                                      L2cache(128-128)   
         /           |                                         |
    io(64)       io(64)                                       /
                                                             mem_xbar(128)
                                                                |
                                                             io(128)
*/


















class Rift2Chip(isFlatten: Boolean = false)(implicit p: Parameters) extends LazyModule with HasRiftParameters{

  val i_rift2Core = LazyModule( new Rift2Core(isFlatten) )
  val i_debugger = if ( hasDebugger) {Some(LazyModule(new Debugger(nComponents = 1)))} else {None}
  val i_aclint = LazyModule( new AClint( nTiles = 1 ) )
  val nDevices = 31
  val i_plic = LazyModule( new Plic( nHarts = 1, nPriorities = 8, nDevices = nDevices ))
  val sifiveCache = LazyModule(new InclusiveCache(
      cache = CacheParameters( level = 2, ways = 8, sets = 2048, blockBytes = 256/8, beatBytes = l1BeatBits/8 ),
      micro = InclusiveCacheMicroParameters( writeBytes = memBeatBits/8, memCycles = 40, portFactor = 4),
      control = None
    ))

  // val ChipLinkParam = ChipLinkParams(
  //   TLUH = Seq(AddressSet(0x00000000L, 0x7FFFFFFFL)),
  //   TLC  = Seq(AddressSet(0x100000000L, 0xFFFFFFFFL)),
  //   sourceBits = 4,
  //   sinkBits = 4,
  //   syncTX = false,
  //   fpgaReset = false
  // )
  // val chiplink = LazyModule(new ChipLink(ChipLinkParam))
  // val chipIO = InModuleBody {chiplink.ioNode.makeSink.makeIO()}

  val chipMaster = LazyModule(new ChipLinkMaster)

  val memRange = AddressSet(0x00000000L, 0xffffffffL).subtract(AddressSet(0x0L, 0x7fffffffL))
  val memAXI4SlaveNode = AXI4SlaveNode(Seq(
    AXI4SlavePortParameters(
      slaves = Seq(
        AXI4SlaveParameters(
          address = memRange,
          regionType = RegionType.UNCACHED,
          executable = true,
          supportsRead = TransferSizes(1, 256/8),
          supportsWrite = TransferSizes(1, 256/8)
        )
      ),
      beatBytes = 128 / 8
    )
  ))

  val sysRange = AddressSet(0x00000000L, 0x7fffffffL).subtract(AddressSet(0x00000000L, 0x1fffffffL))
  val sysAXI4SlaveNode = AXI4SlaveNode(Seq(
    AXI4SlavePortParameters(
      slaves = Seq(
        AXI4SlaveParameters(
          address = sysRange,
          regionType = RegionType.UNCACHED,
          executable = true,
          supportsRead = TransferSizes(1, 256/8),
          supportsWrite = TransferSizes(1, 256/8)
        )
      ),
      beatBytes = 64 / 8
    )
  ))

  // val axiRam =
  //   AXI4RAM(
  //     address = AddressSet(0x80000000L, 0x0fffffffL),
  //     cacheable = true,
  //     parentLogicalTreeNode = None,
  //     executable = true,
  //     beatBytes = 128/8,
  //     devName = Some("ddr"),
  //     errors  = Nil,
  //     wcorrupt = false
  //   )


  val l2_xbarMem = TLXbar()
  val l2_xbar64 = TLXbar()
  val l1_xbarMem = TLXbar()
  val l1_xbar64 = TLXbar()
  // val tlcork = TLCacheCork()

  // val chipline_xbar64 = TLXbar()




        memAXI4SlaveNode := 
          AXI4UserYanker() := 
          AXI4IdIndexer(4) :=
          AXI4Deinterleaver(256/8) :=
          TLToAXI4() :=
          TLWidthWidget(memBeatBits / 8) := l2_xbarMem



        // sysAXI4SlaveNode := 
        //   AXI4UserYanker() := 
        //   AXI4IdIndexer(4) :=
        //   AXI4Deinterleaver(256/8) :=
        //   TLToAXI4() :=
        //   TLWidthWidget(64 / 8) := l2_xbar64

    sysAXI4SlaveNode :=
          AXI4UserYanker() := 
          AXI4IdIndexer(4) :=
          AXI4Deinterleaver(256/8) :=
          TLToAXI4() :=
          TLWidthWidget(64 / 8) := l2_xbar64

    // chiplink.node := chipline_xbar64
    // chipline_xbar64 := TLWidthWidget(memBeatBits / 8) := l2_xbarMem
    // chipline_xbar64 := l2_xbar64

    l2_xbarMem :=* TLBuffer() :=* TLCacheCork() := sifiveCache.node := TLBuffer() := l1_xbarMem
    l2_xbarMem := TLBuffer() := TLWidthWidget(64 / 8) := l1_xbar64
    l2_xbar64 :=  TLBuffer() := TLWidthWidget(l1BeatBits / 8) := TLBuffer()  := l1_xbarMem    
    l2_xbar64 :=  TLBuffer() :=  l1_xbar64



    l1_xbarMem := TLBuffer() := i_rift2Core.icacheClientNode
    l1_xbarMem := TLBuffer() := i_rift2Core.dcacheClientNode
    l1_xbarMem := TLBuffer() := i_rift2Core.mmuClientNode
    l1_xbarMem := TLBuffer() := i_rift2Core.prefetchClinetNode


    l1_xbar64 := TLBuffer() := i_rift2Core.systemClientNode
    l1_xbar64 := TLBuffer() := i_rift2Core.periphClientNode

    if( hasDebugger ) {
      // l1_xbar64 := TLBuffer() := i_debugger.get.dm.sbaClientNode
      i_debugger.get.dm.peripNode := TLBuffer():= TLFragmenter(8, 32) := TLBuffer() := l2_xbar64      
    }

    i_aclint.node := TLBuffer() := l2_xbar64
    i_plic.node   := TLBuffer() := l2_xbar64


  val memory = InModuleBody {
    memAXI4SlaveNode.makeIOs()
  }

  // val system = InModuleBody {
  //   sysAXI4SlaveNode.makeIOs()
  // }
  

  // val managerParameters = TLSlavePortParameters.v1(
  //     managers = Seq(TLSlaveParameters.v1(
  //       address = Seq(AddressSet(0x1000, 0xfff)),
  //       regionType = RegionType.CACHED,
  //       supportsAcquireT = TransferSizes(32),
  //       supportsAcquireB = TransferSizes(32),
  //       alwaysGrantsT = true
  //     )),
  //     beatBytes = 256/8,
  //     endSinkId = 1
  // )

  // val managerNode = TLManagerNode(portParams = Seq(managerParameters))

  
  // val memory1 = InModuleBody {
  //   managerNode.makeIOs()
  // }
      



  // managerNode := l2xbar := TLBuffer() := mdl.clientNode




  lazy val module = new LazyModuleImp(this) {
    val io = IO( new Bundle{
      val JtagIO  = if (hasDebugger) {Some(new JtagIO())} else { None }
      val ndreset = if (hasDebugger) {Some(Output(Bool()))}  else { None }

      val interrupt = Input( Vec(nDevices, Bool()) )
      val rtc_clock = Input(Bool())

      val fpga_io = chiselTypeOf(chipMaster.module.fpga_io)
    })

    (chipMaster.slave zip sysAXI4SlaveNode.in) foreach { case (io, (bundle, _)) => io <> bundle }

    io.fpga_io <> chipMaster.module.fpga_io

    if( hasDebugger ) {
      i_debugger.get.module.io.JtagIO <> io.JtagIO.get
      io.ndreset.get := i_debugger.get.module.io.ndreset
      i_debugger.get.module.io.dm_cmm(0) <> i_rift2Core.module.io.dm.get
    }



    i_rift2Core.module.io.rtc_clock := io.rtc_clock


    i_aclint.module.io.rtc_clock := io.rtc_clock
    i_rift2Core.module.io.aclint := i_aclint.module.io.int(0)
    i_rift2Core.module.io.plic.mei := i_plic.module.io.context(0)
    i_rift2Core.module.io.plic.sei := false.B

    i_plic.module.io.interrupt := io.interrupt
  }




}


