
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
import rift2Core._
import axi._
import debug._

import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba.axi4._
import sifive.blocks.inclusivecache._


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


















class Rift2Chip(implicit p: Parameters) extends LazyModule {

  val i_rift2Core = LazyModule( new Rift2Core )
  val i_debugger = LazyModule(new Debugger(nComponents = 1))

  val sifiveCache = LazyModule(new InclusiveCache(
      cache = CacheParameters( level = 2, ways = 8, sets = 2048, blockBytes = 256/8, beatBytes = 128/8 ),
      micro = InclusiveCacheMicroParameters( writeBytes = 128/8, memCycles = 40, portFactor = 4),
      control = None
    ))


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


  val l2_xbar128 = TLXbar()
  val l2_xbar64 = TLXbar()
  val l1_xbar128 = TLXbar()
  val l1_xbar64 = TLXbar()
  // val tlcork = TLCacheCork()




  memAXI4SlaveNode := 
    AXI4UserYanker() := 
    AXI4IdIndexer(4) :=
    AXI4Deinterleaver(256/8) :=
    TLToAXI4() :=
    TLWidthWidget(128 / 8) := l2_xbar128

  sysAXI4SlaveNode := 
    AXI4UserYanker() := 
    AXI4IdIndexer(4) :=
    AXI4Deinterleaver(256/8) :=
    TLToAXI4() :=
    TLWidthWidget(64 / 8) := l2_xbar64

    l2_xbar128 :=* TLBuffer() :=* TLCacheCork() := sifiveCache.node := TLBuffer() := l1_xbar128
    l2_xbar128 := TLBuffer() := TLWidthWidget(64 / 8) := l1_xbar64
    l2_xbar64 :=  TLBuffer() := TLWidthWidget(128 / 8) := TLBuffer()  :=l1_xbar128    
    l2_xbar64 :=  TLBuffer() :=  l1_xbar64



    l1_xbar128 := TLBuffer() := i_rift2Core.icacheClientNode
    l1_xbar128 := TLBuffer() := i_rift2Core.dcacheClientNode
    l1_xbar128 := TLBuffer() := i_rift2Core.mmuClientNode
    l1_xbar128 := TLBuffer() := i_rift2Core.prefetchClinetNode


   l1_xbar64 := TLBuffer() := i_rift2Core.systemClientNode
   l1_xbar64 := TLBuffer() := i_rift2Core.periphClientNode
   l1_xbar64 := TLBuffer() := i_debugger.dm.sbaClientNode

  i_debugger.dm.peripNode := TLBuffer():= TLFragmenter(8, 32) := TLBuffer() := l2_xbar64


  val memory = InModuleBody {
    memAXI4SlaveNode.makeIOs()
  }

  val system = InModuleBody {
    sysAXI4SlaveNode.makeIOs()
  }
  

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
      val JtagIO = new JtagIO()
      val ndreset     = Output(Bool())


      val rtc_clock = Input(Bool())
    })
  
      // val JtagIO = Flipped(new JtagIO())
      // val ndreset     = Output(Bool())
      // val dm_cmm      = new Info_DM_cmm(nComponents)

    i_debugger.module.io.JtagIO <> io.JtagIO
    io.ndreset := i_debugger.module.io.ndreset
    i_debugger.module.io.dm_cmm(0) <> i_rift2Core.module.io.dm
    i_rift2Core.module.io.rtc_clock := io.rtc_clock
  }




}


