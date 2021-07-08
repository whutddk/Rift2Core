/*
* @Author: Ruige Lee
* @Date:   2021-04-19 14:43:41
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-20 12:04:14
*/


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


package rift2Chip

import chisel3._
import chisel3.util._
import rift2Core._
import cache._
import axi._

import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba.axi4._
import sifive.blocks.inclusivecache._

class Rift2Chip(implicit p: Parameters) extends LazyModule {

  val i_rift2Core = LazyModule( new Rift2Core )











  val sifiveCache = LazyModule(new InclusiveCache(
      cache = CacheParameters( level = 2, ways = 4, sets = 64, blockBytes = 256/8, beatBytes = 128/8 ),
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

  val mem_xbar = TLXbar()

  val tlbuf1 = TLBuffer() 
  tlbuf1 := i_rift2Core.clientNode
  sifiveCache.node := TLBuffer() := TLXbar() := tlbuf1

  val tlcork = TLCacheCork()
  tlcork :=* sifiveCache.node

  mem_xbar :=* TLBuffer() :=* tlcork
  memAXI4SlaveNode :=
    AXI4UserYanker() :=
    AXI4Deinterleaver(256*4/8) :=
    TLToAXI4() :=
    TLWidthWidget(128 / 8) :=
    mem_xbar

  val memory = InModuleBody {
    memAXI4SlaveNode.makeIOs()
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
      val mem_chn_ar = new DecoupledIO(new AXI_chn_a( 32, 1, 1 ))
      val mem_chn_r = Flipped( new DecoupledIO(new AXI_chn_r( 128, 1, 1)) )
      val mem_chn_aw = new DecoupledIO(new AXI_chn_a( 32, 1, 1 ))
      val mem_chn_w = new DecoupledIO(new AXI_chn_w( 128, 1 ))
      val mem_chn_b = Flipped( new DecoupledIO(new AXI_chn_b( 1, 1 )))

      val sys_chn_ar = new DecoupledIO(new AXI_chn_a( 32, 1, 1 ))
      val sys_chn_r = Flipped( new DecoupledIO(new AXI_chn_r( 64, 1, 1)) )
      val sys_chn_aw = new DecoupledIO(new AXI_chn_a( 32, 1, 1 ))
      val sys_chn_w = new DecoupledIO(new AXI_chn_w( 64, 1 )) 
      val sys_chn_b = Flipped( new DecoupledIO(new AXI_chn_b( 1, 1 )))

      val rtc_clock = Input(Bool())
    })



    val l2cache = Module( new L2Cache )
    val l3cache = Module( new L3Cache )

    i_rift2Core.module.io.sys_chn_ar <> io.sys_chn_ar
    i_rift2Core.module.io.sys_chn_r  <> io.sys_chn_r
    i_rift2Core.module.io.sys_chn_aw <> io.sys_chn_aw
    i_rift2Core.module.io.sys_chn_w  <> io.sys_chn_w
    i_rift2Core.module.io.sys_chn_b  <> io.sys_chn_b


    l2cache.io.il1_chn_a <> i_rift2Core.module.io.il1_chn_a
    l2cache.io.il1_chn_d <> i_rift2Core.module.io.il1_chn_d

    l2cache.io.dl1_chn_a.valid := false.B
    l2cache.io.dl1_chn_a.bits  := DontCare
    l2cache.io.dl1_chn_d.ready := false.B

    l2cache.io.l2c_chn_a <> l3cache.io.l2c_chn_a
    l2cache.io.l2c_chn_d <> l3cache.io.l2c_chn_d
    l2cache.io.l2c_fence_req := false.B



    l3cache.io.mem_chn_ar <> io.mem_chn_ar
    l3cache.io.mem_chn_r  <> io.mem_chn_r
    l3cache.io.mem_chn_aw <> io.mem_chn_aw
    l3cache.io.mem_chn_w  <> io.mem_chn_w
    l3cache.io.mem_chn_b  <> io.mem_chn_b
    l3cache.io.l3c_fence_req := false.B
    

    i_rift2Core.module.io.rtc_clock := io.rtc_clock
  }




}


