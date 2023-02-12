
/*
  Copyright (c) 2020 - 2023 Wuhan University of Technology <295054118@whut.edu.cn>

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

import rift2Core.define._

import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._
import chipsalliance.rocketchip.config._

class AClint( nTiles: Int = 1 )(implicit p: Parameters) extends LazyModule {
  require( nTiles < 4096 && nTiles > 0)

  // aclint0 => at most 4095 devices
  val device = new SimpleDevice("AClint", Seq("wuhan university of technology, AClint")) {
    override val alwaysExtended = true
  }

  val node: TLRegisterNode = TLRegisterNode(
    address   = Seq(AddressSet(0x00020000L, 0x0000ffffL)),
    device    = device,
    beatBytes = 8)

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val rtc_clock = Input(Bool())
      val int = Output( Vec(nTiles, new AClint_Bundle) )
    })

    val rtc = ShiftRegisters( io.rtc_clock, 4 )

    val mtime = RegInit( 0.U(64.W) )
    when (rtc(3) ^ rtc(2)) { mtime := mtime + 1.U }

    val mtimecmp = Seq.fill(nTiles) { RegInit(0.U(64.W)) } 
    val mipi     = Seq.fill(nTiles) { RegInit( 0.U(1.W)) }
    val sipi     = Seq.fill(nTiles) { RegInit( 0.U(1.W)) }


    ( 0 until nTiles ).map{ i => {
        io.int(i).msi := ShiftRegister(mipi(i), 2) // msip
        io.int(i).ssi := ShiftRegister(sipi(i), 2) // ssip
        io.int(i).mti := ShiftRegister(mtime >= mtimecmp(i), 2) // mtip
        io.int(i).sti := false.B
      }
    }


    node.regmap(
      0x0000 -> RegFieldGroup("mtimecmp", Some("MTIMECMP for hart x"), ( 0 until nTiles ).map{ i=>
        RegField(64, mtimecmp(i), RegFieldDesc(s"mtimecmp_$i", s"mtimecmp_$i", reset=None))
      }),
      0x8000 -> RegFieldGroup("mtime", Some("Timer Register"), Seq(
        RegField(64, mtime, RegFieldDesc("mtime", "mtime", reset=Some(0), volatile=true)))),
      0x9000 -> RegFieldGroup ("msip", Some("MSIP Bits"), (0 until nTiles).map{ i =>
        RegField(1, mipi(i), RegFieldDesc(s"msip_$i", s"MSIP bit for Hart $i", reset=Some(0)))}),
      0xA000 -> RegFieldGroup ("ssip", Some("SSIP Bits"), (0 until nTiles).map{ i =>
        RegField(1, sipi(i), RegFieldDesc(s"ssip_$i", s"SSIP bit for Hart $i", reset=Some(0)))})
    )
  }
}

