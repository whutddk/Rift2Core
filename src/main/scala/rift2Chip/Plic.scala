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

class GatewayPLICIO extends Bundle{
  val valid = Output(Bool())
  val ready = Output(Bool())
  val complete = Input(Bool())
}

class LevelGateway extends Module{
  val io = new Bundle{
    val interrupt = Input(Bool())
    val plic = new GatewayPLICIO
  }

  val inFlight = RegInit(Bool())
  when (io.interrupt && io.plic.ready) { inFlight := true }
  when (io.plic.complete) { inFlight := false }
  io.plic.valid := io.interrupt && !inFlight
}



class Plic( nHarts: Int = 1 )(implicit p: Parameters) extends LazyModule {

  val device: SimpleDevice = new SimpleDevice("Platform-Level Interrupt Controller", Seq("wuhan university of technology, PLIC"))

  val node : TLRegisterNode = TLRegisterNode(
    address   = Seq(params.address),
    device    = device,
    beatBytes = 8,
    undefZero = true,
    concurrency = 1)


  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle{
      val interrupt = Input( Vec(1024, Bool()) )
    }

    //There are 1024 geatways
    val gateways = io.interrupts.map { case int =>
      val gateway = Module(new LevelGateway)
      gateway.io.interrupt := int
      gateway.io.plic
    }


    val prioBits = log2Ceil(nPriorities+1)
    val priority =
      if (nPriorities > 0) Reg(Vec(nDevices, UInt(width=prioBits)))
      else Wire(init=Vec.fill(nDevices max 1)(UInt(1)))
    val threshold =
      if (nPriorities > 0) Reg(Vec(nHarts, UInt(width=prioBits)))
      else Wire(init=Vec.fill(nHarts)(UInt(0)))
    val pending = Reg(init=Vec.fill(nDevices max 1){Bool(false)})


  }





}

