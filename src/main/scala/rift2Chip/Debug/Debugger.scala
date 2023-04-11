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

package debug

import chisel3._

import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._



class Debugger(nComponents: Int = 1)(implicit p: Parameters) extends LazyModule{

  val device = new SimpleDevice("debugger", Seq("wuhan university of technology,debug-013","riscv,debug-013")){
      override val alwaysExtended = true
      override def describe(resources: ResourceBindings): Description = {
        val Description(name, mapping) = super.describe(resources)
        val attach = Map(
          "debug-attach"     -> (
            Seq(ResourceString("jtag"))  ++
            Seq(ResourceString("dmi")) ))
        Description(name, mapping ++ attach)
      }
    }

  val dm = LazyModule(new DebugModule(device: Device, nComponents = nComponents))



  lazy val module = new Impl

  class Impl extends LazyModuleImp(this) {
    
    class DebuggerIO extends Bundle{
      val JtagIO = new JtagIO()
        val ndreset     = Output(Bool())
        // val dmactive    = Output(Bool())
        // val dmactiveAck = Input(Bool())
        val dm_cmm      = Vec(nComponents, new Info_DM_cmm )
        // val sba_getPut  = new DecoupledIO(new TLBundleA(edge.bundle))
        // val sba_access  = Flipped(new DecoupledIO(new TLBundleD(edge.bundle)))
    }

    val io = IO(new DebuggerIO)



    val dtm = Module(new DebugTransportModuleJTAG)
    val dmi = Module(new DMI)
    
    dmi.io.tck := io.JtagIO.TCK
    dmi.io.trstn := io.JtagIO.TRSTn

    dtm.io.JtagIO <> io.JtagIO
    dtm.io.dmi <> dmi.io.dtm_dmi
    dmi.io.dmi_dm <> dm.module.io.dmi
    io.ndreset := dm.module.io.ndreset
    // io.dmactive := dm.io.dmactive
    // dm.io.dmactiveAck     = Input(Bool())
    dm.module.io.dm_cmm <> io.dm_cmm
    // dm.io.sba_getPut <> io.sba_getPut
    // dm.io.sba_access <> io.sba_access
  }





}
