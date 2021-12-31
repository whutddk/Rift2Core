

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


package test

import chisel3._
import chisel3.util._
import rift2Core._
import rift2Chip._
import rift2Core.privilege._


import chiseltest._
import org.scalatest._
import chisel3.iotesters._
import chisel3.util.experimental._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.config._
import chisel3.stage._



// class WaveformTester(dut: Rift2Chip) extends PeekPokeTester(dut){

  
    // dut.io.if_mmu.bits := 0.U
    // dut.io.if_mmu.valid := true.B

    // dut.io.mmu_if = ValidIO(new Info_mmu_rsp)

    // val iss_mmu = ValidIO(new Info_mmu_req)
    // val mmu_iss = ValidIO(new Info_mmu_rsp)

    // val csr_mmu = Input( new Info_csr_mmu )

    // val mmu_chn_a = new DecoupledIO(new TLchannel_a(64, 32))
    // val mmu_chn_d = Flipped(new DecoupledIO(new TLchannel_d(64)))


    // val flush = Input(Bool())


//   step(1000)

// }

// class WaveformSpec extends FlatSpec with Matchers {
//   val cfg = new miniCfg

//   "WaveformSpec" should "pass" in {
//     chisel3.iotesters.Driver.execute(Array("--generate-vcd-output", "on","--target-dir", "generated"), () => {
//       val mdl = LazyModule(new Rift2Chip()(cfg))
//       mdl.module
//       })
//     {
//       c => new WaveformTester(c)
//     } should be (true)
//   }
// }








// class Rift2Chip_tester extends FlatSpec with Matchers {
// 	"Rift2Chip_tester" should "pass" in {
//     Driver.execute(Array("--generate-vcd-output", "on"), () => new Rift2Chip() ){

// 	} should be (true)
// }

// }

