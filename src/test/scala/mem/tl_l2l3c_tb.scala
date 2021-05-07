/*
* @Author: Ruige Lee
* @Date:   2021-05-07 17:28:10
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-05-07 17:37:11
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

package test.mem

import chisel3._
import chisel3.util._

import axi._
import rift2Core.cache._
import tilelink._


// class Tl_l2l3c_tb extends Module {
//   val io = IO( new Bundle {
//     val is_il1_req = Input(Bool())
//     val il1_a_info = Input(new TLchannel_a(128, 32))

//     val is_dl1_req = Input(Bool())
//     val dl1_a_info = Input(new TLchannel_a(128, 32))
	

//     val il1_data = Output(UInt(128.W))
//     val dl1_data = Output(UInt(128.W))

//   })
// }


import chiseltest._
import org.scalatest._
import chisel3.iotesters._

class l2l3_tester(dut: L2Cache) extends PeekPokeTester(dut){



  step(1000)

}

class Waveforml2l3 extends FlatSpec with Matchers {
  "WaveformSpec" should "pass" in {
	chisel3.iotesters.Driver.execute(Array("--generate-vcd-output", "on","--target-dir", "generated"), () => new L2Cache()){
	  c => new l2l3_tester(c)
	} should be (true)
  }
}

