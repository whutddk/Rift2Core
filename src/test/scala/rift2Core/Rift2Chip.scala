/*
* @Author: Ruige Lee
* @Date:   2021-04-19 14:43:41
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-19 14:49:46
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


package test

// import chisel3._
import chisel3.util._
import rift2Core._
import rift2Chip._

import chiseltest._
import org.scalatest._
import chisel3.iotesters._

// class Rift2Chip extends Module {
// 	val io = IO( new Bundle{
		
// 	})


// 	val i_rift2Core = Module( new Rift2Core )
// 	val iccm = Module( new Tl_CCM )
// 	val dccm = Module( new Tl_CCM )
// 	val sccm = Module( new Tl_CCM )

// 	iccm.io.ccm_chn_a <> i_rift2Core.io.il1_chn_a
// 	iccm.io.ccm_chn_d <> i_rift2Core.io.il1_chn_d
// 	dccm.io.ccm_chn_a <> i_rift2Core.io.dl1_chn_a
// 	dccm.io.ccm_chn_d <> i_rift2Core.io.dl1_chn_d

// 	sccm.io.ccm_chn_a <> i_rift2Core.io.sys_chn_a
// 	sccm.io.ccm_chn_d <> i_rift2Core.io.sys_chn_d

// 	i_rift2Core.io.l2c_fence_end := false.B
// 	i_rift2Core.io.l3c_fence_end := false.B

	
// }




class WaveformTester(dut: Rift2Chip) extends PeekPokeTester(dut){
	step(1000)

}

class WaveformSpec extends FlatSpec with Matchers {
  "WaveformSpec" should "pass" in {
    Driver.execute(Array("--generate-vcd-output", "on","--target-dir", "generated"), () => new Rift2Chip()){
      c => new WaveformTester(c)
    } should be (true)
  }
}








// class Rift2Chip_tester extends FlatSpec with Matchers {
// 	"Rift2Chip_tester" should "pass" in {
//     Driver.execute(Array("--generate-vcd-output", "on"), () => new Rift2Chip() ){

// 	} should be (true)
// }

// }

