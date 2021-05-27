

/*
* @Author: Ruige Lee
* @Date:   2021-03-23 10:42:59
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-03-23 19:17:18
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

package rift2Core.backend

import chisel3._
import chisel3.util._

import rift2Core.define._

class Info_rename_op extends Bundle{
  val raw = UInt(5.W)
  val phy = UInt(6.W)
}

class Info_writeback_op extends Bundle{
  val phy = UInt(6.W)
  val dnxt   = UInt(64.W)
}

class Info_commit_op extends Bundle{
  val raw = UInt(5.W)
  val phy = UInt(6.W)
}


class Regfiles extends Module{
  val io = IO(new Bundle{



    val rn_op = Vec(2, ValidIO( new Info_rename_op ))
    val wb_op = Vec(2, ValidIO( new Info_writeback_op))
    val cm_op = Vec(2, ValidIO( new Info_commit_op ))


    val files = Vec(64, Output(UInt(64.W)))
    val log = Vec(64, Output(UInt(2.W)))
    val rn_ptr = Vec(32, Output(UInt(6.W)))


    val flush = Input(Bool())
  })




  /**
  * Physical register files
  */
  val files = RegInit( VecInit(Seq.fill(64)(0.U(64.W)) ))

  /**
    * register files' operation log
    * 
    * @note "b00".U at Free
    * @note "b01".U at Rename
    * @note "b1x".U at WriteBack
    */
  val regLog = RegInit( VecInit(Seq.fill(64)( 0.U ))   ) // the first reg is wb

  /**
  * Indicate which physical register is the rename one
  * 
  * @note don't care at reset
  * @note 0.U ~ 63.U
  */
  val rename_ptr = RegInit( VecInit( Seq.fill(32)(0.U(6.W))) )

  /**
  * Indicate which physical register is the archiitecture one
  * 
  * @note don't care at reset
  * @note 0.U ~ 63.U
  */
  val archit_ptr = RegInit( VecInit( Seq.fill(32)(0.U(6.W))) )



  io.files := files
  io.log := regLog
  io.rn_ptr := rename_ptr

  for ( i <- 0 until 2 ) yield {
    when( io.wb_op(i).valid ) {
      files(io.wb_op(i).bits.phy) := io.wb_op(i).bits.dnxt
    }
  }





  val is_rn  = VecInit( io.rn_op(0).valid,    io.rn_op(1).valid )
  val rn_raw = VecInit( io.rn_op(0).bits.raw, io.rn_op(1).bits.raw )
  val rn_phy = VecInit( io.rn_op(0).bits.phy, io.rn_op(1).bits.phy )

  val is_wb  = VecInit( io.wb_op(0).valid,    io.wb_op(1).valid )
  val wb_phy = VecInit( io.wb_op(0).bits.phy, io.wb_op(1).bits.phy )

  val is_cm = VecInit( io.cm_op(0).valid,     io.cm_op(1).valid )
  val cm_raw = VecInit( io.cm_op(0).bits.raw, io.cm_op(1).bits.raw )
  val cm_phy = VecInit( io.cm_op(0).bits.phy, io.cm_op(1).bits.phy )


  for ( i <- 0 until 64 ) yield {
    for ( j <- 0 until 2 ) yield {

      regLog(i) := MuxCase( regLog(i), Array(
        ( is_cm(1) & archit_ptr(cm_raw(1)) === i.U ) -> 0.U(2.W), //when reg i is commit, the last one should be free
       
       
       
       
        ( is_cm(0) & Mux( is_cm(1) & (cm_raw(1) === cm_raw(0)), cm_phy(0) === i.U, archit_ptr(cm_raw(0)) === i.U  ) ) -> 0.U(2.W), //when reg i is commit, the last one should be free
        
        ( io.flush )                                -> Mux( archit_ptr.contains(i.U), 3.U, 0.U ),
        ( is_rn(j) & rn_phy(j) === i.U )            -> 1.U(2.W),
        ( is_wb(j) & wb_phy(j) === i.U )            -> (regLog(i) | "b10".U)
      ))
    }
  }



  when( is_cm(1) ) { archit_ptr(cm_raw(1)) := cm_phy(1) }
  when( is_cm(0) & ~(cm_raw(0) === cm_raw(1) & is_cm(1)) ) { archit_ptr(cm_raw(0)) := cm_phy(0) }


  for ( i <- 0 until 64 ) yield {
    // for ( j <- 0 until 2 ) yield {
      rename_ptr(i) := MuxCase( rename_ptr(i), Array(
        io.flush                       -> Mux(
                                            is_cm(1) & i.U === cm_raw(1), cm_phy(1),
                                            Mux(
                                              is_cm(0) & i.U === cm_raw(0), cm_phy(0), archit_ptr(i) )
                                            ),

        (is_rn(1) & i.U === rn_raw(1)) -> rn_phy(1),
        (is_rn(0) & i.U === rn_raw(0)) -> rn_phy(0)
      ))

  }





  // for ( i <- 0 until 64) yield {


  //   when(io.cm_op(i)) {
  //     // it asserts in commit state that only one archit_ptr will commit in one cycle
  //     regLog(archit_ptr(i)) := 0.U(2.W) //when reg i is commit, the last one should be free
  //     archit_ptr(i) := j.U
  //     when( io.flush ) {
  //       rename_ptr(i) := j.U				
  //     }

  //   }
  //   .elsewhen(io.flush) {
  //     regLog(i)(j) := Mux( archit_ptr(i) === j.U , 3.U , 0.U)
  //     when(~(io.cm_op(i).contains(true.B))) {
  //       rename_ptr(i) := archit_ptr(i)				
  //     }
        


  //   }
  //   .otherwise{
  //     when(io.rn_op(i)(j)) {
  //       regLog(i)(j) := 1.U(2.W)
  //       rename_ptr(i) := j.asUInt()
  //     }
  //     when(wb_op(i)(j)) {
  //       regLog(i)(j) := (regLog(i)(j) | "b10".U) //when hint will stay on 2, when normal wb will be 3
  //     }


  //   }

  
  // }


  assert( ~(io.rn_op(0).valid & io.rn_op(1).valid & (io.rn_op(0).bits.phy === io.rn_op(1).bits.phy)), "Assert Fail at Register Files, a physical register is renamed twice in one cycle, that's impossible!")
  assert( ~(io.wb_op(0).valid & io.wb_op(1).valid & (io.wb_op(0).bits.phy === io.wb_op(1).bits.phy)), "Assert Fail at Register Files, a physical register is written back twice in one cycle, that's impossible!")
  assert( ~(io.cm_op(0).valid & io.cm_op(1).valid & (io.cm_op(0).bits.phy === io.cm_op(1).bits.phy)), "Assert Fail at Register Files, a physical register is committed twice in one cycle, that's impossible!")

  // assert( ~(io.rn_op(0).valid & io.rn_op(1).valid & (io.rn_op(0).bits.raw === io.rn_op(1).bits.raw)), "Assert Fail at Register Files, a RAW register is renamed twice in one cycle, it's not allow in this version")
  // assert( ~(io.cm_op(0).valid & io.cm_op(1).valid & (io.cm_op(0).bits.raw === io.cm_op(1).bits.raw)), "Assert Fail at Register Files, a RAW register is committed twice in one cycle, it's not allow in this version")
 


}




