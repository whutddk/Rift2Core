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

package base

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import rift2Core.define._

import rift._

import chisel3.util.random._
import rift2Core.define._


class DatRAM(dw: Int, cl: Int) extends Module {
  def line_w   = log2Ceil(cl)
  val io = IO(new Bundle{
    val addr  = Input(UInt(line_w.W))

    val dataw  = Input( Vec( dw/8, UInt(8.W) ) )
    val datawm = Input( Vec( dw/8, Bool()) )

    val datar = Output( Vec( dw/8, UInt(8.W) ) )
    val enw   = Input(Bool())
    val enr   = Input(Bool())
  })

  val datMem = SyncReadMem( cl, Vec( dw/8, UInt(8.W) ) )

  io.datar := DontCare
  when( io.enr ) {
    io.datar := datMem.read( io.addr )
  } .elsewhen( io.enw & ~io.enr ) {
    datMem.write( io.addr, io.dataw, io.datawm )
  }

}

class TagRAM(tag_w: Int,  cl: Int) extends Module {
  def line_w   = log2Ceil(cl)
  val io = IO(new Bundle{
    val addr  = Input(UInt(line_w.W))

    val dataw = Input( UInt(tag_w.W) )
    val datar = Output( UInt(tag_w.W) )
    val enw   = Input(Bool())
    val enr   = Input(Bool())
  })

  val tagMem = SyncReadMem( cl, UInt(tag_w.W) )


  // io.datar := DontCare
  // when( io.enw ) {
  //   tagMem.write( io.addr, io.dataw )
  // } .otherwise {
  //   io.datar := tagMem.read( io.addr )
  // }


  io.datar := DontCare
  when( io.enr ) {
    io.datar := tagMem.read( io.addr )
  } .elsewhen( io.enw & ~io.enr ) {
    tagMem.write( io.addr, io.dataw )
  }




}

