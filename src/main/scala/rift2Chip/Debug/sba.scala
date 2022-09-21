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

package debug

import chisel3._
import chisel3.util._
import rift2Chip._
import chipsalliance.rocketchip.config._
import freechips.rocketchip.tilelink._

class Info_sba_req extends Bundle{
  val paddr = UInt(64.W)
  val wdata = UInt(64.W)
  val is_byte = Bool()
  val is_half = Bool()
  val is_word = Bool()
  val is_dubl = Bool()
  val is_rd_wrn = Bool()
}

class Info_sba_rsp extends Bundle{
  val rdata = UInt(64.W)
}

class SBA(edge: TLEdgeOut)(implicit p: Parameters) extends RiftModule {
  val io = IO(new Bundle{
    val req = Flipped(Decoupled(new Info_sba_req))
    val rsp = Decoupled(new Info_sba_rsp)

    val getPut    = new DecoupledIO(new TLBundleA(edge.bundle))
    val access = Flipped(new DecoupledIO(new TLBundleD(edge.bundle)))
  })


  val is_busy = RegInit(false.B)
  val pending = Reg(new Info_sba_req)


  io.getPut.valid := io.req.valid
  io.req.ready := io.getPut.fire

  when( io.req.valid ) {
    when( io.req.bits.is_rd_wrn) {
        io.getPut.bits := 
          edge.Get(
            fromSource = 0.U,
            toAddress = io.req.bits.paddr << 3 >> 3,
            lgSize = log2Ceil(64/8).U
          )._2    
      } .elsewhen( ~io.req.bits.is_rd_wrn ) {
        io.getPut.bits :=
          edge.Put(
            fromSource = 0.U,
            toAddress = io.req.bits.paddr << 3 >> 3,
            lgSize = log2Ceil(64/8).U,
            data = io.req.bits.wdata << Cat( io.req.bits.paddr(2,0), 0.U(3.W) ),
            mask = Mux1H(Seq(
                    io.req.bits.is_byte -> "b00000001".U, io.req.bits.is_half -> "b00000011".U,
                    io.req.bits.is_word -> "b00001111".U, io.req.bits.is_dubl -> "b11111111".U
                  )) << io.req.bits.paddr(2,0)
          )._2
      } .otherwise{
        io.getPut.bits := DontCare
        assert(false.B, "Assert Failed at sba")      
      }    
  } .otherwise {
    io.getPut.bits := DontCare
  }
  


  when( io.getPut.fire ) {
    assert( is_busy === false.B  )
    when( io.req.bits.is_byte ) { assert( true.B ) }
    when( io.req.bits.is_half ) { assert( io.req.bits.paddr(0) === 0.U  , "Assert Failed, sba mis-align" ) }
    when( io.req.bits.is_word ) { assert( io.req.bits.paddr(1,0) === 0.U, "Assert Failed, sba mis-align" ) }
    when( io.req.bits.is_dubl ) { assert( io.req.bits.paddr(2,0) === 0.U, "Assert Failed, sba mis-align" ) }

    pending := io.req.bits
    is_busy := true.B
  } .elsewhen( io.access.fire ) {
    assert( is_busy === true.B  )
    is_busy := false.B
  }

  io.rsp.valid    := io.access.valid
  io.rsp.bits.rdata := {

    val paddr = pending.paddr
    val rdata = io.access.bits.data >> Cat( paddr(2,0), 0.U(3.W) )

    Mux1H(Seq(
      pending.is_byte -> rdata(7,0),
      pending.is_half -> rdata(15,0),
      pending.is_word -> rdata(31,0),
      pending.is_dubl -> rdata
    )) 
  }

  io.access.ready := io.rsp.ready

}


