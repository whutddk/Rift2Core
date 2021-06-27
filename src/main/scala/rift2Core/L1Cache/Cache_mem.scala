
/*
  Copyright (c) 2020 - 2021 Ruige Lee <m201772520@hust.edu.cn>

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

package rift2Core.L1Cache

import chisel3._
import chisel3.util._

import base._


class SRAMBundleA(val cl: Int) extends Bundle {
  val cl_sel = Output(UInt(log2Ceil(cl).W))

  def apply(cl_sel: UInt) = {
    this.cl_sel := cl_sel
    this
  }
}

class SRAMBundleR[T <: Data](private val gen: T) extends Bundle {
  val data = Output(gen)
}

class SRAMBundleAW[T <: Data](private val gen: T, cl: Int) extends SRAMBundleA(cl) {
  val data = Output( gen )

  def apply(data: T, cl_sel: UInt): SRAMBundleAW[T] = {
    super.apply(cl_sel)
    this.data := data
    this
  }


}

class SRAMReadBus[T <: Data](private val gen: T, val cl: Int) extends Bundle {
  val req = Decoupled(new SRAMBundleA(cl))
  val resp = Flipped(new SRAMBundleR(gen))

  def apply(valid: Bool, cl: UInt) = {
    this.req.bits.apply(cl)
    this.req.valid := valid
    this
  }
}

class SRAMWriteBus[T <: Data](private val gen: T, val cl: Int) extends Bundle {
  val req = Decoupled(new SRAMBundleAW())

  def apply(valid: Bool, data: T, cl_sel: UInt): SRAMWriteBus[T] = {
    this.req.bits.apply(data, cl_sel)
    this.req.valid := valid
    this
  }

}

class Cache_mem[T <: Data](gen: T, cl: Int) extends Module {
  val io = IO(new Bundle{
    val r = Flipped(new SRAMReadBus(gen, cl))
    val w = Flipped(new SRAMWriteBus(gen, cl))
  })

  val wordType = UInt(gen.getWidth.W)
  val array = SyncReadMem( cl, wordType )

  val (ren, wen) = ( io.r.req.valid, io.w.req.valid )

  {
    val cl_sel = io.w.req.bits.cl_sel
    val wdata = (io.w.req.bits.data).asTypeOf(wordType)

    when(wen) { array.write(cl_sel, wdata) }   
  }

  val raw_rdata = array.read(io.r.req.bits.cl_sel, ren)

  def need_bypass(wen: Bool, waddr: UInt, ren: Bool, raddr: UInt): UInt ={
    val need_check = RegNext(ren & wen)
    val waddr_reg = RegNext(waddr)
    val raddr_reg = RegNext(raddr)

    val bypass = need_check & waddr_reg === raddr_reg 
    bypass
  }

  val bypass_wdata = RegNext(io.w.req.bits.data).asTypeOf(wordType)
  val bypass_mask  = need_bypass(io.w.req.valid, io.w.req.bits.cl_sel, io.r.req.valid, io.r.req.bits.cl_sel)

  val mem_rdata =
    Mux( bypass_mask.asBools, bypass_wdata, raw_rdata )


  val rdata = 
    Mux(
      RegNext(ren),
      mem_rdata,
      RegEnable(mem_rdata, 0.U.asTypeOf(mem_rdata), RegNext(ren))
    ).asTypeOf(gen)

  io.r.resp.data := rdata
  io.r.req.ready := true.B
  io.w.req.ready := true.B

}


