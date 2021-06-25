
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

class SRAMBundleR[T <: Data](private val gen: T, val cb: Int = 1) extends Bundle {
  val data = Output(Vec(cb,gen))
}

class SRAMBundleAW[T <: Data](private val gen: T, cl: Int, val cb: Int = 1) extends SRAMBundleA(cl) {
  val data = Output(Vec(cb, gen))
  val cb_sel = if (cb > 1 ) Some(Output(UInt(cb.W))) else None

  def apply(data: Vec[T], cl_sel: UInt, cb_sel: UInt):SRAMBundleAW[T] = {
    super.apply(cl_sel)
    this.data := data
    this.cb_sel.map(_ := cbMask)
    this
  }

  def apply(data: T, cl_sel: UInt, cb_sel: UInt): SRAMBundleAW[T] = {
    apply(VecInit(Seq.fill(cb)(data)), cl_sel, cb_sel)
  }

}

class SRAMReadBus[T <: Data](private val gen: T, val cl: Int, val cb: Int = 1) extends Bundle {
  val req = Decoupled(new SRAMBundleA(cl))
  val resp = Flipped(new SRAMBundleR(gen, cb))

  def apply(valid: Bool, cl: UInt) = {
    this.req.bits.apply(cl)
    this.req.valid := valid
    this
  }
}

class SRAMWriteBus[T <: Data](private val gen: T, val cl: Int, val cb: Int = 1) extends Bundle {
  val req = Decoupled(new SRAMBundleAW())

  def apply(valid: Bool, data: Vec[T], cl_sel: UInt, cb_sel: UInt): SRAMWriteBus[T] = {
    this.req.bits.apply(data, cl_sel, cb_sel)
    this.req.valid := valid
    this
  }

  def apply(valid: Bool, data: T, cl_sel: UInt, cb_sel: UInt): SRAMWriteBus[T] = {
    apply(valid,VecInit(Seq.fill(cb)(data)), cl_sel, cb_sel)
    this
  }

}

class Cache_mem[T <: Data](gen: T, cl: Int, cb: Int) extends Module {
  val io = IO(new Bundle{
    val r = Flipped(new SRAMReadBus(gen, cl, cb))
    val w = Flipped(new SRAMWriteBus(gen, cl, cb))
  })

  val wordType = UInt(gen.getWidth.W)
  val array = SyncReadMem( cl, Vec(cb, wordType))

  val (ren, wen) = ( io.r.req.valid, io.w.req.valid )

  {
    val cl_sel = io.w.req.bits.cl_sel
    val wdata = VecInit((io.w.req.bits.data).map(_.asTypeOf(wordType)))
    val cb_sel = io.w.req.bits.cb_sel.getOrElse("b1".U)
    when(wen) { array.write(cl_sel, wdata, cb_sel.asBools) }   
  }

  val raw_rdata = array.read(io.r.req.bits.cl_sel, ren)

  def need_bypass(wen: Bool, waddr: UInt, wmask: UInt, ren: Bool, raddr: UInt): UInt ={
    val need_check = RegNext(ren & wen)
    val waddr_reg = RegNext(waddr)
    val raddr_reg = RegNext(raddr)
    require(wmask.getWidth == way)
    val bypass = Fill(cb, need_check & waddr_reg === raddr_reg) & RegNext(wmask)
    bypass.asTypeOf(UInt(cb.W))
  }

  val bypass_wdata = VecInit(RegNext(io.w.req.bits.data).map(_.asTypeOf(wordType)))
  val bypass_mask  = need_bypass(io.w.req.valid, io.w.req.bits.cl_sel, io.w.req.bits.cb_sel.getOrElse("b1.U"), io.r.req.valid, io.r.req.bits.cl_sel)

  val mem_rdata =
    VecInit(bypass_mask.asBools.zip(raw_rdata).zip(bypass_wdata).map {
      case((a,b),c) => Mux(a, b, c)
    })

  val rdata = 
    Mux(
      RegNext(ren),
      mem_rdata,
      RegEnable(mem_rdata, 0.U.asTypeOf(mem_rdata), RegNext(ren))
    ).map(_.asTypeOf(gen))

  io.r.resp.data := VecInit(rdata)
  io.r.req.ready := true.B
  io.w.req.ready := true.B

}


