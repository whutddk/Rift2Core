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
import chipsalliance.rocketchip.config.Parameters

import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tilelink._

import base._



case class DcacheParameters(
  dw: Int,
  bk: Int,
  cb: Int,
  cl: Int,
  aw: Int = 32,
  dataECC: Option[String] = None
) extends L1CacheParameters

trait HasDcacheParameters extends HasL1CacheParameters {
  val cacheParams = dcacheParameters
}

abstract class DCacheModule(implicit p: Parameters) extends L1CacheModule
  with HasDcacheParameters

abstract class DcacheBundle(implicit p: Parameters) extends L1CacheBundle
  with HasDcacheParameters


class L1Metadata(implicit p: Parameters) extends DcacheBundle {
  val coh = new ClientMetadata
  val tag = UInt(tag_w.W)
}

object L1Metadata {
  def apply(tag: Bits, coh: ClientMetadata)(implicit p: Parameters) = {
    val meta = Wire(new L1Metadata)
    meta.tag := tag
    meta.coh := coh
    meta
  }
}

class L1MetaReadReq(implicit p: Parameters) extends DcacheBundle {
  val cl_sel = UInt(line_w.W)
  val cb_sel = UInt(cb.W)
  val tag = UInt(tag_w.W)

}

class L1MetaWriteReq(implicit p: Parameters) extends L1MetaReadReq {
  val data = new L1Metadata
}

class L1DataReadReq(implicit p: Parameters) extends DcacheBundle {
  val bk_sel_rd = Bits(bk.W)
  val cb_sel = Bits(cb.W)
  val addr   = Bits( (addr_lsb + line_w).W )
}

class L1DataWriteReq(implicit p: Parameters) extends L1DataReadReq {
  val bk_sel_wr = Bits(bk.W)
  val data = Vec( bk, Bits(dw.W) )
}

class ReplacementAccessBundle(implicit p:Parameters) extends DcacheBundle {
  val cl_sel = UInt(line_w.W)
  val cb_sel = UInt(cb_w.W)
}


abstract class AbstractDataArray(implicit p: Parameters) extends DCacheModule {
  val io = IO(new DcacheBundle {
    val read = Vec( 2, Flipped(DecoupledIO(new L1DataReadReq)) )
    val write = Flipped(DecoupledIO(new L1DataWriteReq))
    val resp = Output(Vec(2, Vec(bk, Bits(dw.W))))
    val nacks = Output(Vec(2, Bool()))
  })

  def pipieMap[T <: Data](f: Int => T) = VecInit((0 until 2).map(f))
}

class DuplicatedDataArray(implicit p: Parameters) extends AbstractDataArray {
  val waddr = ( io.write.bits.addr >> addr_lsb ).asUInt
  val raddrs = io.read.map( r => (r.bits.addr >> addr_lsb).asUInt )
  io.write.ready := true.B

  class DataSRAMGroup extends Module {
    val io = IO(new Bundle{
      val wen, ren = Input(Bool())
      val waddr, raddr = Input(UInt())
      val wdata = Input(UInt(dw.W))
      val cb_en_wr, cb_en_rd = Input(UInt(cb.W))
      val rdata = Output(UInt()) 
    })
  }









