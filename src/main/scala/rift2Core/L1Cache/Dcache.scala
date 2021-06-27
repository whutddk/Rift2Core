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

    val cb_en_rd_reg = RegNext(io.cb_en_rd)
    val data_array = Array.fill(cb) {
      Module(new Cache_mem(
        Bits(dw.W),
        cl
      ))
    }

    for ( i <- 0 until cb ) yield {
      val wen = io.wen & io.cb_en_wr(i)
      data_array(i).io.w.req.valid := wen
      data_array(i).io.w.req.bits.apply(
        data = io.wdata,
        cl_sel = io.waddr
      )
      data_array(i).io.r.req.valid := io.ren3
      data_array(i).io.r.req.bits.apply( io.raddr )
    }

    // val half = nWays / 2
    val data_read = data_array.map(_.io.r.resp.data)
    // val data_left = Mux1H(r_way_en_reg.tail(half), data_read.take(half))
    // val data_right = Mux1H(r_way_en_reg.head(half), data_read.drop(half))

    // val sel_low = r_way_en_reg.tail(half).orR()
    // val row_data = Mux(sel_low, data_left, data_right)

    io.rdata := Mux1H( cb_en_rd_reg, data_read)

  }

  for ( i <- 0 until 2 ) yield {
    val raddr = raddrs(i)
    val cb_sel = io.read(i).bits.cb_sel

    val rwhazard = io.write.valid & waddr === raddr
    io.read(j).ready := ~rwhazard

    assert(~(RegNext(io.read(i).fire & PopCount(io.read(i).bits.cb_sel) > 1.U)))
    val cb_sel = RegNext(io.read(i).bits.cb_sel)

    val row_error = Wire( Vec(bk, Vec(dw, Bool())) )
    val dataGroup = Moudle(new DataSRAMGroup)

    for ( j <- 0 until bk ) yield {
      dataGroup.io.wen      := io.write.valid & io.write.bits.bk_sel_wr(j)
      dataGroup.io.cb_en_wr := io.write.bits.cb_sel
      dataGroup.io.waddr    := waddr
      dataGroup.io.wdata    := io.write.bits.data(j)
      dataGroup.io.ren      := io.read(i).valid & io.read(i).bits.bk_sel_rd(j)
      dataGroup.io.cb_en_rd := io.read(i).bits.cb_sel
      dataGroup.io.raddr    := raddr


      val data_resp_chosen = Wire( Vec(dw/64, Bits(64.W)) )
      data_resp_chosen := dataGroup.io.rdata.asTypeOf(data_resp_chosen)

      io.resp(i)(j) := Cat( 0 until dw/64 ) reverseMap {
        k => data_resp_chosen(k)
      }
    }
  }
}

class L1MetadataArray()(implicit p: Parameters) extends DCacheModule {


  val io = IO(new Bundle {
    val read = Flipped(DecoupledIO(new L1MetaReadReq))
    val write = Flipped(DecoupledIO(new L1MetaWriteReq))
    val resp = Output(Vec(cb, UInt(encMetaBits.W)))
  })
}









