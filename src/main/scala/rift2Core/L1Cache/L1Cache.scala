


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
import chisel3.util.random._
import rift2Core.define._

trait L1CacheParameters {
  def dw: Int
  def bk: Int
  def cb: Int
  def cl: Int
  def aw: Int
}

case class L1cacheSetting(
  dw: Int,
  bk: Int,
  cb: Int,
  cl: Int,
  aw: Int = 32,
  dataECC: Option[String] = None
) extends L1CacheParameters

trait HasL1CacheParameters extends HasCacheParameters{
  val cacheParams: L1CacheParameters

  def dw = cacheParams.dw
  def bk = cacheParams.bk
  def cb = cacheParams.cb
  def cl = cacheParams.cl
  def aw = cacheParams.aw

  def addr_lsb = log2Ceil(dw*bk/8)
  def line_w   = log2Ceil(cl)
  def cb_w = log2Ceil(cb)

  def tag_w    = aw - addr_lsb - line_w

}

abstract class CacheModule(implicit val p: Parameters) extends MultiIOModule with HasCacheParameters { def io: Record }
abstract class CacheBundle(implicit val p: Parameters) extends Bundle with HasCacheParameters

abstract class L1CacheModule(implicit p: Parameters) extends CacheModule with HasL1CacheParameters
abstract class L1CacheBundle(implicit p: Parameters) extends CacheBundle with HasL1CacheParameters




class Cache_op extends Bundle {
  val load  = Bool()
  val store = Bool()
  val probe = Bool()
  val grant = Bool()
  val lr    = Bool()
  val sc    = Bool()
  val swap  = Bool()
  val add   = Bool()
  val and   = Bool()
  val or    = Bool()
  val xor   = Bool()
  val max   = Bool()
  val maxu  = Bool()
  val min   = Bool()
  val minu  = Bool()

  // assert(  
  //   PopCount(Seq(load, store, probe, grant, lr, sc, swap, add, and, or, xor, max, maxu, min, minu)) === 1.U
  // )
  
  def is_atom = swap | add | and | or | xor | max | maxu | min | minu
  def is_access = is_atom | load | store | lr | sc
  def is_tag_r = is_atom | load | store | lr | sc
  def is_dat_r = is_atom | load | probe | lr
  def is_tag_w = grant
  def is_dat_w = is_atom | store | sc | grant
  def is_dirtyOp = is_atom | store | sc
  def is_wb = is_atom | load | lr

}


trait Info_cache_raw extends L1CacheBundle {
  val paddr = UInt(64.W)
  val is_usi = Bool()
  val wmask  = UInt(8.W)
  val wdata  = Vec(bk,UInt(64.W))
  val op    = new Cache_op

  def tag_sel = paddr(31,32-tag_w)
  def bk_sel  = paddr(addr_lsb-1, addr_lsb-log2Ceil(bk) )
  def cl_sel  = paddr(addr_lsb+line_w-1, addr_lsb)
}


trait Info_tag_dat_rd extends L1CacheBundle {
  val rdata = Vec(cb, Vec(bk, UInt(64.W)))
  val tag   = Vec(cb, UInt(tag_w.W))
}

trait Info_sc_idx extends L1CacheBundle { val chk_idx = UInt(8.W) }

class Info_cache_rd(implicit p: Parameters) extends Info_tag_dat_rd
class Info_cache_s0s1(implicit p: Parameters) extends L1CacheBundle with Info_cache_raw with Info_sc_idx
class Info_cache_s1s2(implicit p: Parameters) extends L1CacheBundle with Info_cache_raw with Info_sc_idx with Info_tag_dat_rd


class Info_cache_sb(implicit p: Parameters) extends L1CacheBundle with Info_cache_raw {
  val rd0_phy = UInt(6.W)
}

class Info_cache_retn(implicit p: Parameters) extends L1CacheBundle with Info_sc_idx {
  val res = UInt(64.W)
  val is_load_amo = Bool()
}




class LS_entry() (implicit p: Parameters) extends L1CacheModule {
  val io = IO(new Bundle{
    val in = Flipped(new DecoupledIO(new Info_cache_s0s1))
    val out = new DecoupledIO(new Info_cache_s0s1)
  })
  def entry_num = 16
  def entry_w = log2Ceil(entry_num)

  val entry_info  = RegInit( VecInit( Seq.fill(entry_num)(0.U.asTypeOf(new Info_cache_s0s1))))
  val entry_valid = RegInit( VecInit( Seq.fill(entry_num)(false.B)))

  val wr_ptr = RegInit(0.U((entry_w+1).W))
  val rd_ptr = RegInit(0.U((entry_w+1).W))

  val full  = (wr_ptr(entry_w-1,0) === rd_ptr(entry_w-1,0)) & (wr_ptr(entry_w) =/= rd_ptr(entry_w))
  val empty = wr_ptr(entry_w,0) === rd_ptr(entry_w,0)

  when( io.in.fire ) { wr_ptr := wr_ptr + 1.U }
  when( io.out.fire ) { rd_ptr := rd_ptr + 1.U }

  when( io.in.fire ) { entry_info(wr_ptr(entry_w-1,0)) := io.in.bits }


  io.out.valid := ~empty
  io.out.bits := entry_info(rd_ptr(entry_w-1,0))

  io.in.ready :=
    ~full 
    // & (
    //   VecInit(entry_valid.zip(entry_info).map{
    //     case(a,b) => (a === false.B | b.paddr =/= io.in.bits.paddr)
    //   }
    // )).asUInt.andR
}



class L1_rd_stage()(implicit p: Parameters) extends L1CacheModule {
  val io = IO(new Bundle {
    val rd_in  = Flipped(DecoupledIO(new Info_cache_s0s1))
    val rd_out = DecoupledIO(new Info_cache_s1s2)

    val dat_addr_r = Output(UInt(aw.W))
    val dat_en_r   = Output( Vec(cb, Vec(bk, Bool()) ))
    val dat_info_r = Input( Vec(cb, Vec(bk, UInt(dw.W)) ))

    val tag_addr_r = Output(UInt(aw.W))
    val tag_en_r   = Output( Vec(cb, Bool()) )	
    val tag_info_r = Input( Vec(cb, UInt(tag_w.W)) )
  })



  val s1s2_pipe = Module( new Queue(new Info_cache_rd, 1, true, true) )

  val bk_sel = io.rd_in.bits.bk_sel


  io.tag_addr_r := io.rd_in.bits.paddr
  io.dat_addr_r := io.rd_in.bits.paddr

  for ( i <- 0 until cb ) yield {
    io.tag_en_r(i) :=
      io.rd_in.fire & io.rd_in.bits.op.is_tag_r
  }



  for ( i <- 0 until cb; j <- 0 until bk ) yield {
    io.dat_en_r(i)(j) := 
      io.rd_in.fire &
      io.rd_in.bits.op.is_dat_r & (
        io.rd_in.bits.op.probe |
        j.U === bk_sel
      )
  }





  s1s2_pipe.io.enq.valid := io.rd_in.fire

  for ( i <- 0 until cb; j <- 0 until bk ) yield { s1s2_pipe.io.enq.bits.rdata(i)(j) := io.dat_info_r(i)(j)}
  for ( i <- 0 until cb )                  yield { s1s2_pipe.io.enq.bits.tag(i) := io.tag_info_r(i) }
  
  io.rd_out.valid := s1s2_pipe.io.deq.valid

  for( i <- 0 until cb; j <- 0 until bk ) yield { io.rd_out.bits.rdata(i)(j) := s1s2_pipe.io.deq.bits.rdata(i)(j) } 
  for( i <- 0 until cb )                  yield { io.rd_out.bits.tag(i)      := s1s2_pipe.io.deq.bits.tag(i) }

  
  io.rd_out.bits.paddr    := RegEnable(io.rd_in.bits.paddr, io.rd_in.fire)
  io.rd_out.bits.wmask    := RegEnable(io.rd_in.bits.wmask, io.rd_in.fire)
  io.rd_out.bits.wdata    := RegEnable(io.rd_in.bits.wdata, io.rd_in.fire)
  io.rd_out.bits.op       := RegEnable(io.rd_in.bits.op,    io.rd_in.fire)
  io.rd_out.bits.chk_idx  := RegEnable(io.rd_in.bits.chk_idx, io.rd_in.fire)
  s1s2_pipe.io.deq.ready := io.rd_out.ready

  io.rd_in.ready := s1s2_pipe.io.enq.ready


}


class L1_wr_stage() (implicit p: Parameters) extends L1CacheModule {
  val io = IO(new Bundle{
    val wr_in  = Flipped(new DecoupledIO(new Info_cache_s1s2))
    val wr_lsReload = new DecoupledIO(new Info_cache_s0s1)
    val dcache_pop = DecoupledIO(new Info_cache_retn)

    val tag_addr_w = Output(UInt(aw.W))
    val tag_en_w = Output( Vec(cb, Bool()) )

    val dat_addr_w = Output(UInt(aw.W))
    val dat_en_w = Output( Vec(cb, Vec(bk, Bool()) ))
    val dat_info_wstrb = Output( Vec(bk, UInt((dw/8).W)) )
    val dat_info_w = Output( Vec(bk, UInt(dw.W)))

    val missUnit_req = new DecoupledIO(new Info_mshr_req)
    val writeBackUnit_req = DecoupledIO(new Info_writeBack_req)
  })

  val bk_sel = io.wr_in.bits.bk_sel
  val cl_sel = io.wr_in.bits.cl_sel
  val tag_sel = io.wr_in.bits.tag_sel


  val is_hit_oh = Wire(Vec(cb, Bool()))
  val is_hit = is_hit_oh.asUInt.orR
  // val hit_cb_sel = OHToUInt(is_hit_oh)

  val is_valid = RegInit( VecInit( Seq.fill(cl)(VecInit(Seq.fill(cb)(false.B))) ) )
  val is_dirty = RegInit( VecInit( Seq.fill(cl)(VecInit(Seq.fill(cb)(false.B))) ) )

  is_hit_oh := {
    val res = 
      for( i <- 0 until cb ) yield {
        (io.wr_in.bits.tag(i) === tag_sel) & is_valid(cl_sel)(i)        
      }
    assert(PopCount(res) <= 1.U)
    VecInit(res)
  }

  val rpl_sel = {
    val is_emptyBlock_exist = is_valid(cl_sel).contains(false.B)
    val emptyBlock_sel = is_valid(cl_sel).indexWhere( (x:Bool) => (x === false.B) )
    Mux( is_emptyBlock_exist, emptyBlock_sel, LFSR(16) )
  }
  
  val cb_sel = 
    Mux1H(Seq(
      io.wr_in.bits.op.is_access -> Mux( is_hit, OHToUInt(is_hit_oh), rpl_sel ),
      io.wr_in.bits.op.probe -> OHToUInt(is_hit_oh),
      io.wr_in.bits.op.grant -> rpl_sel
    ))

    when( io.wr_in.fire ) {
      when( io.wr_in.bits.op.probe ) { assert(is_hit) }
      when( io.wr_in.bits.op.grant ) { assert(is_valid(cl_sel).contains(false.B)) }
    }

  io.dat_addr_w := io.wr_in.bits.paddr

  for ( i <- 0 until cb; j <- 0 until bk ) yield {
    io.dat_en_w(i)(j) :=
      io.wr_in.valid &
      i.U === cb_sel &
      io.wr_in.bits.op.is_dat_w & (
        io.wr_in.bits.op.grant |
        j.U === bk_sel
      )
  }

  for ( j <- 0 until bk ) yield {
    io.dat_info_wstrb(j) :=
      Mux1H(Seq(
        (io.wr_in.bits.op.grant) -> "hFF".U,
        (io.wr_in.bits.op.is_access) -> io.wr_in.bits.wmask
      ))    
  }

  for ( j <- 0 until bk ) yield {
    io.dat_info_w(j) := io.wr_in.bits.wdata(j)
  }

  io.tag_addr_w := io.wr_in.bits.paddr

  for ( i <- 0 until cb ) yield {
    io.tag_en_w(i) :=
      (i.U === cb_sel) & io.wr_in.valid & io.wr_in.bits.op.grant
  }

  when( io.wr_in.fire ) {
    when( io.wr_in.bits.op.grant ) {
      is_valid(cl_sel)(cb_sel) := true.B
      is_dirty(cl_sel)(cb_sel) := false.B
    }
    when( io.wr_in.bits.op.is_dirtyOp ) {
      is_dirty(cl_sel)(cb_sel) := true.B
    }
    when( (io.wr_in.bits.op.is_access) & ~is_hit ) {
      is_valid(cl_sel)(cb_sel) := false.B
    }

  }


  io.wr_in.ready := io.wr_lsReload.ready | io.lu_exe_iwb.ready


  io.missUnit_req.valid := io.wr_in.fire & io.wr_in.bits.op.is_access & is_hit
  io.missUnit_req.bits.paddr := io.wr_in.bits.paddr

  io.writeBackUnit_req.valid := io.wr_in.fire & ((io.wr_in.bits.op.is_access & ~is_hit) | io.wr_in.bits.op.probe)
  io.writeBackUnit_req.bits.addr := io.wr_in.bits.paddr
  io.writeBackUnit_req.bits.data := Cat( for( j <- 0 until bk ) yield { io.wr_in.bits.rdata(cb_sel)(bk-1-j) } )

  io.writeBackUnit_req.bits.is_releaseData := io.wr_in.bits.op.is_access & ~is_hit & is_dirty(cl_sel)(cb_sel)
  io.writeBackUnit_req.bits.is_release := io.wr_in.bits.op.is_access & ~is_hit & ~is_dirty(cl_sel)(cb_sel)
  io.writeBackUnit_req.bits.is_probe := io.wr_in.bits.op.probe & ~is_dirty(cl_sel)(cb_sel)
  io.writeBackUnit_req.bits.is_probeData := io.wr_in.bits.op.probe & is_dirty(cl_sel)(cb_sel)

  io.wr_lsReload.valid := io.wr_in.fire & io.wr_in.bits.op.is_wb & ~is_hit
  assert( ~(io.wr_lsReload.valid & ~io.wr_lsReload.ready), "Assert Failed at wr_state 2, reload failed!" )

  io.wr_lsReload.bits.paddr    := io.wr_in.bits.paddr
  io.wr_lsReload.bits.wdata    := io.wr_in.bits.wdata
  io.wr_lsReload.bits.wmask    := io.wr_in.bits.wmask
  io.wr_lsReload.bits.op.grant := io.wr_in.bits.op.grant
  io.wr_lsReload.bits.op.load  := io.wr_in.bits.op.load
  io.wr_lsReload.bits.op.store := io.wr_in.bits.op.store
  io.wr_lsReload.bits.op.probe := io.wr_in.bits.op.probe
  io.wr_lsReload.bits.op.grant := io.wr_in.bits.op.grant
  io.wr_lsReload.bits.op.lr    := io.wr_in.bits.op.lr
  io.wr_lsReload.bits.op.sc    := io.wr_in.bits.op.sc
  io.wr_lsReload.bits.op.swap  := io.wr_in.bits.op.swap
  io.wr_lsReload.bits.op.add   := io.wr_in.bits.op.add
  io.wr_lsReload.bits.op.and   := io.wr_in.bits.op.and
  io.wr_lsReload.bits.op.or    := io.wr_in.bits.op.or
  io.wr_lsReload.bits.op.xor   := io.wr_in.bits.op.xor
  io.wr_lsReload.bits.op.max   := io.wr_in.bits.op.max
  io.wr_lsReload.bits.op.maxu  := io.wr_in.bits.op.maxu
  io.wr_lsReload.bits.op.min   := io.wr_in.bits.op.min
  io.wr_lsReload.bits.op.minu  := io.wr_in.bits.op.minu
  io.wr_lsReload.bits.chk_idx  := io.wr_in.bits.chk_idx

  io.dcache_pop.valid := io.wr_in.fire & io.wr_in.bits.op.is_wb & is_hit
  io.dcache_pop.bits.res := {
    val rdata = io.wr_in.bits.rdata(cb_sel)(bk_sel)
    val mask = io.wr_in.bits.wmask
    val paddr = io.wr_in.bits.paddr
    val is_usi = io.wr_in.bits.is_usi

    get_loadRes( is_usi, rdata, mask, paddr )
  }
  

  io.dcache_pop.bits.chk_idx := io.wr_in.bits.chk_idx
  io.dcache_pop.bits.is_load_amo := io.wr_in.bits.op.is_wb












  def get_loadRes( is_usi: Bool, rdata: UInt, mask: UInt, paddr: UInt ): Unit = {
    val res = Wire(UInt(64.W))

    def reAlign(rdata: UInt, paddr: UInt): Unit = {
      val res = Wire(UInt(64.W))
      res := MuxLookup(paddr(2,0), 0.U, Array(
        "b000".U -> rdata(63,0),  "b001".U -> rdata(63,8),  "b010".U -> rdata(63,16), "b011".U -> rdata(63,24),
        "b100".U -> rdata(63,32), "b101".U -> rdata(63,40), "b110".U -> rdata(63,48), "b111".U -> rdata(63,56)))    
      res
    }

    def load_byte(is_usi: Bool, rdata: UInt): UInt = Cat( Fill(56, Mux(is_usi, 0.U, rdata(7)) ),  rdata(7,0)  )
    def load_half(is_usi: Bool, rdata: UInt): UInt = Cat( Fill(48, Mux(is_usi, 0.U, rdata(15)) ), rdata(15,0) )
    def load_word(is_usi: Bool, rdata: UInt): UInt = Cat( Fill(32, Mux(is_usi, 0.U, rdata(31)) ), rdata(31,0) )

    val fun = PopCount(mask)
    assert( (fun === 1.U | fun === 2.U | fun === 4.U | fun === 8.U), "Assert Failed at L1cache.scala, unexpect read condition" )
    
    val align = reAlign(rdata, paddr)

    res := Mux1H(Seq(
      (fun === 1.U) -> load_byte(align, paddr),
      (fun === 2.U) -> load_half(align, paddr),
      (fun === 4.U) -> load_word(align, paddr),
      (fun === 8.U) -> align
    ))  

    res
  }


}



