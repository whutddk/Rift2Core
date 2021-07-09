


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
  val fun = new Lsu_isa

  val probe = Bool()
  val grant = Bool()

  def is_atom = fun.is_amo
  def is_access = is_atom | fun.is_lu | fun.is_su | fun.lr | fun.sc
  def is_tag_r = is_atom | fun.is_lu | fun.is_su | fun.lr | fun.sc
  def is_dat_r = is_atom | fun.is_lu | probe | fun.lr
  def is_tag_w = grant
  def is_dat_w = is_atom | fun.is_su | fun.sc | grant
  def is_dirtyOp = is_atom | fun.is_su | fun.sc
  def is_wb = is_atom | fun.is_lu | fun.lr

}


trait Info_cache_raw extends L1CacheBundle {
  val paddr = UInt(64.W)
  val wmask  = UInt(8.W)
  val wdata  = Vec(bk,UInt(64.W))
  val op    = new Cache_op

  def tag_sel = paddr(31,32-tag_w)
  def bk_sel  = paddr(addr_lsb-1, addr_lsb-log2Ceil(bk) )
  def cl_sel  = paddr(addr_lsb+line_w-1, addr_lsb)
}


trait Info_tag_dat extends L1CacheBundle {
  val rdata = Vec(cb, Vec(bk, UInt(64.W)))
  val tag   = Vec(cb, UInt(tag_w.W))
}

trait Info_sc_idx extends L1CacheBundle { val chk_idx = UInt(8.W) }

class Info_cache_rd(implicit p: Parameters) extends Info_tag_dat
class Info_cache_s0s1(implicit p: Parameters) extends L1CacheBundle with Info_cache_raw with Info_sc_idx
class Info_cache_s1s2(implicit p: Parameters) extends L1CacheBundle with Info_cache_raw with Info_sc_idx with Info_tag_dat


class Info_cache_sb extends Lsu_iss_info

class Info_cache_retn(implicit p: Parameters) extends L1CacheBundle with Info_sc_idx {
  val res = UInt(64.W)
  val is_load_amo = Bool()
}




/** the fisrt stage to read out the data */
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

  /** a bypass fifo to store the read out result */
  // val s1s2_pipe = Module( new Queue(new Info_cache_rd, 1, false, true) )

  val bk_sel = io.rd_in.bits.bk_sel
  val info_bypass_fifo = Module(new Queue(new Info_cache_s0s1, 1, true, false))

  io.tag_addr_r := io.rd_in.bits.paddr
  io.dat_addr_r := io.rd_in.bits.paddr

  for ( i <- 0 until cb ) yield {
    io.tag_en_r(i) :=
      info_bypass_fifo.io.enq.fire & io.rd_in.bits.op.is_tag_r
  }



  for ( i <- 0 until cb; j <- 0 until bk ) yield {
    io.dat_en_r(i)(j) := 
      info_bypass_fifo.io.enq.fire &
      io.rd_in.bits.op.is_dat_r & (
        io.rd_in.bits.op.probe |
        j.U === bk_sel
      )
  }


  io.rd_out.valid := RegNext(io.rd_in.valid, false.B)

  for( i <- 0 until cb; j <- 0 until bk ) yield { io.rd_out.bits.rdata(i)(j) := io.dat_info_r(i)(j) } 
  for( i <- 0 until cb )                  yield { io.rd_out.bits.tag(i)      := io.tag_info_r(i) }

  

  info_bypass_fifo.io.enq <> io.rd_in

  io.rd_out.bits.paddr    := info_bypass_fifo.io.deq.bits.paddr
  io.rd_out.bits.wmask    := info_bypass_fifo.io.deq.bits.wmask
  io.rd_out.bits.wdata    := info_bypass_fifo.io.deq.bits.wdata
  io.rd_out.bits.op       := info_bypass_fifo.io.deq.bits.op
  io.rd_out.bits.chk_idx  := info_bypass_fifo.io.deq.bits.chk_idx
  io.rd_out.valid := info_bypass_fifo.io.deq.valid
  info_bypass_fifo.io.deq.ready := io.rd_out.ready

  // io.rd_out.bits.paddr    := RegEnable(io.rd_in.bits.paddr,   io.rd_in.valid)
  // io.rd_out.bits.wmask    := RegEnable(io.rd_in.bits.wmask,   io.rd_in.valid)
  // io.rd_out.bits.wdata    := RegEnable(io.rd_in.bits.wdata,   io.rd_in.valid)
  // io.rd_out.bits.op       := RegEnable(io.rd_in.bits.op,      io.rd_in.valid)
  // io.rd_out.bits.chk_idx  := RegEnable(io.rd_in.bits.chk_idx, io.rd_in.valid)

}

/** stage 2 will write the cache */
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

  /** one hot code indicated which blcok is hit */
  val is_hit_oh = Wire(Vec(cb, Bool()))

  /** flag that indicated that if there is a cache block hit */
  val is_hit = is_hit_oh.asUInt.orR

  /** convert one hot hit to UInt */
  val hit_sel = WireDefault(OHToUInt(is_hit_oh))

  /** flag that indicated that if a cache block is valid */
  val is_valid = RegInit( VecInit( Seq.fill(cl)(VecInit(Seq.fill(cb)(false.B))) ) )

  /** flag that indicated that if a cache block is dirty */
  val is_dirty = RegInit( VecInit( Seq.fill(cl)(VecInit(Seq.fill(cb)(false.B))) ) )

  is_hit_oh := {
    val res = 
      for( i <- 0 until cb ) yield {
        (io.wr_in.bits.tag(i) === tag_sel) & is_valid(cl_sel)(i)        
      }
    assert(PopCount(res) <= 1.U)
    VecInit(res)
  }

  /** when no block is hit or a new grant req comes, we should 1) find out an empty block 2) evict a valid block */
  val rpl_sel = {
    val res = Wire(UInt(cb_w.W))
    val is_emptyBlock_exist = is_valid(cl_sel).contains(false.B)
    val emptyBlock_sel = is_valid(cl_sel).indexWhere( (x:Bool) => (x === false.B) )
    res := Mux( is_emptyBlock_exist, emptyBlock_sel, LFSR(16) )
    res
  }
  
  val cb_sel = WireDefault(
      Mux1H(Seq(
        io.wr_in.bits.op.is_access -> Mux( is_hit, hit_sel, rpl_sel ),
        io.wr_in.bits.op.probe -> hit_sel,
        io.wr_in.bits.op.grant -> rpl_sel
      ))
    )

  when( io.wr_in.fire ) {
    when( io.wr_in.bits.op.probe ) { assert(is_hit) } //l2 will never request a empty probe
    when( io.wr_in.bits.op.grant ) { assert(is_valid(cl_sel).contains(false.B)) } //grant palce is invalid
  }

  io.dat_addr_w := io.wr_in.bits.paddr

  for ( i <- 0 until cb; j <- 0 until bk ) yield {
    io.dat_en_w(i)(j) :=
      io.wr_in.fire &
      i.U === cb_sel &
      io.wr_in.bits.op.is_dat_w & (
        io.wr_in.bits.op.grant |
        (j.U === bk_sel & is_hit)
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
    val op = io.wr_in.bits.op
    
    io.dat_info_w(j) := 
      Mux1H(Seq(
        op.grant -> io.wr_in.bits.wdata(j),
        op.fun.is_su -> io.wr_in.bits.wdata(bk_sel),
        (op.fun.amoswap_w | op.fun.amoswap_d) -> io.wr_in.bits.wdata(bk_sel),
        (op.fun.amoadd_w  | op.fun.amoadd_d ) -> (io.wr_in.bits.wdata(bk_sel) + io.wr_in.bits.rdata(cb_sel)(bk_sel)),
        (op.fun.amoxor_w  | op.fun.amoxor_d ) -> (io.wr_in.bits.wdata(bk_sel) ^ io.wr_in.bits.rdata(cb_sel)(bk_sel)),
        (op.fun.amoand_w  | op.fun.amoand_d ) -> (io.wr_in.bits.wdata(bk_sel) & io.wr_in.bits.rdata(cb_sel)(bk_sel)),
        (op.fun.amoor_w   | op.fun.amoor_d  ) -> (io.wr_in.bits.wdata(bk_sel) | io.wr_in.bits.rdata(cb_sel)(bk_sel)),
        (op.fun.amomin_w  | op.fun.amomin_d ) -> Mux(io.wr_in.bits.wdata(bk_sel).asSInt < io.wr_in.bits.rdata(cb_sel)(bk_sel).asSInt, io.wr_in.bits.wdata(bk_sel), io.wr_in.bits.rdata(cb_sel)(bk_sel)),
        (op.fun.amomax_w  | op.fun.amomax_d ) -> Mux(io.wr_in.bits.wdata(bk_sel).asSInt < io.wr_in.bits.rdata(cb_sel)(bk_sel).asSInt, io.wr_in.bits.rdata(cb_sel)(bk_sel), io.wr_in.bits.wdata(bk_sel)),
        (op.fun.amominu_w | op.fun.amominu_d) -> Mux(io.wr_in.bits.wdata(bk_sel) < io.wr_in.bits.rdata(cb_sel)(bk_sel), io.wr_in.bits.wdata(bk_sel), io.wr_in.bits.rdata(cb_sel)(bk_sel)),
        (op.fun.amomaxu_w | op.fun.amomaxu_d) -> Mux(io.wr_in.bits.wdata(bk_sel) < io.wr_in.bits.rdata(cb_sel)(bk_sel), io.wr_in.bits.rdata(cb_sel)(bk_sel), io.wr_in.bits.wdata(bk_sel)),
              
      ))

  }

  io.tag_addr_w := io.wr_in.bits.paddr

  for ( i <- 0 until cb ) yield {
    io.tag_en_w(i) :=
      (i.U === cb_sel) & io.wr_in.fire & io.wr_in.bits.op.grant
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


  io.wr_in.ready :=
    (io.wr_in.bits.op.is_access & ~is_hit & io.wr_lsReload.ready & io.writeBackUnit_req.ready) |
    (io.wr_in.bits.op.is_access &  is_hit & io.dcache_pop.ready) |
    (io.wr_in.bits.op.probe & io.writeBackUnit_req.ready)        |
    (io.wr_in.bits.op.grant)


  io.missUnit_req.valid := io.wr_in.valid & io.wr_in.bits.op.is_access & ~is_hit & io.wr_lsReload.ready & io.writeBackUnit_req.ready
  io.missUnit_req.bits.paddr := io.wr_in.bits.paddr

  io.writeBackUnit_req.valid := io.wr_in.valid & ((io.wr_in.bits.op.is_access & ~is_hit & io.wr_lsReload.ready & is_valid(cl_sel)(cb_sel)) | io.wr_in.bits.op.probe)
  io.writeBackUnit_req.bits.addr := io.wr_in.bits.paddr
  io.writeBackUnit_req.bits.data := Cat( for( j <- 0 until bk ) yield { io.wr_in.bits.rdata(cb_sel)(bk-1-j) } )

  io.writeBackUnit_req.bits.is_releaseData := io.wr_in.bits.op.is_access & is_dirty(cl_sel)(cb_sel)
  io.writeBackUnit_req.bits.is_release := io.wr_in.bits.op.is_access & ~is_dirty(cl_sel)(cb_sel)
  io.writeBackUnit_req.bits.is_probe := io.wr_in.bits.op.probe & ~is_dirty(cl_sel)(cb_sel)
  io.writeBackUnit_req.bits.is_probeData := io.wr_in.bits.op.probe & is_dirty(cl_sel)(cb_sel)

  io.wr_lsReload.valid := io.wr_in.valid & io.wr_in.bits.op.is_access & ~is_hit & io.writeBackUnit_req.ready
  assert( ~(io.wr_lsReload.valid & ~io.wr_lsReload.ready), "Assert Failed at wr_state 2, reload failed!" )

  
  io.wr_lsReload.bits.paddr   := io.wr_in.bits.paddr
  io.wr_lsReload.bits.wmask   := io.wr_in.bits.wmask
  io.wr_lsReload.bits.wdata   := io.wr_in.bits.wdata
  io.wr_lsReload.bits.op      := io.wr_in.bits.op
  io.wr_lsReload.bits.chk_idx := io.wr_in.bits.chk_idx

  io.dcache_pop.valid := io.wr_in.valid & io.wr_in.bits.op.is_access & is_hit
  io.dcache_pop.bits.res := {
    val rdata = io.wr_in.bits.rdata(cb_sel)(bk_sel)
    val paddr = io.wr_in.bits.paddr
    val op = io.wr_in.bits.op

    get_loadRes( op, paddr, rdata )
  }
  

  io.dcache_pop.bits.chk_idx := io.wr_in.bits.chk_idx
  io.dcache_pop.bits.is_load_amo := io.wr_in.bits.op.is_wb












  def get_loadRes( op: Cache_op, paddr: UInt, rdata: UInt ) = {
    val res = Wire(UInt(64.W))

    def reAlign(rdata: UInt, paddr: UInt) = {
      val res = Wire(UInt(64.W))
      val shift = Wire(UInt(6.W))
      shift := Cat( paddr(2,0), 0.U(3.W) )
      res := rdata >> shift
      res
    }

    def load_byte(is_usi: Bool, rdata: UInt): UInt = Cat( Fill(56, Mux(is_usi, 0.U, rdata(7)) ),  rdata(7,0)  )
    def load_half(is_usi: Bool, rdata: UInt): UInt = Cat( Fill(48, Mux(is_usi, 0.U, rdata(15)) ), rdata(15,0) )
    def load_word(is_usi: Bool, rdata: UInt): UInt = Cat( Fill(32, Mux(is_usi, 0.U, rdata(31)) ), rdata(31,0) )


    
    val align = reAlign(rdata, paddr)

    res := Mux1H(Seq(
      op.fun.is_byte -> load_byte(op.fun.is_usi, align),
      op.fun.is_half -> load_half(op.fun.is_usi, align),
      op.fun.is_word -> load_word(op.fun.is_usi, align),
      op.fun.is_dubl -> align
    ))  

    res
  }


}



