package rift2Core.L1Cache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import rift2Core.define._


import base._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba.axi4._
import sifive.blocks.inclusivecache._
import axi._

case class IcacheParameters(
  dw: Int,
  bk: Int,
  cb: Int,
  cl: Int,
  aw: Int = 32
) extends L1CacheParameters

trait HasIcacheParameters extends HasL1CacheParameters {
  val icacheParams: L1CacheParameters

  def dw = icacheParams.dw
  def bk = icacheParams.bk
  def cb = icacheParams.cb
  def cl = icacheParams.cl
  def aw = icacheParams.aw

  def addr_lsb = log2Ceil(dw*bk/8)
  def line_w   = log2Ceil(cl)
  def cb_w = log2Ceil(cb)

  def tag_w    = aw - addr_lsb - line_w
}

abstract class IcacheModule(implicit p: Parameters) extends L1CacheModule
  with HasIcacheParameters

abstract class IcacheBundle(implicit p: Parameters) extends L1CacheBundle
  with HasIcacheParameters


trait Info_icache_raw extends IcacheBundle {
  val paddr = UInt(64.W)
  val is_probe = Bool()
  val is_fetch = Bool()

  def tag_sel = paddr(31,32-tag_w)
  def bk_sel  = paddr(addr_lsb-1, addr_lsb-log2Ceil(bk) )
  def cl_sel  = paddr(addr_lsb+line_w-1, addr_lsb)
}

trait Info_icache_tagDat extends IcacheBundle {
  val rdata = Vec(cb, Vec(bk, UInt(64.W)))
  val tag   = Vec(cb, UInt(tag_w.W))
}

class Info_icache_s0s1(implicit p: Parameters) extends IcacheBundle with Info_icache_raw
class Info_icache_s1s2(implicit p: Parameters) extends IcacheBundle with Info_icache_raw with Info_icache_tagDat {
  val is_grant = Bool()
  val wdata = Vec(bk,UInt(64.W))
}

class Info_icache_retn(implicit p: Parameters) extends DcacheBundle with Info_sc_idx {
  val res = UInt(64.W)
}

/** the fisrt stage to read out the data */
class L1i_rd_stage()(implicit p: Parameters) extends DcacheModule {
  val io = IO(new Bundle {
    val fetch_req = Flipped(DecoupledIO())
    val probe_req = Flipped(DecoupledIO())
    val grant_req = Flipped(DecoupledIO())

    val rd_in  = Flipped(DecoupledIO(new Info_icache_s0s1))
    val rd_out = DecoupledIO(new Info_icache_s1s2)

    val dat_addr_r = Output(UInt(aw.W))
    val dat_en_r   = Output( Vec(cb, Vec(bk, Bool()) ))
    val dat_info_r = Input( Vec(cb, Vec(bk, UInt(dw.W)) ))

    val tag_addr_r = Output(UInt(aw.W))
    val tag_en_r   = Output( Vec(cb, Bool()) )  
    val tag_info_r = Input( Vec(cb, UInt(tag_w.W)) )
  })



  val bk_sel = io.rd_in.bits.bk_sel

  val icache_state_dnxt = Wire(UInt(4.W))
  val icache_state_qout = RegNext( icache_state_dnxt, 0.U )

  icache_state_dnxt := 
    Mux1H(Seq(
      (icache_state_qout === 0.U) -> 
        MuxCase( 0.U, Array(
          probe_req.valid -> 1.U
          fetch_req.valid -> 2.U
        )),
      (icache_state_qout === 1.U) -> 0.U
      (icache_state_qout === 2.U) ->
      Mux(
         is_hit & icache_pop.ready |
        ~is_hit & writeBackUnit_req.ready,
        0.U, 2.U
      )
    ))



  io.tag_addr_r := io.rd_in.bits.paddr
  io.dat_addr_r := io.rd_in.bits.paddr

  for ( i <- 0 until cb ) yield {
    io.tag_en_r(i) := info_bypass_fifo.io.enq.fire 
  }



  for ( i <- 0 until cb; j <- 0 until bk ) yield {
    io.dat_en_r(i)(j) := 
      info_bypass_fifo.io.enq.fire &
      io.rd_in.bits.is_fetch & 
      j.U === bk_sel
  }


  io.rd_out.valid := RegNext(io.rd_in.valid, false.B)

  for( i <- 0 until cb; j <- 0 until bk ) yield { io.rd_out.bits.rdata(i)(j) := io.dat_info_r(i)(j) } 
  for( i <- 0 until cb )                  yield { io.rd_out.bits.tag(i)      := io.tag_info_r(i) }

  

  info_bypass_fifo.io.enq <> io.rd_in

  io.rd_out.bits.paddr    := info_bypass_fifo.io.deq.bits.paddr
  io.rd_out.bits.wdata    := DontCare
  io.rd_out.bits.is_probe := info_bypass_fifo.io.deq.bits.is_probe
  io.rd_out.bits.is_fetch := info_bypass_fifo.io.deq.bits.is_fetch
  io.rd_out.bits.is_grant := false.B

  io.rd_out.valid := info_bypass_fifo.io.deq.valid
  info_bypass_fifo.io.deq.ready := io.rd_out.ready


}

/** stage 2 will write the cache */
class L1i_wr_stage() (implicit p: Parameters) extends IcacheModule {
  val io = IO(new Bundle{
    val wr_in  = Flipped(new DecoupledIO(new Info_icache_s1s2))
    val icache_pop = DecoupledIO(new Info_icache_retn)

    val tag_addr_w = Output(UInt(aw.W))
    val tag_en_w = Output( Vec(cb, Bool()) )

    val dat_addr_w = Output(UInt(aw.W))
    val dat_en_w = Output( Vec(cb, Vec(bk, Bool()) ))
    val dat_info_wstrb = Output( Vec(bk, UInt((dw/8).W)) )
    val dat_info_w = Output( Vec(bk, UInt(dw.W)))

    val missUnit_req = new DecoupledIO(new Info_miss_req)
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
        io.wr_in.bits.is_fetch -> Mux( is_hit, hit_sel, rpl_sel ),
        io.wr_in.bits.is_probe -> hit_sel,
        io.wr_in.bits.is_grant -> rpl_sel
      ))
    )

  when( io.wr_in.fire ) {
    when( io.wr_in.bits.is_probe ) { assert(is_hit) } //l2 will never request a empty probe
    when( io.wr_in.bits.is_grant ) { assert(is_valid(cl_sel).contains(false.B)) } //grant palce is invalid
  }

  io.dat_addr_w := io.wr_in.bits.paddr

  for ( i <- 0 until cb; j <- 0 until bk ) yield {
    io.dat_en_w(i)(j) :=
      io.wr_in.fire &
      i.U === cb_sel &
      io.wr_in.bits.is_grant
  }

  for ( j <- 0 until bk ) yield {
    io.dat_info_wstrb(j) :="hFF".U
  }

  for ( j <- 0 until bk ) yield {
    io.dat_info_w(j) := io.wr_in.bits.wdata(j)
  }

  io.tag_addr_w := io.wr_in.bits.paddr

  for ( i <- 0 until cb ) yield {
    io.tag_en_w(i) :=
      (i.U === cb_sel) & io.wr_in.fire & io.wr_in.bits.is_grant
  }

  when( io.wr_in.fire ) {
    when( io.wr_in.bits.is_grant ) {
      is_valid(cl_sel)(cb_sel) := true.B
    }

    when( (io.wr_in.bits.op.is_fetch) & ~is_hit ) {
      is_valid(cl_sel)(cb_sel) := false.B
    }

  }


  io.wr_in.ready :=
    (io.wr_in.bits.is_fetch & ~is_hit & io.writeBackUnit_req.ready) |
    (io.wr_in.bits.is_fetch &  is_hit & io.dcache_pop.ready) |
    (io.wr_in.bits.is_probe & io.writeBackUnit_req.ready)    |
    (io.wr_in.bits.is_grant)


  io.missUnit_req.valid := io.wr_in.valid & io.wr_in.bits.op.is_fetch & ~is_hit & io.writeBackUnit_req.ready
  io.missUnit_req.bits.paddr := io.wr_in.bits.paddr
  assert( io.missUnit_req.ready === true.B )

  io.writeBackUnit_req.valid :=
    io.wr_in.valid & (
      (io.wr_in.bits.op.is_fetch & ~is_hit & is_valid(cl_sel)(cb_sel)) |
      io.wr_in.bits.is_probe
    )
  io.writeBackUnit_req.bits.addr := io.wr_in.bits.paddr
  io.writeBackUnit_req.bits.data := Cat( for( j <- 0 until bk ) yield { io.wr_in.bits.rdata(cb_sel)(bk-1-j) } )

  io.writeBackUnit_req.bits.is_releaseData := false.B
  io.writeBackUnit_req.bits.is_release := io.wr_in.bits.is_fetch
  io.writeBackUnit_req.bits.is_probe   := io.wr_in.bits.is_probe
  io.writeBackUnit_req.bits.is_probeData := false.B

  io.icache_pop.valid := io.wr_in.valid & io.wr_in.bits.is_fetch & is_hit
  io.icache_pop.bits.res := {
    val rdata = io.wr_in.bits.rdata(cb_sel)(bk_sel)
    rdata
  }
  
}

  

class Icache(edge: TLEdgeOut)(implicit p: Parameters) extends IcacheModule {

  // val ( bus, edge ) = outer.clientNode.out.head

  val io = IO(new Bundle{
    val pc_if = Flipped(new DecoupledIO( UInt(64.W) ))

    val missUnit_icache_acquire = new DecoupledIO(new TLBundleA(edge.bundle))
    val missUnit_icache_grant   = Flipped(new DecoupledIO(new TLBundleD(edge.bundle)))
    val missUnit_icache_grantAck  = DecoupledIO(new TLBundleE(edge.bundle))

    val probeUnit_icache_probe = Flipped(new DecoupledIO(new TLBundleB(edge.bundle)))

    val writeBackUnit_icache_release = new DecoupledIO(new TLBundleC(edge.bundle))
    val writeBackUnit_icache_grant   = Flipped(new DecoupledIO(new TLBundleD(edge.bundle)))
  })

  val cache_dat = new Cache_dat( dw, aw, bk, cb, cl )
  val cache_tag = new Cache_tag( dw, aw, bk, cb, cl ) 
  val missUnit = Module(new MissUnit(edge = edge, entry = 8))
  val probeUnit = Module(new ProbeUnit(edge = edge))
  val writeBackUnit = Module(new WriteBackUnit(edge = edge))




  io.missUnit_icache_acquire  <> missUnit.io.cache_acquire
  missUnit.io.cache_grant <> io.missUnit_icache_grant
  io.missUnit_icache_grantAck <> missUnit.io.cache_grantAck
  probeUnit.io.cache_probe <> io.probeUnit_icache_probe
  io.writeBackUnit_icache_release <> writeBackUnit.io.cache_release
  writeBackUnit.io.cache_grant <> io.writeBackUnit_icache_grant



  rd_stage.io.dat_addr_r <> cache_dat.dat_addr_r
  rd_stage.io.dat_en_r   <> cache_dat.dat_en_r
  cache_dat.dat_info_r   <> rd_stage.io.dat_info_r
  rd_stage.io.tag_addr_r <> cache_tag.tag_addr_r
  rd_stage.io.tag_en_r   <> cache_tag.tag_en_r
  cache_tag.tag_info_r   <> rd_stage.io.tag_info_r

  wr_stage.io.tag_addr_w        <> cache_tag.tag_addr_w
  wr_stage.io.tag_en_w          <> cache_tag.tag_en_w
  wr_stage.io.dat_addr_w        <> cache_dat.dat_addr_w
  wr_stage.io.dat_en_w          <> cache_dat.dat_en_w
  wr_stage.io.dat_info_wstrb    <> cache_dat.dat_info_wstrb
  wr_stage.io.dat_info_w        <> cache_dat.dat_info_w
  wr_stage.io.missUnit_req      <> missUnit.io.req
  wr_stage.io.writeBackUnit_req <> writeBackUnit.io.req


  missUnit.io.miss_ban := writeBackUnit.io.miss_ban
  writeBackUnit.io.release_ban := missUnit.io.release_ban



  val rd_arb = Module(new Arbiter( new Info_icache_s0s1, 2))
  val wr_arb = Module(new Arbiter( new Info_icache_s1s2, 2))



  probeUnit.io.req <> rd_arb.io.in(0)
  io.icache_push <>  rd_arb.io.in(1)
  rd_arb.io.out <> rd_stage.io.rd_in

  missUnit.io.rsp <> wr_arb.io.in(0)
  rd_stage.io.rd_out <> wr_arb.io.in(1)
  wr_arb.io.out <> wr_stage.io.wr_in



  wr_stage.io.icache_pop <> io.icache_pop

}




