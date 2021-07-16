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
import chisel3.util.random._



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


// trait Info_icache_raw extends IcacheBundle {
//   val paddr = UInt(64.W)
//   val is_probe = Bool()
//   val is_fetch = Bool()

//   def tag_sel = paddr(31,32-tag_w)
//   def bk_sel  = paddr(addr_lsb-1, addr_lsb-log2Ceil(bk) )
//   def cl_sel  = paddr(addr_lsb+line_w-1, addr_lsb)
// }

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


    val pc_if = Flipped(new DecoupledIO( UInt(64.W) ))
    val if_iq = Vec(4, new DecoupledIO(UInt(16.W)) )
    // val rd_in  = Flipped(DecoupledIO(new Info_icache_s0s1))
    // val rd_out = DecoupledIO(new Info_icache_s1s2)

    val missUnit_icache_acquire = new DecoupledIO(new TLBundleA(edge.bundle))
    val missUnit_icache_grant   = Flipped(new DecoupledIO(new TLBundleD(edge.bundle)))
    val missUnit_icache_grantAck  = DecoupledIO(new TLBundleE(edge.bundle))

    val probeUnit_icache_probe = Flipped(new DecoupledIO(new TLBundleB(edge.bundle)))

    val writeBackUnit_icache_release = new DecoupledIO(new TLBundleC(edge.bundle))
    val writeBackUnit_icache_grant   = Flipped(new DecoupledIO(new TLBundleD(edge.bundle)))




  })

  val req_arb = Module(new Arbiter(UInt(64.W), 3))
  val ibuf = Module(new MultiPortFifo( UInt(16.W), 4, 8, 4 ) )

  val cache_dat = new Cache_dat( dw, aw, bk, cb, cl )
  val cache_tag = new Cache_tag( dw, aw, bk, cb, cl ) 
  val missUnit = Module(new MissUnit(edge = edge, entry = 8))
  val probeUnit = Module(new ProbeUnit(edge = edge))
  val writeBackUnit = Module(new WriteBackUnit(edge = edge))


    val fetch_req = Flipped(DecoupledIO(UInt(64.W)))
    val probe_req = Flipped(DecoupledIO(UInt(64.W)))
    val grant_req = Flipped(DecoupledIO(UInt(64.W)))

    val dat_addr_r = Output(UInt(aw.W))
    val dat_en_r   = Output( Vec(cb, Vec(bk, Bool()) ))
    val dat_info_r = Input( Vec(cb, Vec(bk, UInt(dw.W)) ))

    val tag_addr_r = Output(UInt(aw.W))
    val tag_en_r   = Output( Vec(cb, Bool()) )  
    val tag_info_r = Input( Vec(cb, UInt(tag_w.W)) )


  io.missUnit_icache_acquire  <> missUnit.io.cache_acquire
  missUnit.io.cache_grant <> io.missUnit_icache_grant
  io.missUnit_icache_grantAck <> missUnit.io.cache_grantAck
  probeUnit.io.cache_probe <> io.probeUnit_icache_probe
  io.writeBackUnit_icache_release <> writeBackUnit.io.cache_release
  writeBackUnit.io.cache_grant <> io.writeBackUnit_icache_grant
  req_arb.io.in(0) <> io.grant_req
  req_arb.io.in(1) <> io.probe_req
  req_arb.io.in(2) <> io.fetch_req


  val bk_sel  = req_arb.io.out.bits(addr_lsb-1, addr_lsb-log2Ceil(bk))
  val cl_sel  = req_arb.io.out.bits(addr_lsb+line_w-1, addr_lsb)
  val tag_sel = req_arb.io.out.bits(31,32-tag_w)


  val icache_state_dnxt = Wire(UInt(4.W))
  val icache_state_qout = RegNext( icache_state_dnxt, 0.U )


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
        (io.tag_info_r(i) === tag_sel) & is_valid(cl_sel)(i)        
      }
    assert(PopCount(res) <= 1.U)
    VecInit(res)
  }

  val is_emptyBlock_exist = is_valid(cl_sel).contains(false.B)
  val cb_em = is_valid(cl_sel).indexWhere( (x:Bool) => (x === false.B) )
  /** when no block is hit or a new grant req comes, we should 1) find out an empty block 2) evict a valid block */
  val rpl_sel = {
    val res = Wire(UInt(cb_w.W))
    res := Mux( is_emptyBlock_exist, cb_em, LFSR(16) )
    res
  }
  
  val cb_sel = WireDefault(
    Mux1H(Seq(
      (icache_state_qout === 0.U) -> rpl_sel, //for grant
      (icache_state_qout === 1.U) -> hit_sel, //for probe
      (icache_state_qout === 2.U) -> Mux( is_hit, hit_sel, rpl_sel ), // for fetch
    ))
  )



  icache_state_dnxt := 
    Mux1H(Seq(
      (icache_state_qout === 0.U) -> 
        MuxCase( 0.U, Array(
          io.probe_req.valid -> 1.U,
          io.fetch_req.valid -> 2.U
        )),
      (icache_state_qout === 1.U) -> 0.U
      (icache_state_qout === 2.U) ->
      Mux(
        (is_hit & ibuf.io.enq(3).ready) |
        (~is_hit & writeBackUnit.io.ready),
        0.U, 2.U
      )
    ))



  cache_tag.addr_sel_r := req_arb.io.out.bits
  cache_dat.addr_sel_r := req_arb.io.out.bits


  for ( i <- 0 until cb ) yield {
    cache_tag.tag_en_r(i) :=
      icache_state_qout === 0.U & 
      ~missUnit.io.rsp.valid &
      (io.pc_if.valid | probeUnit.io.rsp.valid)
  }

  for ( i <- 0 until cb; j <- 0 until bk ) yield {
    cache_dat.dat_en_r(i)(j) := 
      icache_state_qout === 0.U & 
      ~missUnit.io.rsp.valid &
      ~probeUnit.io.rsp.valid &
      io.pc_if.valid &
      j.U === bk_sel
  }




  ibuf.io.enq(0).bits := cache_dat.dat_info_r(cb_sel)(bk_sel)(15,0)
  ibuf.io.enq(1).bits := cache_dat.dat_info_r(cb_sel)(bk_sel)(31,16)
  ibuf.io.enq(2).bits := cache_dat.dat_info_r(cb_sel)(bk_sel)(47,32)
  ibuf.io.enq(3).bits := cache_dat.dat_info_r(cb_sel)(bk_sel)(63,48)

  ibuf.io.enq(0).valid := icache_state_qout === 2.U & is_hit
  ibuf.io.enq(1).valid := icache_state_qout === 2.U & is_hit
  ibuf.io.enq(2).valid := icache_state_qout === 2.U & is_hit
  ibuf.io.enq(3).valid := icache_state_qout === 2.U & is_hit
  
  missUnit.io.req.bits.paddr := req_arb.io.out.bits
  missUnit.io.req.valid      := icache_state_qout === 2.U & ~is_hit & ( ~is_valid(cl_sel)(cb_sel) | writeBackUnit.io.req.ready)
  writeBackUnit.io.req.bits.addr := req_arb.io.out.bits
  writeBackUnit.io.req.bits.data := DontCare
  writeBackUnit.io.req.bits.is_probe := icache_state_qout === 1.U
  writeBackUnit.io.req.bits.is_probeData := false.B
  writeBackUnit.io.req.bits.is_release := icache_state_qout === 2.U
  writeBackUnit.io.req.bits.is_releaseData := false.B
  writeBackUnit.io.req.valid := 
    (icache_state_qout === 2.U & ~is_hit & is_valid(cl_sel)(cb_sel)) |
    (icache_state_qout === 1.U)

  
  cache_tag.addr_sel_w := missUnit.io.rsp.bits.paddr
  cache_dat.addr_sel_w := missUnit.io.rsp.bits.paddr


  cache_tag.tag_en_w(cb_em) := missUnit.io.rsp.valid
  assert( ~(missUnit.io.rsp.valid & ~is_emptyBlock_exist) )


  for ( j <- 0 until bk ) yield {
    cache_dat.dat_en_w(cb_em)(j) := missUnit.io.rsp.valid
  }

  for ( j <- 0 until bk ) yield {
    cache_dat.dat_info_w(j) := missUnit.io.rsp.bits.wdata(dw*(j+1)-1, dw*j)
    cache_dat.dat_info_wstrb(j) := "hFFFFFFFF".U
  }



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




