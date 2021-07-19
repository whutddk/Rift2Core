package rift2Core.L1Cache


import chisel3._
import chisel3.util._
import rift2Core.define._


import base._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

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




class Icache(edge: TLEdgeOut)(implicit p: Parameters) extends IcacheModule {
  val io = IO(new Bundle {

    val pc_if = Flipped(new DecoupledIO( UInt(64.W) ))
    val if_iq = Vec(4, new DecoupledIO(UInt(16.W)) )



    val missUnit_icache_acquire = new DecoupledIO(new TLBundleA(edge.bundle))
    val missUnit_icache_grant   = Flipped(new DecoupledIO(new TLBundleD(edge.bundle)))
    val missUnit_icache_grantAck  = DecoupledIO(new TLBundleE(edge.bundle))

    val probeUnit_icache_probe = Flipped(new DecoupledIO(new TLBundleB(edge.bundle)))

    val writeBackUnit_icache_release = new DecoupledIO(new TLBundleC(edge.bundle))
    val writeBackUnit_icache_grant   = Flipped(new DecoupledIO(new TLBundleD(edge.bundle)))

    val flush = Input(Bool())
  })



  val ibuf = Module(new MultiPortFifo( UInt(16.W), 4, 8, 4 ) )

  val cache_dat = new Cache_dat( dw, aw, bk, cb, cl )
  val cache_tag = new Cache_tag( dw, aw, bk, cb, cl ) 
  val missUnit = Module(new MissUnit(edge = edge, entry = 8))
  val probeUnit = Module(new ProbeUnit(edge = edge))
  val writeBackUnit = Module(new WriteBackUnit(edge = edge))

  val icache_state_dnxt = Wire(UInt(4.W))
  val icache_state_qout = RegNext( icache_state_dnxt, 0.U )

  ibuf.io.flush := io.flush

  io.pc_if.ready := ibuf.io.enq(0).fire
  io.if_iq <> ibuf.io.deq


  io.missUnit_icache_acquire  <> missUnit.io.cache_acquire
  missUnit.io.cache_grant <> io.missUnit_icache_grant
  io.missUnit_icache_grantAck <> missUnit.io.cache_grantAck
  probeUnit.io.cache_probe <> io.probeUnit_icache_probe
  io.writeBackUnit_icache_release <> writeBackUnit.io.cache_release
  writeBackUnit.io.cache_grant <> io.writeBackUnit_icache_grant




  val bk_sel_r = 
    Mux1H(Seq(
      (icache_state_dnxt === 1.U & icache_state_qout === 1.U) -> probeUnit.io.req.bits.paddr(addr_lsb-1, addr_lsb-log2Ceil(bk)),
      (icache_state_dnxt === 2.U & icache_state_qout === 2.U) -> io.pc_if.bits(addr_lsb-1, addr_lsb-log2Ceil(bk)),
    ))


  val cl_sel_r = 
    Mux1H(Seq(
      (icache_state_qout === 1.U) -> probeUnit.io.req.bits.paddr(addr_lsb+line_w-1, addr_lsb),
      (icache_state_qout === 2.U) -> io.pc_if.bits(addr_lsb+line_w-1, addr_lsb),
    ))

  val cl_sel_w = missUnit.io.rsp.bits.paddr(addr_lsb+line_w-1, addr_lsb)
    

  val tag_sel_r =
      Mux1H(Seq(
      (icache_state_qout === 1.U) -> probeUnit.io.req.bits.paddr(31,32-tag_w),
      (icache_state_qout === 2.U) -> io.pc_if.bits(31,32-tag_w),
    ))





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
        (cache_tag.tag_info_r(i) === tag_sel_r) & is_valid(cl_sel_r)(i)        
      }
    assert(PopCount(res) <= 1.U)
    VecInit(res)
  }

  
  val is_emptyBlock_exist_r = is_valid(cl_sel_r).contains(false.B)
  val is_emptyBlock_exist_w = is_valid(cl_sel_w).contains(false.B)

  val cb_em_r = is_valid(cl_sel_r).indexWhere( (x:Bool) => (x === false.B) )
  val cb_em_w = is_valid(cl_sel_w).indexWhere( (x:Bool) => (x === false.B) )
  
  /** when no block is hit or a new grant req comes, we should 1) find out an empty block 2) evict a valid block */
  val rpl_sel = {
    val res = Wire(UInt(cb_w.W))
    res := Mux( is_emptyBlock_exist_r, cb_em_r, LFSR(16) )
    res
  }
  
  val cb_sel = WireDefault(
    Mux1H(Seq(
      // (icache_state_qout === 0.U) -> rpl_sel, //for grant
      (icache_state_qout === 1.U) -> hit_sel, //for probe
      (icache_state_qout === 2.U) -> Mux( is_hit, hit_sel, rpl_sel ), // for fetch
    ))
  )

  val is_hazard = missUnit.io.rsp.valid & ( cl_sel_r === cl_sel_w )


  icache_state_dnxt := 
    Mux1H(Seq(
      (icache_state_qout === 0.U) -> 
        MuxCase( 0.U, Array(
          probeUnit.io.req.valid -> 1.U,
          (io.pc_if.valid & ~io.flush) -> 2.U
        )),
      (icache_state_qout === 1.U) -> 0.U,
      (icache_state_qout === 2.U) ->
        Mux(
          (is_hit & ibuf.io.enq(7).ready) |
          (~is_hit & ~is_hazard & writeBackUnit.io.req.ready), //when miss, we may evict a block which may ruobt by grant
          0.U, 2.U
        )
    ))

    probeUnit.io.req.ready := true.B

  cache_tag.tag_addr_r := 
    Mux1H(Seq(
      (icache_state_dnxt === 1.U) -> probeUnit.io.req.bits.paddr,
      (icache_state_dnxt === 2.U) -> io.pc_if.bits,
    ))
    
  cache_dat.dat_addr_r := 
    Mux1H(Seq(
      (icache_state_dnxt === 1.U) -> probeUnit.io.req.bits.paddr,
      (icache_state_dnxt === 2.U) -> io.pc_if.bits,
    ))


  for ( i <- 0 until cb ) yield {
    cache_tag.tag_en_r(i) :=
      icache_state_qout === 0.U & icache_state_dnxt =/= 0.U
  }

  for ( i <- 0 until cb; j <- 0 until bk ) yield {
    cache_dat.dat_en_r(i)(j) := 
      icache_state_qout === 0.U & icache_state_dnxt === 2.U &
      j.U === bk_sel_r
  }




  ibuf.io.enq(0).bits := cache_dat.dat_info_r(cb_sel)(bk_sel_r)(15,0)
  ibuf.io.enq(1).bits := cache_dat.dat_info_r(cb_sel)(bk_sel_r)(31,16)
  ibuf.io.enq(2).bits := cache_dat.dat_info_r(cb_sel)(bk_sel_r)(47,32)
  ibuf.io.enq(3).bits := cache_dat.dat_info_r(cb_sel)(bk_sel_r)(63,48)
  ibuf.io.enq(4).bits := cache_dat.dat_info_r(cb_sel)(bk_sel_r)(79,64)
  ibuf.io.enq(5).bits := cache_dat.dat_info_r(cb_sel)(bk_sel_r)(95,80)
  ibuf.io.enq(6).bits := cache_dat.dat_info_r(cb_sel)(bk_sel_r)(111,96)
  ibuf.io.enq(7).bits := cache_dat.dat_info_r(cb_sel)(bk_sel_r)(127,112)

  ibuf.io.enq(0).valid := icache_state_qout === 2.U & is_hit
  ibuf.io.enq(1).valid := icache_state_qout === 2.U & is_hit
  ibuf.io.enq(2).valid := icache_state_qout === 2.U & is_hit
  ibuf.io.enq(3).valid := icache_state_qout === 2.U & is_hit
  ibuf.io.enq(4).valid := icache_state_qout === 2.U & is_hit
  ibuf.io.enq(5).valid := icache_state_qout === 2.U & is_hit
  ibuf.io.enq(6).valid := icache_state_qout === 2.U & is_hit
  ibuf.io.enq(7).valid := icache_state_qout === 2.U & is_hit 


  missUnit.io.req.bits.paddr := io.pc_if.bits
  missUnit.io.req.valid      := icache_state_qout === 2.U & ~is_hit & ~is_hazard & ( ~is_valid(cl_sel_r)(cb_sel) | writeBackUnit.io.req.ready)
  writeBackUnit.io.req.bits.addr :=
    Mux1H(Seq(
      (icache_state_qout === 1.U) -> probeUnit.io.req.bits.paddr,
      (icache_state_qout === 2.U) -> io.pc_if.bits,
    ))

  writeBackUnit.io.req.bits.data := DontCare
  writeBackUnit.io.req.bits.is_probe := icache_state_qout === 1.U
  writeBackUnit.io.req.bits.is_probeData := false.B
  writeBackUnit.io.req.bits.is_release := icache_state_qout === 2.U
  writeBackUnit.io.req.bits.is_releaseData := false.B
  writeBackUnit.io.req.valid := 
    (icache_state_qout === 2.U & ~is_hit & ~is_hazard & is_valid(cl_sel_r)(cb_sel)) |
    (icache_state_qout === 1.U)

  
  cache_tag.tag_addr_w := missUnit.io.rsp.bits.paddr
  cache_dat.dat_addr_w := missUnit.io.rsp.bits.paddr

  for ( i <- 0 until cb ) yield {
    cache_tag.tag_en_w(i) := Mux( cb_em_w === i.U, missUnit.io.rsp.valid, false.B )   
  }



  missUnit.io.rsp.ready := true.B

  for ( i <- 0 until cb; j <- 0 until bk ) yield {
    cache_dat.dat_en_w(i)(j) := Mux(i.U === cb_em_w, missUnit.io.rsp.valid, false.B)
  }

  for ( j <- 0 until bk ) yield {
    cache_dat.dat_info_w(j) := missUnit.io.rsp.bits.wdata(dw*(j+1)-1, dw*j)
    cache_dat.dat_info_wstrb(j) := "hFFFFFFFF".U
  }

  when( missUnit.io.rsp.fire ) {
    assert( is_emptyBlock_exist_w )
    is_valid(cl_sel_w)(cb_em_w) := true.B
  }
  when( writeBackUnit.io.req.fire & is_valid(cl_sel_r)(cb_sel) === true.B) {
    is_valid(cl_sel_r)(cb_sel) := false.B
  }

  missUnit.io.miss_ban := writeBackUnit.io.miss_ban
  writeBackUnit.io.release_ban := missUnit.io.release_ban

}



