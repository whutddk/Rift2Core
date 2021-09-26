package rift2Core.L1Cache


import chisel3._
import chisel3.util._
import rift2Core.define._
import rift2Core.privilege._
import rift2Core.frontend._

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

  require( (addr_lsb + line_w) == 12 )
 
  def tag_w    = aw - addr_lsb - line_w
}

abstract class IcacheModule(implicit p: Parameters) extends L1CacheModule
  with HasIcacheParameters

abstract class IcacheBundle(implicit p: Parameters) extends L1CacheBundle
  with HasIcacheParameters


class Info_if_cmm extends Bundle {
  val ill_vaddr = UInt(64.W)
  val is_paging_fault = Bool()
  val is_access_fault = Bool()
}



class Icache(edge: TLEdgeOut)(implicit p: Parameters) extends IcacheModule {
  val io = IO(new Bundle {

    val pc_if = Flipped(new DecoupledIO( UInt(64.W) ))
    val if_iq = Vec(4, new DecoupledIO(UInt(16.W)) )

    val if_mmu = DecoupledIO(new Info_mmu_req)
    val mmu_if = Flipped(DecoupledIO(new Info_mmu_rsp))

    val if_cmm = Output(new Info_if_cmm)


    val icache_get    = new DecoupledIO(new TLBundleA(edge.bundle))
    val icache_access = Flipped(new DecoupledIO(new TLBundleD(edge.bundle)))

    val flush = Input(Bool())

    val ifence = Input(Bool())
  })

  val (_, _, is_trans_done, transCnt) = edge.count(io.icache_access)
  val icache_access_data_lo = RegInit( 0.U(128.W) )
  val kill_trans = RegInit(false.B)

  val ibuf = Module(new MultiPortFifo( UInt(16.W), 4, 8, 4 ) )

  val cache_dat = new Cache_dat( dw, aw, bk, cb, cl )
  val cache_tag = new Cache_tag( dw, aw, bk, cb, cl ) 


  val icache_state_dnxt = Wire(UInt(4.W))
  val icache_state_qout = RegNext( icache_state_dnxt, 0.U )

  require( bk == 2 )

  when( io.flush & icache_state_qout =/= 0.U ) {
    kill_trans := true.B
  } .elsewhen( icache_state_dnxt === 0.U | icache_state_qout === 0.U ) {
    kill_trans := false.B
  }

  io.if_mmu.valid := io.pc_if.valid
  io.if_mmu.bits.vaddr := io.pc_if.bits
  io.if_mmu.bits.is_R := true.B
  io.if_mmu.bits.is_W := false.B
  io.if_mmu.bits.is_X := true.B
  io.pc_if.ready := io.if_mmu.ready
  
  
  val is_access_fault = io.mmu_if.valid & io.mmu_if.bits.is_access_fault
  val is_paging_fault = io.mmu_if.valid & io.mmu_if.bits.is_paging_fault
  val fault_push = io.mmu_if.valid & io.mmu_if.bits.is_fault & ~io.flush & ibuf.io.enq(7).ready
  assert( ~(is_access_fault & is_paging_fault) )

  io.if_cmm.ill_vaddr := io.pc_if.bits
  io.if_cmm.is_access_fault := is_access_fault
  io.if_cmm.is_paging_fault := is_paging_fault










  ibuf.io.flush := io.flush

  io.mmu_if.ready := ibuf.io.enq(0).fire & ~fault_push
  io.if_iq <> ibuf.io.deq




  val bk_sel = io.mmu_if.bits.paddr(addr_lsb-1, addr_lsb-log2Ceil(bk))
  val cl_sel = io.mmu_if.bits.paddr(addr_lsb+line_w-1, addr_lsb)
  val tag_sel = io.mmu_if.bits.paddr(31,32-tag_w)



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
        (cache_tag.tag_info_r(i) === tag_sel) & is_valid(cl_sel)(i)        
      }
    assert(PopCount(res) <= 1.U)
    VecInit(res)
  }

  
  val is_emptyBlock_exist_r = is_valid(cl_sel).contains(false.B)

  val cb_em = is_valid(cl_sel).indexWhere( (x:Bool) => (x === false.B) )
  
  /** when no block is hit or a new grant req comes, we should 1) find out an empty block 2) evict a valid block */
  val rpl_sel = {
    val res = Wire(UInt(cb_w.W))
    res := Mux( is_emptyBlock_exist_r, cb_em, LFSR(16,icache_state_qout =/= 2.U) )
    res
  }
  
  val cb_sel = WireDefault(
    Mux1H(Seq(
      (icache_state_qout === 1.U) -> Mux( is_hit, hit_sel, rpl_sel ), // for fetch
      (icache_state_qout === 2.U) -> rpl_sel, // for access
    ))
  )

  icache_state_dnxt := 
    Mux1H(Seq(
      (icache_state_qout === 0.U) ->
          Mux((io.mmu_if.valid & ~io.mmu_if.bits.is_fault & ~io.flush & ibuf.io.enq(7).ready ), 1.U, 0.U),
      (icache_state_qout === 1.U) ->
        Mux( io.flush, 0.U,
          Mux( is_hit, 0.U,
                      Mux( io.icache_get.fire, 2.U, 1.U ))),
      (icache_state_qout === 2.U) ->
          Mux( is_trans_done, 0.U, 2.U ),  
    ))


  cache_tag.tag_addr_r := io.mmu_if.bits.paddr
  cache_dat.dat_addr_r := io.mmu_if.bits.paddr
  cache_tag.tag_addr_w := io.mmu_if.bits.paddr
  cache_dat.dat_addr_w := io.mmu_if.bits.paddr

  for ( i <- 0 until cb ) yield {
    cache_tag.tag_en_r(i) :=
      icache_state_qout === 0.U & icache_state_dnxt === 1.U
  }

  for ( i <- 0 until cb; j <- 0 until bk ) yield {
    cache_dat.dat_en_r(i)(j) := 
      icache_state_qout === 0.U & icache_state_dnxt === 1.U & j.U === bk_sel
  }

  for ( i <- 0 until cb ) yield {
    cache_tag.tag_en_w(i) := 
      (icache_state_qout === 2.U & icache_state_dnxt === 0.U) & (cb_sel === i.U) & ~kill_trans & ~io.flush 
  }

  for ( i <- 0 until cb; j <- 0 until bk ) yield {
    cache_dat.dat_en_w(i)(j) :=
      (icache_state_qout === 2.U & icache_state_dnxt === 0.U) & (cb_sel === i.U) & ~kill_trans & ~io.flush
  }

  for ( j <- 0 until bk ) yield {
    cache_dat.dat_info_wstrb(j) := "hFFFFFFFF".U
  }

  cache_dat.dat_info_w(1) := io.icache_access.bits.data
  cache_dat.dat_info_w(0) := icache_access_data_lo

  val reAlign_instr = {
    val res = Wire(UInt(128.W))
    val shift = Wire(UInt(7.W))
    shift := Cat(io.mmu_if.bits.paddr(3,0), 0.U(3.W))
    val instr_raw = Mux1H( Seq(
      (icache_state_qout === 1.U) -> cache_dat.dat_info_r(cb_sel)(bk_sel),
      (icache_state_qout === 2.U) -> Mux( bk_sel === 0.U, icache_access_data_lo, io.icache_access.bits.data ),
    ))
    res := instr_raw >> shift
    res
  }

  ibuf.io.enq(0).bits := reAlign_instr >> 0.U

  ibuf.io.enq(1).bits := reAlign_instr >> 16.U
  ibuf.io.enq(2).bits := reAlign_instr >> 32.U
  ibuf.io.enq(3).bits := reAlign_instr >> 48.U
  ibuf.io.enq(4).bits := reAlign_instr >> 64.U
  ibuf.io.enq(5).bits := reAlign_instr >> 80.U
  ibuf.io.enq(6).bits := reAlign_instr >> 96.U
  ibuf.io.enq(7).bits := reAlign_instr >> 112.U

  ibuf.io.enq(0).valid := ~kill_trans & io.mmu_if.valid & icache_state_qout =/= 0.U & icache_state_dnxt === 0.U & ibuf.io.enq(7).ready & ( io.mmu_if.bits.paddr(3,1) <= 7.U )
  ibuf.io.enq(1).valid := ~kill_trans & io.mmu_if.valid & icache_state_qout =/= 0.U & icache_state_dnxt === 0.U & ibuf.io.enq(7).ready & ( io.mmu_if.bits.paddr(3,1) <= 6.U )
  ibuf.io.enq(2).valid := ~kill_trans & io.mmu_if.valid & icache_state_qout =/= 0.U & icache_state_dnxt === 0.U & ibuf.io.enq(7).ready & ( io.mmu_if.bits.paddr(3,1) <= 5.U )
  ibuf.io.enq(3).valid := ~kill_trans & io.mmu_if.valid & icache_state_qout =/= 0.U & icache_state_dnxt === 0.U & ibuf.io.enq(7).ready & ( io.mmu_if.bits.paddr(3,1) <= 4.U )
  ibuf.io.enq(4).valid := ~kill_trans & io.mmu_if.valid & icache_state_qout =/= 0.U & icache_state_dnxt === 0.U & ibuf.io.enq(7).ready & ( io.mmu_if.bits.paddr(3,1) <= 3.U )
  ibuf.io.enq(5).valid := ~kill_trans & io.mmu_if.valid & icache_state_qout =/= 0.U & icache_state_dnxt === 0.U & ibuf.io.enq(7).ready & ( io.mmu_if.bits.paddr(3,1) <= 2.U )
  ibuf.io.enq(6).valid := ~kill_trans & io.mmu_if.valid & icache_state_qout =/= 0.U & icache_state_dnxt === 0.U & ibuf.io.enq(7).ready & ( io.mmu_if.bits.paddr(3,1) <= 1.U )
  ibuf.io.enq(7).valid := ~kill_trans & io.mmu_if.valid & icache_state_qout =/= 0.U & icache_state_dnxt === 0.U & ibuf.io.enq(7).ready & ( io.mmu_if.bits.paddr(3,1) === 0.U )

  
  io.icache_get.valid := icache_state_qout === 1.U & ~is_hit & ~io.flush
  io.icache_get.bits :=
    edge.Get(
      fromSource = 0.U,
      toAddress = io.mmu_if.bits.paddr & ("hffffffff".U << addr_lsb.U),
      lgSize = log2Ceil(256/8).U
    )._2
  
  io.icache_access.ready := true.B

  when( io.icache_access.fire & ~is_trans_done) {
    icache_access_data_lo := io.icache_access.bits.data
  }




  assert( {
    val chk_tag = {
      for ( i <- 0 until cb ) yield {
        cache_tag.tag_ram(i).read(cl_sel) === io.mmu_if.bits.paddr(31,32-tag_w)
      }
    }

    val chk_all = VecInit(chk_tag.zip(is_valid(cl_sel)).map{case(x,y) => x & y})

    ~( cache_tag.tag_en_w.contains(true.B) & chk_all.contains(true.B) )
  },"Assert Failed at ICache, an existed tag is in cache when wrote"
  )








  when( icache_state_qout =/= 0.U & icache_state_dnxt === 0.U & ~kill_trans & ~io.flush ) {
    is_valid(cl_sel)(cb_sel) := true.B
  }

  for( i <- 0 until cb; k <- 0 until cl ) yield {
    when( io.ifence ) {
       is_valid(k)(i) := false.B
    }
  }

  when( io.ifence ) { assert(io.flush) }

}



