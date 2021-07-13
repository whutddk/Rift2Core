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



  

// class Icache(edge: TLEdgeOut)(implicit p: Parameters) extends IcacheModule {

//   // val ( bus, edge ) = outer.clientNode.out.head

//   val io = IO(new Bundle{
//     val pc_if = Flipped(new DecoupledIO( UInt(64.W) ))

//     val missUnit_icache_acquire = new DecoupledIO(new TLBundleA(edge.bundle))
//     val missUnit_icache_grant   = Flipped(new DecoupledIO(new TLBundleD(edge.bundle)))
//     val missUnit_icache_grantAck  = DecoupledIO(new TLBundleE(edge.bundle))

//     val probeUnit_icache_probe = Flipped(new DecoupledIO(new TLBundleB(edge.bundle)))

//     val writeBackUnit_icache_release = new DecoupledIO(new TLBundleC(edge.bundle))
//     val writeBackUnit_icache_grant   = Flipped(new DecoupledIO(new TLBundleD(edge.bundle)))
//   })

//   val cache_dat = new Cache_dat( dw, aw, bk, cb, cl )
//   val cache_tag = new Cache_tag( dw, aw, bk, cb, cl ) 
//   val missUnit = Module(new MissUnit(edge = edge, entry = 8))
//   val probeUnit = Module(new ProbeUnit(edge = edge))
//   val writeBackUnit = Module(new WriteBackUnit(edge = edge))


//   val paddr = 
//     Mux(probeUnit.io.req.bits. )



























































//   io.missUnit_dcache_acquire  <> missUnit.io.dcache_acquire
//   missUnit.io.dcache_grant <> io.missUnit_dcache_grant
//   io.missUnit_dcache_grantAck <> missUnit.io.dcache_grantAck
//   probeUnit.io.dcache_probe <> io.probeUnit_dcache_probe
//   io.writeBackUnit_dcache_release <> writeBackUnit.io.dcache_release
//   writeBackUnit.io.dcache_grant <> io.writeBackUnit_dcache_grant



//   rd_stage.io.dat_addr_r <> cache_dat.dat_addr_r
//   rd_stage.io.dat_en_r   <> cache_dat.dat_en_r
//   cache_dat.dat_info_r   <> rd_stage.io.dat_info_r
//   rd_stage.io.tag_addr_r <> cache_tag.tag_addr_r
//   rd_stage.io.tag_en_r   <> cache_tag.tag_en_r
//   cache_tag.tag_info_r   <> rd_stage.io.tag_info_r

//   wr_stage.io.tag_addr_w        <> cache_tag.tag_addr_w
//   wr_stage.io.tag_en_w          <> cache_tag.tag_en_w
//   wr_stage.io.dat_addr_w        <> cache_dat.dat_addr_w
//   wr_stage.io.dat_en_w          <> cache_dat.dat_en_w
//   wr_stage.io.dat_info_wstrb    <> cache_dat.dat_info_wstrb
//   wr_stage.io.dat_info_w        <> cache_dat.dat_info_w
//   wr_stage.io.missUnit_req      <> missUnit.io.req
//   wr_stage.io.writeBackUnit_req <> writeBackUnit.io.req


//   missUnit.io.miss_ban := writeBackUnit.io.miss_ban
//   writeBackUnit.io.release_ban := missUnit.io.release_ban


//   val ls_arb = Module(new Arbiter( new Info_cache_s0s1, 2))
//   val rd_arb = Module(new Arbiter( new Info_cache_s0s1, 2))
//   val wr_arb = Module(new Arbiter( new Info_cache_s1s2, 2))

//   val reload_fifo = Module( new Queue( new Info_cache_s0s1, 1, false, true) )

//   wr_stage.io.wr_lsReload <> reload_fifo.io.enq
//   reload_fifo.io.deq <> ls_arb.io.in(0)
//   io.dcache_push <> ls_arb.io.in(1)
//   ls_arb.io.out <> lsEntry.io.enq 

//   probeUnit.io.req <> rd_arb.io.in(0)
//   lsEntry.io.deq <> rd_arb.io.in(1)
//   rd_arb.io.out <> rd_stage.io.rd_in

//   missUnit.io.rsp <> wr_arb.io.in(0)
//   rd_stage.io.rd_out <> wr_arb.io.in(1)
//   wr_arb.io.out <> wr_stage.io.wr_in



//   wr_stage.io.dcache_pop <> io.dcache_pop

// }




