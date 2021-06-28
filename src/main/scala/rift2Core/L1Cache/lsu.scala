package rift2Core.L1Cache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._

import base._
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tilelink._











class Lsu()(implicit p: Parameters) extends LazyModule {
  val clientParameters = TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = "dcache",
      sourceId = IdRange(0, cfg.nMissEntries+1),
      supportsProbe = TransferSizes(cfg.blockBytes)
    ))
  )

  val clientNode = TLClientNode(Seq(clientParameters))

  lazy val module = new LsuImp(this)
}

class LsuImp(outer: Lsu) extends LazyModuleImp(outer) {

  val ( bus, edge ) = outer.clientNode.out.head

  val io = IO(new Bundle{
    val lsu_iss_exe = Flipped(new DecoupledIO(new Lsu_iss_info))
    val lsu_exe_iwb = new DecoupledIO(new Exe_iwb_info)

    // val dl1_chn_a = new DecoupledIO(new TLchannel_a(128, 32))
    // val dl1_chn_d = Flipped(new DecoupledIO( new TLchannel_d(128) ))

    // val sys_chn_ar = new DecoupledIO(new AXI_chn_a( 32, 1, 1 ))
    // val sys_chn_r = Flipped( new DecoupledIO(new AXI_chn_r( 64, 1, 1)) )

    // val sys_chn_aw = new DecoupledIO(new AXI_chn_a( 32, 1, 1 ))
    // val sys_chn_w = new DecoupledIO(new AXI_chn_w( 64, 1 )) 
    // val sys_chn_b = Flipped( new DecoupledIO(new AXI_chn_b( 1, 1 )))

    val cmm_lsu = Input(new Info_cmm_lsu)
    val lsu_cmm = Output( new Info_lsu_cmm )

    val il1_fence_req = Output(Bool())
    // val l2c_fence_req = Output(Bool())
    // val l3c_fence_req = Output(Bool())

    val flush = Input(Bool())
  })


  val lsu_exe_iwb_fifo = Module( new Queue( new Exe_iwb_info, 1, true, false ) )
  io.lsu_exe_iwb <> lsu_exe_iwb_fifo.io.deq
  lsu_exe_iwb_fifo.reset := reset.asBool | io.flush



  val cache_dat = Module(new Cache_dat())
  val cache_tag = Module(new Cache_tag())

  val missUnit = MissUnit(edge = edge, entry = 8)
  val probeUnit = ProbeUnit(edge = edge)
  val writeBackUnit = WriteBackUnit(edge = edge)

  when( bus.d.bits.opcode === TLMessages.Grant || bus.d.bits.opcode === TLMessages.GrantData ) {
    bus.d <> missUnit.io.dcache_grant
  } .elsewhen ( bus.d.bits.opcode === TLMessages.ReleaseAck ) {
    bus.d <> writeBackUnit.io.dcache_grant
  } .otherwise {
    assert(~bus.d.fire)
  }


  bus.a <> missUnit.io.dcache_acquire
  bus.b <> probeUnit.io.dcache_probe 
  bus.c <> writeBackUnit.io.dcache_release
  bus.e <> missUnit.io.dcache_grantAck

  





  missUnit.req 
  missUnit.rsp

  missUnit.miss_ban
  missUnit.release_ban


  probeUnit.req

  writeBackUnit.req

  writeBackUnit.release_ban
  writeBackUnit.miss_ban




  cache_dat.dat_addr_w
  cache_dat.dat_addr_r = Wire(UInt(aw.W))
  cache_dat.dat_en_w = Wire( Vec(cb, Bool()) )
  cache_dat.dat_en_r = Wire( Vec(cb, Bool()) )
  cache_dat.dat_info_wstrb = Wire(UInt((128/8).W))
  cache_dat.dat_info_w = Wire(UInt(128.W))
  cache_dat.dat_info_r = Wire( Vec(cb, UInt(128.W)) )


  cache_tag.tag_addr_r = Wire(UInt(aw.W))
  cache_tag.tag_addr_w = Wire(UInt(aw.W))
  cache_tag.tag_en_w = Wire( Vec(cb, Bool()) )
  cache_tag.tag_en_r = Wire( Vec(cb, Bool()) )  
  cache_tag.tag_info_r = Wire( Vec(cb, UInt(tag_w.W)) )
  cache_tag.addr_sel_w = tag_addr_w(addr_lsb+line_w-1, addr_lsb)
  cache_tag.addr_sel_r = tag_addr_r(addr_lsb+line_w-1, addr_lsb)
  cache_tag.tag_info_w = tag_addr_w(31, 32-tag_w)


}

