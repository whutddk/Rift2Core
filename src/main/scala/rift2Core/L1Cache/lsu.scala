package rift2Core.L1Cache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._

import base._
import rift2Core.cache._
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tilelink._


/**
  * @note all info iss in lsu can be executed out-of-order
  */ 

class Lsu_iss_info extends Bundle {
  val paddr    = UInt(64.W)
  val mask    = UInt(8.W)
  val data = UInt(64.W)
  val op = UInt(8.W)

  val rd0_phy = UInt(6.W)
}

class Info_pd extends Lsu_iss_info {
  val tag = UInt(tag_w.W)
  val is_valid = Bool()
}



class Lsu()(implicit p: Parameters) extends LazyModule {
  val clientParameters = TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = "dcache",
      sourceId = IdRange(0, 1),
      supportsProbe = TransferSizes(32)
    ))
  )

  val clientNode = TLClientNode(Seq(clientParameters))

  lazy val module = new LsuImp(this)
}

class LsuImp(outer: Lsu) extends LazyModuleImp(outer) {

  val ( bus, edge ) = outer.clientNode.out.head

  val io = IO(new Bundle{
    val lsu_iss_exe = Flipped(new DecoupledIO(new Lsu_iss_info))
    val lu_exe_iwb = new DecoupledIO(new Exe_iwb_info)



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

  val ls_queue = Module(new Queue(new Lsu_iss_info,  16, true, false))
  val pd_queue = Module(new Queue(new Info_pd,  1, false, true))

  val cache_dat = Module(new Cache_dat())
  val cache_tag = Module(new Cache_tag())

  val missUnit = Module(new MissUnit(edge = edge, entry = 8))
  val probeUnit =  Module(new ProbeUnit(edge = edge))
  val writeBackUnit =  Module(new WriteBackUnit(edge = edge))

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




  cache_tag.tag_addr_r := ls_queue.io.deq.bits.paddr
  cache_tag.tag_en_r   := ls_queue.io.deq.valid & pd_queue.io.enq.ready
  pd_queue.io.enq.bits.tag := cache_tag.tag_info_r


  cache_dat.dat_addr_r := ls_queue.io.deq.bits.paddr
  cache_dat.dat_en_r   := op === Read & ls_queue.io.deq.valid & pd_queue.io.enq.ready
  pd_queue.io.enq.bits.data := cache_dat.dat_info_r 



  cache_dat.dat_addr_w :=
    Mux(
      ,
      ,
      pd_queue.io.deq.bits.paddr
    )
  cache_dat.dat_en_w :=
    Mux(
      ,
      ,
      is_hit & pd_queue.io.deq.bits.op === write | atom
    )
  cache_dat.dat_info_wstrb :=
    Mux(
      ,
      "hFFFF".U,
      pd_queue.io.deq.bits.mask
    )
  cache_dat.dat_info_w :=
     Mux(
      ,
      ,
      pd_queue.io.deq.bits.data
    )   




  cache_tag.tag_addr_w :=
  cache_tag.tag_en_w
  cache_tag.addr_sel_w
  cache_tag.tag_info_w

  when() {
    ls_queue.io.enq <> io.lsu_iss_exe
  } .elsewhen {
    ls_queue.io.enq <> pd_queue.io.deq
  } .otherwise {
    
  }




}

