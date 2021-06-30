package rift2Core.L1Cache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._

import base._
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tilelink._
// import freechips.rocketchip.diplomaticobjectmodel.model.M


case class DcacheParameters(
  dw: Int,
  bk: Int,
  cb: Int,
  cl: Int,
  aw: Int = 32
) extends L1CacheParameters

trait HasDcacheParameters extends HasL1CacheParameters {
  val cacheParams = dcacheParameters
}

abstract class DcacheModule(implicit p: Parameters) extends L1CacheModule
  with HasDcacheParameters

abstract class DcacheBundle(implicit p: Parameters) extends L1CacheBundle
  with HasDcacheParameters



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

  assert(  
    PopCount(Seq(load, store, probe, grant, lr, sc, swap, add, and, or, xor, max, maxu, min, minu)) === 1.U
  )
  
  def is_atom = swap | add | and | or | xor | max | maxu | min | minu
  def is_tag_r = is_atom | load | store | lr | sc
  def is_dat_r = is_atom | load | probe | lr
  def is_tag_w = grant
  def is_dat_w = is_atom | store | sc | grant
}
  
  /**
  * @note all info iss in lsu can be executed out-of-order
  */ 

class Lsu_iss_info(implicit p: Parameters) extends DcacheBundle {
  val paddr    = UInt(64.W)
  val mask    = UInt(8.W)
  val data = UInt(64.W)
  val op = new Cache_op

  val rd0_phy = UInt(6.W)
}







class Lsu()(implicit p: Parameters) extends LazyModule with HasDcacheParameters{
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

class LsuImp(outer: Lsu) extends LazyModuleImp(outer)  with HasDcacheParameters {

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

  val cache_dat = new Cache_dat(dw, aw, bk, cb, cl)
  val cache_tag = new Cache_tag( dw, aw, bk, cb, cl ) 

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

  








  missUnit.io.req 

  missUnit.io.miss_ban
  missUnit.io.release_ban


  probeUnit.io.req

  writeBackUnit.io.req

  writeBackUnit.io.release_ban
  writeBackUnit.io.miss_ban




  cache_tag.tag_addr_r := 
    PriorityMux(Seq(
      probeUnit.io.req.valid -> probeUnit.io.req.paddr,
      (ls_queue.io.deq.valid & pd_queue.io.enq.ready) -> ls_queue.io.deq.bits.paddr
    ))

  for ( i <- 0 until cb ) yield {
    cache_tag.tag_en_r(i)   :=
      (probeUnit.io.req.valid) |
      (ls_queue.io.deq.valid & pd_queue.io.enq.ready)
  }

  pd_queue.io.enq.bits.tag(i) := cache_tag.tag_info_r(i)


  cache_dat.dat_addr_r := 
    PriorityMux(Seq(
      (probeUnit.io.req.valid) -> probeUnit.io.req.bits.paddr,
      (ls_queue.io.deq.valid & pd_queue.io.enq.ready & op === Read | atom) -> ls_queue.io.deq.bits.paddr
    ))


  for ( i <- 0 until cb ) yield {
    cache_dat.dat_en_r :=
    (probeUnit.io.req.valid) |
    (ls_queue.io.deq.valid & pd_queue.io.enq.ready & op === Read | atom  )

    val bk_sel = get_bk(ls_queue.io.deq.bits.paddr)
    pd_queue.io.enq.bits.rdata(i) := cache_dat.dat_info_r(i)(bk_sel)
  }


  cache_dat.dat_addr_w :=
    Mux(
      missUnit.io.rsp.valid,
      missUnit.io.rsp.bits.paddr,
      pd_queue.io.deq.bits.paddr
    )

  for ( i <- 0 until cb ) yield {
    cache_dat.dat_en_w :=
        missUnit.io.rsp.valid |
        (is_hit & pd_queue.io.deq.bits.op === write | atom)
  }


  cache_dat.dat_info_wstrb :=
    Mux(
      missUnit.io.rsp.valid,
      "hFFFFFFFF".U,
      pd_queue.io.deq.bits.mask
    )
  cache_dat.dat_info_w :=
     Mux(
      missUnit.io.rsp.valid,
      missUnit.io.rsp.bits.data,
      pd_queue.io.deq.bits.data
    )   

  cache_tag.tag_addr_w := missUnit.io.rsp.bits.paddr

  for ( i <- 0 until cb ) yield {
    cache_tag.tag_en_w   := missUnit.io.rsp.valid    
  }

  val is_hit = Wire(Bool())
  val is_hit_oh = Wire(Vec(cb, Bool()))
  val is_valid = RegInit( VecInit( Seq.fill(cl)(VecInit(Seq.fill(cb)(false.B))) ) )

  is_hit_oh := {
    val cl_sel   = get_cl(pd_queue.io.deq.bits.paddr)
    val tag_info = get_tag(pd_queue.io.deq.bits.paddr)
    val res = 
      for( i <- 0 until cb ) yield {
        pd_queue.io.deq.bits.tag(i) === tag_info & is_valid(cl_sel)(i)        
      }
    assert(PopCount(res) <= 1.U)
    VecInit(res)
  }
  is_hit := is_hit_oh.asUInt.orR
  

  def get_tag(paddr: UInt) = paddr(31,32-tag_w)
  def get_cl(paddr:  UInt) = paddr(addr_lsb+line_w-1, addr_lsb)
  def get_bk(paddr:  UInt) = paddr(addr_lsb-1, addr_lsb-log2Ceil(bk) )





  when() {
    ls_queue.io.enq <> io.lsu_iss_exe
  } .elsewhen {
    ls_queue.io.enq <> pd_queue.io.deq
  } .otherwise {
    
  }




}

class Info_lsu_s0s1(implicit p: Parameters) extends DcacheBundle {
  val paddr    = UInt(64.W)
  val wmask    = UInt(8.W)
  val wdata = UInt(64.W)
  val op = new Cache_op

  val rd0_phy = UInt(6.W)

  // def get_tag(paddr: UInt) = paddr(31,32-tag_w)
  // def get_cl(paddr:  UInt) = paddr(addr_lsb+line_w-1, addr_lsb)
  // def get_bk(paddr:  UInt) = paddr(addr_lsb-1, addr_lsb-log2Ceil(bk) )
  def get_bk = paddr(addr_lsb-1, addr_lsb-log2Ceil(bk) )
}

class Info_cache_rd(implicit p: Parameters) extends DcacheBundle {
  val rdata = Vec(cb, Vec(bk, UInt(64.W)))
  val tag   = Vec(cb, UInt(tag_w.W))
}

class Info_lsu_s1s2(implicit p: Parameters) extends DcacheBundle {
  val paddr    = UInt(64.W)
  val wmask    = UInt(8.W)
  val wdata = UInt(64.W)
  val op = new Cache_op

  val rd0_phy = UInt(6.W)

  val rdata = Vec(cb, Vec(bk, UInt(64.W)))
  val tag   = Vec(cb, UInt(tag_w.W))

  def get_tag = paddr(31,32-tag_w)
  def get_cl  = paddr(addr_lsb+line_w-1, addr_lsb)
  def get_bk  = paddr(addr_lsb-1, addr_lsb-log2Ceil(bk) )

}


class L1_rd_stage( cache_dat: Cache_dat, cache_tag: Cache_tag )(implicit p: Parameters) extends DcacheModule {
  val io = IO(new Bundle {
    val rd_in  = Flipped(DecoupledIO(new Info_lsu_s0s1))
    val rd_out = DecoupledIO(new Info_lsu_s1s2)
  })



  val s1s2_pipe = Module( new Queue(new Info_cache_rd, 1, true, true) )

  val bk_sel = io.rd_in.bits.get_bk


  cache_tag.tag_addr_r := io.rd_in.bits.paddr
  cache_dat.dat_addr_r := io.rd_in.bits.paddr

  for ( i <- 0 until cb ) yield {
    cache_tag.tag_en_r(i) :=
      io.rd_in.fire & io.rd_in.bits.op.is_tag_r
  }



  for ( i <- 0 until cb; j <- 0 until bk ) yield {
    cache_dat.dat_en_r(i)(j) := 
      io.rd_in.fire &
      io.rd_in.bits.op.is_dat_r & (
        io.rd_in.bits.op.probe |
        j.U === bk_sel
      )

  }





  s1s2_pipe.io.enq.valid := io.rd_in.fire

  for ( i <- 0 until cb; j <- 0 until bk ) yield { s1s2_pipe.io.enq.bits.rdata(i)(j) := cache_dat.dat_info_r(i)(j)}
  for ( i <- 0 until cb )                  yield { s1s2_pipe.io.enq.bits.tag(i) := cache_tag.tag_info_r(i) }
  
  io.rd_out.valid := s1s2_pipe.io.deq.valid
  io.rd_out.bits.rdata(i)(j) := s1s2_pipe.io.deq.bits.rdata(i)(j)
  io.rd_out.bits.tag(i)      := s1s2_pipe.io.deq.bits.tag(i)
  
  io.rd_out.bits.paddr    := RegEnable(io.rd_in.bits.paddr, io.rd_in.fire)
  io.rd_out.bits.wmask    := RegEnable(io.rd_in.bits.wmask, io.rd_in.fire)
  io.rd_out.bits.wdata    := RegEnable(io.rd_in.bits.wdata, io.rd_in.fire)
  io.rd_out.bits.op       := RegEnable(io.rd_in.bits.op,    io.rd_in.fire)
  io.rd_out.bits.rd0_phy  := RegEnable(io.rd_in.bits.rd0_phy, io.rd_in.fire)
  s1s2_pipe.io.deq.ready := io.rd_out.ready

  io.rd_in.ready := s1s2_pipe.io.enq.ready


}


class L1_wr_stage( cache_dat: Cache_dat, cache_tag: Cache_tag, missUnit: MissUnit, writeBackUnit: WriteBackUnit ) (implicit p: Parameters) extends DcacheModule {
  val io = IO(new Bundle{
    val wr_in  = Flipped(new DecoupledIO(new Info_lsu_s1s2))    
  })



  val is_hit = Wire(Bool())
  val is_hit_oh = Wire(Vec(cb, Bool()))
  val is_valid = RegInit( VecInit( Seq.fill(cl)(VecInit(Seq.fill(cb)(false.B))) ) )
  val is_dirty = RegInit( VecInit( Seq.fill(cl)(VecInit(Seq.fill(cb)(false.B))) ) )

  is_hit_oh := {
    val cl_sel   = get_cl(pd_queue.io.deq.bits.paddr)
    val tag_info = get_tag(pd_queue.io.deq.bits.paddr)
    val res = 
      for( i <- 0 until cb ) yield {
        pd_queue.io.deq.bits.tag(i) === tag_info & is_valid(cl_sel)(i)        
      }
    assert(PopCount(res) <= 1.U)
    VecInit(res)
  }
  val hit_cb_sel = OHToUInt(is_hit_oh)
  is_hit := is_hit_oh.orR
  
  val rpl_sel = Wire(UInt(log2Ceil(cb).W))
  
  rpl_sel := {
    val cl_sel = get_cl(io.wr_in.bits.paddr)
    val is_emptyBlock_exist = is_valid(cl_sel).contains(false.B)
    val emptyBlock_sel = is_valid(cl_sel).indexWhere( (x:Bool) => (x === false.B) )
    Mux( is_emptyBlock_exist, emptyBlock_sel, LFSR(16) )
  }
  


  cache_dat.dat_addr_w := io.wr_in.bits.paddr

  for ( i <- 0 until cb; j <- 0 until bk ) yield {
    cache_dat.dat_en_w(i)(j) :=
      io.wr_in.valid &
      Mux( io.wr_in.bits.op === write | raed | atom,  i.U === hit_cb_sel, i.U === rpl_sel) &
      Mux1H(Seq(
        (io.wr_in.bits.op === grantData) -> true.B,
        (io.wr_in.bits.op === store    ) -> j.U === get_bk(io.wr_in.bits.paddr),
        (io.wr_in.bits.op === atom     ) -> j.U === get_bk(io.wr_in.bits.paddr),
        (io.wr_in.bits.op === read    ) -> false.B
      ))
  }

  for ( j <- 0 until bk ) yield {
    cache_dat.dat_info_wstrb(j) :=
      Mux1H(Seq(
        (io.wr_in.bits.op === grantData) -> "hFF".U,
        (io.wr_in.bits.op === store    ) -> io.wr_in.bits.wmask,
        (io.wr_in.bits.op === atom     ) -> io.wr_in.bits.wmask,
      ))    
  }

  for ( j <- 0 until bk ) yield {
    cache_dat.dat_info_w(j) := io.wr_in.bits.wdata(j)
  }

  cache_tag.tag_addr_w := io.wr_in.bits.paddr

  for ( i <- 0 until cb; j <- 0 until bk ) yield {
    cache_tag.tag_en_w(i)(j) :=
      (i.U === rpl_sel) & io.wr_in.valid & io.wr_in.op === grantData
  }

  when( io.wr_in.fire ) {
    when( io.wr_in.bits.op === grantData ) {
      is_valid(cl_sel)(cb_sel) := true.B
      is_dirty(cl_sel)(cb_sel) := false.B
    }
    when( io.wr_in.bits.op === write | atom ) {
      is_dirty(cl_sel)(cb_sel) := true.B
    }
    when( (io.wr_in.bits.op === read | write | atom) & ~is_hit ) {
      is_valid(cl_sel)(cb_sel) := false.B
    }

  }




  missUnit.io.req.valid := io.wr_in.fire & io.wr_in.bits.op === load | write | atom & is_hit
  missUnit.io.req.bits.paddr := io.wr_in.bits.paddr

  writeBackUnit.io.req.valid := io.wr_in.fire & io.wr_in.bits.op === load | write | atom & ~is_hit
  writeBackUnit.io.req.bits.addr := io.wr_in.bits.paddr
  writeBackUnit.io.req.bits.data := Cat(io.wr_in.bits.rdata.map.reserve)
  writeBackUnit.io.req.bits.is_releaseData := io.wr_in.bits.op === load | write | atom & ~is_hit & is_dirty(cl_sel)(cb_sel)
  writeBackUnit.io.req.bits.is_release := io.wr_in.bits.op === load | write | atom & ~is_hit & ~is_dirty(cl_sel)(cb_sel)
  writeBackUnit.io.req.bits.is_probe := io.wr_in.bits.op === probe & ~is_hit & ~is_dirty(cl_sel)(cb_sel)
  writeBackUnit.io.req.bits.is_probeData := io.wr_in.bits.op === probe & ~is_hit & is_dirty(cl_sel)(cb_sel)

  lu_exe_iwb.valid := io.wr_in.fire & io.wr_in.bits.op === load | atom & is_hit
  
  lu_exe_iwb.bits.res := io.wr_in.bits.rdata(cb_sel)(bk_sel)

  lu_exe_iwb.bits.rd0_phy := io.wr_in.bits.rd0_phy





}





