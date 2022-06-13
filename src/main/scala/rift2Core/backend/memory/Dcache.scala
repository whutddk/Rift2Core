package rift2Core.L1Cache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import rift2Core.define._

import rift._
import base._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba.axi4._
import sifive.blocks.inclusivecache._
import axi._
import chisel3.util.random._
import rift2Core.define._
import rift2Core.backend.memory._


case class DcacheParameters(
  dw: Int,
  bk: Int,
  cb: Int,
  cl: Int,
  aw: Int = 32
) extends L1CacheParameters

trait HasDcacheParameters extends HasL1CacheParameters {
  val dcacheParams: L1CacheParameters

  def dw = dcacheParams.dw
  def bk = dcacheParams.bk
  def cb = dcacheParams.cb
  def cl = dcacheParams.cl
  def aw = dcacheParams.aw

  def addr_lsb = log2Ceil(dw/8)
  def bk_w = log2Ceil(bk)
  def line_w   = log2Ceil(cl)
  def cb_w = log2Ceil(cb)


  def tag_w    = aw - addr_lsb - line_w - bk_w

  // require( (addr_lsb + line_w) == 12 )
  
}

abstract class DcacheModule(implicit p: Parameters) extends L1CacheModule
  with HasDcacheParameters

abstract class DcacheBundle(implicit p: Parameters) extends L1CacheBundle
  with HasDcacheParameters

abstract class DcacheObject(implicit p: Parameters) extends L1CacheObject
  with HasDcacheParameters

class Cache_op extends Lsu_isa {

  val probe = Bool()
  val grant = Bool()
  val preft = Bool()

  def is_atom = is_amo
  def is_access = is_atom | is_lu | is_su | is_lr | is_sc
  def is_tag_r = is_atom | is_lu | is_su | is_lr | is_sc | grant | probe | preft
  def is_dat_r = is_atom | is_lu | is_lr | grant | probe
  def is_tag_w = grant
  def is_dat_w = is_atom | is_su | is_sc | grant
  def is_dirtyOp = is_atom | is_su | is_sc
  def is_wb = is_atom | is_lu | is_lr

}


trait Info_cache_raw extends DcacheBundle {
  val paddr = UInt(64.W)
  val wdata  = UInt(dw.W)
  val wstrb  = UInt((dw/8).W)

  val fun    = new Cache_op
  val rd = new Register_dstntn(64)

  def tag_sel = paddr(31,32-tag_w)
  def cl_sel  = paddr(addr_lsb+bk_w + line_w-1, addr_lsb+bk_w)


  def wmask = Strb2Mask(wstrb)

}


trait Info_tag_dat extends DcacheBundle {
  val rdata = Vec(cb,  UInt(dw.W))
  val tag   = Vec(cb, UInt(tag_w.W))
}

trait Info_sc_idx extends DcacheBundle { val chk_idx = UInt(4.W) }

class Info_cache_rd(implicit p: Parameters) extends Info_tag_dat
class Info_cache_s0s1(implicit p: Parameters) extends DcacheBundle with Info_cache_raw with Info_sc_idx
class Info_cache_s1s2(implicit p: Parameters) extends DcacheBundle with Info_cache_raw with Info_sc_idx with Info_tag_dat


class Info_cache_sb extends Lsu_iss_info

class Info_cache_retn(implicit p: Parameters) extends DcacheBundle with Info_sc_idx {
  val wb = new WriteBack_info(dw=64,dp=64)
  val is_load_amo = Bool()

  val is_flw = Bool()
  val is_fld = Bool()

  def is_iwb = ~is_fwb
  def is_fwb = is_flw | is_fld

}





/** the fisrt stage to read out the data */
class L1d_rd_stage()(implicit p: Parameters) extends DcacheModule {
  val io = IO(new Bundle {
    val rd_in  = Flipped(DecoupledIO(new Info_cache_s0s1))
    val rd_out = DecoupledIO(new Info_cache_s1s2)

    // val overlap_paddr = Output(UInt(64.W))

    val dat_addr_r = Output(UInt(aw.W))
    val dat_en_r   = Output( Vec(cb,  Bool()) )
    val dat_info_r = Input( Vec(cb, UInt(dw.W)) )

    val tag_addr_r = Output(UInt(aw.W))
    val tag_en_r   = Output( Vec(cb, Bool()) )	
    val tag_info_r = Input( Vec(cb, UInt(tag_w.W)) )
  })


  val info_bypass_fifo = Module(new Queue(new Info_cache_s0s1, 1, true, false))

  // io.overlap_paddr := io.rd_in.bits.paddr
  io.tag_addr_r := io.rd_in.bits.paddr
  io.dat_addr_r := io.rd_in.bits.paddr

  for ( i <- 0 until cb ) yield {
    io.tag_en_r(i) :=
      info_bypass_fifo.io.enq.fire & io.rd_in.bits.fun.is_tag_r
  }



  for ( i <- 0 until cb ) yield {
    io.dat_en_r(i) := 
      info_bypass_fifo.io.enq.fire &
      io.rd_in.bits.fun.is_dat_r & (
        (io.rd_in.bits.fun.probe) |
        (io.rd_in.bits.fun.grant) |
        (io.rd_in.bits.fun.is_access )
      )
  }


  io.rd_out.valid := RegNext(io.rd_in.valid, false.B)

  for( i <- 0 until cb ) yield { io.rd_out.bits.rdata(i) := io.dat_info_r(i) } 
  for( i <- 0 until cb ) yield { io.rd_out.bits.tag(i)   := io.tag_info_r(i) }

  

  info_bypass_fifo.io.enq <> io.rd_in

  io.rd_out.bits.paddr    := info_bypass_fifo.io.deq.bits.paddr
  io.rd_out.bits.wstrb    := info_bypass_fifo.io.deq.bits.wstrb
  io.rd_out.bits.wdata    := info_bypass_fifo.io.deq.bits.wdata
  io.rd_out.bits.fun       := info_bypass_fifo.io.deq.bits.fun
  io.rd_out.bits.rd       := info_bypass_fifo.io.deq.bits.rd
  io.rd_out.bits.chk_idx  := info_bypass_fifo.io.deq.bits.chk_idx
  io.rd_out.valid := info_bypass_fifo.io.deq.valid
  info_bypass_fifo.io.deq.ready := io.rd_out.ready

}

/** stage 2 will write the cache */
class L1d_wr_stage() (implicit p: Parameters) extends DcacheModule {
  val io = IO(new Bundle{
    val wr_in  = Flipped(new DecoupledIO(new Info_cache_s1s2))
    val reload = new DecoupledIO(new Info_cache_s0s1)
    val deq = DecoupledIO(new Info_cache_retn)

    val tag_addr_w = Output(UInt(aw.W))
    val tag_en_w = Output( Vec(cb, Bool()) )

    val dat_addr_w = Output(UInt(aw.W))
    val dat_en_w = Output( Vec(cb, Bool()) )
    val dat_info_wstrb = Output( UInt((dw/8).W))
    val dat_info_w = Output( UInt(dw.W))

    val missUnit_req = new DecoupledIO(new Info_miss_req)
    val wb_req = DecoupledIO(new Info_writeBack_req)
    val pb_req = DecoupledIO(new Info_writeBack_req)

    // val overlap_wdata = Input(UInt(64.W))
    // val overlap_wstrb = Input(UInt(8.W))

    val flush = Input(Bool())
  })

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

  val flush_reg_0 = RegNext(io.flush, false.B)
  val flush_reg_1 = RegNext(flush_reg_0, false.B)
  val flush_reg_2 = RegNext(flush_reg_1, false.B)
  val is_pending_lr = RegInit(false.B)
  val is_lr_64_32n = RegInit(false.B)
  val lr_addr = RegInit(0.U(64.W))

  val is_sc_fail = 
    ~is_pending_lr | 
    (is_lr_64_32n & io.wr_in.bits.fun.is_word) |
    (~is_lr_64_32n & io.wr_in.bits.fun.is_dubl) |
    lr_addr =/= io.wr_in.bits.paddr


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
        io.wr_in.bits.fun.is_access -> hit_sel,
        io.wr_in.bits.fun.preft -> hit_sel,
        io.wr_in.bits.fun.probe -> hit_sel,
        io.wr_in.bits.fun.grant -> rpl_sel
      ))
    )

  when( io.wr_in.fire ) {
    when( io.wr_in.bits.fun.probe ) { assert(is_hit) } //l2 will never request a empty probe
    when( io.wr_in.bits.fun.grant ) {  } 
  }

  io.dat_addr_w := io.wr_in.bits.paddr

  for ( i <- 0 until cb ) yield {
    io.dat_en_w(i) :=
      io.wr_in.fire &
      i.U === cb_sel &
      io.wr_in.bits.fun.is_dat_w & (
        (
          io.wr_in.bits.fun.grant
        ) |
        (
          io.wr_in.bits.fun.is_access & is_hit &
          Mux( io.wr_in.bits.fun.is_sc, ~is_sc_fail, true.B)
        )
      )
  }


  io.dat_info_wstrb :=
    Mux1H(Seq(
      (io.wr_in.bits.fun.grant) -> Fill(dw/8, 1.U),
      (io.wr_in.bits.fun.is_access) -> io.wr_in.bits.wstrb
    ))    


  {
    val fun = io.wr_in.bits.fun


    val high_sel  = io.wr_in.bits.paddr(2) === 1.U
    val amo_reAlign_64_a = Wire(UInt(64.W))
    val amo_reAlign_64_b = Wire(UInt(64.W))

    amo_reAlign_64_a := reAlign_data( from = 256, to = 64, io.wr_in.bits.wdata, io.wr_in.bits.paddr )
    amo_reAlign_64_b := reAlign_data( from = 256, to = 64, io.wr_in.bits.rdata(cb_sel), io.wr_in.bits.paddr )

    val cmp_a_sel = Mux(high_sel, amo_reAlign_64_a(63,32), amo_reAlign_64_a(31,0))
    val cmp_b_sel = Mux(high_sel, amo_reAlign_64_b(63,32), amo_reAlign_64_b(31,0))
     
    
    io.dat_info_w := 
      Mux1H(Seq(
        fun.grant -> io.wr_in.bits.wdata,
        fun.is_su -> io.wr_in.bits.wdata,
        fun.is_sc -> io.wr_in.bits.wdata,
        (fun.amoswap_w | fun.amoswap_d) -> reAlign_data( from = 64, to = 256,  amo_reAlign_64_a, io.wr_in.bits.paddr ),
        (fun.amoadd_w                 ) -> reAlign_data( from = 64, to = 256, ( Mux(high_sel, amo_reAlign_64_a >> 32 << 32, amo_reAlign_64_a) + amo_reAlign_64_b), io.wr_in.bits.paddr ), //when sel msb-32, set one of op's lsb-32 to zore to prevent carry-in
        (fun.amoadd_d                 ) -> reAlign_data( from = 64, to = 256, (amo_reAlign_64_a + amo_reAlign_64_b), io.wr_in.bits.paddr ),
        (fun.amoxor_w  | fun.amoxor_d ) -> reAlign_data( from = 64, to = 256, (amo_reAlign_64_a ^ amo_reAlign_64_b), io.wr_in.bits.paddr ),
        (fun.amoand_w  | fun.amoand_d ) -> reAlign_data( from = 64, to = 256, (amo_reAlign_64_a & amo_reAlign_64_b), io.wr_in.bits.paddr ),
        (fun.amoor_w   | fun.amoor_d  ) -> reAlign_data( from = 64, to = 256, (amo_reAlign_64_a | amo_reAlign_64_b), io.wr_in.bits.paddr ),


        (fun.amomin_w ) -> reAlign_data( from = 64, to = 256, Mux(cmp_a_sel.asSInt        < cmp_b_sel.asSInt,        amo_reAlign_64_a, amo_reAlign_64_b), io.wr_in.bits.paddr),
        (fun.amomin_d ) -> reAlign_data( from = 64, to = 256, Mux(amo_reAlign_64_a.asSInt < amo_reAlign_64_b.asSInt, amo_reAlign_64_a, amo_reAlign_64_b), io.wr_in.bits.paddr),
        (fun.amomax_w ) -> reAlign_data( from = 64, to = 256, Mux(cmp_a_sel.asSInt        < cmp_b_sel.asSInt,        amo_reAlign_64_b, amo_reAlign_64_a), io.wr_in.bits.paddr),
        (fun.amomax_d ) -> reAlign_data( from = 64, to = 256, Mux(amo_reAlign_64_a.asSInt < amo_reAlign_64_b.asSInt, amo_reAlign_64_b, amo_reAlign_64_a), io.wr_in.bits.paddr),
        (fun.amominu_w) -> reAlign_data( from = 64, to = 256, Mux(cmp_a_sel               < cmp_b_sel,               amo_reAlign_64_a, amo_reAlign_64_b), io.wr_in.bits.paddr),
        (fun.amominu_d) -> reAlign_data( from = 64, to = 256, Mux(amo_reAlign_64_a        < amo_reAlign_64_b,        amo_reAlign_64_a, amo_reAlign_64_b), io.wr_in.bits.paddr),
        (fun.amomaxu_w) -> reAlign_data( from = 64, to = 256, Mux(cmp_a_sel               < cmp_b_sel,               amo_reAlign_64_b, amo_reAlign_64_a), io.wr_in.bits.paddr),
        (fun.amomaxu_d) -> reAlign_data( from = 64, to = 256, Mux(amo_reAlign_64_a        < amo_reAlign_64_b,        amo_reAlign_64_b, amo_reAlign_64_a), io.wr_in.bits.paddr),
              
      ))

  }

  io.tag_addr_w := io.wr_in.bits.paddr

  for ( i <- 0 until cb ) yield {
    io.tag_en_w(i) :=
      (i.U === cb_sel) & io.wr_in.fire & io.wr_in.bits.fun.grant
  }

  when( io.wr_in.fire ) {
    when( io.wr_in.bits.fun.grant ) {
      is_valid(cl_sel)(cb_sel) := true.B
      is_dirty(cl_sel)(cb_sel) := false.B
    }
    when( io.wr_in.bits.fun.is_dirtyOp ) {
      is_dirty(cl_sel)(cb_sel) := true.B
    }

    when( io.wr_in.bits.fun.probe ) {
      is_valid(cl_sel)(cb_sel) := false.B
    }

  }


  io.wr_in.ready :=
    Mux1H(Seq(
      io.wr_in.bits.fun.is_access -> Mux( is_hit, io.deq.ready, io.reload.ready & io.missUnit_req.ready ),
      io.wr_in.bits.fun.probe -> io.pb_req.fire,
      io.wr_in.bits.fun.grant -> io.wb_req.ready,
      io.wr_in.bits.fun.preft -> Mux( is_hit, true.B, io.missUnit_req.ready ),
    ))

  io.missUnit_req.valid := 
    io.wr_in.valid &
      Mux1H(Seq(
        io.wr_in.bits.fun.is_access -> (~is_hit & io.missUnit_req.ready & io.reload.ready ),
        io.wr_in.bits.fun.preft     -> (~is_hit & io.missUnit_req.ready ),
      ))

  io.missUnit_req.bits.paddr := 
    Mux( io.missUnit_req.valid, io.wr_in.bits.paddr & ("hffffffff".U << addr_lsb.U), 0.U )

  io.wb_req.valid :=
    io.wr_in.valid &
      ( io.wr_in.bits.fun.grant & ~is_valid(cl_sel).contains(false.B) )

  io.pb_req.valid :=
    io.wr_in.valid & 
      io.wr_in.bits.fun.probe

  io.wb_req.bits.addr := 
    Mux( io.wb_req.valid, Cat(io.wr_in.bits.tag(cb_sel), cl_sel, 0.U(addr_lsb.W)), 0.U )
    
   io.pb_req.bits.addr :=
    Mux( io.pb_req.valid, (io.wr_in.bits.paddr & ("hffffffff".U << addr_lsb.U)), 0.U )

  io.wb_req.bits.data := 
    Mux( io.wb_req.valid, io.wr_in.bits.rdata(cb_sel), 0.U )

  io.pb_req.bits.data :=
    Mux( io.pb_req.valid, io.wr_in.bits.rdata(cb_sel), 0.U )

  io.wb_req.bits.is_releaseData :=
    Mux( io.wb_req.valid, io.wr_in.bits.fun.grant & is_dirty(cl_sel)(cb_sel), 0.U )

  io.wb_req.bits.is_release :=
    Mux( io.wb_req.valid, io.wr_in.bits.fun.grant & ~is_dirty(cl_sel)(cb_sel), 0.U )

  io.wb_req.bits.is_probeData := false.B
  io.wb_req.bits.is_probe := false.B

  io.pb_req.bits.is_releaseData := false.B
  io.pb_req.bits.is_release := false.B
  io.pb_req.bits.is_probeData :=
    Mux( io.pb_req.valid, io.wr_in.bits.fun.probe & is_dirty(cl_sel)(cb_sel), 0.U )

  io.pb_req.bits.is_probe :=
    Mux( io.pb_req.valid, io.wr_in.bits.fun.probe & ~is_dirty(cl_sel)(cb_sel), 0.U )


  io.reload.valid :=
    io.wr_in.valid & io.wr_in.bits.fun.is_access & ~is_hit & io.missUnit_req.ready & io.reload.ready
  assert( ~(io.reload.valid & ~io.reload.ready), "Assert Failed at wr_state 2, reload failed!" )

  io.reload.bits.paddr := 
    Mux( io.reload.valid, io.wr_in.bits.paddr, 0.U )
    
  io.reload.bits.wstrb :=
    Mux( io.reload.valid, io.wr_in.bits.wstrb, 0.U )
    
  io.reload.bits.wdata :=
    Mux( io.reload.valid, io.wr_in.bits.wdata, 0.U )

  io.reload.bits.fun :=
    Mux( io.reload.valid, io.wr_in.bits.fun, 0.U.asTypeOf(new Cache_op) )

  io.reload.bits.rd :=
    Mux( io.reload.valid, io.wr_in.bits.rd, 0.U.asTypeOf(new Register_dstntn(64)) )

  io.reload.bits.chk_idx :=
    Mux( io.reload.valid, io.wr_in.bits.chk_idx, 0.U )

  io.deq.valid := io.wr_in.valid & io.wr_in.bits.fun.is_access & is_hit
  io.deq.bits.wb.res := {
    val rdata = io.wr_in.bits.rdata(cb_sel)  //align 256
    val paddr = io.wr_in.bits.paddr
    val fun = io.wr_in.bits.fun
    val overlap_wdata = io.wr_in.bits.wdata
    val overlap_wstrb = io.wr_in.bits.wstrb
    
    val res_pre_pre = {
      val res = Wire( UInt(64.W) )
      val (new_data, new_strb) = overlap_wr( rdata, 0.U(32.W), overlap_wdata, overlap_wstrb)
      
      val overlap_data = Mux( io.wr_in.bits.fun.is_lu, new_data, rdata) //align 256

      res := reAlign_data( from = 256, to = 64, overlap_data, paddr )
      res
    }
    val res_pre = get_loadRes( fun, paddr, res_pre_pre ) //align 8

    val res = Mux(
      io.wr_in.bits.fun.is_sc,
      Mux( is_sc_fail, 1.U, 0.U ),
      res_pre
    )
    Mux( io.deq.valid, res, 0.U )
  }

  io.deq.bits.wb.rd0 := 
    Mux( io.deq.valid, io.wr_in.bits.rd.rd0, 0.U )  

  io.deq.bits.chk_idx :=
    Mux( io.deq.valid, io.wr_in.bits.chk_idx, 0.U )
    
  io.deq.bits.is_load_amo :=
    Mux( io.deq.valid, io.wr_in.bits.fun.is_wb, false.B )
    
  io.deq.bits.is_flw := Mux( io.deq.valid, io.wr_in.bits.fun.flw, false.B )
  io.deq.bits.is_fld := Mux( io.deq.valid, io.wr_in.bits.fun.fld, false.B )



  when( io.flush | flush_reg_0 | flush_reg_1 | flush_reg_2 ) {
    is_pending_lr := false.B
  }
  .elsewhen( io.deq.fire & io.wr_in.bits.fun.is_lr ) {
    is_pending_lr := true.B
    is_lr_64_32n := io.wr_in.bits.fun.is_dubl
    lr_addr := io.wr_in.bits.paddr

    assert( io.wr_in.bits.fun.is_dubl | io.wr_in.bits.fun.is_word )
  }
  .elsewhen( io.deq.fire & io.wr_in.bits.fun.is_sc ) {
    is_pending_lr := false.B
  }
  .elsewhen( io.wr_in.fire & io.wr_in.bits.fun.probe ) {
    when( tag_sel === lr_addr(31,32-tag_w) ) {
      is_pending_lr := false.B
    }
  }
  .elsewhen( io.deq.fire & (io.wr_in.bits.fun.is_su | (io.wr_in.bits.fun.is_amo & ~io.wr_in.bits.fun.is_lrsc)) ) {
    when( tag_sel === lr_addr(31,32-tag_w) ) {
      is_pending_lr := false.B
    }   
  }








}

class Dcache(edge: TLEdgeOut, id: Int)(implicit p: Parameters) extends DcacheModule {
  val io = IO(new Bundle{
    val enq = Flipped(new DecoupledIO(new Info_cache_s0s1))
    val deq = new DecoupledIO(new Info_cache_retn)
    val is_empty = Output(Bool())


    val flush = Input(Bool())

    val missUnit_dcache_acquire = new DecoupledIO(new TLBundleA(edge.bundle))
    val missUnit_dcache_grant   = Flipped(new DecoupledIO(new TLBundleD(edge.bundle)))
    val missUnit_dcache_grantAck  = DecoupledIO(new TLBundleE(edge.bundle))

    val probeUnit_dcache_probe = Flipped(new DecoupledIO(new TLBundleB(edge.bundle)))

    val writeBackUnit_dcache_release = new DecoupledIO(new TLBundleC(edge.bundle))
    val writeBackUnit_dcache_grant   = Flipped(new DecoupledIO(new TLBundleD(edge.bundle)))
  })

  val cache_dat = new Cache_dat( dw, aw, cb, cl, bk = bk )
  val cache_tag = new Cache_tag( dw, aw, cb, cl, bk = bk ) 
  val missUnit = Module(new MissUnit(edge = edge, entry = 8, setting = 2, id = id))
  val probeUnit = Module(new ProbeUnit(edge = edge, id = id))
  val writeBackUnit = Module(new WriteBackUnit(edge = edge, setting = 2, id = id))

  val lsEntry = Module(new Queue(new Info_cache_s0s1, 16, pipe = false, flow = true))
  val rd_stage = Module(new L1d_rd_stage())
  val wr_stage = Module(new L1d_wr_stage())

  val cache_buffer = Module(new Cache_buffer)

  io.missUnit_dcache_acquire  <> missUnit.io.cache_acquire
  missUnit.io.cache_grant <> io.missUnit_dcache_grant
  io.missUnit_dcache_grantAck <> missUnit.io.cache_grantAck
  probeUnit.io.cache_probe <> io.probeUnit_dcache_probe
  io.writeBackUnit_dcache_release <> writeBackUnit.io.cache_release
  writeBackUnit.io.cache_grant <> io.writeBackUnit_dcache_grant



  rd_stage.io.dat_addr_r <> cache_dat.dat_addr_r
  rd_stage.io.dat_en_r   <> cache_dat.dat_en_r
  cache_dat.dat_info_r   <> rd_stage.io.dat_info_r
  rd_stage.io.tag_addr_r <> cache_tag.tag_addr_r
  rd_stage.io.tag_en_r   <> cache_tag.tag_en_r
  // io.overlap.paddr  := rd_stage.io.overlap_paddr
  cache_tag.tag_info_r   <> rd_stage.io.tag_info_r

  wr_stage.io.tag_addr_w        <> cache_tag.tag_addr_w
  wr_stage.io.tag_en_w          <> cache_tag.tag_en_w
  wr_stage.io.dat_addr_w        <> cache_dat.dat_addr_w
  wr_stage.io.dat_en_w          <> cache_dat.dat_en_w
  wr_stage.io.dat_info_wstrb    <> cache_dat.dat_info_wstrb
  wr_stage.io.dat_info_w        <> cache_dat.dat_info_w
  wr_stage.io.missUnit_req      <> missUnit.io.req
  wr_stage.io.wb_req <> writeBackUnit.io.wb_req
  wr_stage.io.pb_req <> writeBackUnit.io.pb_req
  wr_stage.io.flush := io.flush

  missUnit.io.miss_ban := writeBackUnit.io.miss_ban
  writeBackUnit.io.release_ban := missUnit.io.release_ban


  val op_arb = Module(new Arbiter( new Info_cache_s0s1, 2))
  val rd_arb = Module(new Arbiter( new Info_cache_s0s1, 3))

  val reload_fifo = Module( new Queue( new Info_cache_s0s1, 1, false, true) )

  wr_stage.io.reload <> reload_fifo.io.enq
  reload_fifo.io.deq <> op_arb.io.in(0)
  

  io.enq <> Decoupled1toN( VecInit( op_arb.io.in(1), cache_buffer.io.buf_enq ) )
  op_arb.io.in(1).bits.chk_idx := cache_buffer.io.enq_idx //override

  op_arb.io.out <> lsEntry.io.enq 

  rd_arb.io.in(0).bits := pkg_Info_cache_s0s1( missUnit.io.rsp.bits )
  rd_arb.io.in(0).valid := missUnit.io.rsp.valid
  missUnit.io.rsp.ready := rd_arb.io.in(0).ready

  rd_arb.io.in(1).bits := pkg_Info_cache_s0s1(probeUnit.io.req.bits) 
  rd_arb.io.in(1).valid := probeUnit.io.req.valid
  probeUnit.io.req.ready := rd_arb.io.in(1).ready

  lsEntry.io.deq <> rd_arb.io.in(2)
  rd_arb.io.out <> rd_stage.io.rd_in

  rd_stage.io.rd_out <> wr_stage.io.wr_in



  wr_stage.io.deq <> Decoupled1toN( VecInit( io.deq, cache_buffer.io.buf_deq ) )

  io.is_empty := cache_buffer.io.is_storeBuff_empty
}


