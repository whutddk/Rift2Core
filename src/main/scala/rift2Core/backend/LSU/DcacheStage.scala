/*
  Copyright (c) 2020 - 2022 Wuhan University of Technology <295054118@whut.edu.cn>

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

package rift2Core.backend.lsu

import chisel3._
import chisel3.util._
import chisel3.util.random._

import rift2Core.define._

import rift._
import base._

import chipsalliance.rocketchip.config.Parameters






class DcacheStageBase(idx: Int)(implicit p: Parameters) extends DcacheModule {
  val id = idx
  val io = IO(new Bundle {
    val enq = Flipped(Decoupled(new Dcache_Enq_Bundle))
    val reload = Valid(new Dcache_Enq_Bundle)
    val deq = Valid(new Dcache_Deq_Bundle)

    val missUnit_req = Valid(new Info_miss_req)
    val wb_req = Valid(new Info_writeBack_req)
    val pb_req =Valid(new Info_writeBack_req)

    val isCacheEmpty = Input(Bool())
    val flush = Input(Bool())

  })

  // val addr_lsb = log2Ceil(dw/8)
  // val line_w   = log2Ceil(cl)
  // val bk_w     = log2Ceil(bk)
  // val tag_w   = plen - addr_lsb - line_w - bk_w


  val datInfoR = Wire( Vec(cb, Vec(dw/8, UInt(8.W))) )
  val tagInfoR = Wire( Vec(cb, UInt(tag_w.W)) )

  val datInfoWM = Wire( Vec(dw/8, Bool()   ) )
  val datInfoW  = Wire( Vec(dw/8, UInt(8.W)) )

  val tagEnW = Wire( Vec(cb, Bool()) )
  val datEnW = Wire( Vec(cb, Bool()) )
  // val tagEnR = Wire( Vec(cb, Bool()) )
  // val datEnR = Wire( Vec(cb, Bool()) )

  val datRAM = for ( i <- 0 until cb ) yield { Module(new DatRAM(dw, cl)) }
  val tagRAM = for ( i <- 0 until cb ) yield { Module(new TagRAM(tag_w, cl)) }

  val addrSelW = Wire(UInt(line_w.W))
  val addrSelR = Wire(UInt(line_w.W))
  val tagInfoW = Wire(UInt(tag_w.W))

  val isBusy = RegInit(false.B)
  io.enq.ready := ~isBusy
  val pipeStage1Valid = RegNext(io.enq.fire, false.B)
  val pipeStage1Bits  = RegNext(io.enq.bits)

   /** one hot code indicated which blcok is hit */
  val isHitOH = Wire(Vec(cb, Bool()))
  /** flag that indicated that if there is a cache block hit */
  val isHit = isHitOH.asUInt.orR
  /** when no block is hit or a new grant req comes, we should 1) find out an empty block 2) evict a valid block */
  val rplSel = Wire(UInt(cb_w.W))
  /** convert one hot hit to UInt */
  val hitSel = WireDefault(OHToUInt(isHitOH))
  val cbSel = Wire(UInt(cb_w.W))
  /** flag that indicated that if a cache block is valid */
  val isCBValid = RegInit( VecInit( Seq.fill(cl)(VecInit(Seq.fill(cb)(false.B))) ) )
  /** flag that indicated that if a cache block is dirty */
  val isCBDirty = RegInit( VecInit( Seq.fill(cl)(VecInit(Seq.fill(cb)(false.B))) ) )
  
  val is_sc_fail = Wire(Bool())
}

trait DcacheStageSRAM{ this: DcacheStageBase =>

  addrSelR := io.enq.bits.clSel
  addrSelW := pipeStage1Bits.clSel
  tagInfoW := pipeStage1Bits.tagSel


  for ( i <- 0 until cb ) yield {
    datEnW(i) :=
      (i.U === cbSel) & pipeStage1Valid &
      pipeStage1Bits.fun.is_dat_w & (
        ( pipeStage1Bits.fun.grant ) |
        ( pipeStage1Bits.fun.is_access & isHit & Mux( pipeStage1Bits.fun.is_sc, ~is_sc_fail, true.B) )
      )

    tagEnW(i) := (i.U === cbSel) & pipeStage1Valid & pipeStage1Bits.fun.grant
  }

  datInfoWM :=
    Mux1H(Seq(
      (pipeStage1Bits.fun.grant)     -> Fill(dw/8, 1.U),
      (pipeStage1Bits.fun.is_access) -> pipeStage1Bits.wstrb
    )).asBools

  for( i <- 0 until cb ) {

    // tagInfoR(i) := tagRAM(i)(addrSelR)

    tagRAM(i).io.addr  := Mux(tagEnW(i), addrSelW, addrSelR)
    tagRAM(i).io.dataw := tagInfoW
    tagInfoR(i) := tagRAM(i).io.datar
    tagRAM(i).io.enw   := tagEnW(i)
    tagRAM(i).io.enr   := io.enq.fire


    // when( tagEnW(i)  ) {
    //   tagRAM(i).write( addrSelW, tagInfoW )
    // } .otherwise {

    // }


    datRAM(i).io.enr    := io.enq.fire
    datRAM(i).io.addr   := Mux(datEnW(i), addrSelW, addrSelR)
    datRAM(i).io.dataw  := datInfoW
    datRAM(i).io.datawm := datInfoWM
    datInfoR(i) := datRAM(i).io.datar
    datRAM(i).io.enw    := datEnW(i)

    // when()
    // when( datEnW(i) ) {
    //   datRAM(i).write( addrSelW, datInfoW, datInfoWM )
    // }
  }

}


trait DcacheStageLRSC { this: DcacheStageBase =>
  val killTrans = RegInit(false.B) 
  val is_pending_lr = RegInit(false.B)
  val is_lr_64_32n = RegInit(false.B)
  val lr_addr = Reg(UInt(plen.W))
  is_sc_fail := 
    ~is_pending_lr | 
    (is_lr_64_32n  & pipeStage1Bits.fun.is_word) |
    (~is_lr_64_32n & pipeStage1Bits.fun.is_dubl) |
    lr_addr =/= pipeStage1Bits.paddr


  when( io.flush | killTrans ) {
    is_pending_lr := false.B
  } .elsewhen( pipeStage1Valid & pipeStage1Bits.fun.is_access & isHit ) {// going to deq fire (validIO)
    when( pipeStage1Bits.fun.is_lr ) {
      is_pending_lr := true.B
      is_lr_64_32n := pipeStage1Bits.fun.is_dubl
      lr_addr := pipeStage1Bits.paddr
      assert( pipeStage1Bits.fun.is_dubl | pipeStage1Bits.fun.is_word )
    } .elsewhen( pipeStage1Bits.fun.is_sc ) {
      is_pending_lr := false.B
    } .elsewhen( (pipeStage1Bits.fun.is_su | (pipeStage1Bits.fun.is_amo & ~pipeStage1Bits.fun.is_lrsc)) ) {
      when( tagInfoW === lr_addr(plen-1,plen-tag_w) ) {
        is_pending_lr := false.B
      }   
    }
  } .elsewhen( pipeStage1Valid & pipeStage1Bits.fun.probe ) {
    when( tagInfoW === lr_addr(plen-1,plen-tag_w) ) {
      is_pending_lr := false.B
    }
  }

  when( io.flush ) {
    killTrans := true.B
  } .elsewhen ( io.isCacheEmpty) {
    killTrans := false.B
  }
}

trait DcacheStageWData { this: DcacheStageBase =>
  val high_sel  = pipeStage1Bits.paddr(2) === 1.U
  val amo_reAlign_64_a = Wire(UInt(64.W))
  val amo_reAlign_64_b = Wire(UInt(64.W))

  amo_reAlign_64_a := reAlign_data( from = 256, to = 64, pipeStage1Bits.wdata, pipeStage1Bits.paddr )
  amo_reAlign_64_b := reAlign_data( from = 256, to = 64, Cat(datInfoR(cbSel).reverse),      pipeStage1Bits.paddr )

  val cmp_a_sel = Mux(high_sel, amo_reAlign_64_a(63,32), amo_reAlign_64_a(31,0))
  val cmp_b_sel = Mux(high_sel, amo_reAlign_64_b(63,32), amo_reAlign_64_b(31,0))
    
  
  val dataW = 
    Mux1H(Seq(
      pipeStage1Bits.fun.grant -> pipeStage1Bits.wdata,
      pipeStage1Bits.fun.is_su -> pipeStage1Bits.wdata,
      pipeStage1Bits.fun.is_sc -> pipeStage1Bits.wdata,
      (pipeStage1Bits.fun.amoswap_w | pipeStage1Bits.fun.amoswap_d) -> reAlign_data( from = 64, to = 256,  amo_reAlign_64_a, pipeStage1Bits.paddr ),
      (pipeStage1Bits.fun.amoadd_w                 ) -> reAlign_data( from = 64, to = 256, ( Mux(high_sel, amo_reAlign_64_a >> 32 << 32, amo_reAlign_64_a) + amo_reAlign_64_b), pipeStage1Bits.paddr ), //when sel msb-32, set one of op's lsb-32 to zore to prevent carry-in
      (pipeStage1Bits.fun.amoadd_d                 ) -> reAlign_data( from = 64, to = 256, (amo_reAlign_64_a + amo_reAlign_64_b), pipeStage1Bits.paddr ),
      (pipeStage1Bits.fun.amoxor_w  | pipeStage1Bits.fun.amoxor_d ) -> reAlign_data( from = 64, to = 256, (amo_reAlign_64_a ^ amo_reAlign_64_b), pipeStage1Bits.paddr ),
      (pipeStage1Bits.fun.amoand_w  | pipeStage1Bits.fun.amoand_d ) -> reAlign_data( from = 64, to = 256, (amo_reAlign_64_a & amo_reAlign_64_b), pipeStage1Bits.paddr ),
      (pipeStage1Bits.fun.amoor_w   | pipeStage1Bits.fun.amoor_d  ) -> reAlign_data( from = 64, to = 256, (amo_reAlign_64_a | amo_reAlign_64_b), pipeStage1Bits.paddr ),


      (pipeStage1Bits.fun.amomin_w ) -> reAlign_data( from = 64, to = 256, Mux(cmp_a_sel.asSInt        < cmp_b_sel.asSInt,        amo_reAlign_64_a, amo_reAlign_64_b), pipeStage1Bits.paddr),
      (pipeStage1Bits.fun.amomin_d ) -> reAlign_data( from = 64, to = 256, Mux(amo_reAlign_64_a.asSInt < amo_reAlign_64_b.asSInt, amo_reAlign_64_a, amo_reAlign_64_b), pipeStage1Bits.paddr),
      (pipeStage1Bits.fun.amomax_w ) -> reAlign_data( from = 64, to = 256, Mux(cmp_a_sel.asSInt        < cmp_b_sel.asSInt,        amo_reAlign_64_b, amo_reAlign_64_a), pipeStage1Bits.paddr),
      (pipeStage1Bits.fun.amomax_d ) -> reAlign_data( from = 64, to = 256, Mux(amo_reAlign_64_a.asSInt < amo_reAlign_64_b.asSInt, amo_reAlign_64_b, amo_reAlign_64_a), pipeStage1Bits.paddr),
      (pipeStage1Bits.fun.amominu_w) -> reAlign_data( from = 64, to = 256, Mux(cmp_a_sel               < cmp_b_sel,               amo_reAlign_64_a, amo_reAlign_64_b), pipeStage1Bits.paddr),
      (pipeStage1Bits.fun.amominu_d) -> reAlign_data( from = 64, to = 256, Mux(amo_reAlign_64_a        < amo_reAlign_64_b,        amo_reAlign_64_a, amo_reAlign_64_b), pipeStage1Bits.paddr),
      (pipeStage1Bits.fun.amomaxu_w) -> reAlign_data( from = 64, to = 256, Mux(cmp_a_sel               < cmp_b_sel,               amo_reAlign_64_b, amo_reAlign_64_a), pipeStage1Bits.paddr),
      (pipeStage1Bits.fun.amomaxu_d) -> reAlign_data( from = 64, to = 256, Mux(amo_reAlign_64_a        < amo_reAlign_64_b,        amo_reAlign_64_b, amo_reAlign_64_a), pipeStage1Bits.paddr),
            
    ))
      
      
  datInfoW := VecInit(for ( k <- 0 until dw/8 ) yield dataW(8*k+7, 8*k))
}

trait DcacheStageBlock{ this: DcacheStageBase => 
  when( pipeStage1Valid ) {
    when( pipeStage1Bits.fun.grant ) {
      isCBValid(addrSelW)(cbSel) := true.B
      isCBDirty(addrSelW)(cbSel) := false.B
    }
    when( pipeStage1Bits.fun.isDirtyOp ) {
      isCBDirty(addrSelW)(cbSel) := true.B
    }

    when( pipeStage1Bits.fun.probe ) {
      isCBValid(addrSelW)(cbSel) := false.B
    }
  }

  isHitOH := {
    val res = for( i <- 0 until cb ) yield { (tagInfoR(i) === tagInfoW) & isCBValid(addrSelW)(i) }
    when( pipeStage1Valid ) {assert(PopCount(res) <= 1.U)}
    VecInit(res)
  }

  rplSel := { 
    val is_emptyBlock_exist = isCBValid(addrSelW).contains(false.B)
    val emptyBlock_sel = isCBValid(addrSelW).indexWhere( (x:Bool) => (x === false.B) )
    Mux( is_emptyBlock_exist, emptyBlock_sel, LFSR(16) )
  }
  
  cbSel := 
    Mux1H(Seq(
      pipeStage1Bits.fun.is_access -> hitSel,
      pipeStage1Bits.fun.preft     -> hitSel,
      pipeStage1Bits.fun.probe     -> hitSel,
      pipeStage1Bits.fun.grant     -> rplSel
    ))

  when( pipeStage1Valid ) {
    when( pipeStage1Bits.fun.probe ) { assert(isHit) } //l2 will never request a empty probe
    when( pipeStage1Bits.fun.grant ) {  } 
  }
}

trait DcacheStageRTN{ this: DcacheStageBase =>

  val missUnitReqValid = RegInit(false.B)
  val missUnitReqPaddr = Reg(UInt(plen.W))

  io.missUnit_req.valid := missUnitReqValid
  io.missUnit_req.bits.paddr := missUnitReqPaddr
  
  when( pipeStage1Valid & ( pipeStage1Bits.fun.is_access | pipeStage1Bits.fun.preft ) & ( ~isHit ) ) {
    missUnitReqValid := true.B
    missUnitReqPaddr := pipeStage1Bits.paddr >> addr_lsb.U << addr_lsb.U 
  } .otherwise {
    missUnitReqValid := false.B
    if(isLowPower) {
      missUnitReqPaddr := 0.U
    }
  }

  val wbReqValid = RegInit(false.B)
  val pbReqValid = RegInit(false.B)
  val wbReqPaddr = Reg(UInt(plen.W))
  val pbReqPaddr = Reg(UInt(plen.W))
  val wbReqData  = Reg(UInt(256.W))
  val pbReqData  = Reg(UInt(256.W))
  val wbReqisData = Reg(Bool())
  val pbReqisData = Reg(Bool())

  io.wb_req.valid := wbReqValid
  io.pb_req.valid := pbReqValid

  io.wb_req.bits.paddr := wbReqPaddr
  io.pb_req.bits.paddr := pbReqPaddr

  io.wb_req.bits.data := wbReqData
  io.pb_req.bits.data := pbReqData

  io.wb_req.bits.is_releaseData :=  wbReqisData
  io.wb_req.bits.is_release     := ~wbReqisData
  io.wb_req.bits.is_probeData   := false.B
  io.wb_req.bits.is_probe       := false.B

  io.pb_req.bits.is_releaseData := false.B
  io.pb_req.bits.is_release     := false.B
  io.pb_req.bits.is_probeData   :=  pbReqisData
  io.pb_req.bits.is_probe       := ~pbReqisData

  when( pipeStage1Valid & ( pipeStage1Bits.fun.grant & ~isCBValid(addrSelW).contains(false.B) ) ) {
    wbReqValid  := true.B
    wbReqPaddr  := Cat( tagInfoR(cbSel), addrSelW, id.U(bk_w.W), 0.U(addr_lsb.W) )
    wbReqData   := Cat(datInfoR(cbSel).reverse)
    wbReqisData := isCBDirty(addrSelW)(cbSel)
  } .otherwise {
    wbReqValid  := false.B

    if( isLowPower ) {
      wbReqPaddr  := 0.U
      wbReqData   := 0.U
      wbReqisData := false.B
    }
  }

  when( pipeStage1Valid & pipeStage1Bits.fun.probe) {
    pbReqValid  := true.B
    pbReqPaddr  := pipeStage1Bits.paddr >> addr_lsb.U << addr_lsb.U 
    pbReqData   := Cat(datInfoR(cbSel).reverse)
    pbReqisData := isCBDirty(addrSelW)(cbSel)
  } .otherwise {
    pbReqValid  := false.B

    if( isLowPower ) {
      pbReqPaddr  := 0.U
      pbReqData   := 0.U
      pbReqisData := false.B
    }
  
  }


  val reloadValid = RegInit(false.B)
  val reloadBits  = Reg(new Dcache_Enq_Bundle)

  when( pipeStage1Valid & pipeStage1Bits.fun.is_access & ~isHit ) {
    reloadValid := true.B
    reloadBits  := pipeStage1Bits
  } .otherwise {
    reloadValid := false.B

    if( isLowPower ) { reloadBits  := 0.U.asTypeOf(new Dcache_Enq_Bundle) }
  }

  io.reload.valid := reloadValid
  io.reload.bits  := reloadBits

  val deqValid = RegInit(false.B)
  val deqBits  = Reg(new Dcache_Deq_Bundle) 

  io.deq.valid := deqValid
  io.deq.bits  := deqBits

  when( pipeStage1Valid & pipeStage1Bits.fun.is_access & isHit ) {
    deqValid := true.B
    deqBits.wb.res := {
      val rdata = Cat( datInfoR(cbSel).reverse ) //align 256
      val paddr = pipeStage1Bits.paddr
      val fun   = pipeStage1Bits.fun
      val overlap_wdata = pipeStage1Bits.wdata
      val overlap_wstrb = pipeStage1Bits.wstrb
      
      val res_pre_pre = {
        val res = Wire( UInt(64.W) )
        val (new_data, new_strb) = overlap_wr( rdata, 0.U(32.W), overlap_wdata, overlap_wstrb)
        val overlap_data = Mux( fun.is_lu, new_data, rdata) //align 256
        res := reAlign_data( from = 256, to = 64, overlap_data, paddr )
        res
      }
      val res_pre = get_loadRes( fun, paddr, res_pre_pre ) //align 8

      val res = Mux(
        pipeStage1Bits.fun.is_sc,
        Mux( is_sc_fail, 1.U, 0.U ),
        res_pre
      )
      res
    }

    deqBits.wb.rd0      := pipeStage1Bits.rd.rd0 
    deqBits.chkIdx     := pipeStage1Bits.chkIdx
    deqBits.is_load_amo := pipeStage1Bits.fun.is_wb
    deqBits.is_flw      := pipeStage1Bits.fun.flw
    deqBits.is_fld      := pipeStage1Bits.fun.fld
  } .otherwise {
    deqValid := false.B
    if( isLowPower ) {
      deqBits := 0.U.asTypeOf(new Dcache_Deq_Bundle)
    }
  }

}


class DcacheStage(idx: Int)(implicit p: Parameters) extends DcacheStageBase((idx))
  with DcacheStageSRAM
  with DcacheStageLRSC 
  with DcacheStageWData
  with DcacheStageBlock
  with DcacheStageRTN {

  when( isBusy /*& ( datEnW.reduce(_|_) | tagEnW.reduce(_|_) | io.reload.fire )*/ ) {
    isBusy := false.B
  } .elsewhen( io.enq.fire & (io.enq.bits.fun.is_dat_w | io.enq.bits.fun.is_tag_w) ) {
    assert( ~isBusy )
    isBusy := true.B
  } 











}


  








































  // when( io.enq.fire ) {
    // io.tag_addr_r := io.enq.bits.paddr
    // io.dat_addr_r := io.enq.bits.paddr

    // for ( i <- 0 until cb ) yield {
    //   io.tag_en_r(i) := io.enq.bits.fun.is_tag_r
    // }

    // for ( i <- 0 until cb ) yield {
    //   io.dat_en_r(i) := 
    //     io.enq.bits.fun.is_dat_r & (
    //       (io.enq.bits.fun.probe) |
    //       (io.enq.bits.fun.grant) |
    //       (io.enq.bits.fun.is_access )
    //     )
    // }
  // }

  // for ( i <- 0 until cb ) yield {
  //   val data_i = VecInit(for ( k <- 0 until dw/8 ) yield dat_info_w(8*k+7, 8*k))
  //   val data_o = Cat( for ( k <- 0 until dw/8 ) yield { dat_ram(i).read(addr_sel_r, dat_en_r(i))(dw/8-1-k) } )

  //   when( dat_en_w(i) ) {
  //     dat_ram(i).write(addr_sel_w, data_i, dat_info_wstrb.asBools)
  //   }

  //   dat_info_r(i) := {
  //     val mask = dat_info_wstrb.asBools

  //     val wdata    = RegNext( dat_info_w )
  //     val ext_mask = RegNext(Cat( for ( k <- 0 until dw/8 ) yield Fill(8, mask(dw/8-1-k)) ))

  //     val isBypass = RegNext(addr_sel_r === addr_sel_w & dat_en_w(i) & dat_en_r(i), false.B)
  //     val isEnable = RegNext(dat_en_r(i), false.B)

  //     Mux( isEnable, Mux(isBypass, (wdata & ext_mask) | (data_o & ~ext_mask), data_o), 0.U )

  //   }    
  // }

  // for ( i <- 0 until cb ) yield {
  //   when( tag_en_w(i) ) {
  //     tag_ram(i).write(addr_sel_w, tag_info_w)
  //   }

  //   val isBypass = RegNext(addr_sel_r === addr_sel_w & tag_en_w(i) & tag_en_r(i), false.B)
  //   val isEnable = RegNext(tag_en_r(i), false.B)
  //   val tag_o = tag_ram(i).read(addr_sel_r, tag_en_r(i))


  //   tag_info_r(i) := 
  //     Mux( isEnable, Mux( isBypass, RegNext(tag_info_w), tag_o ), 0.U )
  // }

  

  










