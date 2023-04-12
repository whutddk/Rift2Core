/*
  Copyright (c) 2020 - 2023 Wuhan University of Technology <295054118@whut.edu.cn>

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

import rift2Chip._
import base._

import org.chipsalliance.cde.config._






class DcacheStageBase(idx: Int)(implicit p: Parameters) extends DcacheModule {
  val id = idx

  class DcacheStageIO extends Bundle{
    val enq = Flipped(Decoupled(new Dcache_Enq_Bundle))
    val reload = Valid(new Dcache_Enq_Bundle)
    val deq = Valid(new Dcache_Deq_Bundle)

    val missUnit_req = Valid(new Info_miss_req)
    val wb_req = Valid(new Info_writeBack_req)

    val isCacheEmpty = Input(Bool())
    val flush = Input(Bool())    
  }

  val io: DcacheStageIO = IO(new DcacheStageIO)


  val datInfoR = Wire( Vec(cb, Vec(dw/8, UInt(8.W))) )
  val tagInfoR = Wire( Vec(cb, UInt(tag_w.W)) )

  val datInfoWM = Wire( Vec(dw/8, Bool()   ) )
  val datInfoW  = Wire( Vec(dw/8, UInt(8.W)) )

  val tagEnW = Wire( Vec(cb, Bool()) )
  val datEnW = Wire( Vec(cb, Bool()) )

  val datRAM = for ( _ <- 0 until cb ) yield { Module(new DatRAM(dw, cl)) }
  val tagRAM = for ( _ <- 0 until cb ) yield { Module(new TagRAM(tag_w, cl)) }

  val addrSelW = Wire(UInt(line_w.W))
  val addrSelR = Wire(UInt(line_w.W))
  val tagInfoW = Wire(UInt(tag_w.W))


  io.enq.ready := true.B
  val pipeStage1Valid = RegNext(io.enq.fire, false.B)
  val pipeStage1Bits  = RegNext(io.enq.bits)

   /** one hot code indicated which blcok is hit */
  val isHitOH = Wire(Vec(cb, Bool()))
  /** flag that indicated that if there is a cache block hit */
  val isHit = isHitOH.asUInt.orR
  /** when no block is hit or a new grant req comes, we should 1) find out an empty block 2) evict a valid block */
  val rplSel = Wire(UInt( (1 max cb_w).W))
  /** convert one hot hit to UInt */
  val hitSel = WireDefault(OHToUInt(isHitOH))
  val cbSel = Wire(UInt( (1 max cb_w).W))
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

    val isTagBypass   = RegEnable( tagEnW(i) & (addrSelR === addrSelW), io.enq.fire )
    val tagBypassData = RegEnable( tagInfoW, io.enq.fire & tagEnW(i) & (addrSelR === addrSelW) )
    val isDatBypass   = RegEnable( datEnW(i) & (addrSelR === addrSelW), io.enq.fire )
    val datBypassData = RegEnable( datInfoW, io.enq.fire & datEnW(i) & (addrSelR === addrSelW) )
    val datBypassWM   = RegEnable( datInfoWM, io.enq.fire & datEnW(i) & (addrSelR === addrSelW) )
    

    tagRAM(i).io.addrr  := addrSelR
    tagRAM(i).io.addrw  := addrSelW
    tagRAM(i).io.dataw  := tagInfoW
    tagInfoR(i)         := Mux( isTagBypass, tagBypassData, tagRAM(i).io.datar ) 
    tagRAM(i).io.enw   := tagEnW(i)
    tagRAM(i).io.enr   := io.enq.fire


    datRAM(i).io.addrr  := addrSelR
    datRAM(i).io.addrw  := addrSelW
    datRAM(i).io.dataw  := datInfoW
    datRAM(i).io.datawm := datInfoWM
    datInfoR(i)         := (for( w <- 0 until dw/8 ) yield { Mux(isDatBypass & datBypassWM(w), datBypassData(w), datRAM(i).io.datar(w)) })
    datRAM(i).io.enw    := datEnW(i)
    datRAM(i).io.enr    := io.enq.fire

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

  amo_reAlign_64_a := reAlign_data( from = dw, to = 64, pipeStage1Bits.wdata, pipeStage1Bits.paddr )
  amo_reAlign_64_b := reAlign_data( from = dw, to = 64, Cat(datInfoR(cbSel).reverse),      pipeStage1Bits.paddr )

  val cmp_a_sel = Mux(high_sel, amo_reAlign_64_a(63,32), amo_reAlign_64_a(31,0))
  val cmp_b_sel = Mux(high_sel, amo_reAlign_64_b(63,32), amo_reAlign_64_b(31,0))
    
  
  val dataW = 
    Mux1H(Seq(
      pipeStage1Bits.fun.grant -> pipeStage1Bits.wdata,
      pipeStage1Bits.fun.is_su -> pipeStage1Bits.wdata,
      pipeStage1Bits.fun.is_sc -> pipeStage1Bits.wdata,
      (pipeStage1Bits.fun.amoswap_w | pipeStage1Bits.fun.amoswap_d) -> reAlign_data( from = 64, to = dw,  amo_reAlign_64_a, pipeStage1Bits.paddr ),
      (pipeStage1Bits.fun.amoadd_w                 )                -> reAlign_data( from = 64, to = dw, ( Mux(high_sel, amo_reAlign_64_a >> 32 << 32, amo_reAlign_64_a) + amo_reAlign_64_b), pipeStage1Bits.paddr ), //when sel msb-32, set one of op's lsb-32 to zore to prevent carry-in
      (pipeStage1Bits.fun.amoadd_d                 )                -> reAlign_data( from = 64, to = dw, (amo_reAlign_64_a + amo_reAlign_64_b), pipeStage1Bits.paddr ),
      (pipeStage1Bits.fun.amoxor_w  | pipeStage1Bits.fun.amoxor_d ) -> reAlign_data( from = 64, to = dw, (amo_reAlign_64_a ^ amo_reAlign_64_b), pipeStage1Bits.paddr ),
      (pipeStage1Bits.fun.amoand_w  | pipeStage1Bits.fun.amoand_d ) -> reAlign_data( from = 64, to = dw, (amo_reAlign_64_a & amo_reAlign_64_b), pipeStage1Bits.paddr ),
      (pipeStage1Bits.fun.amoor_w   | pipeStage1Bits.fun.amoor_d  ) -> reAlign_data( from = 64, to = dw, (amo_reAlign_64_a | amo_reAlign_64_b), pipeStage1Bits.paddr ),


      (pipeStage1Bits.fun.amomin_w ) -> reAlign_data( from = 64, to = dw, Mux(cmp_a_sel.asSInt        < cmp_b_sel.asSInt,        amo_reAlign_64_a, amo_reAlign_64_b), pipeStage1Bits.paddr),
      (pipeStage1Bits.fun.amomin_d ) -> reAlign_data( from = 64, to = dw, Mux(amo_reAlign_64_a.asSInt < amo_reAlign_64_b.asSInt, amo_reAlign_64_a, amo_reAlign_64_b), pipeStage1Bits.paddr),
      (pipeStage1Bits.fun.amomax_w ) -> reAlign_data( from = 64, to = dw, Mux(cmp_a_sel.asSInt        < cmp_b_sel.asSInt,        amo_reAlign_64_b, amo_reAlign_64_a), pipeStage1Bits.paddr),
      (pipeStage1Bits.fun.amomax_d ) -> reAlign_data( from = 64, to = dw, Mux(amo_reAlign_64_a.asSInt < amo_reAlign_64_b.asSInt, amo_reAlign_64_b, amo_reAlign_64_a), pipeStage1Bits.paddr),
      (pipeStage1Bits.fun.amominu_w) -> reAlign_data( from = 64, to = dw, Mux(cmp_a_sel               < cmp_b_sel,               amo_reAlign_64_a, amo_reAlign_64_b), pipeStage1Bits.paddr),
      (pipeStage1Bits.fun.amominu_d) -> reAlign_data( from = 64, to = dw, Mux(amo_reAlign_64_a        < amo_reAlign_64_b,        amo_reAlign_64_a, amo_reAlign_64_b), pipeStage1Bits.paddr),
      (pipeStage1Bits.fun.amomaxu_w) -> reAlign_data( from = 64, to = dw, Mux(cmp_a_sel               < cmp_b_sel,               amo_reAlign_64_b, amo_reAlign_64_a), pipeStage1Bits.paddr),
      (pipeStage1Bits.fun.amomaxu_d) -> reAlign_data( from = 64, to = dw, Mux(amo_reAlign_64_a        < amo_reAlign_64_b,        amo_reAlign_64_b, amo_reAlign_64_a), pipeStage1Bits.paddr),
            
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

    when( pipeStage1Bits.fun.probe & isHit ) {
      isCBValid(addrSelW)(cbSel) := false.B
    }
  }

  isHitOH := {
    val res = for( i <- 0 until cb ) yield { (tagInfoR(i) === tagInfoW) & isCBValid(addrSelW)(i) }
    when( pipeStage1Valid ) {assert(PopCount(res) <= 1.U)}
    VecInit(res)
  }


  
  cbSel := {
    if ( cb != 1 ) {
      Mux1H(Seq(
        pipeStage1Bits.fun.is_access -> hitSel,
        pipeStage1Bits.fun.preft     -> hitSel,
        pipeStage1Bits.fun.probe     -> hitSel,
        pipeStage1Bits.fun.grant     -> rplSel
      ))
    } else 0.U    
  }



  rplSel := {
    if ( cb != 1 ) {
      val is_emptyBlock_exist = isCBValid(addrSelW).contains(false.B)
      val emptyBlock_sel = isCBValid(addrSelW).indexWhere( (x:Bool) => (x === false.B) )

      val rpl = {
        if( hasLRU ) {
          val lru = new LRU(cb, cl)
          when( pipeStage1Valid & ((pipeStage1Bits.fun.is_access & isHit) | pipeStage1Bits.fun.grant) ) { lru.update(cbSel, pipeStage1Bits.clSel) }
          lru.replace(pipeStage1Bits.clSel)
        } else {
          LFSR(16)
        }        
      }
      Mux( is_emptyBlock_exist, emptyBlock_sel, rpl )

    } else 0.U
  }



  when( pipeStage1Valid ) {
    when( pipeStage1Bits.fun.probe ) { when( ~isHit ) { printf("Warning, l2 will never request a empty probe, is it in writeback unit?\n") } } //
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
  val wbReqPaddr = Reg(UInt(plen.W))
  val wbReqData  = Reg(UInt(dw.W))
  val wbReqisData = Reg(Bool())
  val isPb        = Reg(Bool())

  io.wb_req.valid := wbReqValid
  io.wb_req.bits.paddr := wbReqPaddr
  io.wb_req.bits.data := wbReqData

  io.wb_req.bits.is_releaseData :=  wbReqisData & ~isPb
  io.wb_req.bits.is_release     := ~wbReqisData & ~isPb
  io.wb_req.bits.is_probeData   :=  wbReqisData &  isPb
  io.wb_req.bits.is_probe       := ~wbReqisData &  isPb

  when( pipeStage1Valid & ( pipeStage1Bits.fun.grant & ~isCBValid(addrSelW).contains(false.B) ) ) {
    wbReqValid  := true.B
    wbReqPaddr  := (if(bk > 1) {Cat( tagInfoR(cbSel), addrSelW, id.U(bk_w.W), 0.U(addr_lsb.W) )} else {Cat( tagInfoR(cbSel), addrSelW, 0.U(addr_lsb.W) )}) 
    wbReqData   := Cat(datInfoR(cbSel).reverse)
    wbReqisData := isCBDirty(addrSelW)(cbSel)
    isPb        := false.B
  } .elsewhen( pipeStage1Valid & pipeStage1Bits.fun.probe) {
    wbReqValid  := true.B
    wbReqPaddr  := pipeStage1Bits.paddr >> addr_lsb.U << addr_lsb.U 
    wbReqData   := Cat(datInfoR(cbSel).reverse)
    wbReqisData := isCBDirty(addrSelW)(cbSel) & isHit
    isPb        := true.B
  } .otherwise {
    wbReqValid  := false.B

    if( isLowPower ) {
      wbReqPaddr  := 0.U
      wbReqData   := 0.U
      wbReqisData := false.B
      isPb        := false.B
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
        val (newData, _) = overlap_wr( rdata, 0.U((dw/8).W), overlap_wdata, overlap_wstrb)
        val overlap_data = Mux( fun.is_lu, newData, rdata) //align 256
        res := reAlign_data( from = dw, to = 64, overlap_data, paddr )
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
    deqBits.chkIdx      := pipeStage1Bits.chkIdx
    deqBits.is_load_amo := pipeStage1Bits.fun.is_wb
    deqBits.is_flw      := pipeStage1Bits.fun.flw
    deqBits.is_fld      := pipeStage1Bits.fun.fld
    if(hasVector) { deqBits.vAttach.get    := pipeStage1Bits.vAttach.get }

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




}


  




































