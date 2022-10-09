

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


package rift2Core.frontend

import chisel3._
import chisel3.util._
import chisel3.util.random._

import rift2Chip._
import base._

import rift2Core.define._
import rift2Core.privilege._

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._






abstract class IF2Base(edge: TLEdgeOut)(implicit p: Parameters) extends IcacheModule {
  val iEdge = edge

  val io = IO(new Bundle {

    val if2_req  = Flipped(new DecoupledIO( new IF1_Bundle ))
    val if2_resp = Vec( 4, new DecoupledIO(new IF2_Bundle) )

    val if_mmu = DecoupledIO(new Info_mmu_req)
    val mmu_if = Flipped(DecoupledIO(new Info_mmu_rsp))

    val if_cmm = Output(new Info_if_cmm)


    val icache_get    = new DecoupledIO(new TLBundleA(iEdge.bundle))
    val icache_access = Flipped(new DecoupledIO(new TLBundleD(iEdge.bundle)))

    val flush = Input(Bool())

    val ifence = Input(Bool())

    /** prefetch is not guarantee to be accepted by cache*/
    val preFetch = ValidIO( new PreFetch_Req_Bundle )
  })


  val (_, _, is_trans_done, transCnt) = iEdge.count(io.icache_access)
  val icache_access_data = Wire(UInt(dw.W))
  val icache_sramrd_data = Wire(UInt(dw.W))
  val icache_access_data_lo = Reg( Vec((dw/l1BeatBits-1), UInt(l1BeatBits.W) ))
  val kill_trans = RegInit(false.B)

  val ibuf = Module(new MultiPortFifo( new IF2_Bundle, aw= (if (!isMinArea) 6 else log2Ceil(2*ftChn)), in=ftChn, out=4 ) )
  ibuf.io.flush := io.flush
  io.if2_resp <> ibuf.io.deq



  val cl_sel = io.mmu_if.bits.paddr(addr_lsb+line_w-1, addr_lsb)
  val tag_sel = io.mmu_if.bits.paddr(plen-1,plen-tag_w)

   /** one hot code indicated which blcok is hit */
  val is_hit_oh = Wire(Vec(cb, Bool()))

  /** flag that indicated that if there is a cache block hit */
  val is_hit = is_hit_oh.asUInt.orR

  /** convert one hot hit to UInt */
  val hit_sel = WireDefault(OHToUInt(is_hit_oh))







  val icache_state_dnxt = Wire(UInt(4.W))
  val icache_state_qout = RegNext( icache_state_dnxt, 0.U )

  val fault_lock = RegInit( false.B )
  val is_access_fault = Wire(Bool())
  val is_paging_fault = Wire(Bool())
}

trait IF2MMU { this: IF2Base =>
  io.if_mmu.valid := io.if2_req.valid
  io.if_mmu.bits.vaddr := io.if2_req.bits.pc
  io.if_mmu.bits.is_R := true.B
  io.if_mmu.bits.is_W := false.B
  io.if_mmu.bits.is_X := true.B
  io.if2_req.ready := io.if_mmu.ready

  io.mmu_if.ready := ibuf.io.enq(0).fire & ~fault_lock & ~io.mmu_if.bits.is_fault
}

trait IF2Fault { this: IF2Base =>
  is_access_fault := io.mmu_if.valid & io.mmu_if.bits.is_access_fault
  is_paging_fault := io.mmu_if.valid & io.mmu_if.bits.is_paging_fault

  when( io.flush ) { fault_lock := false.B }
  .elsewhen( io.mmu_if.valid & io.mmu_if.bits.is_fault & ~io.flush & ibuf.io.enq(0).fire ) { fault_lock := true.B }
  // val fault_push = io.mmu_if.valid & io.mmu_if.bits.is_fault & ~io.flush & ibuf.io.enq(7).ready
  assert( ~(is_access_fault & is_paging_fault) )

  io.if_cmm.ill_vaddr := io.mmu_if.bits.vaddr
  // io.if_cmm.is_access_fault := is_access_fault
  // io.if_cmm.is_paging_fault := is_paging_fault
}

trait IF2FSM { this: IF2Base =>
  icache_state_dnxt := 
    Mux1H(Seq(
      (icache_state_qout === 0.U) ->
          Mux((io.mmu_if.valid & ~io.mmu_if.bits.is_fault & ~io.flush & ibuf.io.enq(ftChn-1).ready & ~fault_lock ), 1.U, 0.U),
      (icache_state_qout === 1.U) ->
        Mux( io.flush, 0.U,
          Mux( is_hit, 0.U,
                      Mux( io.icache_get.fire, 2.U, 1.U ))),
      (icache_state_qout === 2.U) ->
          Mux( is_trans_done, 0.U, 2.U ),  
    ))
}























trait IF2NCache { this: IF2Base =>
  is_hit_oh := VecInit( Seq.fill(cb){false.B} )
  icache_sramrd_data := 0.U

}






trait IF2ICache { this: IF2Base =>

  /** flag that indicated that if a cache block is valid */
  val is_valid = RegInit( VecInit( Seq.fill(cl)(VecInit(Seq.fill(cb)(false.B))) ) )

  val datRAM = for ( i <- 0 until cb ) yield { Module(new DatRAM(dw, cl)) }
  val tagRAM = for ( i <- 0 until cb ) yield { Module(new TagRAM(tag_w, cl)) }


  is_hit_oh := {
    val res = 
      for( i <- 0 until cb ) yield {
        (tagRAM(i).io.datar === tag_sel) & is_valid(cl_sel)(i)        
      }
      
    when( icache_state_qout === 1.U ) {
      assert(PopCount(res) <= 1.U)        
    }
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

  val cb_sel = Wire(UInt(cb_w.W))
  cb_sel := 
    Mux1H(Seq(
      (icache_state_qout === 1.U) -> Mux( is_hit, hit_sel, rpl_sel ), // for fetch
      (icache_state_qout === 2.U) -> rpl_sel, // for access
    ))


  for ( i <- 0 until cb ) {
    datRAM(i).io.addrr := io.mmu_if.bits.paddr( addr_lsb+line_w-1, addr_lsb )
    datRAM(i).io.addrw := io.mmu_if.bits.paddr( addr_lsb+line_w-1, addr_lsb )
    tagRAM(i).io.addrr := io.mmu_if.bits.paddr( addr_lsb+line_w-1, addr_lsb )
    tagRAM(i).io.addrw := io.mmu_if.bits.paddr( addr_lsb+line_w-1, addr_lsb )

    datRAM(i).io.enr  := icache_state_qout === 0.U | icache_state_qout === 1.U
    tagRAM(i).io.enr  := icache_state_qout === 0.U | icache_state_qout === 1.U

    datRAM(i).io.enw  := (icache_state_qout === 2.U & icache_state_dnxt === 0.U) & (cb_sel === i.U) & ~kill_trans & ~io.flush
    tagRAM(i).io.enw  := (icache_state_qout === 2.U & icache_state_dnxt === 0.U) & (cb_sel === i.U) & ~kill_trans & ~io.flush
  
    datRAM(i).io.datawm := VecInit( Seq.fill(dw/8)(true.B) )
    datRAM(i).io.dataw  := 
      VecInit( for( t <- 0 until dw/8 ) yield icache_access_data(8*t+7, 8*t) )
    
    // VecInit(icache_access_data_lo ++ Seq(io.icache_access.bits.data))
    tagRAM(i).io.dataw  := tag_sel
  }

  when( icache_state_qout === 1.U ) {
    for( i <- 0 until cb-1 ) {
      for ( j <- i+1 until cb ) {
        assert( ~( tagRAM(i).io.datar === tagRAM(j).io.datar & is_valid(cl_sel)(i) & is_valid(cl_sel)(j) ), "Assert Failed at icache, multi-tag are equal!" )
      }
    }
  }

  icache_sramrd_data := Mux1H(is_hit_oh, datRAM.map{ x => Cat(x.io.datar.reverse) } )

  when( icache_state_qout === 2.U & icache_state_dnxt === 0.U & ~kill_trans & ~io.flush ) {
    is_valid(cl_sel)(cb_sel) := true.B
  }

  for( i <- 0 until cb; k <- 0 until cl ) yield {
    when( io.ifence ) {
       is_valid(k)(i) := false.B
    }
  }
}




trait IF2Bus{ this: IF2Base =>


  io.icache_get.valid := icache_state_qout === 1.U & ~is_hit & ~io.flush
  io.icache_get.bits :=
    iEdge.Get(
      fromSource = 0.U,
      toAddress = io.mmu_if.bits.paddr(plen-1, 0) & ("hffffffff".U << addr_lsb.U),
      lgSize = log2Ceil(dw/8).U
    )._2
  
  io.icache_access.ready := true.B

  when( io.icache_access.fire & ~is_trans_done) {
    icache_access_data_lo(transCnt) := io.icache_access.bits.data
  }




  
  assert( ~((RegNext(io.mmu_if.bits.paddr) =/= io.mmu_if.bits.paddr) & icache_state_qout === 2.U & ~RegNext(kill_trans) & ~RegNext(io.flush)), "Assert Failed, req paddr cannot change without flush." )





}

trait IF2LoadIBuf { this: IF2Base =>

  icache_access_data := Cat( Seq (io.icache_access.bits.data) ++ icache_access_data_lo.reverse)
  
  val reAlign_instr = {
    val res = Wire(UInt((16*ftChn).W))
    val shift = Wire(UInt((log2Ceil(16*ftChn)).W))
    shift := Cat(io.mmu_if.bits.paddr((log2Ceil(ftChn*16/8)-1),0), 0.U(3.W))

    val instr_raw = Wire( UInt((16*ftChn).W) )
    instr_raw := 
     Mux1H( Seq(
          (icache_state_qout === 1.U) -> icache_sramrd_data,
          (icache_state_qout === 2.U) -> icache_access_data,
        )) >> (if (dw > ftChn*16) { io.mmu_if.bits.paddr( log2Ceil(dw/8)-1, log2Ceil(ftChn*16/8) ) << log2Ceil(ftChn*16) } else {0.U})

    res := instr_raw >> shift
    res
  }



  for( i <- 0 until ftChn ) yield {
    ibuf.io.enq(i).bits.instr := reAlign_instr >> (16*i)
    ibuf.io.enq(i).bits.pc    := io.mmu_if.bits.vaddr + (2*i).U
    ibuf.io.enq(i).bits.isFault := false.B
    if(hasuBTB) {
      ibuf.io.enq(i).bits.isRedirect := io.if2_req.bits.isRedirect(i)
      ibuf.io.enq(i).bits.target := Mux( io.if2_req.bits.isRedirect(i), io.if2_req.bits.target, 0.U )      
    } else {
      ibuf.io.enq(i).bits.isRedirect := false.B
      ibuf.io.enq(i).bits.target := 0.U
    }
  }
  //override chn0
  when( io.mmu_if.valid & io.mmu_if.bits.is_access_fault ) {
    ibuf.io.enq(0).bits.instr := "b1001110001000001".U
    ibuf.io.enq(0).bits.pc    := io.if2_req.bits.pc 
    ibuf.io.enq(0).bits.isRedirect := false.B
    ibuf.io.enq(0).bits.target := 0.U
    ibuf.io.enq(0).bits.isFault := true.B

  } .elsewhen( io.mmu_if.valid & io.mmu_if.bits.is_paging_fault ) {
    ibuf.io.enq(0).bits.instr := "b1001110001000101".U
    ibuf.io.enq(0).bits.pc    := io.if2_req.bits.pc
    ibuf.io.enq(0).bits.isRedirect := false.B
    ibuf.io.enq(0).bits.target := 0.U
    ibuf.io.enq(0).bits.isFault := true.B
  }



  ibuf.io.enq(0).valid :=
    (~kill_trans & io.mmu_if.valid & icache_state_qout =/= 0.U & icache_state_dnxt === 0.U & ibuf.io.enq(ftChn-1).ready & ( io.mmu_if.bits.paddr((log2Ceil(ftChn*16/8)-1),1) <= (ftChn-1).U ) & io.if2_req.bits.isActive(0)) |
    (~kill_trans & io.mmu_if.valid & io.mmu_if.bits.is_fault & ~fault_lock)

  for ( i <- 1 until ftChn ) {
    ibuf.io.enq(i).valid := ~kill_trans & io.mmu_if.valid & icache_state_qout =/= 0.U & icache_state_dnxt === 0.U & ibuf.io.enq(ftChn-1).ready & ( io.mmu_if.bits.paddr((log2Ceil(ftChn*16/8)-1),1) <= (ftChn-1-i).U )  & io.if2_req.bits.isActive(i)
  }

}

trait IF2Fence { this: IF2Base =>
  when( io.flush & icache_state_qout =/= 0.U ) {
    kill_trans := true.B
  } .elsewhen( icache_state_dnxt === 0.U | icache_state_qout === 0.U ) {
    kill_trans := false.B
  }

  when( io.ifence ) { assert(io.flush) }
}



trait IF2PreFetch { this: IF2Base => 
  if (hasPreFetch) {
    io.preFetch.valid := io.icache_get.fire
    io.preFetch.bits.paddr := io.icache_get.bits.address + "b100000".U    
  } else {
    io.preFetch.valid := false.B
    io.preFetch.bits.paddr := 0.U
  }

}

class IF2(edge: TLEdgeOut)(implicit p: Parameters) extends IF2Base(edge)
  with IF2MMU
  with IF2Fault
  with IF2FSM
  with IF2ICache
  with IF2Bus
  with IF2LoadIBuf
  with IF2Fence
  with IF2PreFetch {


}








