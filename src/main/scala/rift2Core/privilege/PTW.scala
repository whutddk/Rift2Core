
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


package rift2Core.privilege

import chisel3._
import chisel3.util._
import rift2Core.L1Cache._


import rift._

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

class Info_ptw_rsp extends Bundle with Info_access_lvl{
  val pte = new Info_pte_sv39
  val is_ptw_fail = Bool()
  val is_access_fault = Bool()

}

object PTWState {
  sealed abstract class State(val id: Int) {
    def U: UInt = id.U(State.width.W)
  }

  object State {
    import scala.language.implicitConversions

    implicit def toInt(x: State) = x.id
    implicit def toBigInt(x: State):BigInt = x.id

    val all: Set[State] = Set(
      Free,
      Lvl2,
      Lvl1,
      Lvl0,
    )
    val width = log2Ceil(all.size)
    def chiselType() = UInt(width.W)
  }

  case object Free extends State(0)
  case object Lvl2 extends State(1)
  case object Lvl1 extends State(2)
  case object Lvl0 extends State(3)
}



class PTWBase(edge: TLEdgeOut, id: Int)(implicit p: Parameters) extends RiftModule {
  val mEdge = edge
  val mid   = id
  val io = IO(new Bundle{
    val ptw_i = Flipped(DecoupledIO(new Info_mmu_req))
    val ptw_o = DecoupledIO(new Info_ptw_rsp)

    val cmm_mmu = Input(new Info_cmm_mmu)

    val sfence_vma = Input(Bool())

    val ptw_get    = new DecoupledIO(new TLBundleA(mEdge.bundle))
    val ptw_access = Flipped(new DecoupledIO(new TLBundleD(mEdge.bundle)))
  })

  val nextState = WireInit(PTWState.Free.U)
  val currState = RegNext(next=nextState, init=PTWState.Free.U)

  val kill_trans = RegInit(false.B)

  val (_, _, is_trans_done, transCnt) = mEdge.count(io.ptw_access)
  val is_hit = Wire(Bool())

  val addr_dnxt = Wire(UInt(56.W))
  val addr_qout = RegNext(addr_dnxt, 0.U(56.W))

  val is_ptw_end = Wire(Bool())
  val is_ptw_fail = Wire(Bool())

  val pte = Wire(new Info_pte_sv39)

  val walkReq = RegEnable( io.ptw_i.bits, (currState === PTWState.Free.U & nextState =/= PTWState.Free.U) )
  val walkRspBits = RegInit( 0.U.asTypeOf(new Info_ptw_rsp) )
  val walkRspValid = RegInit(false.B)
}

trait PTWStateMachine{ this: PTWBase => 

  switch( currState ) {
    is( PTWState.Free.U ) {
      nextState := Mux( io.ptw_i.fire, PTWState.Lvl2.U, PTWState.Free.U )
    }
    is( PTWState.Lvl2.U ) {
      nextState :=
        Mux(
          (is_trans_done) | is_hit,
          Mux( is_ptw_end | is_ptw_fail | kill_trans, PTWState.Free.U, PTWState.Lvl1.U ),
          PTWState.Lvl2.U
        )
    }
    is( PTWState.Lvl1.U ) {
      nextState :=
        Mux(
          (is_trans_done) | is_hit,
          Mux( is_ptw_end | is_ptw_fail | kill_trans, PTWState.Free.U, PTWState.Lvl0.U ),
          PTWState.Lvl1.U
        )
    }
    is( PTWState.Lvl0.U ) {
      nextState :=
        Mux(
          (is_trans_done) | is_hit,
          Mux( is_ptw_end | is_ptw_fail | kill_trans, PTWState.Free.U, PTWState.Free.U ),
          PTWState.Lvl0.U
        )
    }
  }
}


trait PTWWALK { this: PTWBase => 

  val a     = Wire(UInt(44.W))
 
  a :=
    Mux1H(Seq(
      ( nextState === PTWState.Lvl2.U ) -> io.cmm_mmu.satp(43,0),
      ( nextState === PTWState.Lvl1.U ) -> Cat(pte.ppn(2), pte.ppn(1), pte.ppn(0)),
      ( nextState === PTWState.Lvl0.U ) -> Cat(pte.ppn(2), pte.ppn(1), pte.ppn(0)),
    ))

  addr_dnxt :=
    Cat(a, Mux1H(Seq(
      (nextState === PTWState.Lvl2.U ) -> Cat(io.ptw_i.bits.vaddr(38,30), 0.U(3.W)),
      (nextState === PTWState.Lvl1.U ) -> Cat(walkReq.vaddr(29,21), 0.U(3.W)),
      (nextState === PTWState.Lvl0.U ) -> Cat(walkReq.vaddr(20,12), 0.U(3.W)),
      ))
    )



  is_ptw_end := 
    pte.R === true.B & 
    pte.X === true.B


  is_ptw_fail :=
    ( is_ptw_end & 
      Mux1H(Seq(
        (currState === PTWState.Free.U ) -> false.B,
        (currState === PTWState.Lvl2.U ) -> (pte.ppn(0) =/= 0.U | pte.ppn(1) =/= 0.U),
        (currState === PTWState.Lvl1.U ) -> (pte.ppn(0) =/= 0.U),
        (currState === PTWState.Lvl0.U ) -> false.B,
      ))
    ) |
    ( 
      (pte.V === false.B) |
      (pte.R === false.B & pte.W === true.B)
    )

          
  pte.is_4K_page   := is_ptw_end & currState === PTWState.Lvl0.U
  pte.is_mega_page := is_ptw_end & currState === PTWState.Lvl1.U      
  pte.is_giga_page := is_ptw_end & currState === PTWState.Lvl2.U


  when( currState === PTWState.Free.U & nextState === PTWState.Lvl2.U ) {
    walkRspBits.is_access_fault := false.B
  }
  .elsewhen( io.ptw_get.fire ) {
    walkRspBits.is_access_fault := 
      walkRspBits.is_access_fault |
      PMP( io.cmm_mmu, io.ptw_get.bits.address, Cat(false.B, false.B, true.B))
  }

  when( nextState === PTWState.Free.U & currState =/= PTWState.Free.U ) {
    walkRspBits.is_ptw_fail := is_ptw_fail
    walkRspBits.is_X := walkReq.is_X
    walkRspBits.is_R := walkReq.is_R
    walkRspBits.is_W := walkReq.is_W
    walkRspBits.pte := pte

    walkRspValid := true.B

    assert( io.ptw_o.valid === false.B )
  } .elsewhen( io.ptw_o.fire ) {
    walkRspValid := false.B
  }

}

trait PTWCache { this: PTWBase => 
  val cl_sel = addr_qout(11,5)
  val tag_sel = addr_qout(55,12)

  val cache_dat = new Cache_dat( 256, 56, 1, 128, bk = 1 )
  val cache_tag = new Cache_tag( 256, 56, 1, 128, bk = 1 ){ require ( tag_w == 44 ) }
  val is_cache_valid = RegInit( VecInit( Seq.fill(128)(false.B) ) )


  val ptw_access_data_lo = RegInit( 0.U(128.W) )


  cache_dat.dat_addr_r := addr_dnxt
  cache_tag.tag_addr_r := addr_dnxt

  cache_dat.dat_addr_w := addr_qout
  cache_tag.tag_addr_w := addr_qout


  for( i <- 0 until 1 ) yield {
    val rd_enable = 
      (currState === PTWState.Lvl1.U & nextState === PTWState.Lvl0.U)

    val wr_enable = 
      (currState === PTWState.Lvl0.U & is_trans_done & ~is_ptw_fail & ~kill_trans & ~io.cmm_mmu.sfence_vma)

    cache_tag.tag_en_w(i) := wr_enable
    cache_tag.tag_en_r(i) := rd_enable


    cache_dat.dat_en_r(i) := rd_enable
    cache_dat.dat_en_w(i) := wr_enable


    when( wr_enable ) {
      is_cache_valid(cl_sel) := true.B
    } .elsewhen( io.cmm_mmu.sfence_vma ) {
      for ( c <- 0 until 128 ) yield { is_cache_valid(c) := false.B }
    }
  }


  cache_dat.dat_info_wstrb := Fill(256/8, 1.U)


  cache_dat.dat_info_w := Cat( io.ptw_access.bits.data(127,64), io.ptw_access.bits.data(63,0), ptw_access_data_lo(127,64), ptw_access_data_lo(63,0))

  is_hit := (tag_sel === cache_tag.tag_info_r(0)) & is_cache_valid(cl_sel) & currState === PTWState.Lvl0.U


  pte.value := {
    val data = WireDefault(0.U(256.W))
    val data_sel = Wire(UInt(64.W))

    when( is_hit ) {
      data := cache_dat.dat_info_r(0)
    } .elsewhen( is_trans_done ) {
      data := Cat( io.ptw_access.bits.data, ptw_access_data_lo )
    }

    data_sel := Mux1H(Seq(
      (addr_qout(4,3) === 0.U ) -> data(63,0),
      (addr_qout(4,3) === 1.U ) -> data(127,64),
      (addr_qout(4,3) === 2.U ) -> data(191,128),
      (addr_qout(4,3) === 3.U ) -> data(255,192),
    ))
    
    assert( PopCount( Seq(is_hit, is_trans_done) ) <= 1.U )

    val value = RegInit(0.U(64.W))

    when( is_hit ) {
      value := data_sel
    } .elsewhen( is_trans_done ) {
      value := data_sel
    }

    MuxCase(value, Array(
      is_hit        -> data_sel,
      is_trans_done -> data_sel
    ))
  }

  when( io.ptw_access.fire & ~is_trans_done) {
    ptw_access_data_lo := io.ptw_access.bits.data
  }
}



trait PTWBus { this: PTWBase => 
  val ptw_get_valid = RegInit(false.B)
  val ptw_access_ready = RegInit(false.B)

  io.ptw_get.valid := ptw_get_valid
  io.ptw_access.ready := ptw_access_ready

  val is_get_reqed = RegInit(false.B)
  when( io.ptw_get.fire ) { is_get_reqed := true.B }
  .elsewhen( nextState =/= currState ) { is_get_reqed := false.B }


  when( (currState === PTWState.Lvl2.U | currState === PTWState.Lvl1.U | currState === PTWState.Lvl0.U) & ~is_hit & ~io.ptw_get.valid & ~is_get_reqed ) {
    ptw_get_valid := true.B
  } .elsewhen( io.ptw_get.fire ) {
    ptw_get_valid := false.B
  }

  when( io.ptw_access.valid & ~io.ptw_access.ready) {
    ptw_access_ready := true.B
  } .elsewhen( io.ptw_access.fire ) {
    ptw_access_ready := false.B
  }

  io.ptw_get.bits := mEdge.Get(fromSource = mid.U, toAddress = addr_qout & ("hFFFFFFFF".U << 5), lgSize = log2Ceil(256/8).U)._2



}


/** 
  * page table walker
  */ 
class PTW(edge: TLEdgeOut, id: Int)(implicit p: Parameters) extends PTWBase(edge, id) 
  with PTWStateMachine
  with PTWWALK
  with PTWCache
  with PTWBus{


  when( io.cmm_mmu.sfence_vma & currState =/= PTWState.Free.U ) {
    kill_trans := true.B
  } .elsewhen( currState === PTWState.Free.U ) {
    kill_trans := false.B
  }


  io.ptw_o.bits  := walkRspBits
  io.ptw_o.valid := walkRspValid & ~kill_trans


  io.ptw_i.ready := currState === PTWState.Free.U & ~io.ptw_o.valid & ~io.cmm_mmu.sfence_vma & ~kill_trans


}


