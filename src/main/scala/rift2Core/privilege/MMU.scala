/*
  Copyright (c) 2020 - 2021 Ruige Lee <wut.ruigeli@gmail.com>

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

import chisel3.util.random._

import tilelink._

class Info_pte_sv39 extends Bundle {
  val V = Bool()
  val R = Bool()
  val W = Bool()
  val X = Bool()
  val U = Bool()
  val G = Bool()
  val A = Bool()
  val D = Bool()
  val rsw = UInt(2.W)
  val ppn = MixedVec( Seq( UInt(9.W), UInt(9.W), UInt(26.W) ) )
  val reserved = UInt(10.W)

}

class Info_tlb_tag extends Bundle {
  val is_valid = Bool()
  val asid = UInt( 16.W )
  val vpn = Vec( 3, UInt(9.W) )
  val is_4K_page = Bool()
  val is_mega_page = Bool()
  val is_giga_page = Bool()

}

class Info_ptw_tlb extends Bundle {
  val pte = new Info_pte_sv39
  val is_4K_page = Bool()
  val is_giga_page = Bool()
  val is_mega_page = Bool()
}

class Info_pmpcfg extends Bundle {
  val R    = Bool()
  val W    = Bool()
  val X    = Bool()
  val A    = UInt(2.W)
  val reserved = UInt(2.W)
  val L = Bool()

}

class Info_mmu_req extends Bundle {
  val vaddr  = UInt(64.W)
  val asid_i = UInt(16.W)
}

class Info_mmu_rsp extends Bundle {
  val paddr  = UInt(64.W)
  val is_page_fault = Bool()
  val is_pmp_fault = Bool()
}


class Info_csr_mmu extends Bundle {
  val satp = UInt(64.W)

	val pmpcfg0    = UInt(64.W)
	val pmpcfg2    = UInt(64.W)
	val pmpcfg4    = UInt(64.W)
	val pmpcfg6    = UInt(64.W)
	val pmpcfg8    = UInt(64.W)
	val pmpcfg10   = UInt(64.W)
	val pmpcfg12   = UInt(64.W)
	val pmpcfg14   = UInt(64.W)

  

}
    val csr_mmu = 

// object Page {

//   def apply( info: UInt ) = {
//     val pte = new Info_pte_sv39

//     pte.V      := info(0).asBool
//     pte.R      := info(1).asBool
//     pte.W      := info(2).asBool
//     pte.X      := info(3).asBool
//     pte.U      := info(4).asBool
//     pte.G      := info(5).asBool
//     pte.A      := info(6).asBool
//     pte.D      := info(7).asBool
//     pte.rsw    := info(9,8)
//     pte.ppn(0) := info(18,10)
//     pte.ppn(1) := info(27,19)
//     pte.ppn(2) := info(53,28)

//     pte
//   }
// }


/** 
  * translation lookaside buffer for instance both itlb and dtlb
  * @param entry the entries number of fully associative tlb
  * 
  * 
  */ 
class TLB( entry: Int = 32 ) extends Module {
  val io = IO(new Bundle{

    val pte_o  = Output( new Info_pte_sv39 )
    val vaddr  = Input( UInt(64.W) )
    val asid_i = Input(UInt(16.W))

    val ptw_tlb = Flipped(ValidIO(new Info_ptw_tlb))
    val is_tlb_hit = Output(Bool())

    val flush = Input(Bool())
  })

  val tag = RegInit( VecInit( Seq.fill(entry)(0.U.asTypeOf( new Info_tlb_tag  ))))
  val pte = RegInit( VecInit( Seq.fill(entry)(0.U.asTypeOf( new Info_pte_sv39 ))))

  when( io.flush ) {
    for( i <- 0 until entry ) yield tag(i) := 0.U.asTypeOf( new Info_tlb_tag )
  }
  .elsewhen(io.ptw_tlb.valid){
    tag(tlb_update_idx).is_valid := true.B
    tag(tlb_update_idx).asid := io.asid_i
    tag(tlb_update_idx).is_4K_page   := io.ptw_tlb.bits.is_4K_page
    tag(tlb_update_idx).is_giga_page := io.ptw_tlb.bits.is_giga_page
    tag(tlb_update_idx).is_mega_page := io.ptw_tlb.bits.is_mega_page
    tag(tlb_update_idx).vpn(0) := io.vaddr(20,12)
    tag(tlb_update_idx).vpn(1) := io.vaddr(29,21)
    tag(tlb_update_idx).vpn(2) := io.vaddr(38,30)


    pte(tlb_update_idx) := io.ptw_tlb.bits.pte
  }






  /** 
    * tlb replace in random 
    * @return idx one idx to update tlb entry
    * 
    */
  def tlb_update_idx: UInt = {
    val is_runout = tag.forall( (x:Info_tlb_tag) => (x.is_valid === true.B) )

    val random_idx = LFSR( log2Ceil(entry) )
    val empty_idx  = tag.indexWhere( (x:Info_tlb_tag) => (x.is_valid === false.B) )

    return Mux( is_runout, random_idx, empty_idx )
  }




  val tlb_hit = Wire(Vec( entry, Bool()))
  for ( i <- 0 until entry ) yield {
    tlb_hit :=
      tag(i).is_valid & io.asid_i === tag(i).asid & io.vaddr(38,30) === tag(i).vpn(2) & (
        tag(i).is_giga_page | (
          io.vaddr(29,21) === tag(i).vpn(1) & (
            tag(i).is_mega_page |
            io.vaddr(20,12) === tag(i).vpn(0)
          )
        )
      )
  }

  assert( PopCount( tlb_hit.asUInt ) <= 1.U, "Assert Fail at tlb, more than 1 entry hit!"  )

  io.pte_o := Mux1H( tlb_hit zip pte )
  io.is_tlb_hit := tlb_hit.contains(true.B)



}




/** 
  * page table walker
  */ 
class PTW extends Module {
  val io = IO(new Bundle{
    val vaddr = Flipped(ValidIO(UInt(64.W)))
    val ptw_tlb = ValidIO(new Info_ptw_tlb)
    val satp_ppn = Input(UInt(44.W))

    val ptw_chn_a = new DecoupledIO(new TLchannel_a(64, 32))
    val ptw_chn_d = Flipped(new DecoupledIO(new TLchannel_d(64)))
  })

  object state {
    val free = 0.U
    val read = 1.U
    val walk = 2.U
  }

  val ptw_mst = Module( new TileLink_mst_heavy(64, 32, 2))



  val fsm = new Bundle {
    val state_dnxt = Wire(UInt(2.W))
    val state_qout = RegNext( state_dnxt, state.free )

  }


//    SSSSSSSSSSSSSSS TTTTTTTTTTTTTTTTTTTTTTT         AAA         TTTTTTTTTTTTTTTTTTTTTTTEEEEEEEEEEEEEEEEEEEEEE
//  SS:::::::::::::::ST:::::::::::::::::::::T        A:::A        T:::::::::::::::::::::TE::::::::::::::::::::E
// S:::::SSSSSS::::::ST:::::::::::::::::::::T       A:::::A       T:::::::::::::::::::::TE::::::::::::::::::::E
// S:::::S     SSSSSSST:::::TT:::::::TT:::::T      A:::::::A      T:::::TT:::::::TT:::::TEE::::::EEEEEEEEE::::E
// S:::::S            TTTTTT  T:::::T  TTTTTT     A:::::::::A     TTTTTT  T:::::T  TTTTTT  E:::::E       EEEEEE
// S:::::S                    T:::::T            A:::::A:::::A            T:::::T          E:::::E             
//  S::::SSSS                 T:::::T           A:::::A A:::::A           T:::::T          E::::::EEEEEEEEEE   
//   SS::::::SSSSS            T:::::T          A:::::A   A:::::A          T:::::T          E:::::::::::::::E   
//     SSS::::::::SS          T:::::T         A:::::A     A:::::A         T:::::T          E:::::::::::::::E   
//        SSSSSS::::S         T:::::T        A:::::AAAAAAAAA:::::A        T:::::T          E::::::EEEEEEEEEE   
//             S:::::S        T:::::T       A:::::::::::::::::::::A       T:::::T          E:::::E             
//             S:::::S        T:::::T      A:::::AAAAAAAAAAAAA:::::A      T:::::T          E:::::E       EEEEEE
// SSSSSSS     S:::::S      TT:::::::TT   A:::::A             A:::::A   TT:::::::TT      EE::::::EEEEEEEE:::::E
// S::::::SSSSSS:::::S      T:::::::::T  A:::::A               A:::::A  T:::::::::T      E::::::::::::::::::::E
// S:::::::::::::::SS       T:::::::::T A:::::A                 A:::::A T:::::::::T      E::::::::::::::::::::E
//  SSSSSSSSSSSSSSS         TTTTTTTTTTTAAAAAAA                   AAAAAAATTTTTTTTTTT      EEEEEEEEEEEEEEEEEEEEEE


  val ptw_state_dnxt_in_free = Mux( io.vaddr.valid, state.walk, state.free )
  val ptw_state_dnxt_in_read = Mux( io.ptw_chn_d.fire, state.walk, state.read )
  val ptw_state_dnxt_in_walk = Mux( walk.is_ptw_end | walk.is_ptw_fail , state.free, state.read)

  fsm.state_dnxt := Mux1H( Seq(
    (fsm.state_qout === state.free) -> ptw_state_dnxt_in_free,
    (fsm.state_qout === state.read) -> ptw_state_dnxt_in_read,
    (fsm.state_qout === state.walk) -> ptw_state_dnxt_in_walk,
  ))

  assert( 
    PopCount( Seq(
      fsm.state_qout === state.free,
      fsm.state_qout === state.read,
      fsm.state_qout === state.walk
    )) === 1.U, "Assert Faild at ptw FSM!"
  )




// WWWWWWWW                           WWWWWWWW   AAA               LLLLLLLLLLL             KKKKKKKKK    KKKKKKK
// W::::::W                           W::::::W  A:::A              L:::::::::L             K:::::::K    K:::::K
// W::::::W                           W::::::W A:::::A             L:::::::::L             K:::::::K    K:::::K
// W::::::W                           W::::::WA:::::::A            LL:::::::LL             K:::::::K   K::::::K
//  W:::::W           WWWWW           W:::::WA:::::::::A             L:::::L               KK::::::K  K:::::KKK
//   W:::::W         W:::::W         W:::::WA:::::A:::::A            L:::::L                 K:::::K K:::::K   
//    W:::::W       W:::::::W       W:::::WA:::::A A:::::A           L:::::L                 K::::::K:::::K    
//     W:::::W     W:::::::::W     W:::::WA:::::A   A:::::A          L:::::L                 K:::::::::::K     
//      W:::::W   W:::::W:::::W   W:::::WA:::::A     A:::::A         L:::::L                 K:::::::::::K     
//       W:::::W W:::::W W:::::W W:::::WA:::::AAAAAAAAA:::::A        L:::::L                 K::::::K:::::K    
//        W:::::W:::::W   W:::::W:::::WA:::::::::::::::::::::A       L:::::L                 K:::::K K:::::K   
//         W:::::::::W     W:::::::::WA:::::AAAAAAAAAAAAA:::::A      L:::::L         LLLLLLKK::::::K  K:::::KKK
//          W:::::::W       W:::::::WA:::::A             A:::::A   LL:::::::LLLLLLLLL:::::LK:::::::K   K::::::K
//           W:::::W         W:::::WA:::::A               A:::::A  L::::::::::::::::::::::LK:::::::K    K:::::K
//            W:::W           W:::WA:::::A                 A:::::A L::::::::::::::::::::::LK:::::::K    K:::::K
//             WWW             WWWAAAAAAA                   AAAAAAALLLLLLLLLLLLLLLLLLLLLLLLKKKKKKKKK    KKKKKKK

  val walk = new Bundle {
    val a     = RegInit(0.U(44.W))
    val level = RegInit(0.U(2.W))
    val is_ptw_end = Wire(Bool())
    val is_ptw_fail = Wire(Bool())
    val pte  = RegEnable( ptw_mst.io.d.bits.data, ptw_mst.io.d.fire ).asTypeOf(new Info_pte_sv39)

    val addr  = Cat(a, Mux1H(Seq(
              (level === 0.U) -> Cat(io.vaddr.bits(20,12), 0.U(3.W)),
              (level === 1.U) -> Cat(io.vaddr.bits(29,21), 0.U(3.W)),
              (level === 2.U) -> Cat(io.vaddr.bits(38,30), 0.U(3.W)),
              ))
          )
  }

  walk.is_ptw_end := 
          fsm.state_qout === state.walk & 
          walk.pte.R === true.B & 
          walk.pte.X === true.B

  walk.is_ptw_fail :=
          fsm.state_qout === state.walk & (
            ( ~walk.is_ptw_end & MuxCase( false.B, Seq( (walk.level === 0.U) -> false.B, (walk.level === 1.U) -> (walk.pte.ppn(0) =/= 0.U), (walk.level === 2.U) -> (walk.pte.ppn(0) =/= 0.U | walk.pte.ppn(1) =/= 0.U)) )) |
            ( walk.pte.V === false.B | (walk.pte.R === false.B & walk.pte.W === true.B))
          )
            
  io.ptw_tlb.bits.is_4K_page   := walk.is_ptw_end & walk.level === 0.U
  io.ptw_tlb.bits.is_mega_page := walk.is_ptw_end & walk.level === 1.U       
  io.ptw_tlb.bits.is_giga_page := walk.is_ptw_end & walk.level === 2.U

  when( fsm.state_qout === state.free & fsm.state_dnxt === state.read ) {
    walk.a     := io.satp_ppn
    walk.level := 2.U
  }
  .elsewhen( fsm.state_qout === state.walk & fsm.state_dnxt === state.read ) {
    walk.a     := Cat(walk.pte.ppn(2), walk.pte.ppn(1), walk.pte.ppn(0))
    walk.level := walk.level - 1.U
  }





// TTTTTTTTTTTTTTTTTTTTTTT  iiii  lllllll                   LLLLLLLLLLL               iiii                   kkkkkkkk           
// T:::::::::::::::::::::T i::::i l:::::l                   L:::::::::L              i::::i                  k::::::k           
// T:::::::::::::::::::::T  iiii  l:::::l                   L:::::::::L               iiii                   k::::::k           
// T:::::TT:::::::TT:::::T        l:::::l                   LL:::::::LL                                      k::::::k           
// TTTTTT  T:::::T  TTTTTTiiiiiii  l::::l     eeeeeeeeeeee    L:::::L               iiiiiiinnnn  nnnnnnnn     k:::::k    kkkkkkk
//         T:::::T        i:::::i  l::::l   ee::::::::::::ee  L:::::L               i:::::in:::nn::::::::nn   k:::::k   k:::::k 
//         T:::::T         i::::i  l::::l  e::::::eeeee:::::eeL:::::L                i::::in::::::::::::::nn  k:::::k  k:::::k  
//         T:::::T         i::::i  l::::l e::::::e     e:::::eL:::::L                i::::inn:::::::::::::::n k:::::k k:::::k   
//         T:::::T         i::::i  l::::l e:::::::eeeee::::::eL:::::L                i::::i  n:::::nnnn:::::n k::::::k:::::k    
//         T:::::T         i::::i  l::::l e:::::::::::::::::e L:::::L                i::::i  n::::n    n::::n k:::::::::::k     
//         T:::::T         i::::i  l::::l e::::::eeeeeeeeeee  L:::::L                i::::i  n::::n    n::::n k:::::::::::k     
//         T:::::T         i::::i  l::::l e:::::::e           L:::::L         LLLLLL i::::i  n::::n    n::::n k::::::k:::::k    
//       TT:::::::TT      i::::::il::::::le::::::::e        LL:::::::LLLLLLLLL:::::Li::::::i n::::n    n::::nk::::::k k:::::k   
//       T:::::::::T      i::::::il::::::l e::::::::eeeeeeeeL::::::::::::::::::::::Li::::::i n::::n    n::::nk::::::k  k:::::k  
//       T:::::::::T      i::::::il::::::l  ee:::::::::::::eL::::::::::::::::::::::Li::::::i n::::n    n::::nk::::::k   k:::::k 
//       TTTTTTTTTTT      iiiiiiiillllllll    eeeeeeeeeeeeeeLLLLLLLLLLLLLLLLLLLLLLLLiiiiiiii nnnnnn    nnnnnnkkkkkkkk    kkkkkkk

  io.ptw_chn_a <> ptw_mst.io.a
  io.ptw_chn_d <> ptw_mst.io.d

  ptw_mst.io.a_info.address := walk.addr
  ptw_mst.io.a_info.corrupt := false.B
  ptw_mst.io.a_info.data := DontCare
  ptw_mst.io.a_info.mask := DontCare
  ptw_mst.io.a_info.opcode := ptw_mst.Get
  ptw_mst.io.a_info.param := DontCare
  ptw_mst.io.a_info.size := 3.U
  ptw_mst.io.a_info.source := 2.U

  ptw_mst.io.is_req := (fsm.state_dnxt === state.read & fsm.state_qout =/= state.read)





}



/** 
  * physical memory protection
  * It asserts that only '''8''' pmp entry is implemented
  */ 


class PMP(entry: Int) extends RawModule {
  val io = IO(new Bundle{
    val pmp_addr = Input(Vec( entry+1, UInt(54.W)))
    val pmp_cfg  = Input(Vec( entry, new Info_pmpcfg))

    val chk_addr = Input(UInt(64.W))
    val chk_priv = Input(UInt(2.W))
    val chk_type = Input(UInt(3.W))

    val is_valid = Output(Bool())
  })

  
  /**
    * when pmp in off mode 
    *
    * @return Bool false
    */

  def off_range: Bool = return false.B

  /**
    * when pmp in TOR mode 
    *
    * @param addr_b (54.W) addr_b << 2 is the bottom of PMP address range
    * @param addr_t (54.W) addr_t << 2 is the top of PMP address range
    * @param addr_c (56.W) the address needed to be check
    * @return Bool whether the addr_c is in the range
    */
 
  def tor_range( addr_b: UInt, addr_t: UInt, addr_c: UInt): Bool = {
    return (addr_c < (addr_t << 2)) & (addr_c > (addr_b << 2))
  }

  /**
    * 
    * When pmp in NA4 mode
    * @param addr_p (54.W) the address range with mask info
    * @param addr_c (56.W) the address needed to be check
    * @return Bool whether the addr_c is in the range
    */

  def na4_range( addr_p: UInt, addr_c: UInt ): Bool = {
    val mask = "hffffffffffffffff".U << 2 
    return ((addr_c & mask) === ((addr_p << 2) & mask))
  }

  /**
    * 
    * When pmp in NAPOT mode
    * @param addr_p (54.W) the address range with mask info
    * @param addr_c (56.W) the address needed to be check
    * @return Bool whether the addr_c is in the range
    */

  def napot_range( addr_p: UInt, addr_c: UInt ): Bool = {
    val zero_pos = for( i <- 0 until 54 ) yield { addr_p(i) === 0.U }
    val cnt_idx  = for( i <- 0 until 54 ) yield { i.U + 3.U }
    val cnt = MuxCase(54.U + 3.U, zero_pos zip cnt_idx)

    val mask = "hffffffffffffffff".U << cnt 
    return ((addr_c & mask) === ((addr_p << 2) & mask))
  }

  /**
    * check if the access type is allowed
    *
    * @param chk_type (UINT(3.W)) the req access type
    * @param is_X (Bool()) if execute is allowed in pmp
    * @param is_W (Bool()) if write is allowed in pmp
    * @param is_R (Bool()) if read is allowed in pmp
    * @return Bool() whether the access is match 
    */
  def cmp_type( chk_type: UInt, is_X: Bool, is_W: Bool, is_R: Bool ): Bool = {
    return ( (chk_type & Cat(is_X, is_W, is_R)) === chk_type )
  }

  /**
    * check if the pmp  should be bypass
    * when in M-mode and the pmp is unlock, the type matching should bypass the pmp limitation
    *
    * @param chk_priv (UInt(2.W)) the current privlege mode
    * @param is_L whether is lock
    * @return Bool() whether the type matching is bypass 
    */
  def cmp_priv( chk_priv: UInt, is_L: Bool ): Bool = {
    return (chk_priv === "b11".U & ~is_L)
  }

  val is_inRange = Wire( UInt(entry.W) )
  val is_inType  = Wire( UInt(entry.W) )
  val is_inEnfce = Wire( UInt(entry.W) )

  for( i <- 0 until entry ) yield {
    is_inRange := Mux1H( Seq(
      (io.pmp_cfg(i).A === 0.U) -> off_range,
      (io.pmp_cfg(i).A === 1.U) -> tor_range  (io.pmp_addr(i),   io.pmp_addr(i+1), io.chk_addr),
      (io.pmp_cfg(i).A === 2.U) -> na4_range  (io.pmp_addr(i+1), io.chk_addr),
      (io.pmp_cfg(i).A === 3.U) -> napot_range(io.pmp_addr(i+1), io.chk_addr)
    ))
    is_inType  := cmp_type( io.chk_type, io.pmp_cfg(i).X, io.pmp_cfg(i).W, io.pmp_cfg(i).R )
    is_inEnfce := cmp_priv( io.chk_priv, io.pmp_cfg(i).L )
  }

  /**
    * when no range match, but in M-mode, it's valid!
    */
  val is_Mbp = ~is_inRange.orR & (io.chk_priv === "b11".U)
  val idx = is_inRange.asBools.indexWhere( (x:Bool) => (x === true.B ))

  io.is_valid :=
    is_Mbp |
    ( is_inRange.orR & (is_inType(idx) | is_inEnfce(idx) ))
}

/**
  * make up a factory to instance the pmp
  */
object PMP{
  def apply(
    entry: Int,
    pmp_addr: Vec[UInt],
    pmp_cfg:  Vec[Info_pmpcfg],
    chk_addr: UInt,
    chk_priv: UInt,
    chk_type: UInt
  ) = {
    val mdl = Module( new PMP(entry) )
    mdl.io.pmp_addr := pmp_addr
    mdl.io.pmp_cfg  := pmp_cfg
    mdl.io.chk_addr := chk_addr
    mdl.io.chk_priv := chk_priv
    mdl.io.chk_type := chk_type

    mdl.io.is_valid
  }

}



/**
  * Top layer of Memory management unit
  * 
  * including instruction-translation-lookaside-buffer with physical-memory-protection
  * 
  * including data-translation-lookaside-buffer with physical-memory-protection
  * 
  * including page-table-walker with physical-memory-protection
  *
  */
class MMU extends Module {
  val io = IO(new Bundle{
    val if_mmu = ValidIO(new Info_mmu_req)
    val mmu_if = ValidIO(new Info_mmu_rsp)

    val iss_mmu = ValidIO(new Info_mmu_req)
    val mmu_iss = ValidIO(new Info_mmu_rsp)

    val csr_mmu = 
    val mmu_cmm = 



    val mmu_chn_a = new DecoupledIO(new TLchannel_a(64, 32))
    val mmu_chn_d = Flipped(new DecoupledIO(new TLchannel_d(64)))


    val flush = Input(Bool())
  })

  val itlb = Module( new TLB(32) )
  val dtlb = Module( new TLB(32) )
  val ptw  = Module( new PTW )



    itlb.io.pte_o  = Output( new Info_pte_sv39 )
    itlb.io.vaddr  = Input( UInt(64.W) )
    itlb.io.asid_i = Input(UInt(16.W))
    itlb.io.ptw_tlb = Flipped(ValidIO(new Info_ptw_tlb))
    itlb.io.is_tlb_hit = Output(Bool())
    itlb.io.flush = Input(Bool())

    dtlb.io.pte_o  = Output( new Info_pte_sv39 )
    dtlb.io.vaddr  = Input( UInt(64.W) )
    dtlb.io.asid_i = Input(UInt(16.W))
    dtlb.io.ptw_tlb = Flipped(ValidIO(new Info_ptw_tlb))
    dtlb.io.is_tlb_hit = Output(Bool())
    dtlb.io.flush = Input(Bool())



    ptw.io.vaddr = Flipped(ValidIO(UInt(64.W)))
    ptw.io.ptw_tlb = ValidIO(new Info_ptw_tlb)
    ptw.io.satp_ppn = Input(UInt(44.W))
    ptw.io.ptw_chn_a = new DecoupledIO(new TLchannel_a(64, 32))
    ptw.io.ptw_chn_d = Flipped(new DecoupledIO(new TLchannel_d(64)))
}
