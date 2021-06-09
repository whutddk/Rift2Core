

/*
* @Author: Ruige Lee
* @Date:   2021-04-27 17:08:56
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-30 11:12:27
*/


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

package rift2Core.cache

import chisel3._
import chisel3.util._
import chisel3.util.random._
import chisel3.experimental.ChiselEnum

import tilelink._
import base._


class L2Cache( dw:Int = 512, bk:Int = 2, cb:Int = 4, cl:Int = 512 ) extends Module {
  val io = IO( new Bundle{
    val il1_chn_a = Flipped( new DecoupledIO(new TLchannel_a(128, 32)) )
    val il1_chn_d = new DecoupledIO( new TLchannel_d(128) )

    val dl1_chn_a = Flipped(new DecoupledIO(new TLchannel_a(128, 32)))
    val dl1_chn_d = new DecoupledIO( new TLchannel_d(128) )

    val l2c_chn_a = new DecoupledIO(new TLchannel_a(128, 32))
    val l2c_chn_d = Flipped(new DecoupledIO( new TLchannel_d(128) )	)	


    val l2c_fence_req = Input(Bool())
  })

  def addr_lsb = log2Ceil(dw*bk/8)
  def line_w   = log2Ceil(cl)
  def tag_w    = 32 - addr_lsb - line_w

  val il1_slv = Module(new TileLink_slv_heavy(128, 32))
  val dl1_slv = Module(new TileLink_slv_heavy(128, 32))
  val l2c_mst = Module(new TileLink_mst_heavy(128, 32, 3))

  val cache_mem = new Cache_mem( dw, 32, bk, cb, cl )

  val cache_addr_dnxt = Wire( UInt(32.W) )
  val cache_addr_qout = RegInit(0.U(32.W))


  val tag_addr = RegInit(0.U(32.W))
  val tag_info = tag_addr(31, 32-tag_w)


  val req_no_dnxt = Wire( UInt(3.W) )
  val req_no_qout = RegInit(0.U(3.W))


  val is_fence = RegInit(false.B)




  val bram = new Bundle{
    val random_res = LFSR(log2Ceil(cb), true.B )
    val is_cb_vhit = Wire( Vec(cb, Bool()) )
    val cache_valid = RegInit( VecInit( Seq.fill(cl)( VecInit( Seq.fill(cb)(false.B ) ) ) ))
    val cl_sel = Wire( UInt(line_w.W) )
    val replace_sel = Wire( UInt(log2Ceil(cb).W) )

    val mem_dat = Wire( UInt(128.W) )
  }

  object L2C_state extends ChiselEnum {
    val cfree, cktag, flash, rspir, rspdr, rspdw, rspda, fence = Value
  }

  val fsm = new Bundle{
    val state_qout = RegInit( L2C_state.cfree )
    val state_dnxt = WireDefault( L2C_state.cfree )
  }




// BBBBBBBBBBBBBBBBB   RRRRRRRRRRRRRRRRR                  AAA               MMMMMMMM               MMMMMMMM
// B::::::::::::::::B  R::::::::::::::::R                A:::A              M:::::::M             M:::::::M
// B::::::BBBBBB:::::B R::::::RRRRRR:::::R              A:::::A             M::::::::M           M::::::::M
// BB:::::B     B:::::BRR:::::R     R:::::R            A:::::::A            M:::::::::M         M:::::::::M
//   B::::B     B:::::B  R::::R     R:::::R           A:::::::::A           M::::::::::M       M::::::::::M
//   B::::B     B:::::B  R::::R     R:::::R          A:::::A:::::A          M:::::::::::M     M:::::::::::M
//   B::::BBBBBB:::::B   R::::RRRRRR:::::R          A:::::A A:::::A         M:::::::M::::M   M::::M:::::::M
//   B:::::::::::::BB    R:::::::::::::RR          A:::::A   A:::::A        M::::::M M::::M M::::M M::::::M
//   B::::BBBBBB:::::B   R::::RRRRRR:::::R        A:::::A     A:::::A       M::::::M  M::::M::::M  M::::::M
//   B::::B     B:::::B  R::::R     R:::::R      A:::::AAAAAAAAA:::::A      M::::::M   M:::::::M   M::::::M
//   B::::B     B:::::B  R::::R     R:::::R     A:::::::::::::::::::::A     M::::::M    M:::::M    M::::::M
//   B::::B     B:::::B  R::::R     R:::::R    A:::::AAAAAAAAAAAAA:::::A    M::::::M     MMMMM     M::::::M
// BB:::::BBBBBB::::::BRR:::::R     R:::::R   A:::::A             A:::::A   M::::::M               M::::::M
// B:::::::::::::::::B R::::::R     R:::::R  A:::::A               A:::::A  M::::::M               M::::::M
// B::::::::::::::::B  R::::::R     R:::::R A:::::A                 A:::::A M::::::M               M::::::M
// BBBBBBBBBBBBBBBBB   RRRRRRRR     RRRRRRRAAAAAAA                   AAAAAAAMMMMMMMM               MMMMMMMM

  cache_addr_dnxt := Mux1H( Seq(
    (fsm.state_qout === L2C_state.cfree) -> Mux1H( Seq(
                            (req_no_dnxt === 1.U) -> il1_slv.io.a.bits.address,
                            (req_no_dnxt === 2.U) -> dl1_slv.io.a.bits.address,
                            (req_no_dnxt === 3.U) -> dl1_slv.io.a.bits.address,
                            (req_no_dnxt === 4.U) -> dl1_slv.io.a.bits.address
                          )),
    (fsm.state_qout === L2C_state.cktag) -> (	cache_addr_qout &
                          Mux(
                            fsm.state_dnxt === L2C_state.flash,
                            Cat( Fill(32-addr_lsb, 1.U), 0.U(addr_lsb.W) ), Fill(32, 1.U)
                          )
                        ),
    (fsm.state_qout === L2C_state.flash) -> Mux( l2c_mst.io.d.fire,
                          Mux( (cache_addr_qout + "b10000".U)(6,4) =/= 0.U,
                            cache_addr_qout + "b10000".U,
                            Mux1H( Seq(
                              (req_no_qout === 1.U) -> il1_slv.io.a.bits.address,
                              (req_no_qout === 2.U) -> dl1_slv.io.a.bits.address,
                              (req_no_qout === 3.U) -> dl1_slv.io.a.bits.address,
                              (req_no_qout === 4.U) -> dl1_slv.io.a.bits.address
                            ))
                          ),
                        cache_addr_qout
                      ),
    (fsm.state_qout === L2C_state.rspir) -> Mux( il1_slv.io.d.fire, cache_addr_qout + "b10000".U, cache_addr_qout ),
    (fsm.state_qout === L2C_state.rspdr) -> Mux( dl1_slv.io.d.fire, cache_addr_qout + "b10000".U, cache_addr_qout ),
    (fsm.state_qout === L2C_state.rspdw) -> cache_addr_qout,
    (fsm.state_qout === L2C_state.rspda) -> cache_addr_qout,
    (fsm.state_qout === L2C_state.fence) -> cache_addr_qout
  ))
  cache_addr_qout := cache_addr_dnxt


  def is_block_replace(i: Int) = UIntToOH(bram.replace_sel)(i).asBool

  bram.cl_sel := tag_addr(addr_lsb+line_w-1, addr_lsb)
  
  when( fsm.state_qout === L2C_state.cfree & fsm.state_dnxt === L2C_state.cktag){
    tag_addr := MuxCase( 0.U, Array(
      il1_slv.io.a.valid -> il1_slv.io.a.bits.address,
      dl1_slv.io.a.valid -> dl1_slv.io.a.bits.address
    ))			
  }


  
  

  bram.replace_sel := 
    Mux(
      bram.cache_valid(bram.cl_sel).contains(false.B),
      bram.cache_valid(bram.cl_sel).indexWhere((a: Bool) => (a === false.B)),
      bram.random_res
    )

  for ( i <- 0 until cb ) yield {
    cache_mem.dat_en_w(i) := 
      bram.is_cb_vhit(i) & l2c_mst.io.d.fire & ( fsm.state_qout === L2C_state.flash | fsm.state_qout === L2C_state.rspdw )

    cache_mem.dat_en_r(i) := 
      bram.is_cb_vhit(i) & ( fsm.state_dnxt === L2C_state.rspir | fsm.state_dnxt === L2C_state.rspdr )
  }

  cache_mem.dat_info_wstrb := 
    Mux1H( Seq(
      (fsm.state_qout === L2C_state.flash) -> "hffff".U,
      (fsm.state_qout === L2C_state.rspdw) -> dl1_slv.io.a.bits.mask
    ))

  cache_mem.dat_info_w :=
    Mux1H( Seq(
      (fsm.state_qout === L2C_state.flash) -> l2c_mst.io.d.bits.data,
      (fsm.state_qout === L2C_state.rspdw) -> dl1_slv.io.a.bits.data
    ))



  for ( i <- 0 until cb ) yield {
    cache_mem.tag_en_w(i) := 
      is_block_replace(i) & (fsm.state_qout === L2C_state.cktag) & ( fsm.state_dnxt === L2C_state.flash ) 
  
    cache_mem.tag_en_r(i) :=
      ( fsm.state_dnxt === L2C_state.cktag ) |
      l2c_mst.io.a.fire

  }


  cache_mem.cache_addr := Mux( fsm.state_qout === L2C_state.cfree, cache_addr_dnxt, cache_addr_qout )

  when( fsm.state_qout === L2C_state.cktag & fsm.state_dnxt === L2C_state.flash ) {
    bram.cache_valid(bram.cl_sel)(bram.replace_sel) := true.B
  }
  .elsewhen( fsm.state_qout === L2C_state.fence & fsm.state_dnxt === L2C_state.cfree ) {
    for ( i <- 0 until cl; j <- 0 until cb ) yield bram.cache_valid(i)(j) := false.B
  }
  .elsewhen( fsm.state_qout === L2C_state.rspda & fsm.state_dnxt === L2C_state.cfree ) {
    for ( i <- 0 until cb ) yield {
      bram.cache_valid(bram.cl_sel)(i) := Mux( bram.is_cb_vhit(i), false.B, bram.cache_valid(bram.cl_sel)(i) )
    }
  }
  
  for ( i <- 0 until cb ) yield {
    bram.is_cb_vhit(i) := bram.cache_valid(bram.cl_sel)(i) & ( cache_mem.tag_info_r(i) === tag_info  )
  }

  bram.mem_dat := Mux1H( bram.is_cb_vhit zip cache_mem.dat_info_r )


// FFFFFFFFFFFFFFFFFFFFFFEEEEEEEEEEEEEEEEEEEEEENNNNNNNN        NNNNNNNN        CCCCCCCCCCCCCEEEEEEEEEEEEEEEEEEEEEE
// F::::::::::::::::::::FE::::::::::::::::::::EN:::::::N       N::::::N     CCC::::::::::::CE::::::::::::::::::::E
// F::::::::::::::::::::FE::::::::::::::::::::EN::::::::N      N::::::N   CC:::::::::::::::CE::::::::::::::::::::E
// FF::::::FFFFFFFFF::::FEE::::::EEEEEEEEE::::EN:::::::::N     N::::::N  C:::::CCCCCCCC::::CEE::::::EEEEEEEEE::::E
//   F:::::F       FFFFFF  E:::::E       EEEEEEN::::::::::N    N::::::N C:::::C       CCCCCC  E:::::E       EEEEEE
//   F:::::F               E:::::E             N:::::::::::N   N::::::NC:::::C                E:::::E             
//   F::::::FFFFFFFFFF     E::::::EEEEEEEEEE   N:::::::N::::N  N::::::NC:::::C                E::::::EEEEEEEEEE   
//   F:::::::::::::::F     E:::::::::::::::E   N::::::N N::::N N::::::NC:::::C                E:::::::::::::::E   
//   F:::::::::::::::F     E:::::::::::::::E   N::::::N  N::::N:::::::NC:::::C                E:::::::::::::::E   
//   F::::::FFFFFFFFFF     E::::::EEEEEEEEEE   N::::::N   N:::::::::::NC:::::C                E::::::EEEEEEEEEE   
//   F:::::F               E:::::E             N::::::N    N::::::::::NC:::::C                E:::::E             
//   F:::::F               E:::::E       EEEEEEN::::::N     N:::::::::N C:::::C       CCCCCC  E:::::E       EEEEEE
// FF:::::::FF           EE::::::EEEEEEEE:::::EN::::::N      N::::::::N  C:::::CCCCCCCC::::CEE::::::EEEEEEEE:::::E
// F::::::::FF           E::::::::::::::::::::EN::::::N       N:::::::N   CC:::::::::::::::CE::::::::::::::::::::E
// F::::::::FF           E::::::::::::::::::::EN::::::N        N::::::N     CCC::::::::::::CE::::::::::::::::::::E
// FFFFFFFFFFF           EEEEEEEEEEEEEEEEEEEEEENNNNNNNN         NNNNNNN        CCCCCCCCCCCCCEEEEEEEEEEEEEEEEEEEEEE



  when( io.l2c_fence_req & ~is_fence ) { is_fence := true.B }
  .elsewhen( fsm.state_qout === L2C_state.fence ) { is_fence := false.B }






// FFFFFFFFFFFFFFFFFFFFFF   SSSSSSSSSSSSSSS MMMMMMMM               MMMMMMMM
// F::::::::::::::::::::F SS:::::::::::::::SM:::::::M             M:::::::M
// F::::::::::::::::::::FS:::::SSSSSS::::::SM::::::::M           M::::::::M
// FF::::::FFFFFFFFF::::FS:::::S     SSSSSSSM:::::::::M         M:::::::::M
//   F:::::F       FFFFFFS:::::S            M::::::::::M       M::::::::::M
//   F:::::F             S:::::S            M:::::::::::M     M:::::::::::M
//   F::::::FFFFFFFFFF    S::::SSSS         M:::::::M::::M   M::::M:::::::M
//   F:::::::::::::::F     SS::::::SSSSS    M::::::M M::::M M::::M M::::::M
//   F:::::::::::::::F       SSS::::::::SS  M::::::M  M::::M::::M  M::::::M
//   F::::::FFFFFFFFFF          SSSSSS::::S M::::::M   M:::::::M   M::::::M
//   F:::::F                         S:::::SM::::::M    M:::::M    M::::::M
//   F:::::F                         S:::::SM::::::M     MMMMM     M::::::M
// FF:::::::FF           SSSSSSS     S:::::SM::::::M               M::::::M
// F::::::::FF           S::::::SSSSSS:::::SM::::::M               M::::::M
// F::::::::FF           S:::::::::::::::SS M::::::M               M::::::M
// FFFFFFFFFFF            SSSSSSSSSSSSSSS   MMMMMMMM               MMMMMMMM


  val l2c_state_dnxt_in_cfree = 
    Mux( is_fence, L2C_state.fence,
      Mux( (il1_slv.io.a.valid | dl1_slv.io.a.valid), L2C_state.cktag, L2C_state.cfree )
    )


  val l2c_state_dnxt_in_cktag = Mux1H( Seq(
    (req_no_qout === 1.U) -> ( Mux( bram.is_cb_vhit.contains(true.B), L2C_state.rspir, L2C_state.flash )  ),
    (req_no_qout === 2.U) -> ( Mux( bram.is_cb_vhit.contains(true.B), L2C_state.rspdr, L2C_state.flash )  ),
    (req_no_qout === 3.U) -> ( L2C_state.rspdw ),
    (req_no_qout === 4.U) -> ( L2C_state.rspda )
  ) )

  val l2c_state_dnxt_in_flash = Mux( l2c_mst.io.mode === 7.U, L2C_state.cktag, L2C_state.flash )
  val l2c_state_dnxt_in_rspir = Mux( il1_slv.io.mode === 7.U, L2C_state.cfree, L2C_state.rspir )
  val l2c_state_dnxt_in_rspdr = Mux( dl1_slv.io.mode === 7.U, L2C_state.cfree, L2C_state.rspdr )
  val l2c_state_dnxt_in_rspda = Mux( l2c_mst.io.mode === 7.U, L2C_state.cfree, L2C_state.rspda )
  val l2c_state_dnxt_in_rspdw = Mux( l2c_mst.io.mode === 7.U, L2C_state.cfree, L2C_state.rspdw )
  val l2c_state_dnxt_in_fence = L2C_state.cfree

  fsm.state_dnxt := MuxCase(L2C_state.cfree, Array(
    (fsm.state_qout === L2C_state.cfree) -> l2c_state_dnxt_in_cfree,
    (fsm.state_qout === L2C_state.cktag) -> l2c_state_dnxt_in_cktag,
    (fsm.state_qout === L2C_state.flash) -> l2c_state_dnxt_in_flash,
    (fsm.state_qout === L2C_state.rspir) -> l2c_state_dnxt_in_rspir,
    (fsm.state_qout === L2C_state.rspdr) -> l2c_state_dnxt_in_rspdr,
    (fsm.state_qout === L2C_state.rspdw) -> l2c_state_dnxt_in_rspdw,
    (fsm.state_qout === L2C_state.rspda) -> l2c_state_dnxt_in_rspda,
    (fsm.state_qout === L2C_state.fence) -> l2c_state_dnxt_in_fence
  ))

  fsm.state_qout := fsm.state_dnxt
  


// BBBBBBBBBBBBBBBBB   UUUUUUUU     UUUUUUUU   SSSSSSSSSSSSSSS 
// B::::::::::::::::B  U::::::U     U::::::U SS:::::::::::::::S
// B::::::BBBBBB:::::B U::::::U     U::::::US:::::SSSSSS::::::S
// BB:::::B     B:::::BUU:::::U     U:::::UUS:::::S     SSSSSSS
//   B::::B     B:::::B U:::::U     U:::::U S:::::S            
//   B::::B     B:::::B U:::::D     D:::::U S:::::S            
//   B::::BBBBBB:::::B  U:::::D     D:::::U  S::::SSSS         
//   B:::::::::::::BB   U:::::D     D:::::U   SS::::::SSSSS    
//   B::::BBBBBB:::::B  U:::::D     D:::::U     SSS::::::::SS  
//   B::::B     B:::::B U:::::D     D:::::U        SSSSSS::::S 
//   B::::B     B:::::B U:::::D     D:::::U             S:::::S
//   B::::B     B:::::B U::::::U   U::::::U             S:::::S
// BB:::::BBBBBB::::::B U:::::::UUU:::::::U SSSSSSS     S:::::S
// B:::::::::::::::::B   UU:::::::::::::UU  S::::::SSSSSS:::::S
// B::::::::::::::::B      UU:::::::::UU    S:::::::::::::::SS 
// BBBBBBBBBBBBBBBBB         UUUUUUUUU       SSSSSSSSSSSSSSS  


  req_no_dnxt :=
    Mux( fsm.state_qout === L2C_state.cfree & fsm.state_dnxt === L2C_state.cktag,
      MuxCase( 0.U, Array(
        (il1_slv.io.a.valid)                                                        -> 1.U,
        (dl1_slv.io.a.valid & dl1_slv.io.a.bits.opcode === Opcode.Get)             -> 2.U,
        (dl1_slv.io.a.valid & dl1_slv.io.a.bits.opcode === Opcode.PutFullData)     -> 3.U,
        (dl1_slv.io.a.valid & (dl1_slv.io.a.bits.opcode === Opcode.ArithmeticData |
                    dl1_slv.io.a.bits.opcode === Opcode.LogicalData) ) -> 4.U,
      )),
      req_no_qout
    )
  req_no_qout := req_no_dnxt

  il1_slv.io.is_rsp :=
          ( fsm.state_qout === L2C_state.cktag & fsm.state_dnxt === L2C_state.rspir)

  dl1_slv.io.is_rsp :=
    Mux1H( Seq(
    (req_no_qout === 2.U) -> (fsm.state_qout === L2C_state.cktag & fsm.state_dnxt === L2C_state.rspdr),
    (req_no_qout === 3.U) -> (fsm.state_qout === L2C_state.rspdw & l2c_mst.io.d.fire),
    (req_no_qout === 4.U) -> (fsm.state_qout === L2C_state.rspda & l2c_mst.io.d.fire)
  ) )

  l2c_mst.io.is_req := 	
            ( fsm.state_qout === L2C_state.cktag ) &
              (
                fsm.state_dnxt === L2C_state.flash |
                fsm.state_dnxt === L2C_state.rspdw |
                fsm.state_dnxt === L2C_state.rspda
              )

  il1_slv.io.rsp_data := bram.mem_dat


  dl1_slv.io.rsp_data := MuxCase( DontCare, Array(
                  (fsm.state_qout === L2C_state.rspdw) -> DontCare,
                  (fsm.state_qout === L2C_state.rspdr) -> bram.mem_dat,
                  (fsm.state_qout === L2C_state.rspda) -> RegEnable(l2c_mst.io.d.bits.data,0.U(128.W), l2c_mst.io.d.fire)
                ))



  l2c_mst.io.a_info.opcode :=
              Mux1H( Seq(
                (req_no_qout === 1.U) -> Opcode.Get,
                (req_no_qout === 2.U) -> Opcode.Get,
                (req_no_qout === 3.U) -> dl1_slv.io.a.bits.opcode,
                (req_no_qout === 4.U) -> dl1_slv.io.a.bits.opcode,
              ) )


  l2c_mst.io.a_info.param   := 
              Mux1H( Seq(
                (req_no_qout === 1.U) -> 0.U,
                (req_no_qout === 2.U) -> 0.U,
                (req_no_qout === 3.U) -> dl1_slv.io.a.bits.param,
                (req_no_qout === 4.U) -> dl1_slv.io.a.bits.param
              ) )

  l2c_mst.io.a_info.size    := 
              Mux1H( Seq(
                (req_no_qout === 1.U) -> 7.U, //(1024bits)
                (req_no_qout === 2.U) -> 7.U, //(1024bits)
                (req_no_qout === 3.U) -> dl1_slv.io.a.bits.size,
                (req_no_qout === 4.U) -> dl1_slv.io.a.bits.size
              ) )


  l2c_mst.io.a_info.source  := 
              Mux1H( Seq(
                (req_no_qout === 1.U) -> 3.U,
                (req_no_qout === 2.U) -> 3.U,
                (req_no_qout === 3.U) -> 1.U,
                (req_no_qout === 4.U) -> 1.U
              ) )


  l2c_mst.io.a_info.address := 
              Mux1H( Seq(
                (req_no_qout === 1.U) -> (cache_addr_dnxt & ~("h3f".U(32.W))),
                (req_no_qout === 2.U) -> (cache_addr_dnxt & ~("h3f".U(32.W))),
                (req_no_qout === 3.U) -> dl1_slv.io.a.bits.address,
                (req_no_qout === 4.U) -> dl1_slv.io.a.bits.address
              ) )

  l2c_mst.io.a_info.mask    := 
              Mux1H( Seq(
                (req_no_qout === 1.U) -> 0.U,
                (req_no_qout === 2.U) -> 0.U,
                (req_no_qout === 3.U) -> dl1_slv.io.a.bits.mask,
                (req_no_qout === 4.U) -> dl1_slv.io.a.bits.mask
              ) )



  l2c_mst.io.a_info.data    := 
              Mux1H( Seq(
                (req_no_qout === 1.U) -> 0.U,
                (req_no_qout === 2.U) -> 0.U,
                (req_no_qout === 3.U) -> dl1_slv.io.a.bits.data,
                (req_no_qout === 4.U) -> dl1_slv.io.a.bits.data
              ) )




    io.il1_chn_a <> il1_slv.io.a
    io.il1_chn_d <> il1_slv.io.d
    io.dl1_chn_a <> dl1_slv.io.a
    io.dl1_chn_d <> dl1_slv.io.d
    io.l2c_chn_a <> l2c_mst.io.a
    io.l2c_chn_d <> l2c_mst.io.d


  l2c_mst.io.a_info.corrupt := false.B



}

