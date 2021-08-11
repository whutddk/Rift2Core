
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


class Info_pmpcfg extends Bundle {
  val value = UInt(8.W)

  def L = value(7).asBool
  def reserved = value(6,5)
  def A = value(4,3)
  def X = value(2).asBool
  def W = value(1).asBool
  def R = value(0).asBool

}


/** 
  * physical memory protection
  * It asserts that only '''8''' pmp entry is implemented
  */ 
class PMP(entry: Int) extends RawModule {
  val io = IO(new Bundle{
    // val pmp_addr = Input(Vec( entry+1, UInt(54.W)))
    // val pmp_cfg  = Input(Vec( entry, new Info_pmpcfg))

    val cmm_mmu = Input( new Info_cmm_mmu )

    val chk_addr = Input(UInt(64.W))
    // val chk_priv = Input(UInt(2.W))
    val chk_type = Input(UInt(3.W))

    val is_fault = Output(Bool())
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

  val is_inRange = Wire( Vec(entry, Bool()) )
  val is_inType  = Wire( Vec(entry, Bool()) )
  val is_inEnfce = Wire( Vec(entry, Bool()) )

  val pmpcfg = VecInit(
     io.cmm_mmu.pmpcfg(0)( 7, 0).asTypeOf(new Info_pmpcfg), io.cmm_mmu.pmpcfg(0)(15, 8).asTypeOf(new Info_pmpcfg),
     io.cmm_mmu.pmpcfg(0)(23,16).asTypeOf(new Info_pmpcfg), io.cmm_mmu.pmpcfg(0)(31,24).asTypeOf(new Info_pmpcfg),
     io.cmm_mmu.pmpcfg(0)(39,32).asTypeOf(new Info_pmpcfg), io.cmm_mmu.pmpcfg(0)(47,40).asTypeOf(new Info_pmpcfg),
     io.cmm_mmu.pmpcfg(0)(55,48).asTypeOf(new Info_pmpcfg), io.cmm_mmu.pmpcfg(0)(63,56).asTypeOf(new Info_pmpcfg)
  )

  val pmp_addr = VecInit( Seq(0.U(64.W)) ++ (for ( i <- 0 until 8 ) yield io.cmm_mmu.pmpaddr(i)) )



  for( i <- 0 until entry ) yield {
    is_inRange(i) := Mux1H( Seq(
      (pmp_cfg(i).A === 0.U) -> off_range,
      (pmp_cfg(i).A === 1.U) -> tor_range  (pmp_addr(i),   pmp_addr(i+1), chk_addr),
      (pmp_cfg(i).A === 2.U) -> na4_range  (pmp_addr(i+1), chk_addr),
      (pmp_cfg(i).A === 3.U) -> napot_range(pmp_addr(i+1), chk_addr)
    ))
    is_inType(i)  := cmp_type( io.chk_type, pmp_cfg(i).X, pmp_cfg(i).W, pmp_cfg(i).R )
    is_inEnfce(i) := cmp_priv( io.cmm_mmu.priv_lvl, pmp_cfg(i).L )
  }

  /**
    * when no range match, but in M-mode, it's valid!
    */
  val is_Mbp = ~is_inRange.asUInt.orR & (io.cmm_mmu.priv_lvl === "b11".U)
  val idx = is_inRange.indexWhere( (x:Bool) => (x === true.B ))

  io.is_fault := 
  ~(
    is_Mbp |
    ( is_inRange.asUInt.orR & (is_inType(idx) | is_inEnfce(idx) ))    
  )

}

/**
  * make up a factory to instance the pmp
  */
object PMP{
  def apply(
    cmm_mmu: Vec[Info_cmm_mmu],
    chk_addr: UInt,
    chk_type: UInt
  ) = {
    val mdl = Module( new PMP(8) )
    mdl.io.cmm_mmu  := cmm_mmu
    mdl.io.chk_addr := chk_addr
    mdl.io.chk_type := chk_type

    mdl.io.is_fault
  }

}

