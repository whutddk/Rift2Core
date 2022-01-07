


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

package rift2Core.backend.mem

import chisel3._
import chisel3.util._
import rift2Core.define._
import rift2Core.backend._

class addrTrans extends Module {
  val io = IO(new Bundle{
    val iss_exe = Flipped(new DecoupledIO(new Lsu_iss_info))
    val mmu_lsu = Flipped(DecoupledIO(new Info_mmu_rsp))

    val deq = new DecoupledIO(new Lsu_iss_info)
  })

  io.deq.valid := io.mmu_lsu.valid
  {
    io.deq.bits := io.iss_exe
    io.deq.bits.param.op1 := io.mmu_lsu.bits.paddr
    io.mmu_lsu.ready := io.deq.ready
  }
}

object addrTrans {
  def apply( iss_exe: DecoupledIO[Lsu_iss_info], mmu_lsu: DecoupledIO[Info_mmu_rsp] ): DecoupledIO[Lsu_iss_info] = {
    val mdl = Module(new addrTrans)
    mdl.io.iss_exe := iss_exe
    mdl.io.mmu_lsu := mmu_lsu

    return mdl.io.deq
  }
}

class regionMux extends Module{
  val io = IO(new Bundle{
    val enq = Flipped(new DecoupledIO(new Lsu_iss_info))
    val deq = Vec(3, new DecoupledIO(new Lsu_iss_info))
  })

  val psel = io.enq.bits.paddr(31,28)
  val sel = Mux( psel(3), 2.U, Mux( psel(2), 1.U, 0.U) )


  when( sel === 0.U ) {
    io.deq(0) <> io.enq
  } .otherwise {
    io.deq(0).valid := false.B
    io.deq(0).bits  := DontCare
  }

  when( sel === 1.U ) {
    io.deq(1) <> io.enq
  } .otherwise {
    io.deq(1).valid := false.B
    io.deq(1).bits  := DontCare
  }

  when( sel === 2.U ) {
    io.deq(2) <> io.enq
  } .otherwise {
    io.deq(2).valid := false.B
    io.deq(2).bits  := DontCare
  }
}

class cacheMux(implicit p: Parameters) extends DcacheModule{
  val io = IO(new Bundle{
    val enq = Flipped(new DecoupledIO(new Lsu_iss_info))
    val deq = Vec(nm, new DecoupledIO(new Lsu_iss_info))
  })

  val chn = io.enq.bits.paddr(12+nm_w-1,12)

  for ( i <- 0 until nm ) yield {
    when( i.U === chn ) {
      io.deq(i) <> io.enq
    } .otherwise {
      io.deq(i).valid := false.B
      io.deq(i).bits  := DontCare
    }
  }
}



class OpMux extends Module {
  val io = IO(new Bundle{
    val enq = Flipped(new DecoupledIO(new Lsu_iss_info))
    val st_deq = DecoupledIO(new Lsu_iss_info)
    val ld_deq = DecoupledIO(new Lsu_iss_info)
    val am_deq = DecoupledIO(new Lsu_iss_info)
  })

  when( io.enq.bits.fun.is_lu ) {
    io.ld_deq <> io.enq
  } .otherwise{
    io.ld_deq.valid := false.B
    io.ld_deq.bits  := DontCare
  }

  when( io.enq.bits.fun.is_su ) {
    io.st_deq <> io.enq
  } .otherwise{
    io.st_deq.valid := false.B
    io.st_deq.bits  := DontCare
  }

  when( io.enq.bits.fun.is_amo ) {
    io.am_deq <> io.enq
  } .otherwise{
    io.am_deq.valid := false.B
    io.am_deq.bits  := DontCare
  }
}

object Strb2Mask{
  def apply(strb: UInt): UInt = {
    val mask = Wire(UInt(64.W))

    for ( i <- 0 unitl 8 ) yield {
      mask(8*i+7,8*i) := strb(i)
    } 
    mask
  } 
}

object pkg_Info_cache_s0s1{
  def apply( ori: Info_miss_rsp ) = {
    val res = Wire(new Info_cache_s0s1)

    res.paddr := ori.paddr
    res.wstrb := "hFF".U
    res.wdata(0) := ori.wdata(63,0)
    res.wdata(1) := ori.wdata(127,64)
    res.wdata(2) := ori.wdata(191,128)
    res.wdata(3) := ori.wdata(255,192)

    {
      res.op := 0.U.asTypeOf(new Cache_op)
      res.op.grant := true.B      
    }

    res.chk_idx := DontCare
    res
  }
  
  def apply( ori: Info_probe_req ) = {
    val res = Wire(new Info_cache_s0s1)
    res.paddr := ori.paddr
    res.wstrb := DontCare
    res.wdata := DontCare
    {
      res.op := 0.U.asTypeOf(new Cache_op)
      res.op.probe := true.B      
    }
    res.chk_idx := DontCare
    res
  }

  def apply( ori: Lsu_iss_info ) = {

    val res = Wire(new Info_cache_s0s1)

    val wdata = {
      val res = Wire(UInt(64.W))
      val paddr = ori.param.op1
      val shift = Wire(UInt(6.W))
      shift := Cat( paddr(2,0), 0.U(3.W) )
      res   := ori.param.op2 << shift  
      res
    }

    val wstrb = {
      val paddr = ori.param.op1
      val op = ori.fun

      Mux1H(Seq(
        op.is_byte -> "b00000001".U, op.is_half -> "b00000011".U,
        op.is_word -> "b00001111".U, op.is_dubl -> "b11111111".U
      )) << Cat(paddr(2,0), 0.U(3.W) )
    }

    res.paddr := ori.param.op1 & ~(("b111".U)(64.W))
    res.wdata := VecInit( Seq.fill(4)(wdata) )
    res.wstrb := wstrb


    {
      res.op := 0.U.asTypeOf(new Cache_op)
      res.op.fun := ori.fun     
    }
    res.chk_idx := DontCare
    res
  
  }


}


