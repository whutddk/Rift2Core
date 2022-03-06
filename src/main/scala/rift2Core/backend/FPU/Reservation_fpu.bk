
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

package rift2Core.backend.fpu

import chisel3._
import chisel3.util._
import rift2Core.define._
import rift2Core.backend._


/**
  * This mini Reservation is located between dpt and iss, replacing multi-fifo, mainly for 
  * @note assert dpt width is 2
  * @note re-sort order of fcsr, a monitor to alu is required
  * @note resolve RAW and read-operator in high efficiency
  */
class Reservation_fpu(gp_w: Int = 2, pp_w: Int = 2) extends Module {
  def gp = { var res = 1;  for ( i <- 0 until gp_w ) { res = res * 2 }; res }
  def pp = { var res = 1;  for ( i <- 0 until pp_w ) { res = res * 2 }; res }

  val io = IO(new Bundle{
    val enq = Vec( 2, Flipped(new DecoupledIO(new Fpu_dpt_info)))
    val deq = Vec( 2, (new DecoupledIO(new Fpu_dpt_info)))

    val is_op_rsl = Vec(pp, new DecoupledIO(new Reg_phy ) )
    val is_wb_ack = Input(Vec(3, Bool()))

  })

  val buff_rd = Wire( Vec( pp, new Fpu_dpt_info ) )
  val valid_rd = Wire( Vec( pp, Bool() ) )



  val buff = Reg(Vec( gp, Vec(pp, new Fpu_dpt_info) ))
  val valid =
    RegInit(
      VecInit(
        Seq.fill(gp)( VecInit(Seq.fill(pp)(false.B) )) 
    ))


  // val gp_valid = RegInit( VecInit( Seq.fill(gp)(false.B)) )
  val wr_ptr_reg   = RegInit( 0.U((gp_w+1).W) )
  val rd_ptr_reg   = RegInit( 0.U((gp_w+1).W) )  

  val wr_ptr      = wr_ptr_reg(gp_w-1,0)
  val rd_ptr      = rd_ptr_reg(gp_w-1,0)
  val wr_ptr_post = (wr_ptr_reg+1.U)(gp_w-1,0)




  val is_gp_full     = ( wr_ptr      === rd_ptr ) & (wr_ptr_reg(gp_w) =/= rd_ptr_reg(gp_w))
  val is_gp_full_pre = ( wr_ptr_post === rd_ptr )
  val is_gp_emty     = ( wr_ptr      === rd_ptr ) & (wr_ptr_reg(gp_w) === rd_ptr_reg(gp_w))




  val is_dpt_csr =
    io.enq.map( x => x.bits.isa.is_fun_floatCsr )

  
  io.enq(0).ready := ~is_gp_full
  io.enq(1).ready := ~is_gp_full & ~is_gp_full_pre

  val is_post_push = 
    (valid(wr_ptr).count((x:Bool) => (x === false.B)) === 1.U) |
    io.enq(0).bits.isa.is_fun_floatCsr

  val gp_switch_cnt = Wire( UInt(2.W) )

  assert( valid(wr_ptr).contains( false.B ) | is_gp_full )
  
    when( io.enq(0).fire & io.enq(1).fire ) {
      gp_switch_cnt := Mux1H( Seq(
        ( io.enq(1).bits.isa.is_fun_floatCsr &  io.enq(0).bits.isa.is_fun_floatCsr) -> 2.U,
        ( io.enq(1).bits.isa.is_fun_floatCsr & ~io.enq(0).bits.isa.is_fun_floatCsr) -> 
          Mux( (valid(wr_ptr).count((x:Bool) => (x === false.B)) === 1.U), 2.U, 1.U ),
        (~io.enq(1).bits.isa.is_fun_floatCsr &  io.enq(0).bits.isa.is_fun_floatCsr) -> 1.U,
        (~io.enq(1).bits.isa.is_fun_floatCsr & ~io.enq(0).bits.isa.is_fun_floatCsr) -> 
          Mux(
            (valid(wr_ptr).count((x:Bool) => (x === false.B)) === 1.U), 1.U,
            Mux(
              (valid(wr_ptr).count((x:Bool) => (x === false.B)) === 2.U) & io.enq(1).fire(),
              1.U, 0.U
            )
          )
      ))      
    } .elsewhen ( io.enq(0).fire ) {
      gp_switch_cnt := 
        Mux(
          io.enq(0).bits.isa.is_fun_floatCsr | valid(wr_ptr).count((x:Bool) => (x === false.B)) === 1.U,
          1.U, 0.U
        )
    } .otherwise {
      gp_switch_cnt := 0.U
    }

  

  when( io.enq(0).fire ) {
    val wr_ptr_chn0 = wr_ptr
    val buff_sel = valid(wr_ptr_chn0).indexWhere( (x:Bool) => (x === false.B) )

    buff (wr_ptr_chn0)(buff_sel) := io.enq(0).bits
    valid(wr_ptr_chn0)(buff_sel) := true.B

    wr_ptr_reg := wr_ptr_reg + gp_switch_cnt

    //never overflow
    when( gp_switch_cnt === 1.U ) {
      assert( ~((wr_ptr_reg(gp_w) =/= rd_ptr_reg(gp_w)) & (wr_ptr_reg (gp_w-1,0) === rd_ptr_reg(gp_w-1,0))) )      
    } .elsewhen( gp_switch_cnt === 2.U ) {
      assert( ~(((wr_ptr_reg + 1.U)(gp_w) =/= rd_ptr_reg(gp_w)) & ((wr_ptr_reg + 1.U)(gp_w-1,0) === rd_ptr_reg(gp_w-1,0))) )
    }
  }

  when( io.enq(1).fire ) {
    val wr_ptr_chn1 = Mux( is_post_push, wr_ptr_post ,wr_ptr )
    val buff_sel = valid(wr_ptr_chn1).lastIndexWhere( (x:Bool) => (x === false.B) )


    buff (wr_ptr_chn1)(buff_sel) := io.enq(1).bits
    valid(wr_ptr_chn1)(buff_sel) := true.B

    assert( io.enq(0).fire )


  }























  buff_rd  := buff(rd_ptr)
  valid_rd := valid(rd_ptr)



  val is_buf_csr =
    (valid_rd zip buff_rd).map{
      case(x, y) => {
        (x === true.B) & y.isa.is_fun_floatCsr
      }
    }
  assert(PopCount(is_buf_csr) <= 1.U)



  val is_op_rdy = io.is_op_rsl.map(x => x.fire)

  val is_ncsr_valid = VecInit((is_op_rdy zip is_buf_csr).map{case (x,y) => (x & ~y)})
  val is_csr_valid  = VecInit((is_op_rdy zip is_buf_csr).map{case (x,y) => (x &  y)})
  val is_csr_clr = Wire(Bool())

  val deq_sel_chn0 =
    Mux(
      is_csr_clr,
      is_csr_valid.indexWhere ( (x:Bool) => (x === true.B) ),
      is_ncsr_valid.indexWhere( (x:Bool) => (x === true.B) ),
    )
  val deq_sel_chn1 = is_ncsr_valid.lastIndexWhere( (x:Bool) => (x === true.B) )




  val is_rd_ptr_inc =
    ~is_gp_emty & (
      (io.deq(1).fire & (valid_rd.count((x:Bool) => (x === true.B)) === 2.U)) |
      (io.deq(0).fire & (valid_rd.count((x:Bool) => (x === true.B)) === 1.U))
    )


  io.deq(0).valid :=
    Mux(
      is_csr_clr,
      is_csr_valid.reduce(_|_),
      (is_ncsr_valid.count( (x:Bool) => (x === true.B) ) >= 1.U)
    )

  io.deq(1).valid :=
    ~is_csr_clr &
    is_ncsr_valid.count( (x:Bool) => (x === true.B) ) >= 2.U 

  io.deq(0).bits := buff_rd(deq_sel_chn0)
  io.deq(1).bits := buff_rd(deq_sel_chn1)

  when( io.deq(0).fire ) {
    valid(rd_ptr)(deq_sel_chn0) := false.B

    rd_ptr_reg := rd_ptr_reg + Mux(is_rd_ptr_inc, 1.U, 0.U)
  }

  when( io.deq(1).fire ) {
    valid(rd_ptr)(deq_sel_chn1) := false.B

    assert( io.deq(0).fire )
  }








  val reserve_cnt = RegInit(0.U( (gp_w + pp_w).W ))
  val reserve_inc = Mux( io.deq(1).fire, 2.U, Mux(io.deq(0).fire, 1.U, 0.U) )
  val reserve_dec = io.is_wb_ack.count( (x: Bool) => (x === true.B) )
  
  reserve_cnt := reserve_cnt + reserve_inc - reserve_dec
  is_csr_clr := (reserve_cnt === 0.U) & ~is_ncsr_valid.reduce(_|_) & is_csr_valid.reduce(_|_)


  
  for ( i <- 0 until pp ) yield {
    io.is_op_rsl(i).valid := valid_rd(i)
    io.is_op_rsl(i).bits := buff_rd(i).phy
  }



}


