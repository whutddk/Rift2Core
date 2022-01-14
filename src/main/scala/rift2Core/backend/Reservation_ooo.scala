
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
  */
class Reservation_ooo(pp_w: Int = 4) extends Module {
  def pp = { var res = 1;  for ( i <- 0 until pp_w ) { res = res * 2 }; res }

  val io = IO(new Bundle{
    val enq = Vec( 2, Flipped(new DecoupledIO(new Fpu_dpt_info)))
    val deq = Vec( 2, (new DecoupledIO(new Fpu_dpt_info)))

    val ooo_isOpRsl = Vec(4, Flipped(new DecoupledIO( new Register_source(dp) )))

  })

  val buff_rd = Wire( Vec( pp, new Fpu_dpt_info ) )
  val valid_rd = Wire( Vec( pp, Bool() ) )



  val buff = Reg(Vec(pp, new Fpu_dpt_info) ))
  val valid =
    RegInit(
      VecInit(
        Seq.fill(gp)( VecInit(Seq.fill(pp)(false.B) )) 
    ))



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


