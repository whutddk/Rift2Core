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
import chipsalliance.rocketchip.config.Parameters

case class TageParams(
  //                                       nSets, histLen
  tableInfo: Seq[Tuple3[Int, Int]] = Seq((  128,       2  ),
                                         (  128,       4  ),
                                         (  256,       8  ),
                                         (  256,      16  ),
                                         (  128,      32  ),
                                         (  128,      64  ))
)



class TageTable(nRows: Int, histlen: Int)(implicit p: Parameters) extends IFetchModule {
  require( isPow2(nRows) )
  require( isPow2(histlen) )
  val cl_w = log2Ceil( nRows )

  val io = IO( new Bundle{
    val req = Input(new TageTableReq_Bundle)
    val combResp = Output( new TageTableResp_Bundle)
    val update = Flipped(Valid(new TageTableUpdate_Bundle))

    val isReady = Output(Bool())
    val flush = Input(Bool())
  })

  /** tage_table needs poweron reset to initialize the ram */
  val por_reset = RegInit(true.B)
  val (reset_cl, reset_end) = Counter( range(0, nRows), por_reset )
  when( reset_end ) { por_reset := false.B }
  io.isReady := ~por_reset

  /** tage_table needs clear to reduce usage */
  val (clear_u_cnt, _) = Counter( range(0, nRows << 1 << 10), true.B )
  val is_clear_hi =  clear_u_cnt(cl_w+11 -1) & clear_u_cnt(9, 0) === 0.U
  val is_clear_lo = ~clear_u_cnt(cl_w+11 -1) & clear_u_cnt(9, 0) === 0.U
  val clear_cl    = clear_u_cnt( 10+cl_w, 10)



  val tage_uhi = Mem(nRows, UInt(1.W))
  val tage_ulo = Mem(nRows, UInt(1.W))
  val tage_tag = Mem(nRows, UInt(tage_tag_w.W))
  val tage_ctl = Mem(nRows, UInt(3.W))

  val rd_cl  = HashTo0( in = HashTwo0( in1 = io.req.pc, in2 = io.req.ghist & Fill(histlen, 1.U) ), len = cl_w )
  val wr_cl  = HashTo0( in = HashTwo0( in1 = io.update_table.bits.pc, in2 = io.update_table.bits.ghist & Fill(histlen, 1.U) ), len = cl_w )

  // val req_tag = HashTo1( in = HashTwo1( in1 = io.req.pc, in2 = io.req.ghist & Fill(histlen, 1.U) ), len = tage_tag_w )

  io.combResp.ctr    := tage_ctl.read(rd_cl)
  io.combResp.use    := Cat( tage_uhi.read(rd_cl), tage_ulo.read(rd_cl) )
  io.combResp.is_hit := true.B //tage_tag.read(req_cl) === req_tag

  when( por_reset ) {
    tage_uhi.write( reset_cl, 0.U )
    tage_ulo.write( reset_cl, 0.U )
    // tage_tag.write( reset_cl, 0.U )
    tage_ctl.write( reset_cl, 0.U )
  } .elsewhen( io.update.valid ) {
    
    // val update_tag = HashTo1( in = HashTwo1( in1 = io.update_table.bits.pc, in2 = io.update_table.bits.ghist & Fill(histlen, 1.U) ), len = tage_tag_w )
    tage_uhi.write( wr_cl, io.update.bits.use(1) )
    tage_ulo.write( wr_cl, io.update.bits.use(0) )
    // tage_tag.write( wr_cl, update_tag )
    tage_ctl.write( wr_cl, io.update.bits.ctl )    
  }
  //tage-table may giveup clear this usage if collision with write
  .elsewhen( is_clear_hi ) {
    tage_uhi.write( clear_cl, 0.U )
  } .elsewhen( is_clear_lo ) {
    tage_ulo.write( clear_cl, 0.U )
  }

  
}



class TAGE(param: TageParams = TageParams())(implicit p: Parameters) extends IFetchModule {
  val io = IO(new Bundle{
    val req = Input(new TageReq_Bundle)
    val combResp = Output( Vec(6, new TageTableResp_Bundle ) )
    val update = Flipped(Valid(new TageUpdate_Bundle))

    val isReady = Output(Bool())
    val flush = Input(Bool())
  })

  val tageTable = param.tableInfo.map{
    case ( nRows, len ) => {
      val mdl = Module(new TageTable(nRows = nRows, histlen = len))
      mdl.io.req  := io.req
      io.combResp := mdl.io.combResp
      mdl.io.flush := io.flush
      
      mdl
    }
  }

  io.isReady := tageTable.map{_.io.is_Ready}.reduce(_|_)






  for ( i <- 0 unitl 6 ) yield {
    tageTable(i).io.update.valid      := false.B
    tageTable(i).io.update.bits.ctl   := 0.U
    tageTable(i).io.update.bits.pc    := 0.U
    tageTable(i).io.update.bits.ghist := 0.U
    tageTable(i).io.update.bits.use   := 0.U
  } // override
  when( io.update.valid ) {
    when( io.update.is_alloc.reduce(_&_) ) {
      tageTable(0).io.update.valid      := true.B
      tageTable(0).io.update.bits.ctl   := Mux( is_finalTaken, 4.U, 3.U)
      tageTable(0).io.update.bits.pc    := io.update.bits.pc
      tageTable(0).io.update.bits.ghist := io.update.bits.ghist
      tageTable(0).io.update.bits.use   := 1.U
      assert( io.update.bits.ftq_tage(i).use === 0.U )
    } .otherwise{
      when( io.update.is_misPredict ) {
        for ( i <- 0 until 6 ) yield {
          when( io.update.bits.is_provider(i) === i.U ) {
            tageTable(i).io.update.valid      := true.B
            tageTable(i).io.update.bits.ctl   := inc_ctl( in = io.update.bits.ftq_tage(i).ctl, is_taken = ~io.update.bits.is_predictTaken )
            tageTable(i).io.update.bits.pc    := io.update.bits.pc
            tageTable(i).io.update.bits.ghist := io.update.bits.ghist
            tageTable(i).io.update.bits.use   := inc_use( in = io.update.bits.ftq_tage(i).use, is_inc = false.B )
          }

          when( io.update.bits.is_disAgree(i) ) {
            tageTable(i).io.update.valid      := true.B
            tageTable(i).io.update.bits.ctl   := io.update.bits.ftq_tage(i).ctl
            tageTable(i).io.update.bits.pc    := io.update.bits.pc
            tageTable(i).io.update.bits.ghist := io.update.bits.ghist
            tageTable(i).io.update.bits.use   := inc_use( in = io.update.bits.ftq_tage(i).use, is_inc = true.B )
            assert( io.update.bits.ftq_tage(i).use =/= 0.U )
          }

          when( io.update.is_alloc(i) & (i.U === (provider_sel + 1.U)) & ( i.U =/= 0.U ) ) {
            tageTable(i).io.update.valid      := true.B
            tageTable(i).io.update.bits.ctl   := Mux( io.update.bits.is_predictTaken, 3.U, 4.U) //revert, because mis-predict here 
            tageTable(i).io.update.bits.pc    := io.update.bits.pc
            tageTable(i).io.update.bits.ghist := io.update.bits.ghist
            tageTable(i).io.update.bits.use   := 1.U

            assert( io.update.bits.ftq_tage(i).use === 0.U )
          }
        }
      } .otherwise {
        for ( i <- 0 until 6 ) yield {
          when( io.update.bits.is_provider(i) === i.U ) {
            tageTable(i).io.update.valid      := true.B
            tageTable(i).io.update.bits.ctl   := inc_ctl( in = io.update.bits.ftq_tage(i).ctl, is_taken = io.update.bits.is_predictTaken )
            tageTable(i).io.update.bits.use   := inc_use( in = io.update.bits.ftq_tage(i).use, is_inc = true.B )
            tageTable(i).io.update.bits.pc    := io.update.bits.pc
            tageTable(i).io.update.bits.ghist := io.update.bits.ghist
          }

          when( io.update.bits.is_agree(i) ) {
            tageTable(i).io.update.valid      := true.B
            tageTable(i).io.update.bits.ctl   := io.update.bits.ftq_tage(i).ctl
            tageTable(i).io.update.bits.pc    := io.update.bits.pc
            tageTable(i).io.update.bits.ghist := io.update.bits.ghist
            tageTable(i).io.update.bits.use   := inc_use( in = io.update.bits.ftq_tage(i).use, is_inc = true.B )
          }
        }
      }
    }

  }

  def inc_ctl( in: UInt, is_taken: Bool ): UInt = {
    val new_ctl = Wire(UInt(3.W))

    new_ctl := 
      Mux( is_taken,
        Mux( in === 7.U, 7.U, in + 1.U ),
        Mux( in === 0.U, 0.U, in - 1.U ),
      )
    return new_ctl
  }

  def inc_use( in: UInt, is_inc: Bool ): UInt = {
    val new_use = Wire(UInt(2.W))
    new_use := 
      Mux( is_inc,
        Mux( in === 3.U, 3.U, in + 1.U ),
        Mux( in === 0.U, 0.U, in - 1.U ),
      )
    return new_use
  }


  for ( i <- 0 until 6 ) yield {
    assert( io.resp.bits.is_altpred(i) === (io.resp.bits.is_provider(i) ^ is_table_sel(i)) )
  }
  assert( PopCount( io.resp.bits.is_provider ) <= 1.U )
}

object Tage_Decode{
  def apply( in: Vec[TageTableResp_Bundle] ): TageResp_Bundle = {
    require( in.length == 6 )
    val resp = Wire(new TageResp_Bundle)
    val is_table_sel = in.map{ _.usage =/= 0.U & _.is_hit }

    // set is_provider by override
    for( i <- 0 until 6 ) yield {
      when( is_table_sel(i) ) {
        resp.is_provider(i) := true.B
        for( j <- 0 until i ) yield {
          resp.is_provider(j) := false.B
        }
      } .otherwise { resp.is_provider(i) := false.B }
    }

    for( i <- 0 until 6 ) yield {
      resp.is_altpred(i) := is_table_sel(i) & ~resp.is_provider(i)
    }

    resp.is_predictTaken := PriorityMux(is_table_sel.reverse, in.map{ _.is_taken }.reverse )
    for( i <- 0 until 6 ) yield { resp.ftq_tage(i) := in }
  }
}

