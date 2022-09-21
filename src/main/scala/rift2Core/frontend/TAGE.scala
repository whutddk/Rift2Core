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
import rift2Core.define._
import base._

case class TageParams(
  //                                       nSets, histLen
  tableInfo: Seq[Tuple2[Int, Int]] = Seq((  128,       2  ),
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
    val resp = Output( new TageTableResp_Bundle)
    val update = Flipped(Valid(new TageTableUpdate_Bundle))

    val isReady = Output(Bool())
    val flush = Input(Bool())
  })

  /** tage_table needs poweron reset to initialize the ram */
  val por_reset = RegInit(true.B)
  val (reset_cl, reset_end) = Counter( Range(0, nRows), por_reset )
  when( reset_end ) { por_reset := false.B }
  io.isReady := ~por_reset

  /** tage_table needs clear to reduce usage */
  val (clear_u_cnt, _) = Counter( Range(0, nRows << 1 << 10), true.B )
  val is_clear_hi =  clear_u_cnt(cl_w+11 -1) & clear_u_cnt(9, 0) === 0.U
  val is_clear_lo = ~clear_u_cnt(cl_w+11 -1) & clear_u_cnt(9, 0) === 0.U
  val clear_cl    = clear_u_cnt( 10+cl_w, 10)



  val tage_uhi = SyncReadMem(nRows, UInt(1.W))
  val tage_ulo = SyncReadMem(nRows, UInt(1.W))
  // val tage_tag = SyncReadMem(nRows, UInt(tage_tag_w.W))
  val tage_ctl = SyncReadMem(nRows, UInt(3.W))

  val rd_cl  = HashTo0( in = HashTwo0( in1 = io.req.pc, in2 = io.req.ghist & Fill(histlen, 1.U) ), len = cl_w )
  val wr_cl  = HashTo0( in = HashTwo0( in1 = io.update.bits.pc, in2 = io.update.bits.ghist & Fill(histlen, 1.U) ), len = cl_w )

  // val req_tag = HashTo1( in = HashTwo1( in1 = io.req.pc, in2 = io.req.ghist & Fill(histlen, 1.U) ), len = tage_tag_w )

  io.resp.ctl    := tage_ctl.read(rd_cl)
  io.resp.use    := Cat( tage_uhi.read(rd_cl), tage_ulo.read(rd_cl) )
  io.resp.is_hit := RegNext(true.B) //tage_tag.read(req_cl) === req_tag

  when( por_reset | io.update.fire ) {
    tage_ctl.write(
      Mux(por_reset, reset_cl, wr_cl),
      Mux(por_reset, 0.U, io.update.bits.ctl)
    )
  }

  when( por_reset | io.update.fire | is_clear_hi) {
    tage_uhi.write(
      Mux(por_reset, reset_cl, Mux( io.update.fire, wr_cl, clear_cl)),
      Mux(por_reset, 0.U,      Mux( io.update.fire, io.update.bits.use(1), 0.U))
    )
  }

  when( por_reset | io.update.fire | is_clear_lo) {
    tage_ulo.write(
      Mux(por_reset, reset_cl, Mux( io.update.fire, wr_cl, clear_cl)),
      Mux(por_reset, 0.U,      Mux( io.update.fire, io.update.bits.use(0), 0.U))
    )
  }  
}



class TAGE(param: TageParams = TageParams())(implicit p: Parameters) extends IFetchModule {
  val io = IO(new Bundle{
    val req = Flipped(Decoupled(new TageReq_Bundle))
    val resp = Decoupled( Vec(6, new TageTableResp_Bundle ) )
    val update = Flipped(Valid(new TageUpdate_Bundle))

    val isReady = Output(Bool())
    val flush = Input(Bool())
  })

  if (false) {
    val tageTable = param.tableInfo.map{
      case ( nRows, len ) => {
        val mdl = Module(new TageTable(nRows = nRows, histlen = len))
        mdl.io.req  := io.req.bits
        mdl.io.flush := io.flush
        
        mdl
      }
    }
  
    for ( i <- 0 until 6 ) yield { io.resp.bits(i) := tageTable(i).io.resp; printf("Warning, Bugs no de!")}
    io.resp.valid := RegNext(io.req.fire)
    io.req.ready  := io.resp.ready

    io.isReady := tageTable.map{_.io.isReady}.reduce(_|_)






    for ( i <- 0 until 6 ) yield {
      tageTable(i).io.update.valid      := false.B
      tageTable(i).io.update.bits.ctl   := 0.U
      tageTable(i).io.update.bits.pc    := 0.U
      tageTable(i).io.update.bits.ghist := 0.U
      tageTable(i).io.update.bits.use   := 0.U
    } // override
    when( io.update.valid ) {
      when( io.update.bits.isAlloc.reduce(_&_) ) {
        tageTable(0).io.update.valid      := true.B
        tageTable(0).io.update.bits.ctl   := Mux( io.update.bits.isFinalTaken, 4.U, 3.U)
        tageTable(0).io.update.bits.pc    := io.update.bits.pc
        tageTable(0).io.update.bits.ghist := io.update.bits.ghist
        tageTable(0).io.update.bits.use   := 1.U
        assert( io.update.bits.ftqTage(0).use === 0.U )
      } .otherwise{
        when( io.update.bits.isMisPredict ) {
          for ( i <- 0 until 6 ) yield {
            when( io.update.bits.isProvider(i) === i.U ) {
              tageTable(i).io.update.valid      := true.B
              tageTable(i).io.update.bits.ctl   := inc_ctl( in = io.update.bits.ftqTage(i).ctl, isTaken = ~io.update.bits.isPredictTaken )
              tageTable(i).io.update.bits.pc    := io.update.bits.pc
              tageTable(i).io.update.bits.ghist := io.update.bits.ghist
              tageTable(i).io.update.bits.use   := inc_use( in = io.update.bits.ftqTage(i).use, isInc = false.B )
            }

            when( io.update.bits.isDisAgree(i) ) {
              tageTable(i).io.update.valid      := true.B
              tageTable(i).io.update.bits.ctl   := io.update.bits.ftqTage(i).ctl
              tageTable(i).io.update.bits.pc    := io.update.bits.pc
              tageTable(i).io.update.bits.ghist := io.update.bits.ghist
              tageTable(i).io.update.bits.use   := inc_use( in = io.update.bits.ftqTage(i).use, isInc = true.B )
              assert( io.update.bits.ftqTage(i).use =/= 0.U )
            }

            when( io.update.bits.isAlloc(i) & (i.U === (io.update.bits.providerSel + 1.U)) & ( i.U =/= 0.U ) ) {
              tageTable(i).io.update.valid      := true.B
              tageTable(i).io.update.bits.ctl   := Mux( io.update.bits.isPredictTaken, 3.U, 4.U) //revert, because mis-predict here 
              tageTable(i).io.update.bits.pc    := io.update.bits.pc
              tageTable(i).io.update.bits.ghist := io.update.bits.ghist
              tageTable(i).io.update.bits.use   := 1.U

              assert( io.update.bits.ftqTage(i).use === 0.U )
            }
          }
        } .otherwise {
          for ( i <- 0 until 6 ) yield {
            when( io.update.bits.isProvider(i) === i.U ) {
              tageTable(i).io.update.valid      := true.B
              tageTable(i).io.update.bits.ctl   := inc_ctl( in = io.update.bits.ftqTage(i).ctl, isTaken = io.update.bits.isPredictTaken )
              tageTable(i).io.update.bits.use   := inc_use( in = io.update.bits.ftqTage(i).use, isInc = true.B )
              tageTable(i).io.update.bits.pc    := io.update.bits.pc
              tageTable(i).io.update.bits.ghist := io.update.bits.ghist
            }

            when( io.update.bits.isAgree(i) ) {
              tageTable(i).io.update.valid      := true.B
              tageTable(i).io.update.bits.ctl   := io.update.bits.ftqTage(i).ctl
              tageTable(i).io.update.bits.pc    := io.update.bits.pc
              tageTable(i).io.update.bits.ghist := io.update.bits.ghist
              tageTable(i).io.update.bits.use   := inc_use( in = io.update.bits.ftqTage(i).use, isInc = true.B )
            }
          }
        }
      }     
    }
  } else {
    io.isReady := true.B
    io.req.ready := true.B
    io.resp.valid := false.B
    io.resp.bits := 0.U.asTypeOf(Vec(6, new TageTableResp_Bundle ))
  }




  def inc_ctl( in: UInt, isTaken: Bool ): UInt = {
    val newCtl = Wire(UInt(3.W))

    newCtl := 
      Mux( isTaken,
        Mux( in === 7.U, 7.U, in + 1.U ),
        Mux( in === 0.U, 0.U, in - 1.U ),
      )
    return newCtl
  }

  def inc_use( in: UInt, isInc: Bool ): UInt = {
    val newUse = Wire(UInt(2.W))
    newUse := 
      Mux( isInc,
        Mux( in === 3.U, 3.U, in + 1.U ),
        Mux( in === 0.U, 0.U, in - 1.U ),
      )
    return newUse
  }


}



object Tage_Decode{
  def apply( in: Vec[TageTableResp_Bundle] )(implicit p: Parameters): TageResp_Bundle = {
    require( in.length == 6 )
    val resp = Wire(new TageResp_Bundle)
    val isTableSel = in.map{ x => (x.use =/= 0.U & x.is_hit) }

    // set is_provider by override
    for( i <- 0 until 6 ) yield {
      when( isTableSel(i) ) {
        resp.isProvider(i) := true.B
        for( j <- 0 until i ) yield {
          resp.isProvider(j) := false.B
        }
      } .otherwise { resp.isProvider(i) := false.B }
    }

    for( i <- 0 until 6 ) yield {
      resp.isAltpred(i) := isTableSel(i) & ~resp.isProvider(i)
    }

    resp.isPredictTaken := PriorityMux(isTableSel.reverse, in.map{ _.isTaken }.reverse )
    for( i <- 0 until 6 ) yield { resp.ftqTage(i) := in(i) }


    for ( i <- 0 until 6 ) yield {
      assert( resp.isAltpred(i) === (resp.isProvider(i) ^ isTableSel(i)) )
    }
    assert( PopCount( resp.isProvider ) <= 1.U )

    return resp
  }
}

