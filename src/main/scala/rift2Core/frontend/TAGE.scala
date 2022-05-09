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



class TageTableUpdate_Bundle extends IFetchBundle {
  val use   = UInt(2.W)
  val ctl   = UInt(3.W)
  val pc    = UInt(64.W)
  val ghist = UInt(64.W)
}



class TageTableReq_Bundle extends IFetchBundle {
  val pc = UInt(64.W)
  val ghist = UInt(64.W)
}

class TageReq_Bundle extends TageTableReq_Bundle


class TageTableResp_Bundle extends IFetchBundle {
  val pc = UInt(64.W)
  val ghist = UInt(64.W)
  val ctr = UInt(3.W)
  val use = UInt(2.W)
  val is_hit = Bool()

  def is_taken = (ctr(3) === 1.U).asBool
}

class TageResp_Bundle extends IFetchBundle {
  val ftq_tage = Vec( 6, new TageTableResp_Bundle )

}

class TageUpdate_Bundle extends TageResp_Bundle {
  val is_misPredict = Bool()
}

class TageTable(nRows: Int, histlen: Int) extends IFetchModule {
  require( isPow2(nRows) )
  require( isPow2(histlen) )
  val cl_w = log2Ceil( nRows )

  val io = IO( new Bundle{
    val req = Flipped(Valid(new TageTableReq_Bundle))
    val resp = Valid( new TageTableResp_Bundle)

    val update_table = Flipped(Valid(new TageTableUpdate_Bundle))

    val is_Ready = Output(Bool())
    val flush = Input(Bool())
  })

  /** tage_table needs poweron reset to initialize the ram */
  val por_reset = RegInit(true.B)
  val (reset_cl, reset_end) = Counter( range(0, nRows), por_reset )
  when( reset_end ) { por_reset := false.B }
  io.is_Ready := ~por_reset

  /** tage_table needs clear to reduce usage */
  val (clear_u_cnt, _) = Counter( range(0, nRows << 1 << 10), true.B )
  val is_clear_hi =  clear_u_cnt(cl_w+11 -1) & clear_u_cnt(9, 0) === 0.U
  val is_clear_lo = ~clear_u_cnt(cl_w+11 -1) & clear_u_cnt(9, 0) === 0.U
  val clear_cl    = clear_u_cnt( 10+cl_w, 10)



  val tage_uhi = Mem(nRows, UInt(1.W))
  val tage_ulo = Mem(nRows, UInt(1.W))
  val tage_tag = Mem(nRows, UInt(tage_tag_w.W))
  val tage_ctl = Mem(nRows, UInt(3.W))

  val req_cl  = HashTo0( in = HashTwo0( in1 = io.req.bits.pc, in2 = io.req.bits.ghist & Fill(histlen, 1.U) ), len = cl_w )
  val req_tag = HashTo1( in = HashTwo1( in1 = io.req.bits.pc, in2 = io.req.bits.ghist & Fill(histlen, 1.U) ), len = tage_tag_w )
  io.resp.valid      = RegNext(io.req.valid, false.B) & ~io.flush
  io.resp.bits.pc    = RegNext(io.req.bits.pc)
  io.resp.bits.ghist = RegNext(io.req.bits.ghist)

  io.resp.bits.ctr    := Mux(io.resp.valid, tage_ctl.read(req_cl), 0.U)
  io.resp.bits.use    := Mux(io.resp.valid, Cat( tage_uhi.read(req_cl), tage_ulo.read(req_cl) ), 0.U)
  io.resp.bits.is_hit := Mux(io.resp.valid, tage_tag.read(req_cl) === RegNext(req_tag), false.B)

  when( por_reset ) {
    tage_uhi.write( reset_cl, 0.U )
    tage_ulo.write( reset_cl, 0.U )
    tage_tag.write( reset_cl, 0.U )
    tage_ctl.write( reset_cl, 0.U )
  } .elsewhen( io.update_table.valid ) {
    val update_cl  = HashTo0( in = HashTwo0( in1 = io.update_table.bits.pc, in2 = io.update_table.bits.ghist & Fill(histlen, 1.U) ), len = cl_w )
    val update_tag = HashTo1( in = HashTwo1( in1 = io.update_table.bits.pc, in2 = io.update_table.bits.ghist & Fill(histlen, 1.U) ), len = tage_tag_w )
    tage_uhi.write( update_cl, io.update_table.bits.use(1) )
    tage_ulo.write( update_cl, io.update_table.bits.use(0) )
    tage_tag.write( update_cl, update_tag )
    tage_ctl.write( update_cl, io.update_table.bits.ctl )    
  }
  //tage-table may giveup clear this usage if collision with write
  .elsewhen( is_clear_hi ) {
    tage_uhi.write( clear_cl, 0.U )
  } .elsewhen( is_clear_lo ) {
    tage_ulo.write( clear_cl, 0.U )
  }

  when( por_reset ) { assert( ~io.req.valid ); assert( ~io.update_table.valid ) }
  
}

case class TageParams(
  //                                       nSets, histLen
  tableInfo: Seq[Tuple3[Int, Int]] = Seq((  128,       2  ),
                                         (  128,       4  ),
                                         (  256,       8  ),
                                         (  256,      16  ),
                                         (  128,      32  ),
                                         (  128,      64  ))
)

class Tage(param: TageParams = TageParams()) extends IFetchModule {
  val io = IO(new Bundle{
    val req = Flipped(Decouple(new TageReq_Bundle))
    val resp = Valid( new TageResp_Bundle )

    val update_table = Flipped(Decouple(new TageUpdate_Bundle))
    val flush = Input(Bool())
  })

  val tageTable = param.tableInfo.map{
    case ( nRows, len ) => {
      val mdl = Module(new TageTable(nRows = nRows, histlen = len))
      mdl.io.req.valid := io.req.valid
      mdl.io.req.bits  := io.req.bits
      mdl.io.flush := io.flush

      mdl
    }
  }

  io.req.ready          := tageTable.map{_.io.is_Ready}.reduce(_|_)
  io.update_table.ready := tageTable.map{_.io.is_Ready}.reduce(_|_)

  val is_table_sel = tageTable.map{ _.io.resp.valid & _.io.resp.bits.usage =/= 0.U & (_.io.resp.bits.ctr =/= 3.U | _.io.resp.bits.ctr =/= 4.U) & _.io.resp.bits.is_hit }
  io.resp.valid := is_table_sel.reduce(_|_)
  io.resp.bits.is_taken := PriorityMux(is_table_sel.reverse, tageTable.map{ _.io.resp.bits.is_taken }.reverse )
  for( i <- 0 until 6 ) yield { io.resp.bits.ftq_tage(i) := tageTable(i).io.resp.bits }


  when( io.update_table.valid ) {
    when( io.update_table.is_misPredict ) {

    } .otherwise { //

    }
  }


}

