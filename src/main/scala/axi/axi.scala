/*
* @Author: Ruige Lee
* @Date:   2021-04-27 10:08:43
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-04-27 10:27:34
*/


package axi


import chisel3._
import chisel3.util._



class AXI_chn_a( aw: Int, idw: Int = 1, usw: Int = 1 ) extends Bundle {
	val id    = UInt(idw.W)
	val addr  = UInt(aw.W)
	val len   = UInt(8.W)
	val size  = UInt(3.W)
	val burst = UInt(2.W)
	val lock  = Bool()
	val cache = UInt(4.W)
	val port  = UInt(3.W)
	val qos   = UInt(4.W)
	val user  = UInt(usw.W)

	override def cloneType = ( new AXI_chn_a(aw, idw, usw) ).asInstanceOf[this.type]
}

class AXI_chn_w( dw: Int, usw: Int = 1 ) extends Bundle {
	val data = UInt(dw.W)
	val strb = UInt( (dw/8).W )
	val last = Bool()
	val user = UInt(usw.W)

	override def cloneType = ( new AXI_chn_w(dw, usw) ).asInstanceOf[this.type]
}


class AXI_chn_b( idw: Int = 1, usw: Int = 1 ) extends Bundle {
	val id   = UInt( idw.W )
	val rsp  = UInt( 2.W )
	val user = UInt( usw.W )

	override def cloneType = ( new AXI_chn_b(idw, usw) ).asInstanceOf[this.type]
}

class AXI_chn_r( dw: Int, idw: Int = 1, usw: Int = 1 ) extends Bundle {
	val id    = UInt(idw.W)
	val data = UInt(dw.W)
	val rsp  = UInt(2.W)
	val last = Bool()
	val user = UInt(usw.W)

	override def cloneType = ( new AXI_chn_r(dw, idw, usw) ).asInstanceOf[this.type]
}

class AXI_mst_r(addrw: Int, dw: Int, idw: Int = 1, usw: Int = 1, len: Int ) extends Module{
	val io = IO( new Bundle {
		// val ar_info = Input(new AXI_chn_a( addrw, idw, usw ))
		val ar_req  = Input(Bool())
		val end     = Output(Bool())
		val ar_info = Input( new AXI_chn_a( addrw, idw, usw ) )

		val ar = new DecoupledIO(new AXI_chn_a( addrw, idw, usw ))
		val r  = Flipped( new DecoupledIO(new AXI_chn_r( dw, idw, usw)) )
	})

	val ar_ack   = io.ar.valid & io.ar.ready
	val r_ack    = io.r.valid  & io.r.ready

	val arvalid  = RegInit(false.B)
	val rready   = RegInit(false.B)
	val read_idx = RegInit(0.U(8.W))

	when ( ~io.ar.valid & io.ar_req ) { arvalid := true.B }
	.elsewhen( ar_ack ) { arvalid := false.B }

	when ( io.r.valid & ( ~io.r.bits.last | ~io.r.ready ) ) { rready := true.B }
	.elsewhen( r_ack & io.r.bits.last ) { rready := false.B }

	when( io.ar_req ) { read_idx := 0.U }
	.elsewhen( r_ack & (read_idx =/= (len.U - 1.U)) ) { read_idx := read_idx + 1.U }

	io.end := r_ack & io.r.bits.last
	io.ar.valid := arvalid
	io.r.ready  := rready

	
	io.ar.bits := io.ar_info



}

class AXI_slv_r(addrw: Int, dw: Int, idw: Int = 1, usw: Int = 1) extends Module {
	val io = IO( new Bundle {
		// val r_info = Input(new AXI_chn_r( dw, idw, usw))
		val is_busy = Output(Bool())
		val r_info = Input ( new AXI_chn_r( dw, idw, usw) )

		val ar = Flipped( new DecoupledIO(new AXI_chn_a( addrw, idw, usw )))
		val r  = new DecoupledIO(new AXI_chn_r( dw, idw, usw))
	})

	def addr_lsb = dw/8

	val ar_ack = io.ar.valid & io.ar.ready
	val r_ack  = io.r.valid  & io.r.ready

	val arready = RegInit(false.B)
	val is_busy = RegInit(false.B)
	val araddr  = RegInit(0.U(addrw.W))
	val arburst = RegInit(0.U(2.W))
	val arlen   = RegInit(0.U(8.W))
	val rlast   = RegInit(false.B)
	val arlen_cnt = RegInit(0.U(8.W))
	val rvalid  = RegInit(false.B)

	when( ~io.ar.ready & io.ar.valid & ~is_busy ) { arready := true.B }
	.elsewhen( ~(r_ack & arlen === arlen_cnt) ) { arready := false.B }

	when( ~io.ar.ready & io.ar.valid & ~is_busy ) { is_busy := true.B }
	.elsewhen( (r_ack & arlen === arlen_cnt) ) { is_busy := false.B }	

	when( ~io.ar.ready & io.ar.valid & ~is_busy ) { araddr := io.ar.bits.addr }
	.elsewhen( r_ack & arlen_cnt <= arlen ) { araddr := Mux1H( Seq( (arburst === 0.U) -> araddr, (arburst === 1.U) -> (araddr + ( 1.U << addr_lsb ))  ) ) }

	when( ~io.ar.ready & io.ar.valid & ~is_busy ) { arburst := io.ar.bits.burst; arlen := io.ar.bits.len }

	when ( arlen_cnt === arlen & ~rlast & is_busy ) { rlast := true.B }
	.elsewhen( (io.ar.valid & ~io.ar.ready & ~is_busy) | r_ack ) { rlast := false.B }

	when ( ~io.ar.ready & io.ar.valid & ~is_busy ) { arlen_cnt := 0.U }
	.elsewhen( r_ack & arlen_cnt <= arlen ) { arlen_cnt := arlen_cnt + 1.U }

	when( ~io.r.valid & is_busy ) { rvalid := true.B }
	.elsewhen( r_ack ) { rvalid := false.B }


	io.ar.ready := arready
	io.r.valid := rvalid
	io.r.bits := io.r_info
	io.is_busy := is_busy


	assert( io.ar.valid & io.ar.bits.burst =/= 2.U & io.ar.bits.burst =/= 3.U, "Assert Fail at axi_slv_read, Unsupport Burst Mode" )
}

class AXI_mst_w(addrw: Int, dw: Int, idw: Int = 1, usw: Int = 1, len: Int ) extends Module {
	val io = IO( new Bundle {
		// val aw_info = Input(new AXI_chn_a( addrw, idw, usw ))
		// val w_info = Input(new AXI_chn_w( dw, usw ))
		val aw_req  = Input(Bool())
		val end     = Output(Bool())
		val aw_info = Input( new AXI_chn_a( (addrw: Int), idw, usw ) )
		val w_info  = Input( new AXI_chn_w( dw, usw ) )
		


		val aw = new DecoupledIO(new AXI_chn_a( (addrw: Int), idw, usw ))
		val w  = new DecoupledIO(new AXI_chn_w( dw, usw )) 
		val b  = Flipped( new DecoupledIO(new AXI_chn_b( idw, usw )))
	})

	val aw_ack = io.aw.valid & io.aw.ready
	val w_ack  = io.w.valid  & io.w.ready
	val b_ack  = io.b.valid  & io.b.ready

	val awvalid   = RegInit(false.B)
	val wvalid    = RegInit(false.B)
	val bready    = RegInit(false.B)
	val wlast     = RegInit(false.B)
	val write_idx = RegInit(0.U(64.W))



	when( ~io.aw.valid & io.aw_req ) { awvalid := true.B }
	.elsewhen( aw_ack ) { awvalid := false.B }

	when( ~io.w.valid & io.aw_req ) { wvalid := true.B }
	.elsewhen( w_ack ) { wvalid := false.B }

	when( io.b.valid & ~io.b.ready ) { bready := true.B}
	.elsewhen( io.b.ready ) { bready := false.B }

	when( ((write_idx === (len.U - 2.U)) & len.U >= 2.U & w_ack) | ( len.U === 1.U ) ) { wlast := true.B }
	.elsewhen( w_ack | (wlast & len.U === 1.U) ) { wlast := false.B }

	when ( io.aw_req ) { write_idx := 0.U }
	.elsewhen( w_ack & (write_idx =/= (len.U - 1.U)) ) { write_idx := write_idx + 1.U }

	io.aw.valid := awvalid
	io.w.valid := wvalid
	io.b.ready := bready
	io.end := w_ack & io.w.bits.last

	io.aw.bits := io.aw_info
	io.w.bits  := io.w_info
	io.w.bits.last := wlast

	// io.aw.bits.cache := 0.U
	// io.aw.bits.lock := 0.U
	// io.aw.bits.port := 0.U
	// io.aw.bits.qos := 0.U





}


class AXI_slv_w(addrw: Int, dw: Int, idw: Int = 1, usw: Int = 1) extends Module {
	val io = IO( new Bundle {
		val is_busy = Output(Bool())

		val aw = Flipped( new DecoupledIO(new AXI_chn_a( addrw, idw, usw )))
		val w  = Flipped( new DecoupledIO(new AXI_chn_w( dw, usw)))
		val b  = new DecoupledIO(new AXI_chn_b(idw, usw))
	})

	def addr_lsb = dw/8

	val aw_ack = io.aw.valid & io.aw.ready
	val w_ack  = io.w.valid  & io.w.ready
	val b_ack  = io.b.valid  & io.b.ready

	val awready = RegInit(false.B)
	val is_busy = RegInit(false.B)
	val awaddr  = RegInit(0.U(addrw.W))
	val awburst = RegInit(0.U(2.W))
	val awlen   = RegInit(0.U(8.W))
	val awlen_cnt = RegInit(0.U(8.W))
	val wready  = RegInit(false.B)
	val bvalid  = RegInit(false.B)

	when( io.aw.valid & ~io.aw.ready & ~is_busy ) { awready := true.B }
	.elsewhen( ~io.w.bits.last | ~io.w.ready )  { awready := false.B }

	when( io.aw.valid & ~io.aw.ready & ~is_busy ) { is_busy := true.B }
	.elsewhen( io.w.bits.last & io.w.ready )  { is_busy := false.B }

	when( io.aw.valid & ~io.aw.ready & ~is_busy ) { awaddr := io.aw.bits.addr }
	.elsewhen( w_ack & awlen_cnt <= awlen ) { awaddr := Mux1H( Seq( (awburst === 0.U) -> awaddr, (awburst === 1.U) -> (awaddr + (1.U << addr_lsb)) ) ) }

	when( io.aw.valid & ~io.aw.ready & ~is_busy ) { awburst := io.aw.bits.burst; awlen := io.aw.bits.len }

	when( io.aw.valid & ~io.aw.ready & ~is_busy ) { awlen_cnt := 0.U }
	.elsewhen( w_ack & ( awlen_cnt <= awlen ) ) { awlen_cnt := awlen_cnt + 1.U }

	when( io.w.valid & ~io.w.ready & is_busy ) { wready := true.B }
	.elsewhen( io.w.bits.last & io.w.ready ) { wready := false.B }

	when( ~io.b.valid & is_busy & w_ack & io.w.bits.last ) { bvalid := true.B }
	.elsewhen( b_ack ) { bvalid := false.B }


	io.aw.ready := awready
	io.is_busy := is_busy
	io.w.ready := wready
	io.b.valid := bvalid
	io.b.bits.id := 0.U
	io.b.bits.rsp := 0.U
	io.b.bits.user := 0.U

	assert( io.aw.valid & io.aw.bits.burst =/= 2.U & io.aw.bits.burst =/= 3.U, "Assert Fail at axi_slv_read, Unsupport Burst Mode" )

}
