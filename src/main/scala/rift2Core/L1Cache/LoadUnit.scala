
package rift2Core.L1Cache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._

import base._

// class Info_load extends Bundle {
//   val addr    = UInt(64.W)
//   val mask    = UInt(8.W)

//   val rd0_phy = UInt(6.W)
// }

// class LoadUnit extends Module {
//   val io = IO(new Bundle{

//     val lu_dpt_exe = Flipped(new DecoupledIO())

//     val lu_iss_exe = new DecoupledIO(new Lsu_iss_info)


//     val load_harazd = Output(Bool())

//     val flush = Output(Bool())
//   })


//   val load_entry = Reg(Vec(16, new Info_load))
//   val wr_ptr     = RegInit(0.U(5.W))
//   val rd_ptr     = RegInit(0.U(5.W))

//   io.lu_iss_exe.ready :=
//     (wr_ptr(3,0) === rd_ptr(3,0)) &
//     (wr_ptr(4) =/= rd_ptr(4))

//   io.dcache_load.valid := wr_ptr =/= rd_ptr
//   io.dcache_load.bits  := 

// }

