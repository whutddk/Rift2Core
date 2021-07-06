package base

import chisel3._
import chisel3.util._

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp, TransferSizes}

import rift2Core.L1Cache._


abstract class RiftModule(implicit val p: Parameters) extends MultiIOModule with HasDcacheParameters { def io: Record }
