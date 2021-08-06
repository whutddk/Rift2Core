package rift

import chisel3._
import chisel3.util._

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp, TransferSizes}


abstract class RiftModule(implicit val p: Parameters) extends MultiIOModule with HasRiftParameters { def io: Record }
