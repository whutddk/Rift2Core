package rift

import chisel3._
import chisel3.util._

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp, TransferSizes}


abstract class RiftModule(implicit val p: Parameters) extends Module with HasRiftParameters { def io: Record }
abstract class RiftBundle(implicit val p: Parameters) extends Bundle with HasRiftParameters

