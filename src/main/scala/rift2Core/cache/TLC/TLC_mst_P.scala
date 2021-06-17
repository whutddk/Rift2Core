/*
  Copyright (c) 2020 - 2021 Ruige Lee <m201772520@hust.edu.cn>

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

package rift2Core.cache.TLC

import chisel3._
import chisel3.util._

import tilelink._

import base._
import rift2Core.cache._

trait mst_Probe extends TLC_base {
  val mst_chn_b = IO(Flipped(new DecoupledIO(new TLchannel_b(128, 32))))

  is_mstProbe_allowen :=
    // ~is_slvAcquire_StateOn &
    ~is_slvGrantData_StateOn &
    ~is_slvGrantAck_StateOn &
    ~is_slvProbe_StateOn &
    ~is_slvProbeData_StateOn &
    ~is_slvProbeAck_StateOn &
    ~is_slvReleaseData_StateOn &
    ~is_slvReleaseAck_StateOn &
    // ~is_mstAcquire_StateOn &
    ~is_mstGrantData_StateOn &
    ~is_mstGrantAck_StateOn &
    ~is_mstProbe_StateOn &
    ~is_mstProbeData_StateOn &
    ~is_mstProbeAck_StateOn &
    ~is_mstReleaseData_StateOn &
    ~is_mstReleaseAck_StateOn &
    // ~is_slvGrantData_Waiting &
    // ~is_slvGrantAck_valid &
    // ~is_slvProbe_Waiting &
    // ~is_slvProbeData_valid &
    // ~is_slvProbeAck_valid &
    // ~is_slvReleaseData_valid &
    // ~is_slvReleaseAck_Waiting &
    // ~is_mstAcquire_Waiting &
    // ~is_mstGrantData_valid &
    // ~is_mstGrantAck_Waiting &
    is_mstProbe_valid &
    ~is_mstProbeAck_Waiting &
    ~is_mstProbeData_Waiting
    // ~is_mstReleaseData_Waiting &
    // ~is_mstReleaseAck_valid

  val mst_chn_b_ready = RegInit(false.B)
  mst_chn_b.ready := mst_chn_b_ready

  when( is_mstProbe_allowen ) { mst_chn_b_ready := true.B; is_mstProbe_StateOn := true.B }
  .elsewhen( mst_chn_b.fire ) { mst_chn_b_ready := false.B }

  info_mstProbe_address := RegEnable( mst_chn_b.bits.address, is_mstProbe_allowen )
  
  info_mstProbe_cache_tag_ren := is_mstProbe_allowen
  info_mstProbe_cache_tag_raddr := mst_chn_b.bits.address
  info_mstProbe_cb := {
    val tag_info = mst_chn_b.bits.address( 31, 32-tag_w )
    val is_cb_hit = VecInit( for ( i <- 0 until cb ) yield cache_tag.tag_info_r(i) === tag_info )
    val hit_cb = OHToUInt(is_cb_hit.asUInt)

    assert( ~(mst_chn_b.fire &
      is_cb_hit.forall((x:Bool) => (x === false.B))), "Assert Failed at TLC_mst_P.scala!! Your Slave agent request to Probe a missing block!"  )
    
    RegEnable( hit_cb, mst_chn_b.fire )
  }

}


trait mst_probe_Ack_Data extends TLC_base {
  val mst_chn_c0 = IO(new DecoupledIO(new TLchannel_c(128, 32)))

  is_mstProbeAck_Data_allowen :=
    // (is_slvAcquire_StateOn | is_mstProbe_StateOn) &
    // ~is_slvGrantData_StateOn &
    // ~is_slvGrantAck_StateOn &
    // is_slvProbe_StateOn &
    // ~is_slvProbeData_StateOn &
    // ~is_slvProbeAck_StateOn &
    // ~is_slvReleaseData_StateOn &
    // ~is_slvReleaseAck_StateOn &
    // ~is_mstAcquire_StateOn &
    // ~is_mstGrantData_StateOn &
    // ~is_mstGrantAck_StateOn &
    // ~is_mstProbeData_StateOn &
    // ~is_mstProbeAck_StateOn &
    // ~is_mstReleaseData_StateOn &
    // ~is_mstReleaseAck_StateOn &
    // ~is_slvGrantData_Waiting &
    // ~is_slvGrantAck_valid &
    // ~is_slvProbe_Waiting &
    // ~is_slvProbeData_valid &
    // is_slvProbeAck_valid &
    // ~is_slvReleaseData_valid &
    // ~is_slvReleaseAck_Waiting &
    // ~is_mstAcquire_Waiting &
    // ~is_mstGrantData_valid &
    // ~is_mstGrantAck_Waiting &
    // ~is_mstProbe_valid &
    // ~is_mstProbeAck_Waiting &
    // ~is_mstProbeData_Waiting &
    // ~is_mstReleaseData_Waiting &
    // ~is_mstReleaseAck_valid


}












