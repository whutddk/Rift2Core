

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
import chisel3.util.random._


trait slv_probe extends TLC_base {
  val slv_chn_b = IO(new DecoupledIO(new TLchannel_b(128, 32)))

  is_slvProbe_allowen :=
    (is_slvAcquire_StateOn | is_mstProbe_StateOn) &
    ~is_slvGrantData_StateOn &
    ~is_slvGrantAck_StateOn &
    ~is_slvProbe_StateOn &
    ~is_slvProbeData_StateOn &
    ~is_slvProbeAck_StateOn &
    ~is_slvReleaseData_StateOn &
    ~is_slvReleaseAck_StateOn &
    ~is_mstAcquire_StateOn &
    ~is_mstGrantData_StateOn &
    ~is_mstGrantAck_StateOn &
    ~is_mstProbeData_StateOn &
    ~is_mstProbeAck_StateOn &
    ~is_mstReleaseData_StateOn &
    ~is_mstReleaseAck_StateOn &
    ~is_slvGrantData_Waiting &
    ~is_slvGrantAck_valid &
     is_slvProbe_Waiting &
    ~is_slvProbeData_valid &
    ~is_slvProbeAck_valid &
    ~is_slvReleaseData_valid &
    ~is_slvReleaseAck_Waiting &
    ~is_mstAcquire_Waiting &
    ~is_mstGrantData_valid &
    ~is_mstGrantAck_Waiting &
    ~is_mstProbe_valid &
    ~is_mstProbeAck_Waiting &
    ~is_mstProbeData_Waiting &
    ~is_mstReleaseData_Waiting &
    ~is_mstReleaseAck_valid

  val b_valid = RegInit(false.B)


  when( slv_chn_b.fire ) {
    b_valid := false.B


  }
  when( is_slvProbe_allowen ) {
    b_valid := true.B
    is_slvProbe_Waiting := false.B
    is_slvProbe_StateOn := true.B
  }

  slv_chn_b.bits.address := 
    Mux1H( Seq(
      is_slvAcquire_StateOn -> info_slvAcquire_address,
      is_mstProbe_StateOn   -> info_mstProbe_address   
    ))

  slv_chn_b.bits.corrupt := false.B
  slv_chn_b.bits.data    := DontCare
  slv_chn_b.bits.mask    := DontCare
  slv_chn_b.bits.opcode  := Opcode.ProbeBlock
  slv_chn_b.bits.param   := TLparam.toN
  slv_chn_b.bits.size    := mst_lsb.U
  slv_chn_b.bits.source  := DontCare

}




