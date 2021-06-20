

/*
  Copyright (c) 2020 - 2021 Ruige Lee <wut.ruigeli@gmail.com>

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

package rift2Core.cache

import chisel3._
import chisel3.util._

import tilelink._

import base._
import rift2Core.cache._
import rift2Core.cache.TLC._

class TLC_L2 extends TLC_base with TLC_slv_A with TLC_slv_P with TLC_slv_R with TLC_mst_A with TLC_mst_P with TLC_mst_R {

  override def dw = 1024
  override def aw = 32
  override def bk = 4
  override def cb = 4
  override def cl = 64
  override def agent_no = 4

  /**
    * slvAcquire：passive
    * 
    */
  is_slvAcquire_allowen :=
    ~is_slvAcquire_StateOn       & ~is_slvGrantData_StateOn     & ~is_slvGrantAck_StateOn &
    ~is_slvProbe_StateOn         & ~is_slvProbeData_StateOn     & ~is_slvProbeAck_StateOn &
    ~is_slvReleaseData_StateOn   & ~is_slvReleaseAck_StateOn    & ~is_mstAcquire_StateOn &
    ~is_mstGrantData_StateOn     & ~is_mstGrantAck_StateOn      & ~is_mstProbe_StateOn &
    ~is_mstProbeAck_Data_StateOn & ~is_mstReleaseData_StateOn   & ~is_mstReleaseAck_StateOn &
     is_slvAcquire_valid         & ~is_slvGrantData_Waiting     & ~is_slvGrantAck_valid &
    ~is_slvProbe_Waiting         & ~is_slvProbeData_valid       & ~is_slvProbeAck_valid &
    ~is_slvReleaseData_valid     & ~is_slvReleaseAck_Waiting    &
    ~is_mstAcquire_Waiting       & ~is_mstGrantData_valid       & ~is_mstGrantAck_Waiting &
    ~is_mstProbe_valid           & ~is_mstProbeAck_Data_Waiting &
    ~is_mstReleaseData_Waiting   & ~is_mstReleaseAck_valid





  /**
    * slvGrantData：
    * 
    */  
  is_slvGrantData_allowen := 
     // is_slvAcquire_StateOn     & 
    ~is_slvGrantData_StateOn   & ~is_slvGrantAck_StateOn &
    ~is_slvProbe_StateOn       & ~is_slvProbeData_StateOn     & ~is_slvProbeAck_StateOn &
    ~is_slvReleaseData_StateOn & ~is_slvReleaseAck_StateOn    &
    // ~is_mstAcquire_StateOn     & ~is_mstGrantData_StateOn     & ~is_mstGrantAck_StateOn &
    ~is_mstProbe_StateOn       & ~is_mstProbeAck_Data_StateOn &
    // ~is_mstReleaseData_StateOn & ~is_mstReleaseAck_StateOn    &
     is_slvGrantData_Waiting   & ~is_slvGrantAck_valid        &
    ~is_slvProbe_Waiting       & ~is_slvProbeData_valid       & ~is_slvProbeAck_valid   &
    ~is_slvReleaseData_valid   & ~is_slvReleaseAck_Waiting  &
    // ~is_mstAcquire_Waiting  &
    ~is_mstGrantData_valid     &
    // ~is_mstGrantAck_Waiting &
    ~is_mstProbe_valid         & ~is_mstProbeAck_Data_Waiting &
    // ~is_mstReleaseData_Waiting &
    ~is_mstReleaseAck_valid

  assert( ~(is_slvGrantData_Waiting & ~is_slvAcquire_StateOn), "Assert Failed at TLC_L2, slvGrantData is requested without slv Acquire state, that's impossible" ) 
  assert( ~(is_slvGrantData_Waiting & (is_mstAcquire_StateOn | is_mstGrantData_StateOn | is_mstGrantAck_StateOn | is_mstReleaseData_StateOn | is_mstReleaseAck_StateOn) ), "Assert Failed at TLC_L2, slvGrantData is requested with other parasitical state on, that's impossible" )
  assert( ~(is_slvGrantData_Waiting & (is_mstAcquire_Waiting | is_mstGrantAck_Waiting | is_mstReleaseData_Waiting) ), "Assert Failed at TLC_L2, slvGrantData is requested with other parasitical messages, that's impossible" )



  is_slvGrantAck_allowen :=
    is_slvAcquire_StateOn &
    is_slvGrantData_StateOn &
    ~is_slvGrantAck_StateOn &
    ~is_slvProbe_StateOn &
    ~is_slvProbeData_StateOn &
    ~is_slvProbeAck_StateOn &
    ~is_slvReleaseData_StateOn &
    ~is_slvReleaseAck_StateOn &
    ~is_mstAcquire_StateOn &
    ~is_mstGrantData_StateOn &
    ~is_mstGrantAck_StateOn &
    ~is_mstProbe_StateOn &
    ~is_mstProbeAck_Data_StateOn &
    ~is_mstReleaseData_StateOn &
    ~is_mstReleaseAck_StateOn &
    ~is_slvGrantData_Waiting &
     is_slvGrantAck_valid &
    // ~is_SlvProbe_Waiting &
    // ~is_SlvProbeData_valid &
    // ~is_SlvProbeAck_valid &
    // ~is_SlvReleaseData_valid &
    // ~is_SlvReleaseAck_Waiting &
    ~is_mstAcquire_Waiting &
    ~is_mstGrantData_valid &
    ~is_mstGrantAck_Waiting &
    ~is_mstProbe_valid &
    ~is_mstProbeAck_Data_Waiting &
    ~is_mstReleaseData_Waiting &
    ~is_mstReleaseAck_valid



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
    ~is_mstProbeAck_Data_StateOn &
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
    ~is_mstProbeAck_Data_Waiting &
    ~is_mstReleaseData_Waiting &
    ~is_mstReleaseAck_valid


  is_slvProbeAck_allowen :=
    (is_slvAcquire_StateOn | is_mstProbe_StateOn) &
    ~is_slvGrantData_StateOn &
    ~is_slvGrantAck_StateOn &
    is_slvProbe_StateOn &
    ~is_slvProbeData_StateOn &
    ~is_slvProbeAck_StateOn &
    ~is_slvReleaseData_StateOn &
    ~is_slvReleaseAck_StateOn &
    ~is_mstAcquire_StateOn &
    ~is_mstGrantData_StateOn &
    ~is_mstGrantAck_StateOn &
    ~is_mstProbeAck_Data_StateOn &
    ~is_mstReleaseData_StateOn &
    ~is_mstReleaseAck_StateOn &
    ~is_slvGrantData_Waiting &
    ~is_slvGrantAck_valid &
    ~is_slvProbe_Waiting &
    ~is_slvProbeData_valid &
    is_slvProbeAck_valid &
    ~is_slvReleaseData_valid &
    ~is_slvReleaseAck_Waiting &
    ~is_mstAcquire_Waiting &
    ~is_mstGrantData_valid &
    ~is_mstGrantAck_Waiting &
    ~is_mstProbe_valid &
    ~is_mstProbeAck_Data_Waiting &
    ~is_mstReleaseData_Waiting &
    ~is_mstReleaseAck_valid


  is_slvProbeData_allowen :=
    (is_slvAcquire_StateOn | is_mstProbe_StateOn) &
    ~is_slvGrantData_StateOn &
    ~is_slvGrantAck_StateOn &
    is_slvProbe_StateOn &
    ~is_slvProbeData_StateOn &
    ~is_slvProbeAck_StateOn &
    ~is_slvReleaseData_StateOn &
    ~is_slvReleaseAck_StateOn &
    ~is_mstAcquire_StateOn &
    ~is_mstGrantData_StateOn &
    ~is_mstGrantAck_StateOn &
    ~is_mstProbeAck_Data_StateOn &
    ~is_mstReleaseData_StateOn &
    ~is_mstReleaseAck_StateOn &
    ~is_slvGrantData_Waiting &
    ~is_slvGrantAck_valid &
    ~is_slvProbe_Waiting &
    is_slvProbeData_valid &
    ~is_slvProbeAck_valid &
    ~is_slvReleaseData_valid &
    ~is_slvReleaseAck_Waiting &
    ~is_mstAcquire_Waiting &
    ~is_mstGrantData_valid &
    ~is_mstGrantAck_Waiting &
    ~is_mstProbe_valid &
    ~is_mstProbeAck_Data_Waiting &
    ~is_mstReleaseData_Waiting &
    ~is_mstReleaseAck_valid


  is_slvReleaseData_allowen :=
    // ~is_slvAcquire_StateOn &
    ~is_slvGrantData_StateOn &
    ~is_slvGrantAck_StateOn &
    // ~is_slvProbe_StateOn &
    ~is_slvProbeData_StateOn &
    ~is_slvProbeAck_StateOn &
    ~is_slvReleaseData_StateOn &
    ~is_slvReleaseAck_StateOn &
    // ~is_mstAcquire_StateOn &
    ~is_mstGrantData_StateOn &
    ~is_mstGrantAck_StateOn &
    // ~is_mstProbe_StateOn & 
    ~is_mstProbeAck_Data_StateOn &
    ~is_mstReleaseData_StateOn &
    ~is_mstReleaseAck_StateOn &
    // ~is_slvGrantData_Waiting &
    ~is_slvGrantAck_valid &
    // ~is_slvProbe_Waiting &
    ~is_slvProbeData_valid &
    ~is_slvProbeAck_valid &
    is_slvReleaseData_valid &
    ~is_slvReleaseAck_Waiting
    // ~is_mstAcquire_Waiting &
    // ~is_mstGrantData_valid &
    // ~is_mstGrantAck_Waiting &
    // ~is_mstProbe_valid &
    // ~is_mstProbeAck_Data_Waiting &
    // ~is_mstReleaseData_Waiting &
    // ~is_mstReleaseAck_valid


  is_slvReleaseAck_allowen :=
    // ~is_slvAcquire_StateOn &
    ~is_slvGrantData_StateOn &
    ~is_slvGrantAck_StateOn &
    // ~is_slvProbe_StateOn &
    ~is_slvProbeData_StateOn &
    ~is_slvProbeAck_StateOn &
    ~is_slvReleaseData_StateOn &
    ~is_slvReleaseAck_StateOn &
    ~is_mstAcquire_StateOn &
    ~is_mstGrantData_StateOn &
    ~is_mstGrantAck_StateOn &
    ~is_mstProbe_StateOn & 
    ~is_mstProbeAck_Data_StateOn &
    is_mstReleaseData_StateOn &
    ~is_mstReleaseAck_StateOn &
    // ~is_slvGrantData_Waiting &
    // ~is_slvGrantAck_valid &
    // ~is_slvProbe_Waiting &
    // ~is_slvProbeData_valid &
    // ~is_slvProbeAck_valid &
    // ~is_slvReleaseData_valid &
    is_slvReleaseAck_Waiting
    // ~is_mstAcquire_Waiting &
    // ~is_mstGrantData_valid &
    // ~is_mstGrantAck_Waiting &
    // ~is_mstProbe_valid &
    // ~is_mstProbeAck_Waiting &
    // ~is_mstProbeData_Waiting &
    // ~is_mstReleaseData_Waiting &
    // ~is_mstReleaseAck_valid


  is_mstAcquire_allowen :=
    is_slvAcquire_StateOn &
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
    ~is_mstProbe_StateOn &
    ~is_mstProbeAck_Data_StateOn &
    ~is_mstReleaseData_StateOn &
    ~is_mstReleaseAck_StateOn &
    // ~is_slvAcquire_valid &
    ~is_slvGrantData_Waiting &
    ~is_slvGrantAck_valid &
    ~is_slvProbe_Waiting &
    ~is_slvProbeData_valid &
    ~is_slvProbeAck_valid &
    ~is_slvReleaseData_valid &
    ~is_slvReleaseAck_Waiting &
    is_mstAcquire_Waiting &
    ~is_mstGrantData_valid &
    ~is_mstGrantAck_Waiting &
    ~is_mstProbe_valid &
    ~is_mstProbeAck_Data_Waiting &
    ~is_mstReleaseData_Waiting &
    ~is_mstReleaseAck_valid

  is_mstGrantData_allowen := 
     is_slvAcquire_StateOn & 
    ~is_slvGrantData_StateOn &
    ~is_slvGrantAck_StateOn &
    ~is_slvProbe_StateOn &
    ~is_slvProbeData_StateOn &
    ~is_slvProbeAck_StateOn &
    ~is_slvReleaseData_StateOn &
    ~is_slvReleaseAck_StateOn &
    is_mstAcquire_StateOn &
    ~is_mstGrantData_StateOn &
    ~is_mstGrantAck_StateOn &
    ~is_mstProbe_StateOn &
    ~is_mstProbeAck_Data_StateOn &
    ~is_mstReleaseData_StateOn &
    ~is_mstReleaseAck_StateOn &
    ~is_slvGrantData_Waiting &
    ~is_slvGrantAck_valid &
    ~is_slvProbe_Waiting &
    ~is_slvProbeData_valid &
    ~is_slvProbeAck_valid &
    ~is_slvReleaseData_valid &
    ~is_slvReleaseAck_Waiting &
    ~is_mstAcquire_Waiting &
    is_mstGrantData_valid &
    ~is_mstGrantAck_Waiting &
    ~is_mstProbe_valid &
    ~is_mstProbeAck_Data_Waiting &
    ~is_mstReleaseData_Waiting &
    ~is_mstReleaseAck_valid

  is_mstGrantAck_allowen :=
    is_slvAcquire_StateOn &
    ~is_slvGrantData_StateOn &
    ~is_slvGrantAck_StateOn &
    ~is_slvProbe_StateOn &
    ~is_slvProbeData_StateOn &
    ~is_slvProbeAck_StateOn &
    ~is_slvReleaseData_StateOn &
    ~is_slvReleaseAck_StateOn &
    is_mstAcquire_StateOn &
    is_mstGrantData_StateOn &
    ~is_mstGrantAck_StateOn &
    ~is_mstProbe_StateOn &
    ~is_mstProbeAck_Data_StateOn &
    ~is_mstReleaseData_StateOn &
    ~is_mstReleaseAck_StateOn &
    ~is_slvGrantData_Waiting &
    ~is_slvGrantAck_valid &
    // ~is_SlvProbe_Waiting &
    // ~is_SlvProbeData_valid &
    // ~is_SlvProbeAck_valid &
    // ~is_SlvReleaseData_valid &
    // ~is_SlvReleaseAck_Waiting &
    ~is_mstAcquire_Waiting &
    ~is_mstGrantData_valid &
    is_mstGrantAck_Waiting
    // ~is_mstProbe_valid &
    // ~is_mstProbeAck_Waiting &
    // ~is_mstProbeData_Waiting &
    // ~is_mstReleaseData_Waiting &
    // ~is_mstReleaseAck_valid

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
    ~is_mstProbeAck_Data_StateOn &
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
    ~is_mstProbeAck_Data_Waiting
    // ~is_mstReleaseData_Waiting &
    // ~is_mstReleaseAck_valid


  is_mstProbeAck_Data_allowen :=
    // ~is_slvAcquire_StateOn &
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
    is_mstProbe_StateOn &
    ~is_mstProbeAck_Data_StateOn &
    ~is_mstReleaseData_StateOn &
    ~is_mstReleaseAck_StateOn &
    ~is_slvGrantData_Waiting &
    ~is_slvGrantAck_valid &
    ~is_slvProbe_Waiting &
    ~is_slvProbeData_valid &
    ~is_slvProbeAck_valid &
    ~is_slvReleaseData_valid &
    ~is_slvReleaseAck_Waiting &
    ~is_mstAcquire_Waiting &
    ~is_mstGrantData_valid &
    ~is_mstGrantAck_Waiting &
    ~is_mstProbe_valid &
    is_mstProbeAck_Data_Waiting &
    ~is_mstReleaseData_Waiting &
    ~is_mstReleaseAck_valid


  is_slvReleaseData_allowen :=
    is_slvAcquire_StateOn &
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
    // ~is_mstProbe_StateOn & 
    ~is_mstProbeAck_Data_StateOn &
    ~is_mstReleaseData_StateOn &
    ~is_mstReleaseAck_StateOn &
    is_slvGrantData_Waiting &
    ~is_slvGrantAck_valid &
    ~is_slvProbe_Waiting &
    ~is_slvProbeData_valid &
    ~is_slvProbeAck_valid &
    is_slvReleaseData_valid &
    ~is_slvReleaseAck_Waiting
    ~is_mstAcquire_Waiting &
    ~is_mstGrantData_valid &
    ~is_mstGrantAck_Waiting &
    ~is_mstProbe_valid &
    ~is_mstProbeAck_Data_Waiting &
    is_mstReleaseData_Waiting &
    ~is_mstReleaseAck_valid

  is_mstReleaseAck_allowen :=
    is_slvAcquire_StateOn &
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
    ~is_mstProbe_StateOn & 
    ~is_mstProbeAck_Data_StateOn &
    is_mstReleaseData_StateOn &
    // ~is_slvGrantData_Waiting &
    // ~is_slvGrantAck_valid &
    // ~is_slvProbe_Waiting &
    // ~is_slvProbeData_valid &
    // ~is_slvProbeAck_valid &
    // ~is_slvReleaseData_valid &
    // ~is_slvReleaseAck_Waiting
    // ~is_mstAcquire_Waiting &
    // ~is_mstGrantData_valid &
    // ~is_mstGrantAck_Waiting &
    // ~is_mstProbe_valid &
    // ~is_mstProbeAck_Waiting &
    // ~is_mstProbeData_Waiting &
    // ~is_mstReleaseData_Waiting &
    is_mstReleaseAck_valid

}


