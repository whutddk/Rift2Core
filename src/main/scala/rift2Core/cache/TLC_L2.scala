

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
    ~is_slvAcquire_StateOn       & //上一个slvAcquire必须已经解决
    ~is_slvGrantData_StateOn     & //上一个slvAcquire的寄生消息必须已经解决
    ~is_slvGrantAck_StateOn      & //上一个slvAcquire的响应消息必须已经解决
    // ~is_slvProbe_StateOn         & //和主动消息slvProbe没有资源争夺
    ~is_slvProbeAckData_StateOn  & //被动消息slvProbeack——Data有更高优先级
    ~is_slvReleaseData_StateOn   & //被动消息slvReleaseData有更高优先级
    // ~is_slvReleaseAck_StateOn    & //主动消息slvReleaseAck没有资源争夺
    // ~is_mstAcquire_StateOn       & //通过断言
    // ~is_mstGrantData_StateOn     & //通过断言
    // ~is_mstGrantAck_StateOn      & //通过断言
    ~is_mstProbe_StateOn         & //mstProbe 需要读cb，具有最高优先级
    ~is_mstProbeAckData_StateOn  & //返回mstProbe具有更高优先级
    // ~is_mstReleaseData_StateOn   & //通过断言
    // ~is_mstReleaseAck_StateOn    & //通过断言
     is_slvAcquire_valid         & // when requset comes 
    ~is_slvGrantData_Waiting     & //上一个slvAcquire的寄生消息必须已经解决
    ~is_slvGrantAck_valid        & //上一个slvAcquire的响应消息必须已经解决
    // ~is_slvProbe_Waiting         & //和主动消息slvProbe没有资源争夺
    ~is_slvProbeAckData_valid    & //被动消息slvProbeack——Data有更高优先级
    ~is_slvReleaseData_valid     & //被动消息slvReleaseData有更高优先级
    // ~is_slvReleaseAck_Waiting    & //主动消息slvReleaseAck没有资源争夺
    // ~is_mstAcquire_Waiting       & //通过断言
    // ~is_mstGrantData_valid       & //通过断言
    // ~is_mstGrantAck_Waiting      & //通过断言
    ~is_mstProbe_valid           & //mstProbe 需要读cb，具有最高优先级
    ~is_mstProbeAckData_Waiting //返回mstProbe具有更高优先级
    // ~is_mstReleaseData_Waiting   & //通过断言
    // ~is_mstReleaseAck_valid        //通过断言


  assert( ~(~is_slvAcquire_StateOn & (is_mstAcquire_StateOn | is_mstGrantData_StateOn | is_mstGrantAck_StateOn | is_mstReleaseData_StateOn | is_mstReleaseAck_StateOn) ), "Assert Failed at TLC_L2, when is_slvAcquire_StateOn is false.B, its parasitical should never state on" )
  assert( ~(~is_slvAcquire_StateOn & (is_mstAcquire_Waiting | is_mstGrantData_valid   | is_mstGrantAck_Waiting | is_mstReleaseData_Waiting | is_mstReleaseAck_valid) ), "Assert Failed at TLC_L2, when is_slvAcquire_StateOn is false.B,  its parasitical messages should never valid or waitting" )




  /**
    * slvGrantData：
    * 主动操作，轮询类
    */  
  is_slvGrantData_allowen := 
    // is_slvAcquire_StateOn     & //通过断言，请求时必定是打开的
    ~is_slvGrantData_StateOn     & //请求有效时必定是关闭的
    // ~is_slvGrantAck_StateOn      & //通过断言，请求时必定是关闭的
    ~is_slvProbe_StateOn         & //来自mstProbe递归的slvProbe具有更高优先级,也可能时寄生消息
    ~is_slvProbeAckData_StateOn  & //slvProbeACK具有更高优先级
    ~is_slvReleaseData_StateOn   & //被动消息,更高优先级
    ~is_slvReleaseAck_StateOn    & //被动消息,更高优先级
    ~is_mstAcquire_StateOn       & //轮询发现需要请求寄生消息，先请求
    ~is_mstGrantData_StateOn     & //轮询发现需要请求寄生消息，先请求
    ~is_mstGrantAck_StateOn      & //轮询发现需要请求寄生消息，先请求
    ~is_mstProbe_StateOn         & //被动消息,更高优先级
    ~is_mstProbeAckData_StateOn  & //主动消息,更高优先级
    ~is_mstReleaseData_StateOn   & //轮询发现需要请求寄生消息，先请求
    ~is_mstReleaseAck_StateOn    & //轮询发现需要请求寄生消息，先请求
    // ~is_slvAcquire_valid         & // 优先级高于slvAcquire
     is_slvGrantData_Waiting     & //请求到来
    // ~is_slvGrantAck_valid        & //通过断言，请求时必定是关闭的
    ~is_slvProbe_Waiting         & //来自mstProbe递归的slvProbe具有更高优先级,也可能时寄生消息
    ~is_slvProbeAckData_valid    & //slvProbeACK具有更高优先级
    ~is_slvReleaseData_valid     & //更高优先级
    ~is_slvReleaseAck_Waiting    & //更高优先级
    ~is_mstAcquire_Waiting       & //轮询发现需要请求寄生消息，先请求
    ~is_mstGrantData_valid       & //轮询发现需要请求寄生消息，先请求
    ~is_mstGrantAck_Waiting      & //轮询发现需要请求寄生消息，先请求
    ~is_mstProbe_valid           & //被动消息,更高优先级
    ~is_mstProbeAckData_Waiting  & //主动消息,更高优先级
    ~is_mstReleaseData_Waiting   & //轮询发现需要请求寄生消息，先请求
    ~is_mstReleaseAck_valid        //轮询发现需要请求寄生消息，先请求

  assert( ~(is_slvGrantData_Waiting & ~is_slvAcquire_StateOn), "Assert Failed at TLC_L2, slvGrantData is requested without slv Acquire state, that's impossible" ) 
  assert( ~(is_slvGrantData_Waiting & (is_slvGrantAck_StateOn | is_slvGrantAck_valid) ), "Assert Failed at TLC_L2, slvGrantData is requested with its ack message state on, that's impossible" )
 


  is_slvGrantAck_allowen :=
    // is_slvAcquire_StateOn & //通过断言，请求时必定是打开的
    // is_slvGrantData_StateOn & //通过断言，请求时必定是打开的
    ~is_slvGrantAck_StateOn & //请求有效时必定是关闭的
    ~is_slvProbe_StateOn &
    ~is_slvProbeAckData_StateOn &
    ~is_slvReleaseData_StateOn &
    ~is_slvReleaseAck_StateOn &
    ~is_mstAcquire_StateOn &
    ~is_mstGrantData_StateOn &
    ~is_mstGrantAck_StateOn &
    ~is_mstProbe_StateOn &
    ~is_mstProbeAckData_StateOn &
    ~is_mstReleaseData_StateOn &
    ~is_mstReleaseAck_StateOn &
    ~is_slvGrantData_Waiting &
    // ~is_slvAcquire_valid         & // 优先级高于slvAcquire
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
    ~is_mstProbeAckData_Waiting &
    ~is_mstReleaseData_Waiting &
    ~is_mstReleaseAck_valid

  assert( ~(is_slvGrantAck_valid & (~is_slvAcquire_StateOn | ~is_slvGrantData_StateOn)), "Assert Failed at TLC-L2, when slv GrantAck in chn e, the slv acquire and slv grantdata is state-off, that's impossible!" )



  is_slvProbe_allowen :=
    (is_slvAcquire_StateOn | is_mstProbe_StateOn) &
    ~is_slvGrantData_StateOn &
    ~is_slvGrantAck_StateOn &
    ~is_slvProbe_StateOn &
    ~is_slvProbeAckData_StateOn &
    ~is_slvReleaseData_StateOn &
    ~is_slvReleaseAck_StateOn &
    ~is_mstAcquire_StateOn &
    ~is_mstGrantData_StateOn &
    ~is_mstGrantAck_StateOn &
    ~is_mstProbeAckData_StateOn &
    ~is_mstReleaseData_StateOn &
    ~is_mstReleaseAck_StateOn &
    ~is_slvGrantData_Waiting &
    ~is_slvGrantAck_valid &
     is_slvProbe_Waiting &
    ~is_slvProbeAckData_valid &
    ~is_slvReleaseData_valid &
    ~is_slvReleaseAck_Waiting &
    ~is_mstAcquire_Waiting &
    ~is_mstGrantData_valid &
    ~is_mstGrantAck_Waiting &
    ~is_mstProbe_valid &
    ~is_mstProbeAckData_Waiting &
    ~is_mstReleaseData_Waiting &
    ~is_mstReleaseAck_valid


  is_slvProbeAckData_allowen :=
    (is_slvAcquire_StateOn | is_mstProbe_StateOn) &
    ~is_slvGrantData_StateOn &
    ~is_slvGrantAck_StateOn &
    is_slvProbe_StateOn &
    ~is_slvProbeAckData_StateOn &
    ~is_slvReleaseData_StateOn &
    ~is_slvReleaseAck_StateOn &
    ~is_mstAcquire_StateOn &
    ~is_mstGrantData_StateOn &
    ~is_mstGrantAck_StateOn &
    ~is_mstProbeAckData_StateOn &
    ~is_mstReleaseData_StateOn &
    ~is_mstReleaseAck_StateOn &
    ~is_slvGrantData_Waiting &
    ~is_slvGrantAck_valid &
    ~is_slvProbe_Waiting &
    ~is_slvProbeAckData_valid &
    ~is_slvReleaseData_valid &
    ~is_slvReleaseAck_Waiting &
    ~is_mstAcquire_Waiting &
    ~is_mstGrantData_valid &
    ~is_mstGrantAck_Waiting &
    ~is_mstProbe_valid &
    ~is_mstProbeAckData_Waiting &
    ~is_mstReleaseData_Waiting &
    ~is_mstReleaseAck_valid


  is_slvReleaseData_allowen :=
    // ~is_slvAcquire_StateOn &
    ~is_slvGrantData_StateOn &
    ~is_slvGrantAck_StateOn &
    // ~is_slvProbe_StateOn &
    ~is_slvProbeAckData_StateOn &
    ~is_slvReleaseData_StateOn &
    ~is_slvReleaseAck_StateOn &
    // ~is_mstAcquire_StateOn &
    ~is_mstGrantData_StateOn &
    ~is_mstGrantAck_StateOn &
    // ~is_mstProbe_StateOn & 
    ~is_mstProbeAckData_StateOn &
    ~is_mstReleaseData_StateOn &
    ~is_mstReleaseAck_StateOn &
    // ~is_slvGrantData_Waiting &
    ~is_slvGrantAck_valid &
    // ~is_slvProbe_Waiting &
    ~is_slvProbeAckData_valid &
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
    ~is_slvProbeAckData_StateOn &
    ~is_slvReleaseData_StateOn &
    ~is_slvReleaseAck_StateOn &
    ~is_mstAcquire_StateOn &
    ~is_mstGrantData_StateOn &
    ~is_mstGrantAck_StateOn &
    ~is_mstProbe_StateOn & 
    ~is_mstProbeAckData_StateOn &
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
    ~is_slvProbeAckData_StateOn &
    ~is_slvReleaseData_StateOn &
    ~is_slvReleaseAck_StateOn &
    ~is_mstAcquire_StateOn &
    ~is_mstGrantData_StateOn &
    ~is_mstGrantAck_StateOn &
    ~is_mstProbe_StateOn &
    ~is_mstProbeAckData_StateOn &
    ~is_mstReleaseData_StateOn &
    ~is_mstReleaseAck_StateOn &
    // ~is_slvAcquire_valid &
    ~is_slvGrantData_Waiting &
    ~is_slvGrantAck_valid &
    ~is_slvProbe_Waiting &
    ~is_slvProbeAckData_valid &
    ~is_slvReleaseData_valid &
    ~is_slvReleaseAck_Waiting &
    is_mstAcquire_Waiting &
    ~is_mstGrantData_valid &
    ~is_mstGrantAck_Waiting &
    ~is_mstProbe_valid &
    ~is_mstProbeAckData_Waiting &
    ~is_mstReleaseData_Waiting &
    ~is_mstReleaseAck_valid

  is_mstGrantData_allowen := 
     is_slvAcquire_StateOn & 
    ~is_slvGrantData_StateOn &
    ~is_slvGrantAck_StateOn &
    ~is_slvProbe_StateOn &
    ~is_slvProbeAckData_StateOn &
    ~is_slvReleaseData_StateOn &
    ~is_slvReleaseAck_StateOn &
    is_mstAcquire_StateOn &
    ~is_mstGrantData_StateOn &
    ~is_mstGrantAck_StateOn &
    ~is_mstProbe_StateOn &
    ~is_mstProbeAckData_StateOn &
    ~is_mstReleaseData_StateOn &
    ~is_mstReleaseAck_StateOn &
    ~is_slvGrantData_Waiting &
    ~is_slvGrantAck_valid &
    ~is_slvProbe_Waiting &
    ~is_slvProbeAckData_valid &
    ~is_slvReleaseData_valid &
    ~is_slvReleaseAck_Waiting &
    ~is_mstAcquire_Waiting &
    is_mstGrantData_valid &
    ~is_mstGrantAck_Waiting &
    ~is_mstProbe_valid &
    ~is_mstProbeAckData_Waiting &
    ~is_mstReleaseData_Waiting &
    ~is_mstReleaseAck_valid

  is_mstGrantAck_allowen :=
    is_slvAcquire_StateOn &
    ~is_slvGrantData_StateOn &
    ~is_slvGrantAck_StateOn &
    ~is_slvProbe_StateOn &
    ~is_slvProbeAckData_StateOn &
    ~is_slvReleaseData_StateOn &
    ~is_slvReleaseAck_StateOn &
    is_mstAcquire_StateOn &
    is_mstGrantData_StateOn &
    ~is_mstGrantAck_StateOn &
    ~is_mstProbe_StateOn &
    ~is_mstProbeAckData_StateOn &
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
    ~is_slvProbeAckData_StateOn &
    ~is_slvReleaseData_StateOn &
    ~is_slvReleaseAck_StateOn &
    // ~is_mstAcquire_StateOn &
    ~is_mstGrantData_StateOn &
    ~is_mstGrantAck_StateOn &
    ~is_mstProbe_StateOn &
    ~is_mstProbeAckData_StateOn &
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
    ~is_mstProbeAckData_Waiting
    // ~is_mstReleaseData_Waiting &
    // ~is_mstReleaseAck_valid


  is_mstProbeAckData_allowen :=
    // ~is_slvAcquire_StateOn &
    ~is_slvGrantData_StateOn &
    ~is_slvGrantAck_StateOn &
    ~is_slvProbe_StateOn &
    ~is_slvProbeAckData_StateOn &
    ~is_slvReleaseData_StateOn &
    ~is_slvReleaseAck_StateOn &
    ~is_mstAcquire_StateOn &
    ~is_mstGrantData_StateOn &
    ~is_mstGrantAck_StateOn &
    is_mstProbe_StateOn &
    ~is_mstProbeAckData_StateOn &
    ~is_mstReleaseData_StateOn &
    ~is_mstReleaseAck_StateOn &
    ~is_slvGrantData_Waiting &
    ~is_slvGrantAck_valid &
    ~is_slvProbe_Waiting &
    ~is_slvProbeAckData_valid &
    ~is_slvReleaseData_valid &
    ~is_slvReleaseAck_Waiting &
    ~is_mstAcquire_Waiting &
    ~is_mstGrantData_valid &
    ~is_mstGrantAck_Waiting &
    ~is_mstProbe_valid &
    is_mstProbeAckData_Waiting &
    ~is_mstReleaseData_Waiting &
    ~is_mstReleaseAck_valid


  is_mstReleaseData_allowen :=
    is_slvAcquire_StateOn &
    ~is_slvGrantData_StateOn &
    ~is_slvGrantAck_StateOn &
    ~is_slvProbe_StateOn &
    ~is_slvProbeAckData_StateOn &
    ~is_slvReleaseData_StateOn &
    ~is_slvReleaseAck_StateOn &
    ~is_mstAcquire_StateOn &
    ~is_mstGrantData_StateOn &
    ~is_mstGrantAck_StateOn &
    // ~is_mstProbe_StateOn & 
    ~is_mstProbeAckData_StateOn &
    ~is_mstReleaseData_StateOn &
    ~is_mstReleaseAck_StateOn &
    is_slvGrantData_Waiting &
    ~is_slvGrantAck_valid &
    ~is_slvProbe_Waiting &
    ~is_slvProbeAckData_valid &
    is_slvReleaseData_valid &
    ~is_slvReleaseAck_Waiting
    ~is_mstAcquire_Waiting &
    ~is_mstGrantData_valid &
    ~is_mstGrantAck_Waiting &
    ~is_mstProbe_valid &
    ~is_mstProbeAckData_Waiting &
    is_mstReleaseData_Waiting &
    ~is_mstReleaseAck_valid

  is_mstReleaseAck_allowen :=
    is_slvAcquire_StateOn &
    ~is_slvGrantData_StateOn &
    ~is_slvGrantAck_StateOn &
    ~is_slvProbe_StateOn &
    ~is_slvProbeAckData_StateOn &
    ~is_slvReleaseData_StateOn &
    ~is_slvReleaseAck_StateOn &
    ~is_mstAcquire_StateOn &
    ~is_mstGrantData_StateOn &
    ~is_mstGrantAck_StateOn &
    ~is_mstProbe_StateOn & 
    ~is_mstProbeAckData_StateOn &
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


    is_slvAcquire_valid := slv_chn_a.valid
    is_slvGrantAck_valid := slv_chn_e.valid
    is_slvProbeAckData_valid := slv_chn_c0.valid
    is_slvReleaseData_valid := slv_chn_c1.valid
    is_mstGrantData_valid := mst_chn_d0.valid
    is_mstProbe_valid := mst_chn_b.valid
    is_mstReleaseAck_valid := mst_chn_d1.valid






  for ( i <- 0 until cb ) yield {
    cache_dat.dat_en_w(i) :=
      info_slvProbeAck_Data_cache_dat_wen(i) |
      info_slvReleaseData_cache_dat_wen(i) |
      info_mstGrantData_cache_dat_wen(i)
  }

  cache_dat.dat_addr_w :=
    Mux1H(Seq(
      info_slvProbeAck_Data_cache_dat_wen.contains(true.B) -> info_slvProbeAck_Data_cache_dat_waddr,
      info_slvReleaseData_cache_dat_wen.contains(true.B) -> info_slvReleaseData_cache_dat_waddr,
      info_mstGrantData_cache_dat_wen.contains(true.B) -> info_mstGrantData_cache_dat_waddr,
    ))

  cache_dat.dat_info_wstrb := 
    Mux1H(Seq(
     info_slvProbeAck_Data_cache_dat_wen.contains(true.B) -> info_slvProbeAck_Data_cache_dat_wstrb,
      info_slvReleaseData_cache_dat_wen.contains(true.B) -> info_slvReleaseData_cache_dat_wstrb,
      info_mstGrantData_cache_dat_wen.contains(true.B) -> info_mstGrantData_cache_dat_wstrb,
    ))

  cache_dat.dat_info_w :=
    Mux1H(Seq(
      info_slvProbeAck_Data_cache_dat_wen.contains(true.B) -> info_slvProbeAck_Data_cache_dat_winfo,
      info_slvReleaseData_cache_dat_wen.contains(true.B) -> info_slvReleaseData_cache_dat_winfo,
      info_mstGrantData_cache_dat_wen.contains(true.B) -> info_mstGrantData_cache_dat_winfo
    )) 

  assert(PopCount(Cat(
    info_slvProbeAck_Data_cache_dat_wen.contains(true.B),
    info_slvReleaseData_cache_dat_wen.contains(true.B),
    info_mstGrantData_cache_dat_wen.contains(true.B)
    )) <= 1.U, 
    "Assert Failed at TLC_L2.scala cache_data_wen should be One-Hot" )

  for ( i <- 0 until cb ) yield {
    cache_dat.dat_en_r(i) :=
      info_slvGrantData_cache_dat_ren(i) |
      info_mstReleaseData_cache_dat_ren(i) |
      info_mstProbeData_cache_dat_ren(i)   
  }


  cache_dat.dat_addr_r :=
    Mux1H(Seq(
      info_slvGrantData_cache_dat_ren.contains(true.B) -> info_slvGrantData_cache_dat_raddr,
      info_mstReleaseData_cache_dat_ren.contains(true.B) -> info_mstReleaseData_cache_dat_raddr,
      info_mstProbeData_cache_dat_ren.contains(true.B) -> info_mstProbeData_cache_dat_raddr
    ))

  assert(PopCount(Cat(
    info_slvGrantData_cache_dat_ren.contains(true.B),
    info_mstReleaseData_cache_dat_ren.contains(true.B),
   info_mstProbeData_cache_dat_ren.contains(true.B)
    )) <= 1.U, 
    "Assert Failed at TLC_L2.scala cache_data_ren should be One-Hot" )


  for ( i <- 0 until cb ) yield {
    cache_tag.tag_en_w(i) := info_mstGrantData_cache_tag_wen(i)    
  }

  cache_tag.tag_addr_w := info_mstGrantData_cache_tag_waddr



  for ( i <- 0 until cb ) yield {
    cache_tag.tag_en_r(i) :=
      info_slvAcquire_cache_tag_ren(i) |
      info_slvGrantData_cache_tag_ren(i) |
      info_mstProbe_cache_tag_ren(i)
  }


  cache_tag.tag_addr_r :=
    Mux1H(Seq(
      info_slvAcquire_cache_tag_ren.contains(true.B)   -> info_slvAcquire_cache_tag_raddr,
      info_slvGrantData_cache_tag_ren.contains(true.B) -> info_slvGrantData_cache_tag_raddr,
      info_mstProbe_cache_tag_ren.contains(true.B) -> info_mstProbe_cache_tag_raddr
    ))

  assert(PopCount(Cat(
    info_slvAcquire_cache_tag_ren.contains(true.B),
    info_slvGrantData_cache_tag_ren.contains(true.B),
    info_mstProbe_cache_tag_ren.contains(true.B)
    )) <= 1.U, 
    "Assert Failed at TLC_L2.scala cache_tag_ren should be One-Hot" )

  for ( i <- 0 until cb; j <- 0 until bk ) yield {
    cache_coh.coh_en_w(i)(j) :=
      info_slvGrantData_cache_coh_wen(i)(j) |
      info_slvProbeAck_Data_cache_coh_wen(i)(j) |
      info_slvReleaseData_cache_coh_wen(i)(j) |
      info_mstGrantAck_cache_coh_wen(i)(j)    
  }



  cache_coh.coh_addr_w :=
    Mux1H(Seq(
      info_slvGrantData_cache_coh_wen.exists((x:Vec[Bool]) => x.contains(true.B))      -> info_slvGrantData_cache_coh_waddr,
      info_slvProbeAck_Data_cache_coh_wen.exists((x:Vec[Bool]) => x.contains(true.B)) -> info_slvProbeAck_Data_cache_coh_waddr,
      info_slvReleaseData_cache_coh_wen.exists((x:Vec[Bool]) => x.contains(true.B)) -> info_slvReleaseData_cache_coh_waddr,
      info_mstGrantAck_cache_coh_wen.exists((x:Vec[Bool]) => x.contains(true.B)) -> info_mstGrantAck_cache_coh_waddr,

      ))

  cache_coh.coh_info_w := 
    Mux1H(Seq(
      info_slvGrantData_cache_coh_wen.exists((x:Vec[Bool]) => x.contains(true.B))      -> info_slvGrantData_cache_coh_winfo,
      info_slvProbeAck_Data_cache_coh_wen.exists((x:Vec[Bool]) => x.contains(true.B)) -> info_slvProbeAck_Data_cache_coh_winfo,
      info_slvReleaseData_cache_coh_wen.exists((x:Vec[Bool]) => x.contains(true.B))   -> info_slvReleaseData_cache_coh_winfo,
      info_mstGrantAck_cache_coh_wen.exists((x:Vec[Bool]) => x.contains(true.B))     -> info_mstGrantAck_cache_coh_winfo,
      ))

  assert(PopCount(Cat(
    info_slvGrantData_cache_coh_wen.exists((x:Vec[Bool]) => x.contains(true.B)),
    info_slvProbeAck_Data_cache_coh_wen.exists((x:Vec[Bool]) => x.contains(true.B)),
    info_slvReleaseData_cache_coh_wen.exists((x:Vec[Bool]) => x.contains(true.B)),
    info_mstGrantAck_cache_coh_wen.exists((x:Vec[Bool]) => x.contains(true.B))
    )) <= 1.U, 
    "Assert Failed at TLC_L2.scala cache_coh_wen should be One-Hot" )



  for ( i <- 0 until cb ) yield {
    cache_coh.coh_en_r(i) :=
      info_slvGrantData_cache_coh_ren(i) |
      info_mstProbeData_cache_coh_ren(i)    
  }

  cache_coh.coh_addr_r :=
    Mux1H(Seq(
      info_slvGrantData_cache_coh_ren.contains(true.B) -> info_slvGrantData_cache_coh_raddr,
      info_mstProbeData_cache_coh_ren.contains(true.B) -> info_mstProbeData_cache_coh_raddr
    ))

  assert(PopCount(Cat(
    info_slvGrantData_cache_coh_ren.contains(true.B),
    info_mstProbeData_cache_coh_ren.contains(true.B)
    )) <= 1.U, 
    "Assert Failed at TLC_L2.scala cache_coh_ren should be One-Hot" )

}


