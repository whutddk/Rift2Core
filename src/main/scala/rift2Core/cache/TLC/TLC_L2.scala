

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

  override def dw = 512
  override def aw = 32
  override def bk = 2
  override def cb = 4
  override def cl = 512
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
    // ~is_slvProbe_StateOn & //优先级高于slvProbe
    // ~is_slvProbeAckData_StateOn & //优先级高于slvProbe
    // ~is_slvReleaseData_StateOn & //优先级高于slvRelease
    // ~is_slvReleaseAck_StateOn & //优先级高于slvRelease
    // ~is_mstAcquire_StateOn & //通过断言，请求时必定是关闭的
    // ~is_mstGrantData_StateOn & //通过断言，请求时必定是关闭的
    // ~is_mstGrantAck_StateOn & //通过断言，请求时必定是关闭的
    // ~is_mstProbe_StateOn & //优先级高于mstProbe
    // ~is_mstProbeAckData_StateOn & //优先级高于mstProbe
    // ~is_mstReleaseData_StateOn & //通过断言，请求时必定是关闭的
    // ~is_mstReleaseAck_StateOn & //通过断言，请求时必定是关闭的
    // ~is_slvAcquire_valid         & // 优先级高于slvAcquire
    // ~is_slvGrantData_Waiting & //通过断言，请求时必定是关闭的
     is_slvGrantAck_valid 
    // ~is_SlvProbe_Waiting & //优先级高于slvProbe
    // ~is_SlvProbeAckData_valid & //优先级高于slvProbe
    // ~is_SlvReleaseData_valid & //优先级高于slvRelease
    // ~is_SlvReleaseAck_Waiting & //优先级高于slvRelease
    // ~is_mstAcquire_Waiting & //通过断言，请求时必定是关闭的
    // ~is_mstGrantData_valid & //通过断言，请求时必定是关闭的
    // ~is_mstGrantAck_Waiting & //通过断言，请求时必定是关闭的
    // ~is_mstProbe_valid & //优先级高于mstProbe
    // ~is_mstProbeAckData_Waiting & //优先级高于mstProbe
    // ~is_mstReleaseData_Waiting & //通过断言，请求时必定是关闭的
    // ~is_mstReleaseAck_valid //通过断言，请求时必定是关闭的

  assert( ~(is_slvGrantAck_valid & (~is_slvAcquire_StateOn | ~is_slvGrantData_StateOn)), "Assert Failed at TLC-L2, when slv GrantAck in chn e, the slv acquire and slv grantdata is state-off, that's impossible!" )
  assert( ~(is_slvGrantAck_valid &
    ( is_mstAcquire_StateOn | is_mstGrantData_StateOn | is_mstGrantAck_StateOn |
     is_mstReleaseData_StateOn | is_mstReleaseAck_StateOn |
     is_slvGrantData_Waiting | is_mstAcquire_Waiting | is_mstGrantData_valid | 
     is_mstGrantAck_Waiting | is_mstReleaseData_Waiting | is_mstReleaseAck_valid )),
     "Assert Failed at TLC-L2, the and Parents/Parasitic message is not resolved when slv grant ack"
  )


  is_slvProbe_allowen :=
    // is_slvAcquire_StateOn & //通过断言，必定打开一个
    // ~is_slvGrantData_StateOn & //可以与slvGrantData并行处理
    // ~is_slvGrantAck_StateOn & //可以与slvGrantack并行处理
    ~is_slvProbe_StateOn & //占用,则不能请求
    ~is_slvProbeAckData_StateOn & //被占用,可能是另一主消息的寄生消息
    // ~is_slvReleaseData_StateOn & //可以与slvRelease并行处理
    // ~is_slvReleaseAck_StateOn & //可以与slvRelease并行处理
    // ~is_mstAcquire_StateOn & //可以与mstAcquire并行处理
    // ~is_mstGrantData_StateOn & //可以与mstAcquire并行处理
    // ~is_mstGrantAck_StateOn & //可以与mstAcquire并行处理
    // is_mstProbe_StateOn & //通过断言，必定打开一个
    // ~is_mstProbeAckData_StateOn & //可以并行处理
    // ~is_mstReleaseData_StateOn  & //可以并行处理
    // ~is_mstReleaseAck_StateOn   & //可以并行处理
    // ~is_slvAcquire_valid        & //可以并行处理
    // ~is_slvGrantData_Waiting & //可以与slvGrantData并行处理
    // ~is_slvGrantAck_valid & //可以与slvGrantack并行处理
     is_slvProbe_Waiting & //请求到来
    ~is_slvProbeAckData_valid //占用
    // ~is_slvReleaseData_valid & //可以与slvRelease并行处理
    // ~is_slvReleaseAck_Waiting & //可以与slvRelease并行处理
    // ~is_mstAcquire_Waiting & //可以与mstAcquire并行处理
    // ~is_mstGrantData_valid & //可以与mstAcquire并行处理
    // ~is_mstGrantAck_Waiting & //可以与mstAcquire并行处理
    // ~is_mstProbe_valid & //可以并行处理
    // ~is_mstProbeAckData_Waiting & //可以并行处理
    // ~is_mstReleaseData_Waiting & //可以并行处理
    // ~is_mstReleaseAck_valid //可以并行处理

  assert( ~(is_slvProbe_Waiting & ( ~is_slvAcquire_StateOn & ~is_mstProbe_StateOn )), "Assert Failed at TLC-L2, slv Probe is request from slv acquire or mst rec probe" )
// assert( ~(is_slvProbe_Waiting & (is_slvProbeAckData_StateOn | )), "Assert Failed at TLC-L2, slv Probe is request with l")



  is_slvProbeAckData_allowen :=
    //  is_slvAcquire_StateOn &  //通过断言，必定打开一个
    ~is_slvGrantData_StateOn & //资源被占用
    // ~is_slvGrantAck_StateOn & //并行操作
    // is_slvProbe_StateOn &  //通过断言，必定打开
    ~is_slvProbeAckData_StateOn & //资源被占用
    ~is_slvReleaseData_StateOn & //资源被占用
    ~is_slvReleaseAck_StateOn & //资源被占用
    ~is_mstAcquire_StateOn & //资源被占用
    ~is_mstGrantData_StateOn & //资源被占用
    ~is_mstGrantAck_StateOn & //资源被占用
    //  is_mstProbe_StateOn & //通过断言，必定打开一个
    ~is_mstProbeAckData_StateOn & //资源被占用
    ~is_mstReleaseData_StateOn & //资源被占用
    ~is_mstReleaseAck_StateOn & //资源被占用
    // ~is_slvAcquire_valid        & //更高优先级
    // ~is_slvGrantData_Waiting & //优先级高于slv Grant
    // ~is_slvGrantAck_valid &  //并行操作
    // ~is_slvProbe_Waiting & //比slvProbe优先级更高
     is_slvProbeAckData_valid & //请求到来
    ~is_slvReleaseData_valid & //资源被占用 ,release 优先级更高
    ~is_slvReleaseAck_Waiting //资源被占用 ,release 优先级更高
    // ~is_mstAcquire_Waiting & //优先级更高
    // ~is_mstGrantData_valid & //优先级更高
    // ~is_mstGrantAck_Waiting & //并行操作
    // ~is_mstProbe_valid & //并行操作
    // ~is_mstProbeAckData_Waiting & //被动优先级更高
    // ~is_mstReleaseData_Waiting & //被动优先级更高
    // ~is_mstReleaseAck_valid //并行操作

  assert( ~(is_slvProbeAckData_valid & (( ~is_slvAcquire_StateOn & ~is_mstProbe_StateOn) | ~is_mstProbe_StateOn)), "Assert Failed, at TLC-L2, request probeack data at a woring state" )


  is_slvReleaseData_allowen :=
    // ~is_slvAcquire_StateOn & //不关心这个状态
    ~is_slvGrantData_StateOn & //资源被占用
    // ~is_slvGrantAck_StateOn & //并行操作
    // ~is_slvProbe_StateOn & //并行操作
    ~is_slvProbeAckData_StateOn & //资源被占用
    ~is_slvReleaseData_StateOn & //资源被占用
    ~is_slvReleaseAck_StateOn & //资源被占用
    // ~is_mstAcquire_StateOn & //并行操作
    ~is_mstGrantData_StateOn &  //资源被占用
    // ~is_mstGrantAck_StateOn & //并行操作
    ~is_mstProbe_StateOn &   //资源被占用
    ~is_mstProbeAckData_StateOn & //资源被占用
    ~is_mstReleaseData_StateOn & //资源被占用
    ~is_mstReleaseAck_StateOn & //并行操作
    // ~is_slvAcquire_valid        & //更高优先级
    // ~is_slvGrantData_Waiting & //更高优先级
    // ~is_slvGrantAck_valid & //并行操作
    // ~is_slvProbe_Waiting &  //并行操作
    // ~is_slvProbeAckData_valid & //更高优先级
    is_slvReleaseData_valid & //请求到来
    ~is_slvReleaseAck_Waiting //资源被占用
    // ~is_mstAcquire_Waiting & //更高优先级
    // ~is_mstGrantData_valid & //更高优先级
    // ~is_mstGrantAck_Waiting & //并行操作
    // ~is_mstProbe_valid & //并行操作
    // ~is_mstProbeAck_Data_Waiting & //更高优先级
    // ~is_mstReleaseData_Waiting &  //更高优先级
    // ~is_mstReleaseAck_valid  //并行操作


  is_slvReleaseAck_allowen :=
    // ~is_slvAcquire_StateOn &  //不关心这个状态
    // ~is_slvGrantData_StateOn &   //并行操作
    // ~is_slvGrantAck_StateOn &   //并行操作
    // ~is_slvProbe_StateOn &   //并行操作
    // ~is_slvProbeAckData_StateOn &   //并行操作
    ~is_slvReleaseData_StateOn &  //资源被占用
    ~is_slvReleaseAck_StateOn & //资源被占用
    // ~is_mstAcquire_StateOn & //并行操作
    // ~is_mstGrantData_StateOn & //并行操作
    // ~is_mstGrantAck_StateOn & //并行操作
    // ~is_mstProbe_StateOn & //并行操作
    // ~is_mstProbeAckData_StateOn & //并行操作
    // is_mstReleaseData_StateOn & //并行操作
    // ~is_mstReleaseAck_StateOn & //并行操作
    // ~is_slvGrantData_Waiting & //并行操作
    // ~is_slvGrantAck_valid & //并行操作
    // ~is_slvProbe_Waiting & //并行操作
    // ~is_slvProbeData_valid & //并行操作
    // ~is_slvProbeAck_valid & //并行操作
    // ~is_slvReleaseData_valid & //更高优先级
    is_slvReleaseAck_Waiting //请求到来
    // ~is_mstAcquire_Waiting & //并行操作
    // ~is_mstGrantData_valid & //并行操作
    // ~is_mstGrantAck_Waiting & //并行操作
    // ~is_mstProbe_valid & //并行操作
    // ~is_mstProbeAck_Waiting & //并行操作
    // ~is_mstProbeData_Waiting & //并行操作
    // ~is_mstReleaseData_Waiting & //并行操作
    // ~is_mstReleaseAck_valid //并行操作


  is_mstAcquire_allowen :=
    // is_slvAcquire_StateOn & //通过断言，必定打开
    // ~is_slvGrantData_StateOn & //通过断言，必定关闭
    // ~is_slvGrantAck_StateOn & //通过断言，必定关闭
    // ~is_slvProbe_StateOn & //并行操作，当slv Probe为mst Probe的寄生消息
    // ~is_slvProbeAckData_StateOn & //并行操作
    // ~is_slvReleaseData_StateOn & //并行操作
    // ~is_slvReleaseAck_StateOn & //并行操作
    ~is_mstAcquire_StateOn & //资源被占用
    // ~is_mstGrantData_StateOn & //通过断言，必定关闭
    // ~is_mstGrantAck_StateOn & //通过断言，必定关闭
    // ~is_mstProbe_StateOn & //并行操作
    // ~is_mstProbeAckData_StateOn & //并行操作
    // ~is_mstReleaseData_StateOn & //并行操作
    // ~is_mstReleaseAck_StateOn & //并行操作
    // ~is_slvAcquire_valid &
    // is_slvGrantData_Waiting & //通过断言，必定打开
    // ~is_slvGrantAck_valid &  //通过断言，必定关闭
    // ~is_slvProbe_Waiting & //并行操作
    // ~is_slvProbeAckData_valid & //并行操作
    // ~is_slvReleaseData_valid & //并行操作
    // ~is_slvReleaseAck_Waiting & //并行操作
    is_mstAcquire_Waiting //请求到来
    // ~is_mstGrantData_valid & //通过断言，必定关闭
    // ~is_mstGrantAck_Waiting & //通过断言，必定关闭
    // ~is_mstProbe_valid & //并行操作
    // ~is_mstProbeAckData_Waiting & //并行操作
    // ~is_mstReleaseData_Waiting & //并行操作
    // ~is_mstReleaseAck_valid //并行操作

  assert( ~(is_mstAcquire_Waiting & (~is_slvAcquire_StateOn | ~is_slvGrantData_Waiting | is_slvGrantAck_valid | is_slvGrantData_StateOn)), "Assert Failed at TLC_L2.scala, mst acquire is the parasitical messages of slv acquire" )
  assert( ~(is_mstAcquire_Waiting & (is_slvGrantAck_StateOn | is_mstGrantData_StateOn | is_mstGrantAck_StateOn | is_mstGrantData_valid | is_mstGrantAck_Waiting)), "Assert Failed at TLC_L2.scala, mst acquire is the parasitical messages of slv acquire, but slv grantack is the resp messages of slv acquire" )


  is_mstGrantData_allowen := 
    //  is_slvAcquire_StateOn & //通过断言，必定打开
    // ~is_slvGrantData_StateOn & //通过断言，必定关闭
    // ~is_slvGrantAck_StateOn & //通过断言，必定关闭
    // ~is_slvProbe_StateOn & //并行操作
    ~is_slvProbeAckData_StateOn & //让出优先级
    ~is_slvReleaseData_StateOn & //让出优先级
    // ~is_slvReleaseAck_StateOn &  //并行操作
    // is_mstAcquire_StateOn & //通过断言，必定打开
    ~is_mstGrantData_StateOn & //资源占用
    // ~is_mstGrantAck_StateOn & //通过断言，必定关闭
    // ~is_mstProbe_StateOn & //通过断言，必定关闭 
    // ~is_mstProbeAckData_StateOn & //通过断言，必定关闭 
    // ~is_mstReleaseData_StateOn & //通过断言，必定关闭 
    // ~is_mstReleaseAck_StateOn & //通过断言，必定关闭 
    // ~is_slvAcquire_valid &
    // is_slvGrantData_Waiting & //通过断言，必定打开
    // ~is_slvGrantAck_valid &  //通过断言，必定关闭
    // ~is_slvProbe_Waiting & //并行操作
    ~is_slvProbeAckData_valid & //让出优先级
    ~is_slvReleaseData_valid & //让出优先级
    // ~is_slvReleaseAck_Waiting & //并行操作
    // ~is_mstAcquire_Waiting & //通过断言，必定关闭 
    is_mstGrantData_valid  //请求到来
    // ~is_mstGrantAck_Waiting & //通过断言，必定关闭 
    // ~is_mstProbe_valid & //通过断言，必定关闭 
    // ~is_mstProbeAckData_Waiting & //通过断言，必定关闭 
    // ~is_mstReleaseData_Waiting & //通过断言，必定关闭 
    // ~is_mstReleaseAck_valid //通过断言，必定关闭 

    assert( ~(is_mstGrantData_valid &
      ( ~is_slvAcquire_StateOn |
        is_slvGrantData_StateOn |
        is_slvGrantAck_StateOn |
        ~is_mstAcquire_StateOn |
        is_mstGrantAck_StateOn |
        is_mstProbe_StateOn |
        is_mstProbeAckData_StateOn |
        is_mstReleaseData_StateOn |
        is_mstReleaseAck_StateOn |
        ~is_slvGrantData_Waiting |
        is_slvGrantAck_valid |
        is_mstAcquire_Waiting |
        is_mstGrantAck_Waiting |
        is_mstProbe_valid |
        is_mstProbeAckData_Waiting |
        is_mstReleaseData_Waiting |
        is_mstReleaseAck_valid 
      )),
      "Assert Failed at TLC-L2.scala, invalid state in mst GrantData" )

  is_mstGrantAck_allowen :=
    // is_slvAcquire_StateOn & //通过断言，必定打开
    // ~is_slvGrantData_StateOn & //通过断言，必定关闭
    // ~is_slvGrantAck_StateOn & //通过断言，必定关闭
    // ~is_slvProbe_StateOn & //并行操作
    // ~is_slvProbeAckData_StateOn & //并行操作
    // ~is_slvReleaseData_StateOn & //并行操作
    // ~is_slvReleaseAck_StateOn & //并行操作
    // is_mstAcquire_StateOn & //通过断言，必定打开
    // is_mstGrantData_StateOn & //通过断言，必定打开
    ~is_mstGrantAck_StateOn & //资源占用
    // ~is_mstProbe_StateOn & //并行操作
    ~is_mstProbeAckData_StateOn &  //并行操作
    ~is_mstReleaseData_StateOn & //并行操作
    ~is_mstReleaseAck_StateOn & //并行操作
    // ~is_slvAcquire_valid &
    // ~is_slvGrantData_Waiting & //通过断言，必定打开
    // ~is_slvGrantAck_valid & //通过断言，必定关闭
    // ~is_SlvProbe_Waiting & //并行操作
    // ~is_SlvProbeData_valid & //并行操作
    // ~is_SlvProbeAck_valid & //并行操作
    // ~is_SlvReleaseData_valid & //并行操作
    // ~is_SlvReleaseAck_Waiting & //并行操作
    // ~is_mstAcquire_Waiting & //通过断言，必定关闭
    // ~is_mstGrantData_valid & //通过断言，必定关闭
    is_mstGrantAck_Waiting //请求操作
    // ~is_mstProbe_valid &
    // ~is_mstProbeAck_Waiting &
    // ~is_mstProbeData_Waiting &
    // ~is_mstReleaseData_Waiting &
    // ~is_mstReleaseAck_valid

  assert(
    ~(is_mstGrantAck_Waiting & (
      ~is_slvAcquire_StateOn | 
      is_slvGrantData_StateOn |
      is_slvGrantAck_StateOn |
      ~is_mstAcquire_StateOn |
      is_mstGrantData_StateOn |
      ~is_slvGrantData_Waiting |
      is_slvGrantAck_valid |
      is_mstAcquire_Waiting |
      is_mstGrantData_valid
    )),
    "Assert Failed at TLC_L2.scala, invalid state in mst Grant ACK"
  )


  is_mstProbe_allowen :=
    // ~is_slvAcquire_StateOn & //抢夺资源
    ~is_slvGrantData_StateOn & //资源占用
    // ~is_slvGrantAck_StateOn & //并行操作
    // ~is_slvProbe_StateOn & //并行操作
    ~is_slvProbeAckData_StateOn & //资源占用
    ~is_slvReleaseData_StateOn & //资源占用
    // ~is_slvReleaseAck_StateOn & //并行操作
    // ~is_mstAcquire_StateOn & //抢夺资源
    // ~is_mstGrantData_StateOn & //并行操作
    // ~is_mstGrantAck_StateOn & //并行操作
    ~is_mstProbe_StateOn & //资源占用
    // ~is_mstProbeAckData_StateOn & //通过断言，必定关闭
    ~is_mstReleaseData_StateOn & //资源占用
    // ~is_mstReleaseAck_StateOn & //并行操作
    // ~is_slvAcquire_valid & //抢夺资源
    // ~is_slvGrantData_Waiting & //抢夺资源
    // ~is_slvGrantAck_valid & //并行操作
    // ~is_slvProbe_Waiting & //并行操作
    // ~is_slvProbeData_valid & //并行操作
    // ~is_slvProbeAck_valid & //并行操作
    ~is_slvReleaseData_valid & //资源占用
    // ~is_slvReleaseAck_Waiting & //并行操作
    // ~is_mstAcquire_Waiting & //并行操作
    // ~is_mstGrantData_valid & //并行操作
    // ~is_mstGrantAck_Waiting & //并行操作
    is_mstProbe_valid  //请求到来
    // ~is_mstProbeAckData_Waiting //通过断言，必定关闭
    // ~is_mstReleaseData_Waiting & //抢夺资源
    // ~is_mstReleaseAck_valid //资源占用

    assert( ~(is_mstProbe_valid & (is_mstProbeAckData_StateOn | is_mstProbeAckData_Waiting)), 
      "Assert Failed at TLC-L2.scala, invalid mstProbe state"
    )




  is_mstProbeAckData_allowen :=
    // ~is_slvAcquire_StateOn & //抢夺资源
    ~is_slvGrantData_StateOn & //资源占用
    // ~is_slvGrantAck_StateOn & //并行操作
    // ~is_slvProbe_StateOn & //并行操作
    ~is_slvProbeAckData_StateOn & //资源占用
    ~is_slvReleaseData_StateOn & //资源占用
    // ~is_slvReleaseAck_StateOn & //并行操作
    // ~is_mstAcquire_StateOn & //抢夺资源
    ~is_mstGrantData_StateOn & //资源占用
    // ~is_mstGrantAck_StateOn & //并行操作
    // is_mstProbe_StateOn & //通过断言，必定打开
    ~is_mstProbeAckData_StateOn & //资源占用
    ~is_mstReleaseData_StateOn & //资源占用
    ~is_mstReleaseAck_StateOn & //资源占用
    // is_slvAcquire_valid & //抢夺资源
    // ~is_slvGrantData_Waiting & //抢夺资源
    // ~is_slvGrantAck_valid & //并行操作
    // ~is_slvProbe_Waiting & //并行操作
    ~is_slvProbeAckData_valid & //资源占用,让步被动消息
    ~is_slvReleaseData_valid & //资源占用,让步被动消息
    // ~is_slvReleaseAck_Waiting & //并行操作
    // ~is_mstAcquire_Waiting & //并行操作
    ~is_mstGrantData_valid & //资源占用,让步被动消息
    // ~is_mstGrantAck_Waiting & //并行操作
    // ~is_mstProbe_valid & //通过断言，必定关闭
    is_mstProbeAckData_Waiting //请求到来
    // ~is_mstReleaseData_Waiting & //抢夺资源
    // ~is_mstReleaseAck_valid //抢夺资源

    assert( ~(is_mstProbeAckData_Waiting & (~is_mstProbe_StateOn | is_mstProbe_valid) ),
      "Assert Failed at TLC_L2.scala, invalid state in mstProbeAckData"
     )

  is_mstReleaseData_allowen :=
    // is_slvAcquire_StateOn & //通过断言，必定打开
    // ~is_slvGrantData_StateOn & //通过断言，必定关闭
    // ~is_slvGrantAck_StateOn & //通过断言，必定关闭
    // ~is_slvProbe_StateOn & //并行操作
    ~is_slvProbeAckData_StateOn & //资源占用,让步被动消息
    ~is_slvReleaseData_StateOn & //资源占用,让步被动消息
    // ~is_slvReleaseAck_StateOn & //并行操作
    // ~is_mstAcquire_StateOn & //通过断言，必定关闭
    ~is_mstGrantData_StateOn & //资源占用,让步被动消息
    // ~is_mstGrantAck_StateOn & //并行操作
    // ~is_mstProbe_StateOn & //并行操作
    ~is_mstProbeAckData_StateOn & //资源占用
    ~is_mstReleaseData_StateOn & //资源占用
    // ~is_mstReleaseAck_StateOn & //并行操作
    // is_slvAcquire_valid & //抢夺资源
    // is_slvGrantData_Waiting & //通过断言，必定打开
    // ~is_slvGrantAck_valid & //并行操作
    // ~is_slvProbe_Waiting & //并行操作
    ~is_slvProbeAckData_valid & //资源占用,让步被动消息
    is_slvReleaseData_valid & //资源占用,让步被动消息
    // ~is_slvReleaseAck_Waiting //并行操作
    // ~is_mstAcquire_Waiting & //通过断言，必定关闭
    // ~is_mstGrantData_valid & //通过断言，必定关闭
    // ~is_mstGrantAck_Waiting & //并行操作
    // ~is_mstProbe_valid & //并行操作
    ~is_mstProbeAckData_Waiting & //资源占用
    is_mstReleaseData_Waiting  //请求到来
    // ~is_mstReleaseAck_valid //通过断言，必定关闭

    assert(
      ~(is_mstReleaseData_Waiting & (~is_slvAcquire_StateOn | is_slvGrantData_StateOn | is_slvGrantAck_StateOn | is_mstAcquire_StateOn | ~is_slvGrantData_Waiting | is_mstAcquire_Waiting | is_mstGrantData_valid | is_mstReleaseAck_valid)),
      "Assert Failed TLC_L2.scala, invalid state in mst release Data"
    )

  is_mstReleaseAck_allowen :=
    // is_slvAcquire_StateOn & //通过断言，必定打开
    // ~is_slvGrantData_StateOn & //通过断言，必定关闭
    // ~is_slvGrantAck_StateOn & //通过断言，必定关闭
    // ~is_slvProbe_StateOn & //并行操作
    // ~is_slvProbeAckData_StateOn & //并行操作
    // ~is_slvReleaseData_StateOn & //并行操作
    // ~is_slvReleaseAck_StateOn & //并行操作
    // ~is_mstAcquire_StateOn & //并行操作
    // ~is_mstGrantData_StateOn & //并行操作
    // ~is_mstGrantAck_StateOn & //并行操作
    // ~is_mstProbe_StateOn & //并行操作
    // ~is_mstProbeAckData_StateOn & //并行操作
    // is_mstReleaseData_StateOn & //通过断言，必定打开
    // is_slvAcquire_valid & //并行操作
    // ~is_slvGrantData_Waiting & //并行操作
    // ~is_slvGrantAck_valid & //并行操作
    // ~is_slvProbe_Waiting & //并行操作
    // ~is_slvProbeData_valid & //并行操作
    // ~is_slvProbeAck_valid & //并行操作
    // ~is_slvReleaseData_valid & //并行操作
    // ~is_slvReleaseAck_Waiting //并行操作
    // ~is_mstAcquire_Waiting & //并行操作
    // ~is_mstGrantData_valid & //并行操作
    // ~is_mstGrantAck_Waiting & //并行操作
    // ~is_mstProbe_valid & //并行操作
    // ~is_mstProbeAck_Waiting & //并行操作
    // ~is_mstProbeData_Waiting & //并行操作
    // ~is_mstReleaseData_Waiting & //并行操作
    is_mstReleaseAck_valid //请求到来

  assert(
    ~(is_mstReleaseAck_valid & (~is_slvAcquire_StateOn | is_slvGrantData_StateOn | is_slvGrantAck_StateOn | ~is_mstReleaseData_StateOn)),
    "Assert Failed TLC_L2.scala, invalid state in mst ReleaseAck"
  )



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
      info_mstGrantData_cache_coh_wen(i)(j)    
  }



  cache_coh.coh_addr_w :=
    Mux1H(Seq(
      info_slvGrantData_cache_coh_wen.exists((x:Vec[Bool]) => x.contains(true.B))      -> info_slvGrantData_cache_coh_waddr,
      info_slvProbeAck_Data_cache_coh_wen.exists((x:Vec[Bool]) => x.contains(true.B)) -> info_slvProbeAck_Data_cache_coh_waddr,
      info_slvReleaseData_cache_coh_wen.exists((x:Vec[Bool]) => x.contains(true.B)) -> info_slvReleaseData_cache_coh_waddr,
      info_mstGrantData_cache_coh_wen.exists((x:Vec[Bool]) => x.contains(true.B)) -> info_mstGrantData_cache_coh_waddr,

      ))

  cache_coh.coh_info_w := 
    Mux1H(Seq(
      info_slvGrantData_cache_coh_wen.exists((x:Vec[Bool]) => x.contains(true.B))      -> info_slvGrantData_cache_coh_winfo,
      info_slvProbeAck_Data_cache_coh_wen.exists((x:Vec[Bool]) => x.contains(true.B)) -> info_slvProbeAck_Data_cache_coh_winfo,
      info_slvReleaseData_cache_coh_wen.exists((x:Vec[Bool]) => x.contains(true.B))   -> info_slvReleaseData_cache_coh_winfo,
      info_mstGrantData_cache_coh_wen.exists((x:Vec[Bool]) => x.contains(true.B))     -> info_mstGrantData_cache_coh_winfo,
      ))

  assert(PopCount(Cat(
    info_slvGrantData_cache_coh_wen.exists((x:Vec[Bool]) => x.contains(true.B)),
    info_slvProbeAck_Data_cache_coh_wen.exists((x:Vec[Bool]) => x.contains(true.B)),
    info_slvReleaseData_cache_coh_wen.exists((x:Vec[Bool]) => x.contains(true.B)),
    info_mstGrantData_cache_coh_wen.exists((x:Vec[Bool]) => x.contains(true.B))
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


