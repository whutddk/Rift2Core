
/*
  Copyright (c) 2020 - 2021 Ruige Lee <295054118@qq.com>

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
import chisel3.util.random._


import tilelink._
import axi._
import base._


abstract class TLC_mst_axi( dw:Int, bk:Int, cb:Int, cl:Int, mst_num:Int, mst_size:Int ) extends TLC_slv_port( dw, bk, cb, cl, mst_num, mst_size ) {

}


abstract class TLC_mst_tlc( dw:Int, bk:Int, cb:Int, cl:Int, mst_num:Int, mst_size:Int ) extends TLC_slv_port( dw, bk, cb, cl, mst_num, mst_size ) {

}
