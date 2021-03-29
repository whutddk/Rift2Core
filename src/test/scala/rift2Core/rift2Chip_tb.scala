package test

/*
* @Author: Ruige Lee
* @Date:   2021-03-18 16:14:36
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-03-24 11:09:31
*/

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



import chisel3._
import rift2Core._
import rift2Core.backend._
import rift2Core.basicElement._



object testMain extends App {

  Driver.execute(args, () => new Bru() )
}



