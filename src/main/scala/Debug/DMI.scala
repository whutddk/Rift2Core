/*
  Copyright (c) 2020 - 2022 Wuhan University of Technology <295054118@whut.edu.cn>

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

package debug

import chisel3._
import chisel3.util._

import freechips.rocketchip.util._


class DMIReq extends DMIAccess
class DMIResp extends DMIAccess

class DMIIO() extends Bundle {
  val req = new DecoupledIO(new DMIReq())
  val resp = Flipped(new DecoupledIO(new DMIResp))
}

class DMI() extends Module {
  val io = IO(new Bundle {
    val tck = Input(Clock())
    val trstn = Input(Bool())
    val dtm_dmi = Flipped(new DMIIO)

    val dmi_dm = new DMIIO()
  })

  val req_ToAsync = Wire(new AsyncBundle(new DMIReq))
  val resp_ToAsync = Wire(new AsyncBundle(new DMIResp))


  


    io.dmi_dm.req  <> FromAsyncBundle( req_ToAsync )
    resp_ToAsync <> ToAsyncBundle( io.dmi_dm.resp )
    
    withClockAndReset(io.tck, (~io.trstn)) {
      io.dtm_dmi.resp <> FromAsyncBundle( resp_ToAsync )      
      req_ToAsync <> ToAsyncBundle( io.dtm_dmi.req )
}

}
