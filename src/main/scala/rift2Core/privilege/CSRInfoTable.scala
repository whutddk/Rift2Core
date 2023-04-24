/*
  Copyright (c) 2020 - 2023 Wuhan University of Technology <295054118@whut.edu.cn>

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

package rift2Core.privilege


case class CSRInfo( val name: String, val address: Int, val isRO: Boolean ){
}



object CSRInfoTable{

  val pmpcfg  = for( i <- 0 until 16 by 2 ) yield { CSRInfo( name = s"pmpcfg$i",     address = 0x3A0+i, isRO = false ) }
  val pmpaddr = for( i <- 0 until 16*8 )    yield { CSRInfo( name = s"pmpaddr$i",    address = 0x3B0+i, isRO = false ) }
  val mhpmcounter = for( i <- 3 until 32 )  yield { CSRInfo( name = s"mhpmcounter$i",address = 0xB00+i, isRO = false ) }
  val mhpmevent  = for( i <- 3 until 32 )   yield { CSRInfo( name = s"mhpmevent$i",  address = 0x320+i, isRO = false ) }
  val hpmcounter =for( i <- 3 until 32 )    yield { CSRInfo( name = s"hpmcounter$i", address = 0xC00+i, isRO = false ) }

  val XPUCSRGroup: Seq[CSRInfo] =
    Seq(
      CSRInfo( name = "ustatus",       address = 0x000, isRO = false ),
      CSRInfo( name = "uie",           address = 0x004, isRO = false ),
      CSRInfo( name = "utvec",         address = 0x005, isRO = false ),
      CSRInfo( name = "uscratch",      address = 0x040, isRO = false ),
      CSRInfo( name = "uepc",          address = 0x041, isRO = false ),
      CSRInfo( name = "ucause",        address = 0x042, isRO = false ),
      CSRInfo( name = "utval",         address = 0x043, isRO = false ),
      CSRInfo( name = "uip",           address = 0x044, isRO = false ),
      CSRInfo( name = "cycle",         address = 0xC00, isRO = true  ),
      CSRInfo( name = "time",          address = 0xC01, isRO = true  ),
      CSRInfo( name = "instret",       address = 0xC02, isRO = true  ),
      CSRInfo( name = "sstatus",       address = 0x100, isRO = false ),
      CSRInfo( name = "sedeleg",       address = 0x102, isRO = false ),
      CSRInfo( name = "sideleg",       address = 0x103, isRO = false ),
      CSRInfo( name = "sie",           address = 0x104, isRO = false ),
      CSRInfo( name = "stvec",         address = 0x105, isRO = false ),
      CSRInfo( name = "scounteren",    address = 0x106, isRO = false ),
      CSRInfo( name = "sscratch",      address = 0x140, isRO = false ),
      CSRInfo( name = "sepc",          address = 0x141, isRO = false ),
      CSRInfo( name = "scause",        address = 0x142, isRO = false ),
      CSRInfo( name = "stval",         address = 0x143, isRO = false ),
      CSRInfo( name = "sip",           address = 0x144, isRO = false ),
      CSRInfo( name = "satp",          address = 0x180, isRO = false ),
      CSRInfo( name = "hstatus",       address = 0x600, isRO = false ),
      CSRInfo( name = "hedeleg",       address = 0x602, isRO = false ),
      CSRInfo( name = "hideleg",       address = 0x603, isRO = false ),
      CSRInfo( name = "hie",           address = 0x604, isRO = false ),
      CSRInfo( name = "hcounteren",    address = 0x606, isRO = false ),
      CSRInfo( name = "hgeie",         address = 0x607, isRO = false ),
      CSRInfo( name = "htval",         address = 0x643, isRO = false ),
      CSRInfo( name = "hip",           address = 0x644, isRO = false ),
      CSRInfo( name = "hvip",          address = 0x645, isRO = false ),
      CSRInfo( name = "htinst",        address = 0x64A, isRO = false ),
      CSRInfo( name = "hgeip",         address = 0xE12, isRO = false ),
      CSRInfo( name = "hgatp",         address = 0x680, isRO = false ),
      CSRInfo( name = "htimedelta",    address = 0x605, isRO = false ),
      CSRInfo( name = "vsstatus",      address = 0x200, isRO = false ),
      CSRInfo( name = "vsie",          address = 0x204, isRO = false ),
      CSRInfo( name = "vstvec",        address = 0x205, isRO = false ),
      CSRInfo( name = "vsscratch",     address = 0x240, isRO = false ),
      CSRInfo( name = "vsepc",         address = 0x241, isRO = false ),
      CSRInfo( name = "vscause",       address = 0x242, isRO = false ),
      CSRInfo( name = "vstval",        address = 0x243, isRO = false ),
      CSRInfo( name = "vsip",          address = 0x244, isRO = false ),
      CSRInfo( name = "vsatp",         address = 0x280, isRO = false ),
      CSRInfo( name = "mvendorid",     address = 0xF11, isRO = true  ),
      CSRInfo( name = "marchid",       address = 0xF12, isRO = true  ),
      CSRInfo( name = "mimpid",        address = 0xF13, isRO = true ),
      CSRInfo( name = "mhartid",       address = 0xF14, isRO = true ),
      CSRInfo( name = "mstatus",       address = 0x300, isRO = false ),
      CSRInfo( name = "misa",          address = 0x301, isRO = true  ),
      CSRInfo( name = "medeleg",       address = 0x302, isRO = false ),
      CSRInfo( name = "mideleg",       address = 0x303, isRO = false ),
      CSRInfo( name = "mie",           address = 0x304, isRO = false ),
      CSRInfo( name = "mtvec",         address = 0x305, isRO = false ),
      CSRInfo( name = "mcounteren",    address = 0x306, isRO = false ),
      CSRInfo( name = "mscratch",      address = 0x340, isRO = false ),
      CSRInfo( name = "mepc",          address = 0x341, isRO = false ),
      CSRInfo( name = "mcause",        address = 0x342, isRO = false ),
      CSRInfo( name = "mtval",         address = 0x343, isRO = false ),
      CSRInfo( name = "mip",           address = 0x344, isRO = false ),
      CSRInfo( name = "mtinst",        address = 0x34A, isRO = false ),
      CSRInfo( name = "mtval2",        address = 0x34B, isRO = false ),
      CSRInfo( name = "mcycle",        address = 0xB00, isRO = false ),
      CSRInfo( name = "minstret",      address = 0xB02, isRO = false ),
      CSRInfo( name = "mcountinhibit", address = 0x320, isRO = false ),
      CSRInfo( name = "tselect",       address = 0x7A0, isRO = false ),
      CSRInfo( name = "tdata1",        address = 0x7A1, isRO = false ),
      CSRInfo( name = "tdata2",        address = 0x7A2, isRO = false ),
      CSRInfo( name = "tdata3",        address = 0x7A3, isRO = false ),
      CSRInfo( name = "dcsr",          address = 0x7B0, isRO = false ),
      CSRInfo( name = "dpc",           address = 0x7B1, isRO = false ),
      CSRInfo( name = "dscratch0",     address = 0x7B2, isRO = false ),
      CSRInfo( name = "dscratch1",     address = 0x7B3, isRO = false ),
      CSRInfo( name = "dscratch2",     address = 0x7B4, isRO = false ),
      CSRInfo( name = "mcountinhibit", address = 0x320, isRO = false ),
    ) ++ pmpcfg ++ pmpaddr ++ mhpmcounter ++ mhpmevent ++ hpmcounter
  
  val FPUCSRGroup =
    Seq(
      CSRInfo( name = "fflags",        address = 0x001, isRO = false ),
      CSRInfo( name = "frm",           address = 0x002, isRO = false ),
      CSRInfo( name = "fcsr",          address = 0x003, isRO = false ),
    )

  val VPUCSRGroup = 
    Seq(
      CSRInfo( name = "vstart",        address = 0x008, isRO = false ),
      CSRInfo( name = "vxsat",         address = 0x009, isRO = false ),
      CSRInfo( name = "vxrm",          address = 0x00A, isRO = false ),
      CSRInfo( name = "vcsr",          address = 0x00F, isRO = false ),
      CSRInfo( name = "vl",            address = 0xC20, isRO = false ),
      CSRInfo( name = "type",          address = 0xC21, isRO = false ),
      CSRInfo( name = "vConfig",       address = 0xFFE, isRO = false ),
      CSRInfo( name = "vlenb",         address = 0xC22, isRO = true  ),
    )

  val CSRGroup = XPUCSRGroup ++ FPUCSRGroup ++ VPUCSRGroup

}
  
  
