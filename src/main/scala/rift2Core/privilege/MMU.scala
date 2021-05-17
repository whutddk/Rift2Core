/*
* @Author: Ruige Lee
* @Date:   2021-05-17 17:04:01
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-05-17 17:34:32
*/


package rift2Core.privilege


import chisel3._
import chisel3.util._

class Info_pte_sv39 extends Bundle {
	val V = Bool()
	val R = Bool()
	val W = Bool()
	val X = Bool()
	val U = Bool()
	val G = Bool()
	val A = Bool()
	val D = Bool()
	val RSW = UInt(2.W)

	val PPN = MixedVec( Seq( UInt(9.W), UInt(9.W), UInt(26.W) ) )

}




object Page {

	def apply( info: UInt ) = {
		val pte = new Info_pte_sv39

		pte.V      := info(0).asBool
		pte.R      := info(1).asBool
		pte.W      := info(2).asBool
		pte.X      := info(3).asBool
		pte.U      := info(4).asBool
		pte.G      := info(5).asBool
		pte.A      := info(6).asBool
		pte.D      := info(7).asBool
		pte.RSW    := info(9,8)
		pte.PPN(0) := info(18,10)
		pte.PPN(1) := info(27,19)
		pte.PPN(2) := info(53,28)

		return pte
	}


}

class TLB {

}

class PTW {
	
}


