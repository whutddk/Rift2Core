/*
* @Author: Ruige Lee
* @Date:   2021-03-23 09:31:02
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-03-23 10:43:35
*/



package rift2Core

import chisel3._
import chisel3.util._
import rift2Core.basicElement._



class Rename {

}



class Dispatch extends Module {
	val io = IO(new Bundle{
		val id_dpt = Flipped(new DecoupledIO(new Info_id_dpt()))
	})






}


