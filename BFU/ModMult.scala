package Ntt.BFU

import Ntt.NttCfg.{NttCfgParam, multPayload}
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.eda.bench.Rtl
import spinal.lib.eda.xilinx.VivadoFlow

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class ModMult(g: NttCfgParam) extends Component {
  val io = new Bundle {
    val dataIn = slave Flow (multPayload(g))
    val dataOut = master Flow (UInt(g.width bits))
  }
  val uMult = new Mult(g)
  val uMod = new FastMod2414(g)
  uMult.io.dataIn := io.dataIn
  uMod.io.dataIn := uMult.io.dataOut
  io.dataOut := uMod.io.dataOut
}

class ModMultCluster(g: NttCfgParam) extends Component {
  this.setDefinitionName(s"ModMult_Q${g.P.M}_${g.P.N}")
  val io = new Bundle {
    val dataIn = slave Flow (multPayload(g))
    val dataOut = master Flow (UInt(g.width bits))
  }
  val uMult = new Mult(g)
  uMult.io.dataIn := io.dataIn
  val ret =  g.P.M match {
    case 14 => FastMod1412(uMult.io.dataOut,g)
    case 24 => FastMod2414(uMult.io.dataOut,g)
    case 64 => FastMod6432(uMult.io.dataOut,g)
  }
  io.dataOut := ret
//  uMod.io.dataIn := uMult.io.dataOut
//  io.dataOut := uMod.io.dataOut
}
