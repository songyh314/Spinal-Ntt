package Ntt
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.eda.bench.Rtl
import spinal.lib.eda.xilinx.VivadoFlow
import test._
import NttCfg._
import BFU._
import CTRL._
import DataPath._
import myRam._

import scala.collection.mutable.ArrayBuffer

case class CtrlMemTop(g:NttCfg2414) extends Component {
  val io = new Bundle{
    val start = in Bool ()
    val idle = out Bool ()
    val ctrl = in(CtrlBus())
    val outsideAddrOri = slave Flow Vec(UInt(g.BankAddrWidth bits), g.radix)
    val outsideIdxOri = slave Flow Vec(UInt(g.BankIndexWidth bits), g.BI)
    val outsideRdDataArray = master Flow Vec(Bits(g.width bits), g.BI)
    val outsideWrDataArray = slave Flow Vec(Bits(g.width bits), g.BI)

    val NttWriteBack = Array.fill(g.paraNum)(slave Flow DataPayload(g))
    val NttPayload = Array.fill(g.paraNum)(master Flow BfuPayload(g))
  }
  val ctrl = new CtrlOptAddr(g)
  val dut = new DataPathTop(g)
  ctrl.io.start := io.start
  ctrl.io.isNtt := io.ctrl.isNtt

  io.idle := ctrl.io.idle
  dut.io.ctrl := io.ctrl
  dut.io.bfuRdAddrOri := ctrl.io.RdAddrOri
  dut.io.bfuRdIdxOri := ctrl.io.RdLsbOri
  dut.io.twBus := ctrl.io.TwBus

  dut.io.outsideAddrOri := io.outsideAddrOri
  dut.io.outsideIdxOri := io.outsideIdxOri
  io.outsideRdDataArray := dut.io.outsideRdDataArray
  dut.io.outsideWrDataArray := io.outsideWrDataArray

  dut.io.NttWriteBack.zip(io.NttWriteBack).foreach{case(t1,t2) => t1 := t2}
  io.NttPayload.zip(dut.io.NttPayload).foreach{case(t1,t2) => t1 := t2}
}


case class NttTop(g: NttCfg2414, debug: Boolean = false) extends Component {
  val io = new Bundle {
    val start = in Bool ()

    val idle = out Bool ()
    val ctrl = in(CtrlBus())

    val outsideAddrOri = slave Flow Vec(UInt(g.BankAddrWidth bits), g.radix)
    val outsideIdxOri = slave Flow Vec(UInt(g.BankIndexWidth bits), g.BI)
    val outsideRdDataArray = master Flow Vec(Bits(g.width bits), g.BI)
    val outsideWrDataArray = slave Flow Vec(Bits(g.width bits), g.BI)

    val dataOut = g.nttSimPublic generate Array.fill(g.paraNum)(master Flow (DataPayload(g)))
  }
  noIoPrefix()
  val ctrlMem = new CtrlMemTop(g)
  val bfuArray = new BfuArray(g, debug)
  io.idle := ctrlMem.io.idle
  ctrlMem.io.start := io.start
  ctrlMem.io.ctrl := io.ctrl
  ctrlMem.io.outsideAddrOri := io.outsideAddrOri
  ctrlMem.io.outsideIdxOri := io.outsideIdxOri
  io.outsideRdDataArray := ctrlMem.io.outsideRdDataArray
  ctrlMem.io.outsideWrDataArray := io.outsideWrDataArray
  bfuArray.io.isNtt := io.ctrl.isNtt
  bfuArray.io.dataIn.toSeq.zip(ctrlMem.io.NttPayload.toSeq).foreach { case (t1, t2) => t1 := RegNext(t2) }
  ctrlMem.io.NttWriteBack.toSeq.zip(bfuArray.io.dataOut.toSeq).foreach { case (t1, t2) => t1 := RegNext(t2) }
  if (g.nttSimPublic) {
    io.dataOut.toSeq.zip(bfuArray.io.dataOut.toSeq).foreach{case(t1,t2) => t1 := t2}
  }
}
object NttTopGenV extends App {
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "NttOpt/rtl/NttTop",
    genLineComments = true
  ).generate(new NttTop(NttCfg2414()))
}

object NttTopSim extends App {
  val period = 10
  val path = ArrayBuffer("/PRJ/SpinalHDL-prj/PRJ/myTest/test/hw/spinal/Ntt/xilinx_ip/")
  val dut = SimConfig.withXSim
    .withXilinxDevice("xczu9eg-ffvb1156-2-i")
    .workspacePath("./NttOpt/sim/")
    .withXSimSourcesPaths(path, path)
    .withWave
    .compile(new NttTop(NttCfg2414(nttPoint = 128,paraNum = 4),debug = true))
  dut.doSim("test") { dut =>
    import dut._
    SimTimeout(4000 * period)
    clockDomain.forkStimulus(period)

    io.start #= false
    io.start #= false
    io.ctrl.isCal #= false
    io.ctrl.isOutSideRead #= false
    io.ctrl.isOutSideWrite #= false
    io.outsideAddrOri.valid #= false
    io.outsideIdxOri.valid #= false
    clockDomain.waitSampling(10)


    io.ctrl.isOutSideWrite #= true
    clockDomain.waitSampling()
    val seq1 = Seq.range(0, g.BI).map(item => BigInt(item))
    for (i <- 0 until g.nttPoint / g.BI) {
      io.outsideAddrOri.valid #= true
      io.outsideIdxOri.valid #= true
      (0 until g.BI).map { j => g.BI * i + j }.zip(io.outsideWrDataArray.payload).foreach { case (t1, t2) => t2 #= t1 }
      io.outsideAddrOri.payload.foreach(item => item #= i)
      io.outsideIdxOri.payload.zip(seq1).foreach{case(t1,t2) => t1 #= t2}
      clockDomain.waitSampling()
      io.outsideAddrOri.valid #= false
    }
    clockDomain.waitSampling(10)
    io.ctrl.isOutSideWrite #= false
    clockDomain.waitSampling(10)

    io.ctrl.isOutSideRead #= true
    clockDomain.waitSampling()
    for (i <- 0 until g.nttPoint / g.BI) {
      io.outsideAddrOri.valid #= true
      io.outsideIdxOri.valid #= true
      io.outsideAddrOri.payload.foreach(item => item #= i)
      io.outsideIdxOri.payload.zip(seq1).foreach{case(t1,t2) => t1 #= t2}
      clockDomain.waitSampling()
      io.outsideAddrOri.valid #= false
    }
    clockDomain.waitSampling(10)
    io.ctrl.isOutSideRead #= false
    clockDomain.waitSampling(10)

    io.ctrl.isNtt #= true
    io.ctrl.isCal #= true
    clockDomain.waitSampling()
    io.start #= true
    clockDomain.waitSampling()
    io.start #= false
    clockDomain.waitActiveEdgeWhere(io.idle.toBoolean)
    clockDomain.waitSampling(20)
    io.ctrl.isCal #= false
    clockDomain.waitSampling(20)
//
//    io.ctrl.isNtt #= false
//    io.ctrl.isCal #= true
//    clockDomain.waitSampling()
//    io.start #= true
//    clockDomain.waitSampling()
//    io.start #= false
//    clockDomain.waitActiveEdgeWhere(io.idle.toBoolean)
//    clockDomain.waitSampling(20)
//    io.ctrl.isCal #= false
//    clockDomain.waitSampling(20)

    io.ctrl.isOutSideRead #= true
    clockDomain.waitSampling()
    for (i <- 0 until g.nttPoint / g.BI) {
      io.outsideAddrOri.valid #= true
      io.outsideIdxOri.valid #= true
      io.outsideAddrOri.payload.foreach(item => item #= i)
      io.outsideIdxOri.payload.zip(seq1).foreach{case(t1,t2) => t1 #= t2}
      clockDomain.waitSampling()
      io.outsideAddrOri.valid #= false
    }
    clockDomain.waitSampling(10)
    io.ctrl.isOutSideRead #= false
    clockDomain.waitSampling(10)
  }
}