package Ntt.DataPath
import Ntt.NttCfg.NttCfgParam
import myTools.StreamRenameUtil
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import Ntt.NttCfg._
import myRam._
import myTools._
import Ntt.CTRL._
import spinal.lib.eda.bench.Rtl
import spinal.lib.eda.xilinx.VivadoFlow

case class CtrlDecodeUnion(g: NttCfgParam) extends Component {
  val io = new Bundle {
    val start = in Bool ()
    val isNtt = in Bool ()
    val idle = out Bool()
  }
  val ctrl = new CtrlOptAddr(g)
  ctrl.io.isNtt := io.isNtt
  ctrl.io.start := io.start
  io.idle := ctrl.io.idle
  val dec = idxDecode(g)
  dec.io.addrOri := ctrl.io.RdAddrOri.payload
  dec.io.idxOri := ctrl.io.RdLsbOri.payload
}

object CtrlDecodeUnionSim extends App {
  val period = 10
  val dut = SimConfig.withXSim.withWave.workspacePath("./NttOpt/sim/UnionSim").compile(new CtrlDecodeUnion(NttCfgParam(nttPoint = 128, paraNum = 8)))
  dut.doSim("test") { dut =>
    import dut._
    SimTimeout(5000 * period)
    clockDomain.forkStimulus(period)
    clockDomain.waitSampling(10)
    io.start #= true
    io.isNtt #= true
    clockDomain.waitSampling()
    io.start #= false
    clockDomain.waitSampling(10)
    clockDomain.waitActiveEdgeWhere(io.idle.toBoolean)
    clockDomain.waitSampling(10)
    io.isNtt #= false
    io.start #= true
    clockDomain.waitSampling()
    io.start #= false
    clockDomain.waitSampling(10)
    clockDomain.waitActiveEdgeWhere(io.idle.toBoolean)
    clockDomain.waitSampling(10)
    simSuccess()
  }
}

case class ctrl_memForwardCtrl_union(g:NttCfgParam) extends Component {
  val io = new Bundle {
    val start = in Bool ()
    val isNtt = in Bool ()
    val idle = out Bool()
    val isCal = in Bool()
    val isOutRead = in Bool()
    val isOutWrite = in Bool()

    val outsideAddrOri = slave Flow Vec(UInt(g.BankAddrWidth bits), g.radix)
    val outsideIdxOri = slave Flow Vec(UInt(g.BankIndexWidth bits), g.BI)
  }

  val ctrl = CtrlOptAddr(g)
  val dut = memForwardCtrl(g)
  ctrl.io.isNtt := io.isNtt
  ctrl.io.start := io.start
  io.idle := ctrl.io.idle

  dut.io.ctrl.isCal := io.isCal
  dut.io.ctrl.isNtt := io.isNtt
  dut.io.ctrl.isOutSideWrite := io.isOutWrite
  dut.io.ctrl.isOutSideRead := io.isOutRead
  dut.io.bfuRdAddrOri := ctrl.io.RdAddrOri
  dut.io.bfuRdIdxOri := ctrl.io.RdLsbOri
  dut.io.outsideAddrOri := io.outsideAddrOri
  dut.io.outsideIdxOri := io.outsideIdxOri
}

object ctrl_memForwardCtrl_unionSim extends App {
  val period = 10
  val dut = SimConfig.withXSim.withWave.workspacePath("./NttOpt/sim/UnionSim").compile(new ctrl_memForwardCtrl_union(NttCfgParam(nttPoint = 128, paraNum = 8)))
  dut.doSim("test") { dut =>
    import dut._
    SimTimeout(5000 * period)
    clockDomain.forkStimulus(period)
    clockDomain.waitSampling(10)
    io.start #= true
    io.isNtt #= true
    io.isCal #= true
    io.isOutRead #= false
    io.isOutWrite #= false
    io.outsideAddrOri.valid #= false
    io.outsideIdxOri.valid #= false
    clockDomain.waitSampling()
    io.start #= false
    clockDomain.waitSampling(10)
    clockDomain.waitActiveEdgeWhere(io.idle.toBoolean)
    clockDomain.waitSampling(10)
    io.isNtt #= false
    io.start #= true
    clockDomain.waitSampling()
    io.start #= false
    clockDomain.waitSampling(10)
    clockDomain.waitActiveEdgeWhere(io.idle.toBoolean)
    clockDomain.waitSampling(10)

    io.isCal #= false
    io.isOutRead #= true
    clockDomain.waitSampling(10)
    val seq1 = Seq.range(0, g.BI).map(item => BigInt(item))
    for (i <- 0 until (g.nttPoint/g.BI)) {
      io.outsideIdxOri.valid #= true
      io.outsideAddrOri.valid #= true
      io.outsideAddrOri.payload.foreach(item => item #= i)
      io.outsideIdxOri.payload.zip(seq1).foreach{case(t1,t2) => t1 #= t2}
      clockDomain.waitSampling()
      io.outsideIdxOri.valid #= false
      io.outsideAddrOri.valid #= false
    }
    clockDomain.waitSampling(10)
    io.isOutRead #= false


  }
}


case class ctrlpath_datapath_union(g:NttCfgParam) extends Component {
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

object ctrlpath_datapath_unionSim extends App {
  val period = 10
  val dut = SimConfig.withXSim.withWave.workspacePath("./NttOpt/sim/UnionSim").compile(new ctrlpath_datapath_union(NttCfgParam(nttPoint = 128,paraNum = 4)))
  dut.doSim("test") { dut =>
    import dut._
    SimTimeout(4000 * period)
    clockDomain.forkStimulus(period)
    io.ctrl.isNtt #= false

    io.start #= false
    io.ctrl.isCal #= false
    io.ctrl.isOutSideRead #= false
    io.ctrl.isOutSideWrite #= false
    io.outsideAddrOri.valid #= false
    io.outsideIdxOri.valid #= false
    clockDomain.waitSampling(10)

    val seq1 = Seq.range(0, g.BI).map(item => BigInt(item))
    io.ctrl.isOutSideWrite #= true
    clockDomain.waitSampling()
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
    io.outsideAddrOri.valid #= false
    io.outsideIdxOri.valid #= false
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
    clockDomain.waitSampling(10)
    io.ctrl.isCal #= false

    io.ctrl.isNtt #= false
    io.ctrl.isCal #= true
    clockDomain.waitSampling()
    io.start #= true
    clockDomain.waitSampling()
    io.start #= false
    clockDomain.waitActiveEdgeWhere(io.idle.toBoolean)
    clockDomain.waitSampling(10)
  }
}