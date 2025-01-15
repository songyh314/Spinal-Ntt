package Ntt.DataPath
import Ntt.NttCfg.NttCfg2414
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

case class CtrlDecodeUnion(g: NttCfg2414) extends Component {
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
  val dut = SimConfig.withXSim.withWave.workspacePath("./NttOpt/sim/UnionSim").compile(new CtrlDecodeUnion(NttCfg2414(nttPoint = 128, paraNum = 4)))
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

case class ctrl_memForwardCtrl_union(g:NttCfg2414) extends Component {
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
  val dut = SimConfig.withXSim.withWave.workspacePath("./NttOpt/sim/UnionSim").compile(new ctrl_memForwardCtrl_union(NttCfg2414(nttPoint = 128, paraNum = 4)))
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