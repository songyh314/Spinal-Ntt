package Ntt.DataPath
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import Ntt.NttCfg._
import myRam._
import myTools._
import spinal.lib.eda.bench.Rtl
import spinal.lib.eda.xilinx.VivadoFlow
import scala.collection.mutable

object memForwardCtrlOptSim extends App {
  val cfg = new NttCfgParam(nttPoint = 32, paraNum = 4, mode = modeCfg(useTwFile = false))
  val dut = SimConfig.withXSim.withWave.workspacePath("NttOpt/sim/DataPath").compile(new memForwardCtrlOpt(cfg))
  dut.doSim("test") { dut =>
    import dut._
    clockDomain.forkStimulus(10 ns)
    io.ctrl.isCal #= false
    io.ctrl.isNtt #= true
    io.ctrl.isOutSideRead #= false
    io.ctrl.isOutSideWrite #= true
    io.outsideAddrOri.valid #= false
    io.bfuRdAddrOri.valid #= false
    io.bfuRdIdxOri.valid #= false
    clockDomain.waitSampling(10)
    for (i <- 0 until 32){
      io.outsideAddrOri.valid #= true
      io.outsideIdxOri.valid #= true
      io.outsideWrDataArray.valid #= true
      io.outsideAddrOri.payload.foreach(item => item #= BigInt(i >> 3))
      io.outsideIdxOri.payload.foreach{item => item #= (i%8)}
      io.outsideWrDataArray.payload #= i
      clockDomain.waitSampling()
      io.outsideAddrOri.valid #= false
      io.outsideIdxOri.valid #= false
      io.outsideWrDataArray.valid #= false
    }
    clockDomain.waitSampling(10)

  }
}

object DataPathTopOptSim extends App{
  val cfg = new NttCfgParam(nttPoint = 32,paraNum = 4,mode = modeCfg(useTwFile = false))
  val dut = SimConfig.withXSim.withWave.workspacePath("NttOpt/sim/DataPath").compile(new DataPathTopOpt(cfg))
  dut.doSim("test"){dut =>
    import dut._
    clockDomain.forkStimulus(10 ns)
    io.ctrl.isCal #= false
    io.ctrl.isNtt #= true
    io.ctrl.isOutSideRead #= false
    io.ctrl.isOutSideWrite #= true
    io.outsideAddrOri.valid #= false
    io.outsideIdxOri.valid #= false
    io.bfuRdAddrOri.valid #= false
    io.bfuRdIdxOri.valid #= false
    clockDomain.waitSampling(10)
    for (i <- 0 until 32){
      io.outsideAddrOri.valid #= true
      io.outsideIdxOri.valid #= true
      io.outsideWrDataArray.valid #= true
      io.outsideAddrOri.payload.foreach(item => item #= BigInt(i >> 3))
      io.outsideIdxOri.payload.foreach{item => item #= (i%8)}
      io.outsideWrDataArray.payload #= i
      clockDomain.waitSampling()
      io.outsideAddrOri.valid #= false
      io.outsideIdxOri.valid #= false
      io.outsideWrDataArray.valid #= false
    }
    clockDomain.waitSampling(10)
  }
}