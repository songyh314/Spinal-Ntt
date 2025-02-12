package Ntt.CTRL

import Ntt.DataPath.AddrDecode
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import Ntt.NttCfg._
import spinal.core.sim.SimConfigLegacy
import spinal.lib.eda.bench.Rtl
import spinal.lib.eda.xilinx.VivadoFlow
import spinal.lib.fsm._


object CtrlOptAddrGenV extends App {
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "NttOpt/rtl/Ctrl",
    genLineComments = true
  ).generate(new CtrlOptAddr(NttCfgParam(paraNum = 32, debug = false)))
}



object CtrlOptAddrSim extends App {
  val period = 10
  val dut = SimConfig.withXSim.withWave.workspacePath("./NttOpt/sim").compile(new CtrlOptAddr(NttCfgParam(nttPoint = 128, paraNum = 4)))
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


object CtrlOptAddrVivadoFlow extends App {
  val workspace = "NttOpt/fpga/CtrlOptAddr"
  val vivadopath = "/opt/Xilinx/Vivado/2023.1/bin"
  val family = "Zynq UltraScale+ MPSoCS"
  val device = "xczu9eg-ffvb1156-2-i"
  val frequency = 300 MHz
  val cpu = 16
  val rtl = new Rtl {
    override def getName(): String = "CtrlOptAddr"
    override def getRtlPath(): String = "/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/rtl/Ctrl/CtrlOptAddr.v"
  }
  val flow = VivadoFlow(vivadopath, workspace, rtl, family, device, frequency, cpu)
  println(s"${family} -> ${(flow.getFMax / 1e6).toInt} MHz ${flow.getArea}")
}

