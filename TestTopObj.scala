package Ntt

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.eda.bench.Rtl
import spinal.lib.eda.xilinx.VivadoFlow
import test._
import NttCfg._
import BFU._
import CTRL._
import DataPath._

import scala.collection.mutable.ArrayBuffer

object TestTopSimFlow extends App {
  val cfg = new NttCfgParam(P = PrimeCfg(24,14),Bfu = BfuParamCfg(24,"9eg"),nttPoint = 1024, paraNum = 4)
  val path = ArrayBuffer("/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/IP/mul/")
  val dut = SimConfig.withXSim
    .withXilinxDevice(cfg.device)
    .withXSimSourcesPaths(path, path)
    .workspacePath("./NttOpt/sim/Test")
    .withWave
    .compile(new TestTop(cfg))
  val period = 10
  dut.doSimUntilVoid("test"){ dut =>
    SimTimeout(5000*10*1000)
    import dut._
    io.flush #= false
    clockDomain.forkStimulus(period ns)
    clockDomain.assertReset()
    clockDomain.waitSampling(100)
    clockDomain.deassertReset()
    clockDomain.waitSampling(100)
    io.flush #= true
    clockDomain.waitSampling(100)
    io.flush #= false
    clockDomain.waitSampling(100)
    io.test_start #= true
    clockDomain.waitSampling()
    io.test_start #= false
//    clockDomain.waitSamplingWhere(io.fin.toBoolean)
    clockDomain.waitSampling(4000)
    simSuccess()
  }
}

object TestTopVivadoFlow extends App {
  val g = NttCfgParam(P= PrimeCfg(24,14),Bfu = BfuParamCfg(24,"9eg",spiltMul = false),nttPoint = 1024, paraNum = 4, nttSimPublic = false)
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "NttOpt/rtl/Top",
    genLineComments = true
  ).generate(new TestTop(g))

  val workspace = "NttOpt/fpga/TestTop"
  val vivadopath = "/opt/Xilinx/Vivado/2023.1/bin"
  val family = g.family
  val device = g.device

  val frequency = 200 MHz
  val cpu = 16
  val useWrapRom = false
  val xcix = g.Bfu.pathMultIP

  val paths = if (useWrapRom) {
    Seq(
      "/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/rtl/Top/TestTop.v",
      g.twFilePath
    )
  } else {
    Seq(
      "/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/rtl/Top/TestTop.v",
      "/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/rtl/Top/TestTop.v_toplevel_dut_ctrlMem_dut_tw_rom_rom.bin"
    )
  }
  val rtl = new Rtl {
    /** Name */
    override def getName(): String = "TestTop"
    override def getRtlPaths(): Seq[String] = paths
  }
  val flow = VivadoFlow(vivadopath, workspace, rtl, family, device, frequency, cpu, xcix = xcix)
  println(s"${family} -> ${(flow.getFMax / 1e6).toInt} MHz ${flow.getArea} ")
}
