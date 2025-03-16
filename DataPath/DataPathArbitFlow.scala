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

object memInMuxVivadoFlow extends App {
  val cfg = new NttCfgParam(paraNum = 8)
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "NttOpt/rtl/DataPath",
    genLineComments = true
  ).generate(new memInMux(cfg))

  val workspace = "NttOpt/fpga/DataPath/"
  val vivadopath = "/opt/Xilinx/Vivado/2023.1/bin"
  val family = "Virtex 7"
  val device = "xc7vx485tffg1157-1"
  val frequency = 300 MHz
  val cpu = 16
  val paths = Seq(
    "/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/rtl/DataPath/memInMux.v"
  )
  val rtl = new Rtl {

    /** Name */
    override def getName(): String = "memInMux"
    override def getRtlPaths(): Seq[String] = paths
  }
  val flow = VivadoFlow(vivadopath, workspace, rtl, family, device, frequency, cpu)
  println(s"${family} -> ${(flow.getFMax / 1e6).toInt} MHz ${flow.getArea} ")

}

object memOutArbVivadoFlow extends App {
  val cfg = new NttCfgParam(paraNum = 8)
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "NttOpt/rtl/DataPath",
    genLineComments = true
  ).generate(new memOutArb(cfg))

  val workspace = "NttOpt/fpga/DataPath/"
  val vivadopath = "/opt/Xilinx/Vivado/2023.1/bin"
  val family = "Virtex 7"
  val device = "xc7vx485tffg1157-1"
  val frequency = 300 MHz
  val cpu = 16
  val paths = Seq(
    "/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/rtl/DataPath/memOutArb.v"
  )
  val rtl = new Rtl {

    /** Name */
    override def getName(): String = "memOutArb"
    override def getRtlPaths(): Seq[String] = paths
  }
  val flow = VivadoFlow(vivadopath, workspace, rtl, family, device, frequency, cpu)
  println(s"${family} -> ${(flow.getFMax / 1e6).toInt} MHz ${flow.getArea} ")

}

object DataPathTopGenV extends App {
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "NttOpt/rtl/DataPath",
    genLineComments = true
  ).generate(new DataPathTop(NttCfgParam(paraNum = 16, mode = modeCfg(useTwFile = false))))
}

object DataPathTopVivadoFlow extends App {
  val g = NttCfgParam(paraNum = 16)
  val useIp = false
  val workspace = "NttOpt/fpga/DataPathTop"
  val vivadopath = "/opt/Xilinx/Vivado/2023.1/bin"
//  val family = "Zynq UltraScale+ MPSoCS"
//  val device = "xczu9eg-ffvb1156-2-i"
  val family = "Virtex 7"
  val device = "xc7vx485tffg1157-1"
  val frequency = 300 MHz
  val cpu = 16
  val xcix = Seq("/PRJ/SpinalHDL-prj/PRJ/myTest/test/hw/spinal/Ntt/xilinx_ip/mem.xcix")
  val paths = Seq(
    "/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/rtl/DataPath/DataPathTop.v",
    "/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/rtl/DataPath/DataPathTop.v_toplevel_tw_rom_rom.bin",
    "/PRJ/SpinalHDL-prj/PRJ/myTest/test/hw/spinal/Ntt/xilinx_ip/bram.v"
  )
  if (g.useBramIP) {
    val rtl = new Rtl {

      /** Name */
      override def getName(): String = "DataPathTop"
      override def getRtlPaths(): Seq[String] = paths
    }
    val flow = VivadoFlow(vivadopath, workspace, rtl, family, device, frequency, cpu, xcix = xcix)
    println(s"${family} -> ${(flow.getFMax / 1e6).toInt} MHz ${flow.getArea} ")
  } else {
    val rtl = new Rtl {

      /** Name */
      override def getName(): String = "DataPathTop"
      override def getRtlPath(): String = "/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/rtl/DataPath/DataPathTop.v"
    }
    val flow = VivadoFlow(vivadopath, workspace, rtl, family, device, frequency, cpu)
    println(s"${family} -> ${(flow.getFMax / 1e6).toInt} MHz ${flow.getArea} ")
  }

}

object shuffleOptVivadoFlow extends App {
  val g = NttCfgParam()
  val useIp = false
  val workspace = "NttOpt/fpga/DataPath/shuffleOpt"
  val vivadopath = "/opt/Xilinx/Vivado/2023.1/bin"
  val family = "Zynq UltraScale+ MPSoCS"
  val device = "xczu9eg-ffvb1156-2-i"
  //  val family = "Virtex 7"
  //  val device = "xc7vx485tffg1157-1"
  val frequency = 300 MHz
  val cpu = 16
  val paths = Seq(
    "/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/rtl/DataPath/shuffleOpt.v"
  )
  val rtl = new Rtl {

    /** Name */
    override def getName(): String = "shuffleOpt"

    override def getRtlPaths(): Seq[String] = paths
  }
  val flow = VivadoFlow(vivadopath, workspace, rtl, family, device, frequency, cpu)
  println(s"${family} -> ${(flow.getFMax / 1e6).toInt} MHz ${flow.getArea} ")

}

object memInArbOptSim extends App {
  val cfg = new NttCfgParam(nttPoint = 32, paraNum = 4, mode = modeCfg(useTwFile = false))
  val dut = SimConfig.withXSim.withWave.workspacePath("NttOpt/sim/DataPath").compile(new memInArbOpt(cfg))
  dut.doSim("test") { dut =>
    import dut._
    clockDomain.forkStimulus(10 ns)
    clockDomain.waitSampling(10)
    for(i <- 0 until(32)){
      io.addrOri.foreach(item => item #= BigInt(i >> 3))
      io.idxOri.foreach(item => item #= (i%8))
      io.dataOri #= i
      clockDomain.waitSampling()
    }
    clockDomain.waitSampling(10)
  }
}

object memInArbVivadoFlow extends App {
  val cfg = new NttCfgParam(nttPoint = 1024,paraNum = 4)
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "NttOpt/rtl/DataPath",
    genLineComments = true
  ).generate(new memInArb(cfg))

  val workspace = "NttOpt/fpga/DataPath/"
  val vivadopath = "/opt/Xilinx/Vivado/2023.1/bin"
  val family = "Virtex 7"
  val device = "xc7vx485tffg1157-1"
  val frequency = 300 MHz
  val cpu = 16
  val paths = Seq(
    "/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/rtl/DataPath/memInArb.v"
  )
  val rtl = new Rtl {

    /** Name */
    override def getName(): String = "memInArb"
    override def getRtlPaths(): Seq[String] = paths
  }
  val flow = VivadoFlow(vivadopath, workspace, rtl, family, device, frequency, cpu)
  println(s"${family} -> ${(flow.getFMax / 1e6).toInt} MHz ${flow.getArea} ")

}


object memInArbOptVivadoFlow extends App {
  val cfg = new NttCfgParam(nttPoint = 1024,paraNum = 4)
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "NttOpt/rtl/DataPath",
    genLineComments = true
  ).generate(new memInArbOpt(cfg))

  val workspace = "NttOpt/fpga/DataPath/"
  val vivadopath = "/opt/Xilinx/Vivado/2023.1/bin"
  val family = "Virtex 7"
  val device = "xc7vx485tffg1157-1"
  val frequency = 300 MHz
  val cpu = 16
  val paths = Seq(
    "/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/rtl/DataPath/memInArbOpt.v"
  )
  val rtl = new Rtl {

    /** Name */
    override def getName(): String = "memInArbOpt"
    override def getRtlPaths(): Seq[String] = paths
  }
  val flow = VivadoFlow(vivadopath, workspace, rtl, family, device, frequency, cpu)
  println(s"${family} -> ${(flow.getFMax / 1e6).toInt} MHz ${flow.getArea} ")

}
