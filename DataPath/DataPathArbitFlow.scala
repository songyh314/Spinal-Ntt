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


object memOutArbVivadoFlow extends App{
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
    "/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/rtl/DataPath/memOutArb.v",
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
  ).generate(new DataPathTop(NttCfgParam(paraNum = 16,mode = modeCfg(useTwFile = false))))
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



object NttTopVivadoFlow extends App {
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
    "/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/rtl/DataPath/shuffleOpt.v",
  )
  val rtl = new Rtl {

    /** Name */
    override def getName(): String = "shuffleOpt"

    override def getRtlPaths(): Seq[String] = paths
  }
  val flow = VivadoFlow(vivadopath, workspace, rtl, family, device, frequency, cpu)
  println(s"${family} -> ${(flow.getFMax / 1e6).toInt} MHz ${flow.getArea} ")

}
