package Ntt.DataPath
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import Ntt.NttCfg._
import myRam._
import myTools._
import spinal.core
import spinal.lib.eda.bench.Rtl
import spinal.lib.eda.xilinx.VivadoFlow

import scala.collection.mutable

case class MuxCmp(g: NttCfgParam, prefix: String = "") extends Component {
  val io = new Bundle {
    val dataIn = in Vec (Bits(g.width bits), g.BI)
    val idxIn = in Vec (UInt(g.BankIndexWidth bits), g.BI)
    val dataOut = out Vec (Bits(g.width bits), g.BI)
  }
  io.dataOut.zip(io.idxIn).map{case(ret,idx) =>
    ret := io.dataIn.read(idx)
  }
}

object MuxCmpVivadoFlow extends App {
  val cfg = new NttCfgParam(paraNum = 32)
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "NttOpt/rtl/DataPath",
    genLineComments = true
  ).generate(new MuxCmp(cfg))

  val workspace = "NttOpt/fpga/DataPath/"
  val vivadopath = "/opt/Xilinx/Vivado/2023.1/bin"
  val family = cfg.family
  val device = cfg.device
  val frequency = 300 MHz
  val cpu = 16
  val paths = Seq(
    "/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/rtl/DataPath/MuxCmp.v"
  )
  val rtl = new Rtl {

    /** Name */
    override def getName(): String = "MuxCmp"
    override def getRtlPaths(): Seq[String] = paths
  }
  val flow = VivadoFlow(vivadopath, workspace, rtl, family, device, frequency, cpu)
  println(s"${family} -> ${(flow.getFMax / 1e6).toInt} MHz ${flow.getArea} ")
}


object sfCmpVivadoFlow extends App {
  val cfg = new NttCfgParam(P = PrimeCfg(14,12), paraNum = 32,nttPoint = 1024)
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "NttOpt/rtl/DataPath",
    genLineComments = true
  ).generate(new memInMux(cfg))

  val workspace = "NttOpt/fpga/DataPath/"
  val vivadopath = "/opt/Xilinx/Vivado/2023.1/bin"
  val family = cfg.family
  val device = cfg.device
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


object idxCmpVivadoFlow extends App {
  val cfg = new NttCfgParam(P = PrimeCfg(14,12), paraNum = 32,nttPoint = 1024)
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "NttOpt/rtl/DataPath",
    genLineComments = true
  ).generate(new memOutArb(cfg))

  val workspace = "NttOpt/fpga/DataPath/sf"
  val vivadopath = "/opt/Xilinx/Vivado/2023.1/bin"
  val family = cfg.family
  val device = cfg.device
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