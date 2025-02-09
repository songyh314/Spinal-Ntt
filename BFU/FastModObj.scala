package Ntt.BFU

import Ntt.NttCfg.NttCfg2414
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.eda.bench.Rtl
import spinal.lib.eda.xilinx.VivadoFlow

import scala.collection.mutable
import scala.util.Random

object FastModGenVerilog extends App {
  SpinalConfig(
    mode = Verilog,
    targetDirectory = "./rtl/Ntt/FastMod",
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp"
  ).generate(new FastMod2414(NttCfg2414()))
}

object FastModSim extends App {
  val dut = SimConfig.withWave.withVerilator.compile(FastMod2414(NttCfg2414()))
  val period = 10
  dut.doSim("test") { dut =>
    SimTimeout(1000 * period)
    import dut._
    io.dataIn.valid #= false
    clockDomain.forkStimulus(period)
    clockDomain.waitSampling(10)
    val seq: Seq[BigInt] = Seq(233071802950421L, 160250870884935L, 16554875289840L, 277760362779586L)
    for (i <- seq.indices) {
      io.dataIn.valid #= true
      io.dataIn.payload #= seq(i)
      clockDomain.waitSampling()
      io.dataIn.valid #= false
    }
    clockDomain.waitSampling(10)

  }
//  dut.doSim("test") { dut =>
//    import dut._
//    val p = BigInt(2).pow(24) - BigInt(2).pow(14) + 1
//    val max = (p - 1).pow(2)
//
//    var stop = false
//    SimTimeout(5000 * period)
//    clockDomain.forkStimulus(period)
//    simEnvStart(stop)
//    val seq: Seq[BigInt] = Seq(233071802950421L, 160250870884935L, 16554875289840L, 277760362779586L)
//    for (i <- seq.indices) {
////      val randomTest = BigInt(max.bitLength, Random) % (max - p) + p
//      insertData(seq(i))
//    }
//    clockDomain.waitSampling(50)
//    stop = true
////    waitSimDone()
//  }
}
object test extends App {
  override def main(args: Array[String]): Unit = {
    val a = NttCfg2414()
    a.show()
  }
}

object FastModVivadoFlow extends App {

  val workspace = "./vivado_prj/Ntt/FastMod"
  val vivadopath = "/opt/Xilinx/Vivado/2023.1/bin"
//  val family = "Kintex UltraScale"
//  val device = "xcku060-ffva1156-2-i"
  val family = "Virtex 7"
  val device = "xc7vx485tffg1157-1"
  val frequency = 300 MHz
  val cpu = 8
  val rtl = new Rtl {

    /** Name */
    override def getName(): String = "FastMod2414"
    override def getRtlPath(): String = "/PRJ/SpinalHDL-prj/PRJ/myTest/test/rtl/Ntt/FastMod/FastMod2414.v"
  }

  val flow = VivadoFlow(vivadopath, workspace, rtl, family, device, frequency, cpu)
  println(s"${family} -> ${(flow.getFMax / 1e6).toInt} MHz ${flow.getArea} ")
}
