package Ntt.BFU

import Ntt.NttCfg.{BfuParamCfg, NttCfgParam, PrimeCfg, modeCfg}
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
  ).generate(new FastMod2414(NttCfgParam()))
}

object FastMod1412GenVerilog extends App {
  SpinalConfig(
    mode = Verilog,
    targetDirectory = "NttOpt/rtl/BFU/",
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp"
  ).generate(new FastMod1412(NttCfgParam(P = PrimeCfg(14, 12))))
}

object FastMod6432GenVerilog extends App {
  SpinalConfig(
    mode = Verilog,
    targetDirectory = "NttOpt/rtl/BFU/",
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp"
  ).generate(new FastMod6432(NttCfgParam(P = PrimeCfg(64, 32))))
}

object BarretMod2414GenVerilog extends App {
  SpinalConfig(
    mode = Verilog,
    targetDirectory = "NttOpt/rtl/BFU/",
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp"
  ).generate(new BarretMod2414(NttCfgParam(P = PrimeCfg(24, 14))))
}

object FastModSim extends App {
  val dut = SimConfig.withWave.withVerilator.compile(FastMod2414(NttCfgParam()))
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

}

object FastMod_sim extends App {
  val Prime = new PrimeCfg(32, 20)
  val cfg = NttCfgParam(P = Prime, Bfu = BfuParamCfg(32, "9eg"),mode = modeCfg(useTwFile = false))
  class FastMod_simEnv() extends FastMod3220(cfg) {
    val drvQueue = mutable.Queue[BigInt]()
    val drvMon = mutable.Queue[BigInt]()
    val refQueue = mutable.Queue[BigInt]()
    val resQueue = mutable.Queue[BigInt]()

    @volatile private var stop: Boolean = false

    def refModule(dataIn: BigInt): Unit = {
//      val dataIn = io.dataIn.payload
      val ref: BigInt = (dataIn.toBigInt % cfg.Prime)
      refQueue.enqueue(ref)
    }

    def Driver(): Unit = {
      val drv = fork {
        while (!stop) {
          if (drvQueue.nonEmpty) {
            val test = drvQueue.dequeue()
            refModule(test)
            io.dataIn.payload #= test
            io.dataIn.valid #= true
            clockDomain.waitSampling()
            io.dataIn.valid #= false
          } else {
            clockDomain.waitSampling()
          }
        }
      }
    }
    def Monitor(): Unit = {
      val mon = fork {
        while (!stop) {
          if (io.dataOut.valid.toBoolean) {
            resQueue.enqueue(io.dataOut.payload.toBigInt)
          }
          clockDomain.waitSampling()
        }
      }
    }
    def scoreBoard(): Unit = {
      val score = fork {
        while (!stop) {
          if (refQueue.nonEmpty && resQueue.nonEmpty) {
            val drvData = drvMon.dequeue()
            val calRes = resQueue.dequeue()
            val calRef = refQueue.dequeue()
            //          assert(calRes == calRef, s"data mismatch input:${drvData} res:${calRes}  ref:${calRef}")
            //          println(s"data:${drvData} res:${calRes}  ref:${calRef}")
            if (calRes != calRef) {
              println(s"data:${drvData} res:${calRes}  ref:${calRef}")
            }
          }
          clockDomain.waitSampling()
        }
      }
    }

    def simEnvStart(): Unit = {
      //      simInit()
      println(s"Prime = ${cfg.Prime}")
      Driver()
      Monitor()
      scoreBoard()
    }

    def waitSimDone(): Unit = {
      clockDomain.waitSampling(10)
      while (refQueue.nonEmpty || resQueue.nonEmpty) {
        clockDomain.waitSampling(10)
      }
      stop = true
      clockDomain.waitSampling(10)
      println("sim finish")
      simSuccess()
    }

    def waitClean(): Unit = {
      clockDomain.waitSampling(10)
      while (refQueue.nonEmpty || resQueue.nonEmpty) {
        clockDomain.waitSampling(1)
      }
      clockDomain.waitSampling(10)
    }

    def insertData(test: BigInt = 0): Unit = {
      drvQueue.enqueue(test)
      drvMon.enqueue(test)
    }
  }
  val dut = SimConfig.withXSim.withWave.workspacePath("NttOpt/sim/Bfu/FastMod" ).compile(new FastMod_simEnv())
  val period = 10
  dut.doSim("test") { dut =>
    val max = (Prime.Prime - 1).pow(2)
    import dut._
    clockDomain.forkStimulus(period ns)
    simEnvStart()
    io.dataIn.valid #= false
    clockDomain.waitSampling(10)
    for (i <- 0 until 4096) {
      val randomTest = BigInt(max.bitLength, Random) % (max - Prime.Prime) + Prime.Prime
      //      println(randomTest)
      insertData(randomTest)
    }
    clockDomain.waitSampling(10)
    waitSimDone()
  }

}
object FastModVivadoFlow extends App {
  val g = new NttCfgParam(P = PrimeCfg(24, 14), Bfu = BfuParamCfg(24, "v7"))
  SpinalConfig(
    mode = Verilog,
    targetDirectory = "NttOpt/rtl/BFU",
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp"
  ).generate(new FastMod2414(g))
  val workspace = "NttOpt/fpga/bfu/FastMod24"
  val vivadopath = "/opt/Xilinx/Vivado/2023.1/bin"
  val family = g.family
  val device = g.device
  val frequency = 300 MHz
  val cpu = 8
  val rtl = new Rtl {

    /** Name */
    override def getName(): String = "FastMod2414"

    override def getRtlPath(): String = "/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/rtl/BFU/FastMod2414.v"
  }

  val flow = VivadoFlow(vivadopath, workspace, rtl, family, device, frequency, cpu)
  println(s"${family} -> ${(flow.getFMax / 1e6).toInt} MHz ${flow.getArea} ")
}

object FastMod1412VivadoFlow extends App {

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
    override def getName(): String = "FastMod1412"

    override def getRtlPath(): String = "/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/rtl/BFU/FastMod1412.v"
  }

  val flow = VivadoFlow(vivadopath, workspace, rtl, family, device, frequency, cpu)
  println(s"${family} -> ${(flow.getFMax / 1e6).toInt} MHz ${flow.getArea} ")

}

object BarretVivadoFlow extends App {
  val g = new NttCfgParam(P = PrimeCfg(24, 14), Bfu = BfuParamCfg(24, "v7"))
  SpinalConfig(
    mode = Verilog,
    targetDirectory = "NttOpt/rtl/BFU/",
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp"
  ).generate(new BarretMod2414(g))
  val workspace = "./vivado_prj/"
  val vivadopath = "/opt/Xilinx/Vivado/2023.1/bin"
  val family = g.family
  val device = g.device
  val frequency = 300 MHz
  val cpu = 16
  val xcix = g.Bfu.pathMultIP
  val paths = Seq(
    "/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/rtl/BFU/BarretMod2414.v"
    //      "/PRJ/SpinalHDL-prj/PRJ/myTest/test/hw/spinal/Ntt/xilinx_ip/mul.v"
  )
  val rtl = new Rtl {

    /** Name */
    override def getName(): String = "BarretMod2414"

    override def getRtlPaths(): Seq[String] = paths
  }

  val flow = VivadoFlow(vivadopath, workspace, rtl, family, device, frequency, cpu, xcix)
  println(s"${family} -> ${(flow.getFMax / 1e6).toInt} MHz ${flow.getArea} ")
}
