package Ntt.BFU

import Ntt.NttCfg.{BfuParamCfg, NttCfgParam, PrimeCfg, multPayload}
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.eda.bench.Rtl
import spinal.lib.eda.xilinx.VivadoFlow

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random


case class Pair(A: BigInt = 0, B: BigInt = 0)

object ModMultGenV extends App {
  SpinalConfig(mode = Verilog, nameWhenByFile = false, anonymSignalPrefix = "tmp", targetDirectory = "./rtl/Ntt/Bfu")
    .generate(new ModMult(NttCfgParam()))
}

object ModMultClusterGenV extends App {
  SpinalConfig(mode = Verilog, nameWhenByFile = false, anonymSignalPrefix = "tmp", targetDirectory = "NttOpt/rtl/BFU")
    .generate(new ModMultCluster(NttCfgParam(P = PrimeCfg(14,12))))
}

object ModMultSimFLow extends App {
  val cfg = new NttCfgParam(P = PrimeCfg(64,32),Bfu = BfuParamCfg(64,"9eg"),nttPoint = 1024,paraNum = 4)
  case class ModMultSim() extends ModMultCluster(g = cfg) {
    val drvQueue = mutable.Queue[Pair]()
    val drvMon = mutable.Queue[Pair]()
    val refQueue = mutable.Queue[BigInt]()
    val resQueue = mutable.Queue[BigInt]()

    @volatile private var stop: Boolean = false
    def setInit():Unit = {
      io.dataIn.valid #= false
      clockDomain.waitSampling(10)
    }
    def refModule(dataIn: Pair): Unit = {
      val ref = (dataIn.A * dataIn.B) % cfg.Prime
      refQueue.enqueue(ref)
    }
    def Driver(): Unit = {
      val drv = fork {
        while (!stop) {
          if (drvQueue.nonEmpty) {
            val test = drvQueue.dequeue()
            refModule(test)
            io.dataIn.payload.data #= test.A
            io.dataIn.payload.tw #= test.B
            io.dataIn.valid #= true
            clockDomain.waitSampling()
            io.dataIn.valid #= false
          } else { clockDomain.waitSampling() }
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
      setInit()
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
    def insertData(A: BigInt = 0, B: BigInt = 0): Unit = {
      drvQueue.enqueue(Pair(A, B))
      drvMon.enqueue(Pair(A, B))
    }
  }

  val path = ArrayBuffer("/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/IP/mul")
  val dut = SimConfig.withXSim.withWave
    .withXilinxDevice("xczu9eg-ffvb1156-2-i")
    .workspacePath("NttOpt/sim/Bfu")
    .withXSimSourcesPaths(path, path)
    .compile(new ModMultSim())
  val period = 10
  val test = new Pair()
  val p = cfg.Prime

  val max = (p - 1).pow(2)
  dut.doSim("test") { dut =>
    import dut._
//    SimTimeout(100000 * period)
    clockDomain.forkStimulus(period ns)
    simEnvStart()
    for (i <- 0 until 8192 ) {
      val randomA = (BigInt(max.bitLength, Random) % p) - 1
      val randomB = (BigInt(max.bitLength, Random) % p) - 1
      insertData(randomA, randomB)
    }
    waitSimDone()
  }
}

object ModMultVivadoFlow extends App {

  val workspace = "./vivado_prj/"
  val vivadopath = "/opt/Xilinx/Vivado/2023.1/bin"
  val family = "Zynq UltraScale+ MPSoCS"
  val device = "xczu9eg-ffvb1156-2-i"
  val frequency = 300 MHz
  val cpu = 8
  val xcix = Seq("/PRJ/SpinalHDL-prj/PRJ/myTest/test/hw/spinal/Ntt/xilinx_ip/mult_gen_0.xcix")
  val paths = Seq(
    "/PRJ/SpinalHDL-prj/PRJ/myTest/test/rtl/Ntt/Bfu/ModMult.v",
    "/PRJ/SpinalHDL-prj/PRJ/myTest/test/hw/spinal/Ntt/xilinx_ip/mul.v"
  )
  val rtl = new Rtl {

    /** Name */
    override def getName(): String = "ModMult"
    override def getRtlPaths(): Seq[String] = paths
  }

  val flow = VivadoFlow(vivadopath, workspace, rtl, family, device, frequency, cpu, xcix)
  println(s"${family} -> ${(flow.getFMax / 1e6).toInt} MHz ${flow.getArea} ")
}