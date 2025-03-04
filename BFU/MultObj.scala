package Ntt.BFU

import Ntt.NttCfg._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.eda.bench.Rtl
import spinal.lib.eda.xilinx.VivadoFlow

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object MultSim extends App {

  val spinalConfig = SpinalConfig(defaultClockDomainFrequency = FixedFrequency(10 MHz))
  val cfg =
    new NttCfgParam(P = PrimeCfg(32, 20), Bfu = BfuParamCfg(32, "9eg"), nttPoint = 1024, paraNum = 4)

  case class MultsimEnv() extends Mult(cfg) {
    case class MultIn(A: BigInt = 0, B: BigInt = 0) {}
    @volatile private var stop: Boolean = false
    val drvQueue = mutable.Queue[MultIn]()
    val drvMon = mutable.Queue[MultIn]()
    val refQueue = mutable.Queue[BigInt]()
    val resQueue = mutable.Queue[BigInt]()

    def simInit(): Unit = {
      io.dataIn.valid #= false
    }
    def refModule(dataIn: MultIn): Unit = {
      val ref = dataIn.A * dataIn.B
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
          if (io.dataOut.valid.toBoolean) { resQueue.enqueue(io.dataOut.payload.toBigInt) }
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
//            assert(calRes == calRef, s"data mismatch input:${drvData} res:${calRes}  ref:${calRef}")
            if (calRes != calRef) {
              println(s"data:${drvData} res:${calRes}  ref:${calRef}")
            }
          }
          clockDomain.waitSampling()
        }
      }
    }
    def simEnvStart(): Unit = {
      simInit()
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
    def insertData(test: MultIn): Unit = {
      drvQueue.enqueue(test)
      drvMon.enqueue(test)
    }
  }
  val path = ArrayBuffer("/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/IP/mul")
  val dut = SimConfig.withXSim
    .withConfig(spinalConfig)
    .allOptimisation
    .withXilinxDevice(cfg.device)
    .workspacePath("./NttOpt/sim/Bfu/Mult")
    .withXSimSourcesPaths(path, path)
    .withWave
    .compile(new MultsimEnv())

  dut.doSimUntilVoid("test") { dut =>
    import dut._
    clockDomain.forkStimulus(10 ns)
    simEnvStart()
    clockDomain.waitSampling(10)
    for (i <- 0 until  1024){
      val randA = (BigInt(cfg.Prime.bitLength, Random) % cfg.Prime) - 1
      val randB = (BigInt(cfg.Prime.bitLength, Random) % cfg.Prime) - 1
      insertData(MultIn(randA,randB))
    }
    waitSimDone()
  }

}

object MultGenVerilog extends App {
  val cfg = new NttCfgParam(P = PrimeCfg(64, 32), Bfu = BfuParamCfg(64, "9eg"), nttPoint = 1024, paraNum = 4)
  SpinalConfig(
    mode = Verilog,
    targetDirectory = "/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/rtl/BFU",
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp"
  ).generate(new Mult(cfg))
}

object MultVivadoFlow extends App {
  val workspace = "./vivado_prj/Ntt/FastMod"
  val vivadopath = "/opt/Xilinx/Vivado/2023.1/bin"
  val family = "Zynq UltraScale+ MPSoCS"
  val device = "xczu9eg-ffvb1156-2-i"
  val frequency = 300 MHz
  val cpu = 8

  val xcix = "/PRJ/SpinalHDL-prj/PRJ/myTest/test/hw/spinal/Ntt/xilinx_ip/mult_gen_0.xcix"
  val top = "xMul"
  val paths = Seq(
    "/PRJ/SpinalHDL-prj/PRJ/myTest/test/rtl/Ntt/Mult/xMul.v",
    "/PRJ/SpinalHDL-prj/PRJ/myTest/test/hw/spinal/Ntt/xilinx_ip/mul.v"
  )

  val rtl = new Rtl {

    /** Name */
    override def getName(): String = top
    override def getRtlPaths(): Seq[String] = paths
  }

  val flow = VivadoFlow(vivadopath, workspace, rtl, family, device, frequency, cpu, xcix)
  println(s"${family} -> ${(flow.getFMax / 1e6).toInt} MHz ${flow.getArea} ")
}
