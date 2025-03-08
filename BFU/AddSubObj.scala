package Ntt.BFU

import Ntt.NttCfg.{DataPayload, NttCfgParam}
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.eda.bench.Rtl
import spinal.lib.eda.xilinx.VivadoFlow

import scala.collection.mutable
import scala.util.Random


case class drvData(A: BigInt = 0, B: BigInt = 0, mode: Boolean = true)
case class monData(A: BigInt = 0, B: BigInt = 0)

case class AddSubSim(g: NttCfgParam) extends AddSub(NttCfgParam()) {
  val drvQueue = mutable.Queue[drvData]()
  val drvMon = mutable.Queue[drvData]()
  val refQueue = mutable.Queue[monData]()
  val resQueue = mutable.Queue[monData]()

  @volatile private var stop: Boolean = false

  def simInit(): Unit = {
    io.dataIn.valid #= false
    io.dataIn.payload.A #= 0
    io.dataIn.payload.B #= 0
    io.isRescale #= false
    clockDomain.waitSampling(10)
  }
  def refModule(dataIn: drvData): Unit = {
    val A = dataIn.A
    val B = dataIn.B
    val mode = dataIn.mode
    var addRes = (A + B) % g.Prime
    var subRes = if (A > B) A - B else (A + g.Prime) - B
    if (mode == true) {
      if (addRes % 2 == 1) {
        addRes = addRes / 2 + g.HalfPrime
      } else { addRes = addRes / 2 }
      if (subRes % 2 == 1) {
        subRes = subRes / 2 + g.HalfPrime
      } else { subRes = subRes / 2 }
    }
    refQueue.enqueue(monData(addRes, subRes))
  }
  def Driver(): Unit = {
    val drv = fork {
      while (!stop) {
        if (drvQueue.nonEmpty) {
          val test = drvQueue.dequeue()
          refModule(test)
          io.dataIn.payload.A #= test.A
          io.dataIn.payload.B #= test.B
//          io.isRescale #= test.mode
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
          resQueue.enqueue(monData(io.dataOut.payload.A.toBigInt, io.dataOut.payload.B.toBigInt))
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
          assert(calRes == calRef, s"data mismatch input:${drvData} res:${calRes}  ref:${calRef}")
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
  def insertData(test: drvData = drvData(0, 0, false)): Unit = {
    drvQueue.enqueue(test)
    drvMon.enqueue(test)
  }
}


object AddSubGenV extends App {
  SpinalConfig(mode = Verilog, nameWhenByFile = false, anonymSignalPrefix = "tmp", targetDirectory = "./rtl/Ntt/Bfu")
    .generate(new AddSub(NttCfgParam()))
}

object AddSubSimFLow extends App {
  val dut = SimConfig.withXSim.withWave.withXilinxDevice("xczu9eg-ffvb1156-2-i").compile(new AddSubSim(NttCfgParam()))
  val period = 10
  val test = new drvData()
  val p = NttCfgParam().Prime
  //  val p = BigInt(2).pow(24) - BigInt(2).pow(14) + 1
  dut.doSim("test") { dut =>
    import dut._
    SimTimeout(1000 * period)
    clockDomain.forkStimulus(period)
    simEnvStart()
    io.isRescale #= true
    for (i <- 0 until 64) {
      val randomA = (BigInt(p.bitLength, Random) % p) - 1
      val randomB = (BigInt(p.bitLength, Random) % p) - 1
      insertData(drvData(randomA, randomB, true))
    }
    waitClean()
    clockDomain.waitSampling(10)
    io.isRescale #= false
    for (i <- 0 until 64) {
      val randomA = (BigInt(p.bitLength, Random) % p) - 1
      val randomB = (BigInt(p.bitLength, Random) % p) - 1
      insertData(drvData(randomA, randomB, false))
    }
    waitSimDone()
  }
}

object AddSubVivadoFlow extends App {
  val workspace = "./vivado_prj/AddSub"
  val vivadopath = "/opt/Xilinx/Vivado/2023.1/bin"
  val family = "Zynq UltraScale+ MPSoCS"
  val device = "xczu9eg-ffvb1156-2-i"
  val frequency = 300 MHz
  val cpu = 8
  val rtl = new Rtl {
    override def getName(): String = "AddSub"
    override def getRtlPath(): String = "/PRJ/SpinalHDL-prj/PRJ/myTest/test/rtl/Ntt/Bfu/AddSub.v"
  }

  val flow = VivadoFlow(vivadopath, workspace, rtl, family, device, frequency, cpu, xcix = Seq(rtl.getRtlPath()))
  println(s"${family} -> ${(flow.getFMax / 1e6).toInt} MHz ${flow.getArea} ")
}
