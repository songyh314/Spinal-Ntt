package Ntt.BFU

import Ntt.NttCfg.{BfuParamCfg, BfuPayload, DataPayload, NttCfgParam, PrimeCfg}
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.eda.bench.Rtl
import spinal.lib.eda.xilinx.VivadoFlow

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random



case class DrvData(A: BigInt, B: BigInt, TW: BigInt, isNtt: Boolean)
case class MonData(A: BigInt, B: BigInt)

object BfuGenV extends App {
  SpinalConfig(mode = Verilog, nameWhenByFile = false, anonymSignalPrefix = "tmp", targetDirectory = "./rtl/Ntt/Bfu")
    .generate(new Bfu(NttCfgParam(), debug = false))
}

object BfuSimFlow extends App {

  val g = NttCfgParam(P = PrimeCfg(32,20), Bfu = BfuParamCfg(32,"9eg"))
  object BfuGold {
    val debug = false
    def AddSub(A: BigInt = 0, B: BigInt = 0, isRescale: Boolean): (BigInt, BigInt) = {
      var addRes = (A + B) % g.Prime
      var subRes = if (A > B) A - B else (A + g.Prime) - B
      if (isRescale) {
        if (addRes % 2 == 1) {
          addRes = addRes / 2 + g.HalfPrime
        } else { addRes = addRes / 2 }
        if (subRes % 2 == 1) {
          subRes = subRes / 2 + g.HalfPrime
        } else { subRes = subRes / 2 }
      }
      if (debug) println(s"add : $addRes , subu : $subRes")
      (addRes, subRes)
    }
    def ModMult(A: BigInt, B: BigInt): BigInt = {
      val ret = (A * B) % g.Prime
      if (debug) println(s"modmult res : $ret")
      ret
    }
    def Bfu(A: BigInt, B: BigInt, Tw: BigInt, isNtt: Boolean): (BigInt, BigInt) = {
      if (isNtt) {
        val tmpB = ModMult(B, Tw)
        AddSub(A, tmpB, isRescale = false)
      } else {
        val (tmpA, tmpB) = AddSub(A, B, isRescale = true)
        val invTw = g.Prime - Tw
        (tmpA, ModMult(tmpB, invTw))
      }
    }
  }
  case class BfuSim() extends Bfu(g) {
    val drvQueue = mutable.Queue[DrvData]()
    val drvMon = mutable.Queue[DrvData]()
    val refQueue = mutable.Queue[MonData]()
    val resQueue = mutable.Queue[MonData]()

    @volatile private var stop: Boolean = false
    def simInit(): Unit = {
      io.dataIn.valid #= false
      io.isNtt #= true
      clockDomain.waitSampling(10)
    }
    def refModule(dataIn: DrvData): Unit = {
      val (resA, resB) = BfuGold.Bfu(dataIn.A, dataIn.B, dataIn.TW, dataIn.isNtt)
      refQueue.enqueue(MonData(resA, resB))
    }
    def Driver(): Unit = {
      val drv = fork {
        while (!stop) {
          if (drvQueue.nonEmpty) {
            val test = drvQueue.dequeue()
            refModule(test)
            io.dataIn.payload.A #= test.A
            io.dataIn.payload.B #= test.B
            io.dataIn.payload.Tw #= test.TW
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
            resQueue.enqueue(MonData(io.dataOut.payload.A.toBigInt, io.dataOut.payload.B.toBigInt))
          }
          clockDomain.waitSampling()
        }
      }
    }
    def scoreBoard(): Unit = {
      val score = fork {
        while (!stop) {
          if (refQueue.nonEmpty && resQueue.nonEmpty) {
            val drv = drvMon.dequeue()
            val calRes = resQueue.dequeue()
            val calRef = refQueue.dequeue()
            assert(calRes == calRef, s"data mismatch input:${drv} res:${calRes}  ref:${calRef}")
            println(s"data:${drv} res:${calRes}  ref:${calRef}")
            if (calRes != calRef) {
              println(s"error!: data:${drv} res:${calRes}  ref:${calRef}")
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
    def insertData(test: DrvData): Unit = {
      drvQueue.enqueue(test)
      drvMon.enqueue(test)
    }
  }
  val path = ArrayBuffer("/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/IP/mul")
  val dut = SimConfig.withXSim
    .withXilinxDevice("xczu9eg-ffvb1156-2-i")
    .withXSimSourcesPaths(path, path)
    .compile(new BfuSim())
  val period = 10
  dut.doSim("test") { dut =>
    import dut._
    val g = NttCfgParam(P = PrimeCfg(32,20), Bfu = BfuParamCfg(32,"9eg"))
//    SimTimeout(5000 * period)
    clockDomain.forkStimulus(period ns)
    simEnvStart()
    io.isNtt #= true
    for (i <- 0 until (1024)) {
      val randomA = ((BigInt(g.Prime.bitLength, Random) + ((g.Prime - 1) / 2)) % g.Prime) - 1
      val randomB = ((BigInt(g.Prime.bitLength, Random) + ((g.Prime - 1) / 2)) % g.Prime) - 1
      val randomTw = ((BigInt(g.Prime.bitLength, Random) + ((g.Prime - 1) / 2)) % g.Prime) - 1
      insertData(DrvData(randomA, randomB, randomTw, isNtt = true))
    }
    waitClean()
    io.isNtt #= false
    for (i <- 0 until (1024)) {
      val randomA = (BigInt(g.Prime.bitLength, Random) % g.Prime) - 1
      val randomB = (BigInt(g.Prime.bitLength, Random) % g.Prime) - 1
      val randomTw = (BigInt(g.Prime.bitLength, Random) % g.Prime) - 1
      insertData(DrvData(randomA, randomB, randomTw, isNtt = false))
    }
    waitSimDone()
  }
}

object BfuVivadoFlow extends App {
  val workspace = "./vivado_prj/Ntt/Bfu"
  val vivadopath = "/opt/Xilinx/Vivado/2023.1/bin"
  val family = "Zynq UltraScale+ MPSoCS"
  val device = "xczu9eg-ffvb1156-2-i"
  val frequency = 400 MHz
  val cpu = 12
  val xcix = Seq("/PRJ/SpinalHDL-prj/PRJ/myTest/test/hw/spinal/Ntt/xilinx_ip/mult_gen_0.xcix")
  val paths = Seq(
    "/PRJ/SpinalHDL-prj/PRJ/myTest/test/rtl/Ntt/Bfu/Bfu.v",
    "/PRJ/SpinalHDL-prj/PRJ/myTest/test/hw/spinal/Ntt/xilinx_ip/mul.v"
  )
  val rtl = new Rtl {
    override def getName(): String = "Bfu"
    override def getRtlPaths(): Seq[String] = paths
  }
  val flow = VivadoFlow(vivadopath, workspace, rtl, family, device, frequency, cpu, xcix = xcix)
  println(s"${family} -> ${(flow.getFMax / 1e6).toInt} MHz ${flow.getArea}")
}
