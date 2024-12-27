package Ntt.BFU

import Ntt.NttCfg.{BfuPayload, DataPayload, NttCfg2414}
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.eda.bench.Rtl
import spinal.lib.eda.xilinx.VivadoFlow

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class Bfu(g: NttCfg2414) extends Component {
  val io = new Bundle {
    val isNtt = in Bool ()
    val dataIn = slave Flow (BfuPayload(g))
    val dataOut = master Flow (DataPayload(g))
  }
  import io._
  val uAddSub = new AddSub(g)
  val uModMult = new ModMult(g)
  val DelayOutSt1 = Delay(io.dataIn.Tw, g.BfuLatencySt1)
  val DelayOutSt2 = Delay(io.isNtt ? io.dataIn.A | uAddSub.io.dataOut.A, g.BfuLatencySt2)

  uModMult.io.dataIn.valid := isNtt ? io.dataIn.valid | uAddSub.io.dataOut.valid
  uModMult.io.dataIn.payload.data := isNtt ? io.dataIn.payload.B | uAddSub.io.dataOut.B
  uModMult.io.dataIn.payload.tw := isNtt ? io.dataIn.payload.Tw | DelayOutSt1

  uAddSub.io.isRescale := !io.isNtt
  uAddSub.io.dataIn.valid := isNtt ? uModMult.io.dataOut.valid | io.dataIn.valid
  uAddSub.io.dataIn.A := isNtt ? DelayOutSt2 | io.dataIn.A
  uAddSub.io.dataIn.B := isNtt ? uModMult.io.dataOut.payload | io.dataIn.B

  io.dataOut.A := isNtt ? uAddSub.io.dataOut.A | DelayOutSt2
  io.dataOut.B := isNtt ? uAddSub.io.dataOut.B | uModMult.io.dataOut.payload
  io.dataOut.valid := isNtt ? uAddSub.io.dataOut.valid | uModMult.io.dataOut.valid
}


object BfuGold extends App {
  val g = NttCfg2414()
  val debug = false
  def AddSub(A: BigInt = 0, B: BigInt = 0, isRescale: Boolean): (BigInt, BigInt) = {
    var addRes = (A + B) % NttCfg2414().Prime
    var subRes = if (A > B) A - B else (A + NttCfg2414().Prime) - B
    if (isRescale) {
      if (addRes % 2 == 1) {
        addRes = addRes / 2 + NttCfg2414().HalfPrime
      } else { addRes = addRes / 2 }
      if (subRes % 2 == 1) {
        subRes = subRes / 2 + NttCfg2414().HalfPrime
      } else { subRes = subRes / 2 }
    }
    if(debug) println(s"add : $addRes , subu : $subRes")
    (addRes, subRes)
  }
  def ModMult(A: BigInt, B: BigInt): BigInt = {
    val ret = (A * B) % NttCfg2414().Prime
    if(debug) println(s"modmult res : $ret")
    ret
  }
  def Bfu(A: BigInt, B: BigInt, Tw: BigInt, isNtt: Boolean): (BigInt, BigInt) = {
    if (isNtt) {
      val tmpB = ModMult(B, Tw)
      AddSub(A, tmpB, isRescale = false)
    } else {
      val (tmpA, tmpB) = AddSub(A, B, isRescale = true)
      (tmpA, ModMult(tmpB, Tw))
    }
  }
}

case class DrvData(A: BigInt, B: BigInt, TW: BigInt, isNtt: Boolean)
case class MonData(A: BigInt, B: BigInt)

case class BfuSim() extends Bfu(NttCfg2414()) {
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
          if (calRes != calRef) {
            println(s"data:${drv} res:${calRes}  ref:${calRef}")
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
object BfuGenV extends App {
  SpinalConfig(mode = Verilog, nameWhenByFile = false, anonymSignalPrefix = "tmp", targetDirectory = "./rtl/Ntt/Bfu")
    .generate(new Bfu(NttCfg2414()))
}

object BfuSimFlow extends App {
  val path = ArrayBuffer("/PRJ/SpinalHDL-prj/PRJ/myTest/test/hw/spinal/Ntt/xilinx_ip/")
  val dut = SimConfig.withXSim.withWave
    .withXilinxDevice("xczu9eg-ffvb1156-2-i")
    .withXSimSourcesPaths(path, path)
    .compile(new BfuSim())
  val period = 10
  dut.doSim("test") { dut =>
    import dut._
    val g = NttCfg2414()
    SimTimeout(1000 * period)
    clockDomain.forkStimulus(period)
    simEnvStart()
    io.isNtt #= true
    for (i <- 0 until (256)) {
      val randomA = (BigInt(g.Prime.bitLength, Random) % g.Prime) - 1
      val randomB = (BigInt(g.Prime.bitLength, Random) % g.Prime) - 1
      val randomTw = (BigInt(g.Prime.bitLength, Random) % g.Prime) - 1
      insertData(DrvData(randomA, randomB, randomTw, isNtt = true))
    }
    waitClean()
    io.isNtt #= false
    for (i <- 0 until (256)) {
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
  val frequency = 300 MHz
  val cpu = 12
  val xcix = "/PRJ/SpinalHDL-prj/PRJ/myTest/test/hw/spinal/Ntt/xilinx_ip/mult_gen_0.xcix"
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
