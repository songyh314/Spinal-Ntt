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

//class FastMod2420(g: NttConfig2420) extends Component {
//  val prime = g.Prime
//  val io = new Bundle {
//    val dataIn = slave Flow (UInt(2 * g.width bits))
//    val dataOut = master Flow (UInt(g.width bits))
//  }
//  val A = Vec(for (i <- 0 until g.pGroup) yield {
//    RegNextWhen(
//      io.dataIn.payload((2 * g.width - i * g.delta - 1) downto (2 * g.width - (i + 1) * g.delta)),
//      io.dataIn.valid
//    )
//  }).setName("storeA")
//  val B = Vec(for (i <- 0 until (g.pGroup)) yield {
//    RegNextWhen(
//      io.dataIn.payload((2 * g.width - 1) downto (2 * g.width - (i + 1) * g.delta)),
//      io.dataIn.valid
//    )
//  }).setName("storeB")
//
//  val C = RegNext(RegNextWhen(io.dataIn.payload(g.width - 1 downto 0), io.dataIn.valid))
//  val Asum = RegNext(A.reduceBalancedTree(_ +^ _)).setName("sum_A")
//  val Bsum1 = RegNext(
//    B.zipWithIndex.filter(_._2 % 2 == 0).map(_._1).reduceBalancedTree(_ +^ _)
//  )
//  val Bsum2 = RegNext(
//    B.zipWithIndex.filter(_._2 % 2 == 1).map(_._1).reduceBalancedTree(_ +^ _)
//  )
//  val Bsum = RegNext(Bsum1._data + Bsum2._data).setName("sum_B")
//  val slicesNum = scala.math.ceil(widthOf(Asum).toDouble / g.delta).toInt
//  val E =
//    Asum._data.resize(g.delta * slicesNum).subdivideIn(slicesNum slices).reduceBalancedTree(_ +^ _).setName("sum_A_bit")
//  val F1 = E(widthOf(E) - 1 downto g.delta) + E(g.delta - 1 downto 0)
//  val F2 = E(widthOf(E) - 1 downto g.delta) +^ Asum._data(Asum._data.high downto g.delta)
//  val F = ((F1 << 20) - F2).setName("sum_F")
//  val AsumAddC = RegNext((F + C >= prime) ? (F + C - prime) | (F + C)).setName("RES1")
//  val ACsubB = RegNext(
//    (AsumAddC._data >= Bsum._data) ? (AsumAddC._data - Bsum._data) | ((prime - Bsum._data) + AsumAddC._data)
//  ).setName("RES2")
//  val validDealy = Delay(io.dataIn.valid, LatencyAnalysis(io.dataIn.payload, ACsubB) + 1)
//  io.dataOut.payload := ACsubB.resized
//  io.dataOut.valid := validDealy
//}

case class FastMod2414(g: NttCfg2414) extends Component {
  val prime = g.Prime
  val io = new Bundle {
    val dataIn = slave Flow (UInt(2 * g.width bits))
    val dataOut = master Flow (UInt(g.width bits))
  }

  val aPart = Vec(for (i <- 0 until (g.pGroup)) yield {
    if ((i + 1) * g.delta > g.width) {
      io.dataIn.payload(2 * g.width - 1 downto i * g.delta + g.width)
    } else io.dataIn.payload(g.width + (i + 1) * g.delta - 1 downto i * g.delta + g.width)
  })
  val bPart = Vec(for (i <- 0 until (g.pGroup)) yield {
    io.dataIn.payload(2 * g.width - 1 downto i * g.delta + g.width)
  })
  val A = RegNextWhen(aPart.reduce(_ +^ _), io.dataIn.valid) init U(0)
  val B_1 = RegNextWhen(bPart(2), io.dataIn.valid) init U(0)
  val B_2 = RegNextWhen(bPart(1) +^ bPart(0), io.dataIn.valid) init U(0)
  val B = RegNext(B_1 +^ B_2)
  val C = RegNextWhen(io.dataIn.payload(g.width - 1 downto 0), io.dataIn.valid)
  val Aslice = A._data.resize(2 * g.delta).subdivideIn(2 slices)
  val D = Aslice.reduce(_ +^ _)
  val Dslice = D.resize(2 * g.delta).subdivideIn(2 slices)
  val E = Dslice.reduce(_ + _)
  val Fsub = Dslice(1) + Aslice(1)
  val F = (E << g.N) - Fsub
  val Res1_tmp1 = F +^ C._data - g.Prime
  val Res1_tmp2 = F +^ C._data
  val Res1_value = (F +^ C >= g.Prime) ? Res1_tmp1 | Res1_tmp2
  val Res1 = RegNext(Res1_value)
  val Res2_tmp1 = Res1 - B
  val Res2_tmp2 = (Res1 +^ g.Prime) - B
  val Res2_value = (Res1 >= B) ? Res2_tmp1 | Res2_tmp2
  val Res2 = RegNext(Res2_value.resize(g.width bits))
  val valid = Delay(io.dataIn.valid, LatencyAnalysis(io.dataIn.payload, Res2))
  io.dataOut.payload := Res2
  io.dataOut.valid := valid
}

//case class simEnv() extends FastMod2414(NttCfg2414()) {
//  val drvQueue = mutable.Queue[BigInt]()
//  val drvMon = mutable.Queue[BigInt]()
//  val refQueue = mutable.Queue[BigInt]()
//  val resQueue = mutable.Queue[BigInt]()
//
//  @volatile var stop: Boolean = false
//  def refModule(dataIn: BigInt): Unit = {
//    val ref = dataIn % prime
//    refQueue.enqueue(ref)
//  }
//  def Driver(stop: Boolean): Unit = {
//    val drv = fork {
//      while (!stop) {
//        if (drvQueue.nonEmpty) {
//          val test = drvQueue.dequeue()
//          refModule(test)
//          io.dataIn.payload #= test
//          io.dataIn.valid #= true
//          clockDomain.waitSampling()
//          io.dataIn.valid #= false
//        } else { clockDomain.waitSampling() }
//      }
//    }
//    if (stop) { drv.join() }
//  }
//  def Monitor(stop: Boolean): Unit = {
//    val mon = fork {
//      while (!stop) {
//        if (io.dataOut.valid.toBoolean) { resQueue.enqueue(io.dataOut.payload.toBigInt) }
//        clockDomain.waitSampling()
//      }
//    }
//    if (stop) { mon.join() }
//  }
//  def scoreBoard(stop: Boolean): Unit = {
//    val score = fork {
//      while (!stop) {
//        if (refQueue.nonEmpty && resQueue.nonEmpty) {
//          val drvData = drvMon.dequeue()
//          val calRes = resQueue.dequeue()
//          val calRef = refQueue.dequeue()
////          assert(calRes == calRef, s"data mismatch input:${drvData} res:${calRes}  ref:${calRef}")
//          if (calRes != calRef) {
//            println(s"data:${drvData} res:${calRes}  ref:${calRef}")
//          }
//        }
//        clockDomain.waitSampling()
//      }
//    }
//    if (stop) { score.join() }
//  }
//  def simEnvStart(stop: Boolean): Unit = {
//    //      simInit()
//    Driver(stop)
//    Monitor(stop)
//    scoreBoard(stop)
//  }
//  def waitSimDone(): Unit = {
//    while (refQueue.nonEmpty || resQueue.nonEmpty) {
//      clockDomain.waitSampling(10)
//    }
//
//    stop = true
//
//    println("sim finish")
//    simSuccess()
//  }
//  def insertData(test: BigInt = 0): Unit = {
//    drvQueue.enqueue(test)
//    drvMon.enqueue(test)
//  }
//}

object FastModGenVerilog extends App {
  SpinalConfig(
    mode = Verilog,
    targetDirectory = "./rtl/Ntt/FastMod",
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp"
  ).generate(new FastMod2414(NttCfg2414()))
}

object FastModSim extends App {
  val dut = SimConfig.withWave.withXSim.compile(FastMod2414(NttCfg2414()))
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
  val family = "Kintex UltraScale"
  val device = "xcku060-ffva1156-2-i"
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
