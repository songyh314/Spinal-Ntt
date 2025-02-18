package Ntt

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.eda.bench.Rtl
import spinal.lib.eda.xilinx.VivadoFlow
import test._
import NttCfg._
import BFU._
import CTRL._
import DataPath._
import myRam._

import java.io.PrintWriter
import scala.collection.mutable.ArrayBuffer

object NttTopSim extends App {
  val period = 10
  val cfg = new NttCfgParam(P = PrimeCfg(64,32),Bfu = BfuParamCfg(64,"9eg"),nttPoint = 1024, paraNum = 4)
  val path = ArrayBuffer("/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/IP/mul/")
  val dut = SimConfig.withXSim
    .withXilinxDevice("xczu9eg-ffvb1156-2-i")
    .workspacePath("./NttOpt/sim/")
    .withXSimSourcesPaths(path, path)
    .withWave
    .compile(new NttTop(cfg, debug = false))

  dut.doSimUntilVoid { dut =>
    import dut._
    clockDomain.forkStimulus(period ns)

    io.start #= false
    io.start #= false
    io.ctrl.isCal #= false
    io.ctrl.isOutSideRead #= false
    io.ctrl.isOutSideWrite #= false
    io.outsideAddrOri.valid #= false
    io.outsideIdxOri.valid #= false
    clockDomain.waitSampling(10)

    io.ctrl.isOutSideWrite #= true
    clockDomain.waitSampling()
    val seq1 = Seq.range(0, g.BI).map(item => BigInt(item))
    for (i <- 0 until g.nttPoint / g.BI) {
      io.outsideAddrOri.valid #= true
      io.outsideIdxOri.valid #= true
      (0 until g.BI).map { j => g.BI * i + j }.zip(io.outsideWrDataArray.payload).foreach { case (t1, t2) => t2 #= t1 }
      io.outsideAddrOri.payload.foreach(item => item #= i)
      io.outsideIdxOri.payload.zip(seq1).foreach { case (t1, t2) => t1 #= t2 }
      clockDomain.waitSampling()
      io.outsideAddrOri.valid #= false
    }
    clockDomain.waitSampling(10)
    io.ctrl.isOutSideWrite #= false
    clockDomain.waitSampling(10)

    io.ctrl.isOutSideRead #= true
    clockDomain.waitSampling()
    for (i <- 0 until g.nttPoint / g.BI) {
      io.outsideAddrOri.valid #= true
      io.outsideIdxOri.valid #= true
      io.outsideAddrOri.payload.foreach(item => item #= i)
      io.outsideIdxOri.payload.zip(seq1).foreach { case (t1, t2) => t1 #= t2 }
      clockDomain.waitSampling()
      io.outsideAddrOri.valid #= false
    }
    clockDomain.waitSampling(10)
    io.ctrl.isOutSideRead #= false
    clockDomain.waitSampling(10)

    @volatile var flag = true
    val recIn = fork {
      val p_new = new PrintWriter("/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/sim/data/in_new.txt")
      while (flag) {
        if (io.bfuIn(0).valid.toBoolean) {
          val flatSeq = io.bfuIn.map{item => (item.payload.A.toBigInt,item.payload.B.toBigInt,item.payload.Tw.toBigInt)}
          flatSeq.map(p_new.println)

          clockDomain.waitSampling()
        } else { clockDomain.waitSampling() }
      }
      p_new.close()
    }
    val recOut = fork {
      val p_new = new PrintWriter("/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/sim/data/out_new.txt")
      while (flag) {
        if (io.bfuOut(0).valid.toBoolean) {
          val flatSeq = io.bfuOut.map{item => (item.payload.A.toBigInt,item.payload.B.toBigInt)}
          flatSeq.map(p_new.println)

          clockDomain.waitSampling()
        } else { clockDomain.waitSampling() }
      }
      p_new.close()
    }

    io.ctrl.isNtt #= true
    io.ctrl.isCal #= true
    clockDomain.waitSampling()
    io.start #= true
    clockDomain.waitSampling()
    io.start #= false
    clockDomain.waitActiveEdgeWhere(io.idle.toBoolean)
    clockDomain.waitSampling(50)
    io.ctrl.isCal #= false
    clockDomain.waitSampling(20)

    if (dut.io.idle.toBoolean) {
      flag = false
    }
    recIn.join()
    recOut.join()
    clockDomain.waitSampling(10)

    @volatile var flag_intt = true
    val recinttIn = fork {
      val p_new = new PrintWriter("/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/sim/data/intt_in_new.txt")
      while (flag_intt) {
        if (io.bfuIn(0).valid.toBoolean) {

          val flatSeq = io.bfuIn.map{item => (item.payload.A.toBigInt,item.payload.B.toBigInt,(g.Prime.toBigInt - item.payload.Tw.toBigInt))}
          flatSeq.map(p_new.println)
          clockDomain.waitSampling()
        } else { clockDomain.waitSampling() }
      }
      p_new.close()
    }
    val recinttOut = fork {

      val p_new = new PrintWriter("/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/sim/data/intt_out_new.txt")
      while (flag_intt) {
        if (io.bfuOut(0).valid.toBoolean) {
          val flatSeq = io.bfuOut.map{item => (item.payload.A.toBigInt,item.payload.B.toBigInt)}
          flatSeq.map(p_new.println)

          clockDomain.waitSampling()
        } else { clockDomain.waitSampling() }
      }
      p_new.close()
    }

    clockDomain.waitSampling(100)
    io.ctrl.isNtt #= false
    io.ctrl.isCal #= true
    clockDomain.waitSampling()
    io.start #= true
    clockDomain.waitSampling()
    io.start #= false
    clockDomain.waitActiveEdgeWhere(io.idle.toBoolean)
    clockDomain.waitSampling(20)
    io.ctrl.isCal #= false
    clockDomain.waitSampling(20)
    if (dut.io.idle.toBoolean) {
      flag_intt = false
    }
    recinttIn.join()
    recinttOut.join()
    clockDomain.waitSampling(10)

    io.ctrl.isOutSideRead #= true
    clockDomain.waitSampling()
    for (i <- 0 until g.nttPoint / g.BI) {
      io.outsideAddrOri.valid #= true
      io.outsideIdxOri.valid #= true
      io.outsideAddrOri.payload.foreach(item => item #= i)
      io.outsideIdxOri.payload.zip(seq1).foreach { case (t1, t2) => t1 #= t2 }
      clockDomain.waitSampling()
      io.outsideAddrOri.valid #= false
    }
    clockDomain.waitSampling(10)
    io.ctrl.isOutSideRead #= false
    clockDomain.waitSampling(10)
    simSuccess()
  }
}

object NttTopGenV extends App {
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "NttOpt/rtl/NttTop1",
    genLineComments = true
  ).generate(new NttTop(NttCfgParam(P= PrimeCfg(14,12),Bfu = BfuParamCfg(14,"9eg"),nttPoint = 1024, paraNum = 4)))
}

object NttTopVivadoFlow extends App {
  val g = NttCfgParam(P= PrimeCfg(64,32),Bfu = BfuParamCfg(64,"v7",spiltMul = false),nttPoint = 4096, paraNum = 8, nttSimPublic = false)
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "NttOpt/rtl/NttTop1",
    genLineComments = true
  ).generate(new NttTop(g))
  val useIp = false
  val workspace = "NttOpt/fpga/NttTop"
  val vivadopath = "/opt/Xilinx/Vivado/2023.1/bin"
  val family = g.family
  val device = g.device

  val frequency = 300 MHz
  val cpu = 16
  val useWrapRom = false
  val xcix = g.Bfu.pathMultIP

  val paths = if (useWrapRom) {
    Seq(
      "/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/rtl/NttTop1/NttTop.v",
      g.twFilePath
    )
  } else {
    Seq(
      "/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/rtl/NttTop1/NttTop.v",
      "/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/rtl/NttTop1/NttTop.v_toplevel_ctrlMem_dut_tw_rom_rom.bin"
    )
  }
  val rtl = new Rtl {
    /** Name */
    override def getName(): String = "NttTop"
    override def getRtlPaths(): Seq[String] = paths
  }
  val flow = VivadoFlow(vivadopath, workspace, rtl, family, device, frequency, cpu, xcix = xcix)
  println(s"${family} -> ${(flow.getFMax / 1e6).toInt} MHz ${flow.getArea} ")
}


case class test_Case(){
  var A = 0
  var B = 0
  var Tw = 0
}
object top_test {
  def main(args: Array[String]): Unit = {
    val g = new NttCfgParam()


    val testArray = Array.fill(8)(new test_Case)
    for(i <- 0 until 8){
      testArray(i).A = i
      testArray(i).B = i
      testArray(i).Tw = i
    }
    val flatseq = testArray.toSeq.map{ item => (item.A.toBigInt, item.B.toBigInt, item.Tw.toBigInt)}
    flatseq.mkString("\n")
    println(flatseq)
    val p = new PrintWriter("/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/sim/data/test.txt")
    flatseq.foreach(p.println)
    p.close()
  }
}
