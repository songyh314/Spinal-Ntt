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

case class CtrlMemTop(g: NttCfgParam) extends Component {
  val io = new Bundle {
    val start = in Bool ()
    val idle = out Bool ()
    val ctrl = in(CtrlBus())
    val outsideAddrOri = slave Flow Vec(UInt(g.BankAddrWidth bits), g.radix)
    val outsideIdxOri = slave Flow Vec(UInt(g.BankIndexWidth bits), g.BI)
    val outsideRdDataArray = master Flow Vec(Bits(g.width bits), g.BI)
    val outsideWrDataArray = slave Flow Vec(Bits(g.width bits), g.BI)

    val NttWriteBack = Array.fill(g.paraNum)(slave Flow DataPayload(g))
    val NttPayload = Array.fill(g.paraNum)(master Flow BfuPayload(g))
  }
  val ctrl = new CtrlOptAddr(g)
  val dut = new DataPathTop(g)
  ctrl.io.start := io.start
  ctrl.io.isNtt := io.ctrl.isNtt

  io.idle := ctrl.io.idle
  dut.io.ctrl := io.ctrl
  dut.io.bfuRdAddrOri := ctrl.io.RdAddrOri
  dut.io.bfuRdIdxOri := ctrl.io.RdLsbOri
  dut.io.twBus := ctrl.io.TwBus

  dut.io.outsideAddrOri := io.outsideAddrOri
  dut.io.outsideIdxOri := io.outsideIdxOri
  io.outsideRdDataArray := dut.io.outsideRdDataArray
  dut.io.outsideWrDataArray := io.outsideWrDataArray

  dut.io.NttWriteBack.zip(io.NttWriteBack).foreach { case (t1, t2) => t1 := t2 }
  io.NttPayload.zip(dut.io.NttPayload).foreach { case (t1, t2) => t1 := t2 }
}

case class NttTop(g: NttCfgParam, debug: Boolean = false) extends Component {
  val io = new Bundle {
    val start = in Bool ()

    val idle = out Bool ()
    val ctrl = in(CtrlBus())

    val outsideAddrOri = slave Flow Vec(UInt(g.BankAddrWidth bits), g.radix)
    val outsideIdxOri = slave Flow Vec(UInt(g.BankIndexWidth bits), g.BI)
    val outsideRdDataArray = master Flow Vec(Bits(g.width bits), g.BI)
    val outsideWrDataArray = slave Flow Vec(Bits(g.width bits), g.BI)

    val bfuOut = if (g.nttSimPublic) { Array.fill(g.paraNum)(master Flow (DataPayload(g))) }
    else { null }
    val bfuIn = if (g.nttSimPublic) { Array.fill(g.paraNum)(master Flow (BfuPayload(g))) }
    else { null }
  }
  noIoPrefix()
  val ctrlMem = new CtrlMemTop(g)
  val bfuArray = new BfuArray(g, debug)
  io.idle := ctrlMem.io.idle
  ctrlMem.io.start := io.start
  ctrlMem.io.ctrl := io.ctrl
  ctrlMem.io.outsideAddrOri := io.outsideAddrOri
  ctrlMem.io.outsideIdxOri := io.outsideIdxOri
  io.outsideRdDataArray := ctrlMem.io.outsideRdDataArray
  ctrlMem.io.outsideWrDataArray := io.outsideWrDataArray
  bfuArray.io.isNtt := io.ctrl.isNtt
  bfuArray.io.dataIn.toSeq.zip(ctrlMem.io.NttPayload.toSeq).foreach { case (t1, t2) => t1 := (t2) }
  ctrlMem.io.NttWriteBack.toSeq.zip(bfuArray.io.dataOut.toSeq).foreach { case (t1, t2) => t1 := (t2) }
  if (g.nttSimPublic) {
    io.bfuOut.toSeq.zip(bfuArray.io.dataOut.toSeq).foreach { case (t1, t2) => t1 := t2 }
    io.bfuIn.toSeq.zip(ctrlMem.io.NttPayload.toSeq).foreach { case (t1, t2) => t1 := t2 }
  }
}

object NttTopSim extends App {
  val period = 10
  val path = ArrayBuffer("/PRJ/SpinalHDL-prj/PRJ/myTest/test/hw/spinal/Ntt/xilinx_ip/")
  val dut = SimConfig.withXSim
    .withXilinxDevice("xczu9eg-ffvb1156-2-i")
    .workspacePath("./NttOpt/sim/")
    .withXSimSourcesPaths(path, path)
    .withWave
    .compile(new NttTop(NttCfgParam(nttPoint = 1024, paraNum = 4), debug = false))
  dut.doSim { dut =>
    import dut._
    SimTimeout(4000 * period)
    clockDomain.forkStimulus(period)

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
      val p = new PrintWriter("/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/sim/data/in.txt")
      while (flag) {
        if (io.bfuIn(0).valid.toBoolean) {
          val seq0 = (io.bfuIn(0).payload.A.toLong, io.bfuIn(0).payload.B.toLong, io.bfuIn(0).payload.Tw.toLong)
//          println(seq0)
          val seq1 = (io.bfuIn(1).payload.A.toLong, io.bfuIn(1).payload.B.toLong, io.bfuIn(1).payload.Tw.toLong)
//          println(seq1)
          val seq2 = (io.bfuIn(2).payload.A.toLong, io.bfuIn(2).payload.B.toLong, io.bfuIn(2).payload.Tw.toLong)
//          println(seq2)
          val seq3 = (io.bfuIn(3).payload.A.toLong, io.bfuIn(3).payload.B.toLong, io.bfuIn(3).payload.Tw.toLong)
//          println(seq3)
          p.println(f"$seq0")
          p.println(f"$seq1")
          p.println(f"$seq2")
          p.println(f"$seq3")
          clockDomain.waitSampling()
        } else { clockDomain.waitSampling() }
      }
      p.close()
    }
    val recOut = fork {
      val p = new PrintWriter("/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/sim/data/out.txt")
      while (flag) {
        if (io.bfuOut(0).valid.toBoolean) {
          val seq0 = (io.bfuOut(0).payload.A.toLong, io.bfuOut(0).payload.B.toLong)
          //          println(seq0)
          val seq1 = (io.bfuOut(1).payload.A.toLong, io.bfuOut(1).payload.B.toLong)
          //          println(seq1)
          val seq2 = (io.bfuOut(2).payload.A.toLong, io.bfuOut(2).payload.B.toLong)
          //          println(seq2)
          val seq3 = (io.bfuOut(3).payload.A.toLong, io.bfuOut(3).payload.B.toLong)
          //          println(seq3)
          p.println(f"$seq0")
          p.println(f"$seq1")
          p.println(f"$seq2")
          p.println(f"$seq3")
          clockDomain.waitSampling()
        } else { clockDomain.waitSampling() }
      }
      p.close()
    }
DataPathTop
    io.ctrl.isNtt #= true
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
      flag = false
    }
    recIn.join()
    recOut.join()
    clockDomain.waitSampling(10)

    @volatile var flag_intt = true
    val recinttIn = fork {
      val p = new PrintWriter("/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/sim/data/intt_in.txt")
      while (flag_intt) {
        if (io.bfuIn(0).valid.toBoolean) {
          val seq0 =
            (io.bfuIn(0).payload.A.toLong, io.bfuIn(0).payload.B.toLong, (g.Prime - io.bfuIn(0).payload.Tw.toLong))
          //          println(seq0)
          val seq1 =
            (io.bfuIn(1).payload.A.toLong, io.bfuIn(1).payload.B.toLong, (g.Prime - io.bfuIn(1).payload.Tw.toLong))
          //          println(seq1)
          val seq2 =
            (io.bfuIn(2).payload.A.toLong, io.bfuIn(2).payload.B.toLong, (g.Prime - io.bfuIn(2).payload.Tw.toLong))
          //          println(seq2)
          val seq3 =
            (io.bfuIn(3).payload.A.toLong, io.bfuIn(3).payload.B.toLong, (g.Prime - io.bfuIn(3).payload.Tw.toLong))
          //          println(seq3)
          p.println(f"$seq0")
          p.println(f"$seq1")
          p.println(f"$seq2")
          p.println(f"$seq3")
          clockDomain.waitSampling()
        } else { clockDomain.waitSampling() }
      }
      p.close()
    }
    val recinttOut = fork {
      val p = new PrintWriter("/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/sim/data/intt_out.txt")
      while (flag_intt) {
        if (io.bfuOut(0).valid.toBoolean) {
          val seq0 = (io.bfuOut(0).payload.A.toLong, io.bfuOut(0).payload.B.toLong)
          //          println(seq0)
          val seq1 = (io.bfuOut(1).payload.A.toLong, io.bfuOut(1).payload.B.toLong)
          //          println(seq1)
          val seq2 = (io.bfuOut(2).payload.A.toLong, io.bfuOut(2).payload.B.toLong)
          //          println(seq2)
          val seq3 = (io.bfuOut(3).payload.A.toLong, io.bfuOut(3).payload.B.toLong)
          //          println(seq3)
          p.println(f"$seq0")
          p.println(f"$seq1")
          p.println(f"$seq2")
          p.println(f"$seq3")
          clockDomain.waitSampling()
        } else { clockDomain.waitSampling() }
      }
      p.close()
    }

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
  val g = NttCfgParam(P= PrimeCfg(64,32),Bfu = BfuParamCfg(64,"v7"),nttPoint = 4096, paraNum = 8)
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
  val family = g.Bfu.device match {
    case "v7" => "Virtex 7"
    case "9eg" => "Zynq UltraScale+ MPSoCS"
  }
  val device = g.Bfu.device match {
    case "9eg" => "xczu9eg-ffvb1156-2-i"
    case "v7" => "xc7vx485tffg1157-1"
  }

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
