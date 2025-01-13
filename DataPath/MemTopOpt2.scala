package Ntt.DataPath

import Ntt.NttCfg._
import spinal.core._
import spinal.core.sim._
import myRam._
import spinal.lib._
import spinal.lib.eda.bench.Rtl
import spinal.lib.eda.xilinx.VivadoFlow

case class MemTopOpt2(g: NttCfg2414) extends Component {
  val io = new Bundle {
    val isOutSideRead = in Bool ()
    val isOutSideWrite = in Bool ()
    val isCal = in Bool ()
    val isNtt = in Bool ()
    val outsideAddrOri = slave Flow Vec(UInt(g.Log2NttPoints bits), g.BI)
    val outsideRdDataArray = master Flow Vec(Bits(g.width bits), g.BI)
    val outsideWrDataArray = slave Flow Vec(Bits(g.width bits), g.BI)
    val bfuRdAddrOri = slave Flow Vec(UInt(g.Log2NttPoints bits), g.BI)
    val NttPayload = Array.fill(g.paraNum)(master Flow BfuPayload(g))
    val NttWriteBack = Array.fill(g.paraNum)(slave Flow DataPayload(g))

    val twBus = slave Flow twPayload(addrWidth = g.twAddrWidth, muxWidth = log2Up(g.paraNum), para = g.paraNum)
  }
  noIoPrefix()

  val mem = new memBank(g)
  val mem_rd_IF = Array.fill(g.BI)(new myRamReadOnly(config = myRamConfig(g.width, g.BankAddrWidth)))
  val mem_wr_IF = Array.fill(g.BI)(new myRamWriteOnly(config = myRamConfig(g.width, g.BankAddrWidth)))

  val preCal = new Area {
    val preCalInst = new PreCal(g)
    val isOutside = io.isOutSideRead | io.isOutSideWrite
    preCalInst.io.oriAddr := isOutside ? io.outsideAddrOri.payload | io.bfuRdAddrOri.payload
    preCalInst.io.dataIn := io.outsideWrDataArray.payload
    val sel4mem_rd_demux = preCalInst.io.idxTrans
    val sel4mem_wr_mux = preCalInst.io.idxShuffleTrans
    val outsideWrData = preCalInst.io.dataBus
    val memOpAddr = preCalInst.io.AddrBus
    val re = (io.outsideAddrOri.valid && io.isOutSideRead) | (io.bfuRdAddrOri.valid && io.isCal)
    val we = (io.outsideAddrOri.valid && io.isOutSideWrite) | (io.bfuRdAddrOri.valid && io.isCal)
    val sel4mem_rd_demux_delay = Delay(Cat(sel4mem_rd_demux), g.ramLatency).addAttribute("srl_style", "srl")
  }

  val tw = new Area {
    val rom = new twRom(g)
    rom.io.twBus := Delay(io.twBus,(g.DecodeLatency + g.DatDeMuxLatency)).addAttribute("srl_style","srl_reg")
  }

  mem_rd_IF.toSeq.zip(preCal.memOpAddr).foreach { case (t1, t2) =>
    t1.rAddr := t2; t1.re := Delay(preCal.re, g.DecodeLatency)
  }

  val deMuxPayload = Vec(mem_rd_IF.map(item => item.rData).toSeq)
  val dataDeMux = RegNext(
    DataDeMux(deMuxPayload, Vec(preCal.sel4mem_rd_demux_delay.subdivideIn(g.BI slices).map(_.asUInt)))
  )
  val rdValidDelay = Delay(preCal.re, (g.ramLatency + g.DecodeLatency + g.DatDeMuxLatency))
  val wrValidDelay = Delay(preCal.we, (g.DecodeLatency ))
  io.outsideRdDataArray.payload.zip(dataDeMux).foreach { case (t1, t2) => t1 := t2 }
  io.outsideRdDataArray.valid := rdValidDelay && io.isOutSideRead

  io.NttPayload.toSeq.zip(dataDeMux.grouped(2).toSeq).foreach { case (t1, t2) =>
    t1.A := t2(0).asUInt; t1.B := t2(1).asUInt
    t1.valid := rdValidDelay && io.isCal
  }
  io.NttPayload.toSeq.zip(tw.rom.io.twData).foreach { case (t1, t2) => t1.payload.Tw := t2 }

  val innerDelay = new Area {
    val bfuValidDelay = Reg(
      Bits(g.bfuValidLoopLatency bits)
    ) init (0)
    bfuValidDelay := bfuValidDelay(
      g.bfuValidLoopLatency - 2 downto 0
    ) ## preCal.re
    val memAddrDelaySt1 =
      Delay(preCal.memOpAddr, g.addrNttLoopLatency).addAttribute("srl_style", "srl_reg")
    val memAddrDelaySt2 = Delay(memAddrDelaySt1, (g.BfuInttDelay - g.BfuNttDelay))
    val memMuxDealySt1 = {
      Delay(preCal.sel4mem_wr_mux, g.addrNttLoopLatency).addAttribute("srl_style", "srl_reg")
    }
    val memMuxDealySt2 = Delay(memMuxDealySt1, (g.BfuInttDelay - g.BfuNttDelay))
//    val bfuValid = bfuValidDelay(g.BfuInttDelay + g.ramLatency - 1)
    val bfuValid =
      io.isNtt ? bfuValidDelay(
        g.bfuValidLoopNttLatency - 1
      ) | bfuValidDelay(
        g.bfuValidLoopInttLatency - 1
      )
//    val memAddr = memAddrDelaySt1.subdivideIn(g.BI slices).map { _.asUInt }
    val memAddr = (io.isNtt ? memAddrDelaySt1 | memAddrDelaySt2)
//    val memMux = memMuxDealySt1.subdivideIn(g.BI slices).map { _.asUInt }
    val memMux = (io.isNtt ? memMuxDealySt1 | memMuxDealySt2)
  }

  val wrMem = new Area {
    val dutPayload = Vec(io.NttWriteBack.flatMap { item => Seq(item.payload.A, item.payload.B) }.toSeq)
    val payloadReorder = Vec(Bits(g.width bits), g.BI)
    val wrMemAddr = Vec(UInt(g.BankAddrWidth bits), g.BI)
    val wrMemData = Vec(Bits(g.width bits), g.BI)
    val memWe = Bool()
    def unitMux(sel: UInt): Bits = {
      dutPayload.read(sel).asBits
    }
    payloadReorder.zip(innerDelay.memMux).foreach { case (t1, t2) => t1 := unitMux(t2) }
    when(io.isCal) {
      wrMemData := payloadReorder
      wrMemAddr := Vec(innerDelay.memAddr)
    } otherwise {
      wrMemData := preCal.outsideWrData
      wrMemAddr := preCal.memOpAddr
    }
    memWe := io.isCal ? innerDelay.bfuValid | (io.isOutSideWrite ? wrValidDelay | False)
  }
  mem_wr_IF.toSeq.zip(wrMem.wrMemAddr).foreach { case (t1, t2) => t1.wAddr := t2 }
  mem_wr_IF.toSeq.zip(wrMem.wrMemData).foreach { case (t1, t2) => t1.wData := t2 }
  mem_wr_IF.toSeq.foreach { item => item.we := wrMem.memWe }

  mem.io.memIf.zip(mem_rd_IF.toSeq).foreach { case (t1, t2) => t1 << t2 }
  mem.io.memIf.zip(mem_wr_IF.toSeq).foreach { case (t1, t2) => t1 << t2 }
}

object MemTopOpt1GenV extends App {
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "./rtl/Ntt/DataPath/Mem"
  ).generate(new MemTopOpt2(NttCfg2414(nttPoint = 4096, paraNum = 4)))
}

object memTopOpt1Sim extends App {
  val period = 10
  val dut = SimConfig.withXSim.withWave.compile(new MemTopOpt2(NttCfg2414(nttPoint = 64)))
  dut.doSim("test") { dut =>
    SimTimeout(1000 * period)
    import dut._
    clockDomain.forkStimulus(period)
    dut.io.isOutSideRead #= false
    io.isNtt #= true
    io.isOutSideWrite #= false
    io.outsideAddrOri.valid #= false
    io.isCal #= false
    io.twBus.valid #= false
    clockDomain.waitSampling(10)
    io.isOutSideWrite #= true
    for (i <- 0 until g.nttPoint / g.BI) {
      io.outsideAddrOri.valid #= true
      (0 until g.BI).map { j => g.BI * i + j }.zip(io.outsideAddrOri.payload).foreach { case (t1, t2) => t2 #= t1 }
      (0 until g.BI).map { j => g.BI * i + j }.zip(io.outsideWrDataArray.payload).foreach { case (t1, t2) => t2 #= t1 }
      clockDomain.waitSampling()
      io.outsideAddrOri.valid #= false
    }
    clockDomain.waitSampling(10)
    io.isOutSideWrite #= false
    clockDomain.waitSampling(10)
    io.isOutSideRead #= true
    for (i <- 0 until g.nttPoint / g.BI) {
      io.outsideAddrOri.valid #= true
      (0 until g.BI).map { j => g.BI * i + j }.zip(io.outsideAddrOri.payload).foreach { case (t1, t2) => t2 #= t1 }
      clockDomain.waitSampling()
      io.outsideAddrOri.valid #= false
    }
    clockDomain.waitSampling(10)
    io.isOutSideRead #= false
    clockDomain.waitSampling(10)
    io.isCal #= true
    for (i <- 0 until g.nttPoint / g.BI) {
      io.bfuRdAddrOri.valid #= true
      (0 until g.BI).map { j => g.BI * i + j }.zip(io.bfuRdAddrOri.payload).foreach { case (t1, t2) => t2 #= t1 }
      clockDomain.waitSampling()
      io.bfuRdAddrOri.valid #= false
    }
    clockDomain.waitSampling(10)
//    io.isCal #= false
    clockDomain.waitSampling(10)
    for (i <- 0 until g.nttPoint / g.paraNum) {
      io.twBus.valid #= true
      io.twBus.payload.twAddr #= i
      (0 until g.paraNum).zip(io.twBus.payload.twMux).foreach { case (t1, t2) => t2 #= t1 }
      clockDomain.waitSampling()
      io.twBus.valid #= false
    }
    clockDomain.waitSampling(10)
  }
}

object MemTopOpt1VivadoFlow extends App {
  val g = NttCfg2414()
  val useIp = false
  val workspace = "./vivado_prj/Ntt/DataPath/Mem/MemTop2"
  val vivadopath = "/opt/Xilinx/Vivado/2023.1/bin"
  val family = "Zynq UltraScale+ MPSoCS"
  val device = "xczu9eg-ffvb1156-2-i"
//  val family = "Virtex 7"
//  val device = "xc7vx485tffg1157-1"
  val frequency = 300 MHz
  val cpu = 12
  val xcix = "/PRJ/SpinalHDL-prj/PRJ/myTest/test/hw/spinal/Ntt/xilinx_ip/mem.xcix"
  val paths = Seq(
    "/PRJ/SpinalHDL-prj/PRJ/myTest/test/rtl/Ntt/DataPath/Mem/MemTopOpt2.v",
    "/PRJ/SpinalHDL-prj/PRJ/myTest/test/hw/spinal/Ntt/xilinx_ip/bram.v"
  )
  if (g.useBramIP) {
    val rtl = new Rtl {

      /** Name */
      override def getName(): String = "MemTopOpt2"
      override def getRtlPaths(): Seq[String] = paths
    }
    val flow = VivadoFlow(vivadopath, workspace, rtl, family, device, frequency, cpu, xcix = xcix)
    println(s"${family} -> ${(flow.getFMax / 1e6).toInt} MHz ${flow.getArea} ")
  } else {
    val rtl = new Rtl {

      /** Name */
      override def getName(): String = "MemTopOpt2"
      override def getRtlPath(): String = "/PRJ/SpinalHDL-prj/PRJ/myTest/test/rtl/Ntt/DataPath/Mem/MemTopOpt2.v"
    }
    val flow = VivadoFlow(vivadopath, workspace, rtl, family, device, frequency, cpu)
    println(s"${family} -> ${(flow.getFMax / 1e6).toInt} MHz ${flow.getArea} ")
  }

}
