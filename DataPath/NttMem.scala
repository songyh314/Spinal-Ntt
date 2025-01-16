package Ntt.DataPath

import Ntt.NttCfg._
import myRam._
import spinal.core._
import spinal.lib._
import spinal.lib.eda.bench.Rtl
import spinal.lib.eda.xilinx.VivadoFlow

case class DataMem(width: Int, addrWidth: Int, latency: Int = 1, useIP: Boolean = false) extends Component {
  val io = new Bundle {
    val memIf = slave(myRamIF(config = myRamConfig(width, addrWidth, latency)))
  }
  if (useIP) {
    val _ = new Area {
      val bram = new bram()
      bram.io.addra := io.memIf.wAddr
      bram.io.dina := io.memIf.wData
      bram.io.wea := io.memIf.we
      bram.io.addrb := io.memIf.rAddr
      io.memIf.rData := bram.io.doutb
      bram.io.enb := io.memIf.re
    }
  } else {
    val mem = Mem(Bits(width bits), 1 << (addrWidth)).addAttribute("ram_style", "block")
    if (latency == 1) {
      mem.write(io.memIf.wAddr, io.memIf.wData, io.memIf.we)
      io.memIf.rData := mem.readSync(io.memIf.rAddr, io.memIf.re)
    } else if (latency == 2) {
      val wrAddr = RegNext(io.memIf.wAddr)
      val wData = RegNext(io.memIf.wData)
      val we = RegNext(io.memIf.we)
      val rdAddr = RegNext(io.memIf.rAddr)
      val re = RegNext(io.memIf.re)
      mem.write(wrAddr, wData, we)
      io.memIf.rData := mem.readSync(rdAddr, re)
    }
  }

}
case class bram(width: Int = 24, addrWidth: Int = 7, depth: Int = 128) extends BlackBox {
  val io = new Bundle {
    val clk = in Bool ()
    val wea = in Bool ()
    val addra = in UInt (addrWidth bits)
    val dina = in Bits (width bits)
    val enb = in Bool ()
    val addrb = in UInt (addrWidth bits)
    val doutb = out Bits (width bits)
  }
  noIoPrefix()
  mapClockDomain(clock = io.clk)
  addRTLPath("/PRJ/SpinalHDL-prj/PRJ/myTest/test/hw/spinal/Ntt/xilinx_ip/bram.v")
}

case class twRom(g: NttCfg2414) extends Component {
  val io = new Bundle {
//    val rdIf = slave (myRamReadOnly(config = myRamConfig(dataWidth = g.twWidth, addrWidth = g.twAddrWidth, readLatency = 1)))
//    val rAddr = in UInt (g.twAddrWidth bits)
//    val re = in Bool ()
    val twBus = slave Flow twPayload(
      addrWidth = g.twAddrWidth,
      muxWidth = log2Up(g.paraNum),
      para = g.paraNum
    )
    val twData = out Vec (UInt(g.width bits), g.paraNum)
  }

  def initTable = for (i <- 0 until (g.twNum)) yield {
    val ret: BigInt =
      ((BigInt(i) * 4 + 3) << 72) + ((BigInt(i) * 4 + 2) << 48) + ((BigInt(i) * 4 + 1) << 24) + (BigInt(i) * 4)
//    println(ret)
    B(ret, g.twWidth bits)
  }
  val tw = g.twCompress.map(B(_,g.twWidth bits))
  val tw128 = g.twCompress128.map(B(_,g.twWidth bits))
//  val rom = Mem(Bits(g.twWidth bits),g.nttPoint/g.paraNum)
  val rom = Mem(Bits(g.twWidth bits), initialContent = tw)
  val muxReg = RegNextWhen(io.twBus.payload.twMux, io.twBus.valid)
  val readSeq = rom.readSync(io.twBus.payload.twAddr, io.twBus.valid)
  val sliceSeq = readSeq.subdivideIn(g.paraNum slices).map(_.asUInt)
  def readMux(sel: UInt): UInt = {
    val ret = sliceSeq.read(sel)
    ret
  }
  io.twData.zip(muxReg).foreach { case (t1, t2) => t1 := readMux(t2) }
}
object twRomGenV extends App {
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "./rtl/Ntt/memBank",
    genLineComments = true
  ).generate(new twRom(NttCfg2414()))
}

case class memBank(g: NttCfg2414) extends Component {
  val io = new Bundle {
    val memIf = Array.fill(g.BI)(slave(myRamIF(config = myRamConfig(g.width, g.BankAddrWidth))))
  }
  val memArray = Array.fill(g.BI)(new DataMem(g.width, g.BankAddrWidth, g.ramLatency, useIP = g.useBramIP))
  io.memIf.zip(memArray.map(_.io.memIf)).foreach { case (t, t1) => t >> t1 }
}

object memBankGenV extends App {
  SpinalConfig(mode = Verilog, targetDirectory = "./rtl/Ntt/memBank").generate(new memBank(NttCfg2414()))
}

object MemBankVivadoFlow extends App {
  val workspace = "./vivado_prj/Ntt/DataPath/Mem/MemBank"
  val vivadopath = "/opt/Xilinx/Vivado/2023.1/bin"
  val family = "Zynq UltraScale+ MPSoCS"
  val device = "xczu9eg-ffvb1156-2-i"
  val frequency = 300 MHz
  val cpu = 12
  val rtl = new Rtl {

    /** Name */
    override def getName(): String = "memBank"
    override def getRtlPath(): String = "/PRJ/SpinalHDL-prj/PRJ/myTest/test/rtl/Ntt/memBank/memBank.v"
  }

  val flow = VivadoFlow(vivadopath, workspace, rtl, family, device, frequency, cpu)
  println(s"${family} -> ${(flow.getFMax / 1e6).toInt} MHz ${flow.getArea} ")
}
