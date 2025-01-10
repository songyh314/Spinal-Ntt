package Ntt.DataPath
import Ntt.NttCfg.NttCfg2414
import myTools.StreamRenameUtil
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import Ntt.NttCfg._
import myRam._
import myTools._
import spinal.lib.eda.bench.Rtl
import spinal.lib.eda.xilinx.VivadoFlow

import scala.collection.mutable

case class DecodeBus(idx: Int, addr: Int) extends Bundle {
  val BankIdx = UInt(idx bits)
  val BankAddr = UInt(addr bits)
}
case class DecodeUnit(g: NttCfg2414) extends Component {
  val io = new Bundle {
    val unitAddrOri = in UInt (g.Log2NttPoints bits)
    val unitBankIdx = out UInt (g.BankIndexWidth bits)
    val unitBankAddr = out UInt (g.BankAddrWidth bits)
  }
  noIoPrefix()
  val bankidx = Reg(UInt(g.BankIndexWidth bits))
  val bankaddr = Reg(UInt(g.BankAddrWidth bits))
  val msb = io.unitAddrOri(g.Log2NttPoints - 1 downto g.BankIndexWidth)
  val lsb = io.unitAddrOri(g.BankIndexWidth - 1 downto 0)
  val sn = msb.subdivideIn(1 bits).reduceBalancedTree(_ +^ _).resize(3 bits)
  val bi = lsb +^ (sn(log2Up(g.radix) - 1 downto 0) ## (B"1'b0" #* (log2Up(g.paraNum)))).asUInt
  bankaddr := msb
  bankidx := bi(g.BankIndexWidth - 1 downto 0)
  io.unitBankAddr := bankaddr
  io.unitBankIdx := bankidx
}
object DecodeUnit {
  def apply(addr: UInt, config: NttCfg2414): DecodeUnit = {
    val uDecode = new DecodeUnit(config)
    uDecode.io.unitAddrOri := addr
    uDecode
  }
}

case class AddrDecode(g: NttCfg2414) extends Component {
  val io = new Bundle {
    val addrOri = in Vec (UInt(g.Log2NttPoints bits), g.BI)
    val BankBus = out Vec (DecodeBus(idx = g.BankIndexWidth, addr = g.BankAddrWidth), g.BI)
  }
  noIoPrefix()
  StreamRenameUtil(this)
  val DecodeArray = Array.fill(g.BI)(new DecodeUnit(g))
  DecodeArray.toSeq.zip(io.addrOri).foreach { case (t, t1) => t.io.unitAddrOri := t1 }
  io.BankBus.zip(DecodeArray.toSeq).foreach { case (t, t1) =>
    t.BankIdx := t1.io.unitBankIdx
    t.BankAddr := t1.io.unitBankAddr
  }
}
object AddrDecode {
  def apply(dataIn: Vec[UInt], config: NttCfg2414): Vec[DecodeBus] = {
    val uAddrDecode = new AddrDecode(config)
    uAddrDecode.io.addrOri := dataIn
    uAddrDecode.io.BankBus
  }
}

object AddrDecodeGenV extends App {
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "./rtl/Ntt/DataPath"
  ).generate(new AddrDecode(NttCfg2414()))
}

case class idxMux(g: NttCfg2414) extends Component {
  val io = new Bundle {
    val BankIndex = in Vec (UInt(g.BankIndexWidth bits), g.BI)
    val decodeIdx = out Vec (UInt(g.BankIndexWidth bits), g.BI)
  }
  noIoPrefix()
//  val base = Seq(0 until g.BI).map(U(_,g.BankIndexWidth bits))
  val base = Vec(UInt(g.BankIndexWidth * g.BI bits), g.BI)
  base := Vec((0 until g.BI).map(U(_).resized))
  val dec = Vec(
    base
      .zip(io.BankIndex)
      .map { p => p._1 |<< (p._2 * g.BankIndexWidth) }
      .map(_.asBits)
      .reduceBalancedTree(_ | _)
      .subdivideIn(g.BI slices)
      .map(_.asUInt)
  )
  io.decodeIdx := dec
}
object idxMux {
  def apply(dataIn: Vec[UInt], config: NttCfg2414): idxMux = {
    val uidxMux = new idxMux(config)
    uidxMux.io.BankIndex := dataIn
    uidxMux
  }
}
case class idxShuffle(idxWidth: Int, n: Int) extends Component {
  val io = new Bundle {
    val bankIdx = in Vec (UInt(idxWidth bits), n)
    val shuffleIdx = out Vec (UInt(idxWidth bits), n)
  }
  noIoPrefix()
  val base = Vec(UInt(idxWidth * n bits), n)
  base := Vec((0 until n).map(U(_).resized))
  val ret = Vec(
    base
      .zip(io.bankIdx)
      .map { p => p._1 |<< (p._2 * idxWidth) }
      .map(_.asBits)
      .reduceBalancedTree(_ | _)
      .subdivideIn(n slices)
      .map(_.asUInt)
  )
  io.shuffleIdx := ret
}
object idxShuffle {
  def apply(dataIn: Vec[UInt]): Vec[UInt] = {
    val width = dataIn.head.getWidth
    val n = dataIn.size
    val dut = new idxShuffle(width, n)
    dut.io.bankIdx := dataIn
    dut.io.shuffleIdx
  }
}

case class writebackMux(g: NttCfg2414) extends Component {
  val io = new Bundle {
    val dataIn = in Vec (Bits(g.width bits), g.BI)
    val sel = in Vec (UInt(g.BankIndexWidth bits), g.BI)
    val dataOut = out Vec (Bits(g.width bits), g.BI)
  }
  def mux(sel: UInt): Bits = {
    io.dataIn.read(sel)
  }
  io.dataOut.zip(io.sel).foreach { case (t1, t2) => t1 := mux(t2) }
}
object writebackMuxGenV extends App {
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "./rtl/Ntt/DataPath"
  ).generate(new writebackMux(NttCfg2414()))
}
object writebackMuxVivadoFlow extends App {

  val workspace = "./vivado_prj/Ntt/DataPath/AddrDecode"
  val vivadopath = "/opt/Xilinx/Vivado/2023.1/bin"
  val family = "Zynq UltraScale+ MPSoCS"
  val device = "xczu9eg-ffvb1156-2-i"
  val frequency = 300 MHz
  val cpu = 16
  val rtl = new Rtl {

    /** Name */
    override def getName(): String = "writebackMux"

    override def getRtlPath(): String = "/PRJ/SpinalHDL-prj/PRJ/myTest/test/rtl/Ntt/DataPath/writebackMux.v"
  }
  val flow = VivadoFlow(vivadopath, workspace, rtl, family, device, frequency, cpu)
  println(s"${family} -> ${(flow.getFMax / 1e6).toInt} MHz ${flow.getArea} ")
}
case class AddrMux(g: NttCfg2414) extends Component {
  val io = new Bundle {
    val BankBus = in Vec (DecodeBus(idx = g.BankIndexWidth, addr = g.BankAddrWidth), g.BI)
    val AddrBus = out Vec (UInt(g.BankAddrWidth bits), g.BI)
    val IdxTrans = out Vec (UInt(g.BankIndexWidth bits), g.BI)
//    val muxRamIF = Array.fill(g.BI)(master(myRamIF(config = myRamConfig(g.width, g.BankAddrWidth))))
  }
  noIoPrefix()
  val clusterAddr = Vec(for (i <- 0 until g.BI) yield { io.BankBus(i).BankAddr })
  val clusterIdx = Vec(for (j <- 0 until g.BI) yield { io.BankBus(j).BankIdx })
  val decIdx = idxMux(clusterIdx, g).io.decodeIdx
  def AddrMux(idx: UInt): UInt = {
    clusterAddr.read(idx)
  }
  io.AddrBus.zip(decIdx).map { case (t1, t2) => t1 := (AddrMux(t2)) }
  io.IdxTrans := clusterIdx
}

case class reOrder(idxWidth: Int, dataWidth: Int, addrWidth: Int, n: Int, useData: Boolean) extends Component {
  val io = new Bundle {
    val BankBus = in Vec (DecodeBus(idx = idxWidth, addr = addrWidth), n)
    val dataIn = in Vec (Bits(dataWidth bits), n)
    val idxTrans = if (!useData) (out Vec (UInt(idxWidth bits), n)) else (null)
    val AddrBus = out Vec (UInt(addrWidth bits), n)
    val dataBus = if (useData) { out Vec (Bits(dataWidth bits), n) }
    else null
  }
  noIoPrefix()
  val clusterAddr = Vec(for (i <- 0 until n) yield { io.BankBus(i).BankAddr })
  val clusterIdx = Vec(for (j <- 0 until n) yield { io.BankBus(j).BankIdx })
  val shuffleIdx = idxShuffle(clusterIdx)

  def addrMux(idx: UInt): UInt = {
    clusterAddr.read(idx)
  }
  def dataMux(idx: UInt): Bits = {
    io.dataIn.read(idx)
  }
  io.AddrBus.zip(shuffleIdx).map { case (t1, t2) => t1 := addrMux(t2) }
  if (useData) {
    io.dataBus.zip(shuffleIdx).map { case (t1, t2) => t1 := dataMux(t2) }
  } else {
    io.idxTrans := clusterIdx
  }
}

//对外部读取地址进行译码,输出mem的接口数据
//*mem读出的数据仍需要过一遍mux进行重排
//对外部输入数据的地址和数据进行译码和重排
case class PreCal(g: NttCfg2414) extends Component {
  val io = new Bundle {
    val oriAddr = in Vec (UInt(log2Up(g.nttPoint) bits), g.BI)
    val dataIn = in Vec (Bits(g.width bits), g.BI)
    val AddrBus = out Vec (UInt(g.BankAddrWidth bits), g.BI)
    val idxTrans = out Vec (UInt(g.BankIndexWidth bits), g.BI)
    val idxShuffleTrans = out Vec (UInt(g.BankIndexWidth bits), g.BI)
    val dataBus = out Vec (Bits(g.width bits), g.BI)
  }
  val uAddrDecode = new AddrDecode(g)

  uAddrDecode.io.addrOri := io.oriAddr
  val addr_dec = uAddrDecode.io.BankBus
  val datainDelay = Delay(io.dataIn, g.DecodeCalLatency)

  val reorder = new Area {
    val clusterAddr = Vec(for (i <- 0 until g.BI) yield { addr_dec(i).BankAddr })
    val clusterIdx = Vec(for (j <- 0 until g.BI) yield { addr_dec(j).BankIdx })
    val shuffleIdx = idxShuffle(clusterIdx)

    def addrMux(idx: UInt): UInt = {
      clusterAddr.read(idx)
    }
    def dataMux(idx: UInt): Bits = {
      datainDelay.read(idx)
    }
    io.AddrBus.zip(shuffleIdx).foreach { case (t1, t2) => t1 := RegNext(addrMux(t2)) }
    io.dataBus.zip(shuffleIdx).foreach { case (t1, t2) => t1 := RegNext(dataMux(t2)) }
    io.idxTrans := RegNext(clusterIdx)
    io.idxShuffleTrans := RegNext(shuffleIdx)
  }
}

case class memPreCal(g: NttCfg2414, useData: Boolean) extends Component {
  val io = new Bundle {
    val oriAddr = in Vec (UInt(log2Up(g.nttPoint) bits), g.BI)
    val dataIn = if (useData) { in Vec (Bits(g.width bits), g.BI) }
    else { null }
    val AddrBus = out Vec (UInt(g.BankAddrWidth bits), g.BI)
    val idxTrans = if (!useData) (out Vec (UInt(g.BankIndexWidth bits), g.BI)) else { null }
    val dataBus = if (useData) { out Vec (Bits(g.width bits), g.BI) }
    else null
  }
  val uAddrDecode = new AddrDecode(g)
//  val uAddrMux = new AddrMux(g)
  uAddrDecode.io.addrOri := io.oriAddr
  val addr_dec = uAddrDecode.io.BankBus
  private val reOrderInst = new reOrder(
    idxWidth = g.BankIndexWidth,
    dataWidth = g.width,
    addrWidth = g.BankAddrWidth,
    n = g.BI,
    useData = useData
  )
  reOrderInst.io.BankBus := addr_dec
  io.AddrBus := reOrderInst.io.AddrBus
  if (useData) {
    reOrderInst.io.dataIn := RegNext(io.dataIn)
    io.dataBus := reOrderInst.io.dataBus
  } else {
    io.idxTrans := reOrderInst.io.idxTrans
  }
}
object memPreCalGenV extends App {
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "./rtl/Ntt/DataPath"
  ).generate(new memPreCal(NttCfg2414(), true))
}
object reOrderGenV extends App {
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "./rtl/Ntt/DataPath"
  ).generate(new reOrder(3, 24, 7, 8, false))
}
case class DataDeMux(dataWidth: Int, idxWidth: Int, n: Int) extends Component {
  val io = new Bundle {
    val DataBus = in Vec (Bits(dataWidth bits), n)
    val IdxTrans = in Vec (UInt(idxWidth bits), n)
    val DataDeMux = out Vec (Bits(dataWidth bits), n)
  }
  noIoPrefix()
  def DeMux(idx: UInt): Bits = {
    io.DataBus.read(idx)
  }
  io.DataDeMux.zip(io.IdxTrans).foreach { case (t1, t2) => t1 := (DeMux(t2)) }
}
object DataDeMux {
  def apply(dataIn: Vec[Bits], idx: Vec[UInt]) = {
    val dataW = dataIn.head.getWidth
    val idxW = idx.head.getWidth
    val n = dataIn.size
    val dut = new DataDeMux(dataW, idxW, n)
    dut.io.DataBus := dataIn
    dut.io.IdxTrans := idx
    dut.io.DataDeMux
  }
}
case class WriteBackArbit(g: NttCfg2414) extends Component {
  val io = new Bundle {
    val dataIn = in Vec (UInt(g.width bits), g.BI)
    val idxIn = in Vec (UInt(g.BankIndexWidth bits), g.BI)
    val dataOut = out Vec (UInt(g.width bits), g.BI)
  }
  noIoPrefix()
  def DeMux(idx: UInt): UInt = {
    io.dataIn.read(idx)
  }
  io.dataOut.zip(io.idxIn).foreach { case (t1, t2) => t1 := DeMux(t2) }
}
object WriteBackArbit {
  def apply(g: NttCfg2414, dataIn: Array[Flow[DataPayload]], idx: Vec[UInt]): WriteBackArbit = {
    val WriteBackPayload = Vec(dataIn.flatMap { item => Seq(item.payload.A, item.payload.B) }.toSeq)
    val dut = new WriteBackArbit(g)
    dut.io.dataIn := WriteBackPayload; dut.io.idxIn := idx
    dut
  }
}
object WriteBackArbitGenV extends App {
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "./rtl/Ntt/DataPath"
  ).generate(new WriteBackArbit(NttCfg2414()))
}
object WriteBackArbitVivadoFlow extends App {

  val workspace = "./vivado_prj/Ntt/DataPath/AddrDecode"
  val vivadopath = "/opt/Xilinx/Vivado/2023.1/bin"
  val family = "Zynq UltraScale+ MPSoCS"
  val device = "xczu9eg-ffvb1156-2-i"
  val frequency = 300 MHz
  val cpu = 16
  val rtl = new Rtl {

    /** Name */
    override def getName(): String = "WriteBackArbit"
    override def getRtlPath(): String = "/PRJ/SpinalHDL-prj/PRJ/myTest/test/rtl/Ntt/DataPath/WriteBackArbit.v"
  }

  val flow = myVivadoFlow(vivadopath, workspace, rtl, family, device, frequency, cpu)
  println(s"${family} -> ${(flow.getFMax / 1e6).toInt} MHz ${flow.getArea} ")
}

case class rdAddrMux2(g: NttCfg2414) extends Component {
  val io = new Bundle {
    val BankBus = in Vec (DecodeBus(idx = g.BankIndexWidth, addr = g.BankAddrWidth), g.BI)
    val RdAddrBus = out Vec (UInt(g.BankAddrWidth bits), g.BI)
    val IdxTrans = out Vec (UInt(g.BankIndexWidth bits), g.BI)
    //    val muxRamIF = Array.fill(g.BI)(master(myRamIF(config = myRamConfig(g.width, g.BankAddrWidth))))
  }
  noIoPrefix()
  val clusterAddr = Vec(for (i <- 0 until g.BI) yield { io.BankBus(i).BankAddr })
  val clusterIdx = Vec(for (j <- 0 until g.BI) yield { io.BankBus(j).BankIdx })
  val decIdx = idxMux(clusterIdx, g).io.decodeIdx
  def AddrMux(idx: UInt): UInt = {
    clusterAddr.read(idx)
  }
  io.RdAddrBus.zip(decIdx).map { case (t1, t2) => t1 := RegNext(AddrMux(t2)) }
  io.IdxTrans := RegNext(clusterIdx)
}
//object AddrMuxGenV extends App {
//  SpinalConfig(
//    mode = Verilog,
//    nameWhenByFile = false,
//    anonymSignalPrefix = "tmp",
//    targetDirectory = "./rtl/Ntt/DataPath"
//  ).generate(new DataDeMux(NttCfg2414()))
//}

object DecodeUnitSim extends App {
  val period = 10
  val dut = SimConfig.withXSim.withWave.compile(new DecodeUnit(NttCfg2414(nttPoint = 128)))
  dut.doSim("test") { dut =>
    import dut._
    SimTimeout(1000 * period)
    clockDomain.forkStimulus(period)
    io.unitAddrOri #= 0
    clockDomain.waitSampling(10)
    for (i <- 0 until (g.nttPoint)) {
      io.unitAddrOri #= i
      clockDomain.waitSampling()
    }
    clockDomain.waitSampling(10)
    simSuccess()
  }
}

object AddrDecodeVivadoFlow extends App {

  val workspace = "./vivado_prj/Ntt/DataPath/AddrDecode"
  val vivadopath = "/opt/Xilinx/Vivado/2023.1/bin"
  val family = "Zynq UltraScale+ MPSoCS"
  val device = "xczu9eg-ffvb1156-2-i"
  val frequency = 300 MHz
  val cpu = 8
  val rtl = new Rtl {

    /** Name */
    override def getName(): String = "AddrDecode"
    override def getRtlPath(): String = "/PRJ/SpinalHDL-prj/PRJ/myTest/test/rtl/Ntt/DataPath/AddrDecode.v"
  }

  val flow = VivadoFlow(vivadopath, workspace, rtl, family, device, frequency, cpu)
  println(s"${family} -> ${(flow.getFMax / 1e6).toInt} MHz ${flow.getArea} ")
}
object AddrMuxVivadoFlow extends App {

  val workspace = "./vivado_prj/Ntt/DataPath/AddrMux/rdAddrMux2"
  val vivadopath = "/opt/Xilinx/Vivado/2023.1/bin"
  val family = "Zynq UltraScale+ MPSoCS"
  val device = "xczu9eg-ffvb1156-2-i"
  val frequency = 400 MHz
  val cpu = 8
  val rtl = new Rtl {

    /** Name */
    override def getName(): String = "rdAddrMux2"
    override def getRtlPath(): String = "/PRJ/SpinalHDL-prj/PRJ/myTest/test/rtl/Ntt/DataPath/rdAddrMux2.v"
  }

  val flow = VivadoFlow(vivadopath, workspace, rtl, family, device, frequency, cpu)
  println(s"${family} -> ${(flow.getFMax / 1e6).toInt} MHz ${flow.getArea} ")
}
object AddrMuxVivadoFlow2 extends App {

  val workspace = "./vivado_prj/Ntt/DataPath/AddrMux/idxMux"
  val vivadopath = "/opt/Xilinx/Vivado/2023.1/bin"
  val family = "Zynq UltraScale+ MPSoCS"
  val device = "xczu9eg-ffvb1156-2-i"
  val frequency = 300 MHz
  val cpu = 8
  val rtl = new Rtl {

    /** Name */
    override def getName(): String = "idxMux"
    override def getRtlPath(): String = "/PRJ/SpinalHDL-prj/PRJ/myTest/test/rtl/Ntt/DataPath/idxMux.v"
  }

  val flow = VivadoFlow(vivadopath, workspace, rtl, family, device, frequency, cpu)
  println(s"${family} -> ${(flow.getFMax / 1e6).toInt} MHz ${flow.getArea} ")
}

case class unitMux(g: NttCfg2414) extends Component {
  val io = new Bundle {
    //    val BankBus = Array.fill(g.BI)(slave Flow DecodeBus(idx = g.BankIndexWidth, addr = g.BankAddrWidth))
    val idx = in UInt (g.BankIndexWidth bits)
    val BankBus = in Vec (DecodeBus(idx = g.BankIndexWidth, addr = g.BankAddrWidth), g.BI)
    val decAddr = out UInt (g.BankAddrWidth bits)
  }
  noIoPrefix()
  val clusterAddr = Vec(for (i <- 0 until g.BI) yield { io.BankBus(i).BankAddr })
  val clusterIdx = Vec(for (j <- 0 until g.BI) yield { io.BankBus(j).BankIdx })
  val find = clusterIdx.sFindFirst(_ === io.idx)
  io.decAddr := RegNext(clusterAddr(find._2))
}
case class unitMux2(g: NttCfg2414) extends Component {
  val io = new Bundle {
    val idx = in UInt (g.BankIndexWidth bits)
    val BankBus = in Vec (DecodeBus(idx = g.BankIndexWidth, addr = g.BankAddrWidth), g.BI)
    val decAddr = out UInt (g.BankAddrWidth bits)
  }
  noIoPrefix()
  val clusterAddr = Vec(for (i <- 0 until g.BI) yield { io.BankBus(i).BankAddr })
  val onehot = Vec(io.idx, g.BI).zip(io.BankBus).map { case (t1, t2) => t1 === t2.BankIdx }
  io.decAddr := RegNext(MuxOH(onehot, clusterAddr))
}
case class unitMux3(g: NttCfg2414) extends Component {
  val io = new Bundle {
    val BankBus = in Vec (DecodeBus(idx = g.BankIndexWidth, addr = g.BankAddrWidth), g.BI)
    val decAddr = out Vec (UInt(g.BankAddrWidth bits), g.BI)
  }
  noIoPrefix()
  val clusterAddr = Vec(for (i <- 0 until g.BI) yield { io.BankBus(i).BankAddr.resize(g.BI * g.BankAddrWidth bits) })
  val clusterIdx = Vec(for (j <- 0 until g.BI) yield { io.BankBus(j).BankIdx })
  io.decAddr := RegNext(
    Vec(
      clusterAddr
        .zip(clusterIdx)
        .map { p => p._1 |<< (p._2 * g.BankAddrWidth) }
        //        .map { p => p._1 |<< (p._2##(B"1'b0"#*log2Up(g.BI))).asUInt }
        .map(_.asBits)
        .reduceBalancedTree(_ | _)
        .subdivideIn(g.BI slices)
        .map(_.asUInt)
    )
  )
}
case class Shuffle(g: NttCfg2414) extends Component {
  val io = new Bundle {
    val BankBus = slave Flow Vec(DecodeBus(idx = g.BankIndexWidth, addr = g.BankAddrWidth), g.BI)
    val decAddr = master Flow Vec(UInt(g.BankAddrWidth bits), g.BI)
  }
  noIoPrefix()
  val clusterAddr = Vec(for (i <- 0 until g.BI) yield { io.BankBus.payload(i).BankAddr })
  val clusterIdx = Vec(for (j <- 0 until g.BI) yield { io.BankBus.payload(j).BankIdx })
  val ShuffleAddr = Reg(cloneOf(clusterAddr))
  for (i <- 0 until (g.BI)) {
    ShuffleAddr(clusterIdx(i)) := clusterAddr(i)
  }
  io.decAddr.payload := ShuffleAddr
  io.decAddr.valid := RegNext(io.BankBus.valid)
}
