package Ntt.DataPath
import Ntt.NttCfg._
import spinal.core._
import spinal.core.sim._
import myRam._
import spinal.lib._
import myRam._
import spinal.lib.eda.bench.Rtl
import spinal.lib.eda.xilinx.VivadoFlow
import spinal.lib.memory.sdram.dfi.function.RdDataRxd
import spinal.sim.DataType

//val WrBackCal = new Area {
//  val WrBackIdx = Mux(
//    io.isNtt,
//    Vec(IdxDelaySt1.subdivideIn(g.BI slices).map { _.asUInt }),
//    Vec(IdxDelaySt2.subdivideIn(g.BI slices).map { _.asUInt })
//  )
//  val uWriteBackArbit = WriteBackArbit(g, io.NttWriteBack, WrBackIdx)
//  val WrBackData = uWriteBackArbit.io.dataOut
//  val WrBackAddr = io.isNtt ? Vec(AddrDelaySt1.subdivideIn(g.BI slices).map { _.asUInt }) | Vec(
//    AddrDelaySt2.subdivideIn(g.BI slices).map { _.asUInt }
//  )
//  val WrBackValid =
//    io.isNtt ? ValidDelaySeq(g.BfuNttDelay + g.ramLatency - 1) | ValidDelaySeq(g.BfuInttDelay + g.ramLatency - 1)
//}
case class wrBackCal(idxWidth:Int, addrWidth:Int, n:Int) extends Component{
    val io = new Bundle{
      val idxDelaySt1 = in Bits(idxWidth*n bits)
      val idxDelaySt2 = in Bits(idxWidth*n bits)
      val addrDelaySt1 = in Bits(addrWidth*n bits)
      val addrDelaySt2 = in Bits(addrWidth*n bits)


    }
}

case class MemTop(g: NttCfg2414) extends Component {
  val io = new Bundle {
    val isOutSideRead = in Bool ()
    val isOutSideWrite = in Bool ()
    val isNtt = in Bool ()
    val RdAddrOri = slave Flow Vec(UInt(g.Log2NttPoints bits), g.BI)
    val RdDataArray = master Flow Vec(UInt(g.width bits), g.BI)
    val WrBus = slave Flow ParaWriteBus(DataWidth = g.width, AddrWidth = g.Log2NttPoints, para = g.BI)
    val NttPayload = Array.fill(g.paraNum)(master Flow BfuPayload(g))
    val NttWriteBack = Array.fill(g.paraNum)(slave Flow DataPayload(g))

  }
  noIoPrefix()

  // -----------------------------------------------------------
  // ReadAddr->Decode(pip)->AddrMux(cb)->mem(Data)->WrBackArb(cb)->isOutsideRead?io|Bfu
  // AddrMux  -> Delay -> mem(WriteBack)
  // Bfu(pip) -> Mux -> mem(WriteBack)
  // isOutsideWrite: WrAddr -> Decode -> AddrMux -> mem
  // -----------------------------------------------------------
  private def connectMemRead(
      IF: Array[myRamReadOnly],
      BFU: Array[Flow[BfuPayload]],
      ioData: Flow[Vec[UInt]],
      RdAddr: Vec[UInt],
      AddrValid: Bool,
      DataValid: Bool,
      isOut: Bool
  ): Unit = {
    val BfuValid = DataValid && isOut === False
    val ioValid = DataValid && isOut === True
    BFU.toSeq.zip(IF.grouped(2).toSeq).foreach { case (t1, t2) =>
      t1.A := t2(0).rData.asUInt; t1.B := t2(1).rData.asUInt; t1.Tw.assignDontCare();
      t1.valid := BfuValid
    }
    ioData.payload.zip(IF).foreach { case (t1, t2) =>
      t1 := t2.rData.asUInt
    }
    ioData.valid := ioValid
    IF.toSeq.zip(RdAddr).foreach {
      case (t1, t2) => { t1.rAddr := t2; t1.re := AddrValid }
    }
  }
  private def connectMemWrite(
      IF: Array[myRamWriteOnly],
      ioBusData: Vec[UInt],
      ioBusDecodeAddr: Vec[UInt],
      ioBusValid: Bool,
      WrBackAddr: Vec[UInt],
      WrBackData: Vec[UInt],
      WrBackValid: Bool,
      isOut: Bool
  ): Unit = {
    val IfDataVec = WrBackData.zip(ioBusData).map { case (t1, t2) => isOut ? t2 | t1 }
    val IfAddrVec = WrBackAddr.zip(ioBusDecodeAddr).map { case (t1, t2) => isOut ? t2 | t1 }
    val IfValidVec = Array.fill(g.BI)(Bool())
    IfValidVec.foreach(_ := isOut ? ioBusValid | WrBackValid)
    IF.toSeq.zip(IfAddrVec).foreach { case (t1, t2) => t1.wAddr := t2 }
    IF.toSeq.zip(IfValidVec).foreach { case (t1, t2) => t1.we := t2 }
    IF.toSeq.zip(IfDataVec).foreach { case (t1, t2) => t1.wData := t2.asBits }
  }

  private def genFlow[T <: Data](dataIn: T, valid: Bool): Flow[T] = {
    val ret = Flow(cloneOf(dataIn))
    ret.payload := dataIn
    ret.valid := valid
    ret
  }
  val mem = new memBank(g)
  val mem_rd_IF = Array.fill(g.BI)(new myRamReadOnly(config = myRamConfig(g.width, g.BankAddrWidth)))
  val mem_wr_IF = Array.fill(g.BI)(new myRamWriteOnly(config = myRamConfig(g.width, g.BankAddrWidth)))

  // Input: Addr 10bit
  // Output: Mem Read Addr(7bit) & idx(3bit)
  val AddrCal = new Area {
    val uAddrDecode = new AddrDecode(g)
    val uAddrMux = new AddrMux(g)
    uAddrDecode.io.addrOri := io.isOutSideWrite ? io.WrBus.payload.Addr | io.RdAddrOri.payload
    val addr_dec = uAddrDecode.io.BankBus
    uAddrMux.io.BankBus := addr_dec
  }
  import AddrCal._

  connectMemRead(
    IF = mem_rd_IF,
    BFU = io.NttPayload,
    ioData = io.RdDataArray,
    RdAddr = uAddrMux.io.AddrBus,
    AddrValid = io.RdAddrOri.valid,
    DataValid = RegNext(io.RdAddrOri.valid),
    isOut = io.isOutSideRead
  )

  val AddrDelay = new Area {
    val ValidDelaySeq = Reg(Bits(g.BfuInttDelay + g.ramLatency bits)) init (0)
    ValidDelaySeq := ValidDelaySeq(g.BfuInttDelay + g.ramLatency - 1 downto 1) ## io.RdAddrOri.valid
    val AddrDelaySt1 = Delay(Cat(uAddrMux.io.AddrBus), g.BfuNttDelay + g.ramLatency).addAttribute("srl_style","srl")
    val IdxDelaySt1 = Delay(Cat(uAddrMux.io.IdxTrans), g.BfuNttDelay + g.ramLatency).addAttribute("srl_style","srl")
    val AddrDelaySt2 = Delay(AddrDelaySt1, (g.BfuInttDelay - g.BfuNttDelay)).addAttribute("srl_style","srl")
    val IdxDelaySt2 = Delay(IdxDelaySt1, (g.BfuInttDelay - g.BfuNttDelay)).addAttribute("srl_style","srl")
  }
  import AddrDelay._

  val WrBackCal = new Area {
    val WrBackIdx = Mux(
      io.isNtt,
      Vec(IdxDelaySt1.subdivideIn(g.BI slices).map { _.asUInt }),
      Vec(IdxDelaySt2.subdivideIn(g.BI slices).map { _.asUInt })
    )
    val uWriteBackArbit = WriteBackArbit(g, io.NttWriteBack, WrBackIdx)
    val WrBackData = uWriteBackArbit.io.dataOut
    val WrBackAddr = io.isNtt ? Vec(AddrDelaySt1.subdivideIn(g.BI slices).map { _.asUInt }) | Vec(
      AddrDelaySt2.subdivideIn(g.BI slices).map { _.asUInt }
    )
    val WrBackValid =
      io.isNtt ? ValidDelaySeq(g.BfuNttDelay + g.ramLatency - 1) | ValidDelaySeq(g.BfuInttDelay + g.ramLatency - 1)
  }
  import WrBackCal._

  connectMemWrite(
    IF = mem_wr_IF,
    ioBusData = io.WrBus.payload.Data,
    ioBusDecodeAddr = uAddrMux.io.AddrBus,
    ioBusValid = io.WrBus.valid,
    WrBackAddr = WrBackAddr,
    WrBackData = WrBackData,
    WrBackValid = WrBackValid,
    isOut = io.isOutSideWrite
  )
  mem.io.memIf.zip(mem_rd_IF.toSeq).foreach { case (t1, t2) => t1 << t2 }
  mem.io.memIf.zip(mem_wr_IF.toSeq).foreach { case (t1, t2) => t1 << t2 }
}

case class MemTopOpt1(g: NttCfg2414) extends Component {
  val io = new Bundle {
    val isOutSideRead = in Bool ()
    val isOutSideWrite = in Bool ()
    val isNtt = in Bool ()
    val RdAddrOri = slave Flow Vec(UInt(g.Log2NttPoints bits), g.BI)
    val RdDataArray = master Flow Vec(UInt(g.width bits), g.BI)
    val WrBus = slave Flow ParaWriteBus(DataWidth = g.width, AddrWidth = g.Log2NttPoints, para = g.BI)
    val NttPayload = Array.fill(g.paraNum)(master Flow BfuPayload(g))
    val NttWriteBack = Array.fill(g.paraNum)(slave Flow DataPayload(g))

  }
  noIoPrefix()

  // -----------------------------------------------------------
  // ReadAddr->Decode(pip)->AddrMux(cb)->mem(Data)->WrBackArb(cb)->isOutsideRead?io|Bfu
  // AddrMux  -> Delay -> mem(WriteBack)
  // Bfu(pip) -> Mux -> mem(WriteBack)
  // isOutsideWrite: WrAddr -> Decode -> AddrMux -> mem
  // -----------------------------------------------------------
  private def connectMemRead(
                              IF: Array[myRamReadOnly],
                              BFU: Array[Flow[BfuPayload]],
                              ioData: Flow[Vec[UInt]],
                              RdAddr: Vec[UInt],
                              AddrValid: Bool,
                              DataValid: Bool,
                              isOut: Bool
                            ): Unit = {
    val BfuValid = DataValid && isOut === False
    val ioValid = DataValid && isOut === True
    BFU.toSeq.zip(IF.grouped(2).toSeq).foreach { case (t1, t2) =>
      t1.A := t2(0).rData.asUInt; t1.B := t2(1).rData.asUInt; t1.Tw.assignDontCare();
      t1.valid := BfuValid
    }
    ioData.payload.zip(IF).foreach { case (t1, t2) =>
      t1 := t2.rData.asUInt
    }
    ioData.valid := ioValid
    IF.toSeq.zip(RdAddr).foreach {
      case (t1, t2) => { t1.rAddr := t2; t1.re := AddrValid }
    }
  }
  private def connectMemWrite(
                               IF: Array[myRamWriteOnly],
                               ioBusData: Vec[UInt],
                               ioBusDecodeAddr: Vec[UInt],
                               ioBusValid: Bool,
                               WrBackAddr: Vec[UInt],
                               WrBackData: Vec[UInt],
                               WrBackValid: Bool,
                               isOut: Bool
                             ): Unit = {
    val IfDataVec = WrBackData.zip(ioBusData).map { case (t1, t2) => isOut ? t2 | t1 }
    val IfAddrVec = WrBackAddr.zip(ioBusDecodeAddr).map { case (t1, t2) => isOut ? t2 | t1 }
    val IfValidVec = Array.fill(g.BI)(Bool())
    IfValidVec.foreach(_ := isOut ? ioBusValid | WrBackValid)
    IF.toSeq.zip(IfAddrVec).foreach { case (t1, t2) => t1.wAddr := t2 }
    IF.toSeq.zip(IfValidVec).foreach { case (t1, t2) => t1.we := t2 }
    IF.toSeq.zip(IfDataVec).foreach { case (t1, t2) => t1.wData := t2.asBits }
  }

  private def genFlow[T <: Data](dataIn: T, valid: Bool): Flow[T] = {
    val ret = Flow(cloneOf(dataIn))
    ret.payload := dataIn
    ret.valid := valid
    ret
  }
  val mem = new memBank(g)
  val mem_rd_IF = Array.fill(g.BI)(new myRamReadOnly(config = myRamConfig(g.width, g.BankAddrWidth)))
  val mem_wr_IF = Array.fill(g.BI)(new myRamWriteOnly(config = myRamConfig(g.width, g.BankAddrWidth)))

  // Input: Addr 10bit
  // Output: Mem Read Addr(7bit) & idx(3bit)
  val AddrCal = new Area {
    val uAddrDecode = new AddrDecode(g)
    val uAddrMux = new AddrMux(g)
    uAddrDecode.io.addrOri := io.isOutSideWrite ? io.WrBus.payload.Addr | io.RdAddrOri.payload
    val addr_dec = uAddrDecode.io.BankBus
    uAddrMux.io.BankBus := addr_dec
  }
  import AddrCal._

  connectMemRead(
    IF = mem_rd_IF,
    BFU = io.NttPayload,
    ioData = io.RdDataArray,
    RdAddr = uAddrMux.io.AddrBus,
    AddrValid = io.RdAddrOri.valid,
    DataValid = RegNext(io.RdAddrOri.valid),
    isOut = io.isOutSideRead
  )

  val AddrDelay = new Area {
    val ValidDelaySeq = Reg(Bits(g.BfuInttDelay + g.ramLatency bits)) init (0)
    ValidDelaySeq := ValidDelaySeq(g.BfuInttDelay + g.ramLatency - 1 downto 1) ## io.RdAddrOri.valid
    val AddrDelaySt1 = Delay(Cat(uAddrMux.io.AddrBus), g.BfuNttDelay + g.ramLatency).addAttribute("srl_style","srl")
    val IdxDelaySt1 = Delay(Cat(uAddrMux.io.IdxTrans), g.BfuNttDelay + g.ramLatency).addAttribute("srl_style","srl")
    val AddrDelaySt2 = Delay(AddrDelaySt1, (g.BfuInttDelay - g.BfuNttDelay)).addAttribute("srl_style","srl")
    val IdxDelaySt2 = Delay(IdxDelaySt1, (g.BfuInttDelay - g.BfuNttDelay)).addAttribute("srl_style","srl")
  }
  import AddrDelay._

  val WrBackCal = new Area {
    val WrBackIdx = Mux(
      io.isNtt,
      Vec(IdxDelaySt1.subdivideIn(g.BI slices).map { _.asUInt }),
      Vec(IdxDelaySt2.subdivideIn(g.BI slices).map { _.asUInt })
    )
    val uWriteBackArbit = WriteBackArbit(g, io.NttWriteBack, WrBackIdx)
    val WrBackData = uWriteBackArbit.io.dataOut
    val WrBackAddr = io.isNtt ? Vec(AddrDelaySt1.subdivideIn(g.BI slices).map { _.asUInt }) | Vec(
      AddrDelaySt2.subdivideIn(g.BI slices).map { _.asUInt }
    )
    val WrBackValid =
      io.isNtt ? ValidDelaySeq(g.BfuNttDelay + g.ramLatency - 1) | ValidDelaySeq(g.BfuInttDelay + g.ramLatency - 1)
  }
  import WrBackCal._

  connectMemWrite(
    IF = mem_wr_IF,
    ioBusData = io.WrBus.payload.Data,
    ioBusDecodeAddr = uAddrMux.io.AddrBus,
    ioBusValid = io.WrBus.valid,
    WrBackAddr = WrBackAddr,
    WrBackData = WrBackData,
    WrBackValid = WrBackValid,
    isOut = io.isOutSideWrite
  )
  mem.io.memIf.zip(mem_rd_IF.toSeq).foreach { case (t1, t2) => t1 << t2 }
  mem.io.memIf.zip(mem_wr_IF.toSeq).foreach { case (t1, t2) => t1 << t2 }
}
object MemTopGenV extends App {
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "./rtl/Ntt/DataPath/Mem"
  ).generate(new MemTop(NttCfg2414()))
}
object MemTopVivadoFlow extends App {
  val workspace = "./vivado_prj/Ntt/DataPath/Mem/MemTop"
  val vivadopath = "/opt/Xilinx/Vivado/2023.1/bin"
  val family = "Zynq UltraScale+ MPSoCS"
  val device = "xczu9eg-ffvb1156-2-i"
  val frequency = 300 MHz
  val cpu = 12
  val rtl = new Rtl {

    /** Name */
    override def getName(): String = "MemTop"
    override def getRtlPath(): String = "/PRJ/SpinalHDL-prj/PRJ/myTest/test/rtl/Ntt/DataPath/Mem/MemTop.v"
  }

  val flow = VivadoFlow(vivadopath, workspace, rtl, family, device, frequency, cpu)
  println(s"${family} -> ${(flow.getFMax / 1e6).toInt} MHz ${flow.getArea} ")
}
