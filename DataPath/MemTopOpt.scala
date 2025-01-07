package Ntt.DataPath
import Ntt.NttCfg._
import spinal.core._
import spinal.core.sim._
import myRam._
import spinal.lib._
import spinal.lib.eda.bench.Rtl
import spinal.lib.eda.xilinx.VivadoFlow

case class MemTopOpt(g: NttCfg2414) extends Component {
  val io = new Bundle {
    val isOutSideRead = in Bool ()
    val isOutSideWrite = in Bool ()
//    val isNtt = in Bool ()
    val RdAddrOri = slave Flow Vec(UInt(g.Log2NttPoints bits), g.BI)
    val RdDataArray = master Flow Vec(UInt(g.width bits), g.BI)
    val WrBus = slave Flow ParaWriteBus(DataWidth = g.width, AddrWidth = g.Log2NttPoints, para = g.BI)
    val NttPayload = Array.fill(g.paraNum)(master Flow BfuPayload(g))
    val wrAddrOri = slave Flow Vec(UInt(g.Log2NttPoints bits), g.BI)
    val NttWriteBack = Array.fill(g.paraNum)(slave Flow DataPayload(g))
    val twBus = slave Flow twPayload(addrWidth = g.twAddrWidth, muxWidth = log2Up(g.paraNum), para = g.paraNum)
  }
  noIoPrefix()

//  private def genFlow[T <: Data](dataIn: T, valid: Bool): Flow[T] = {
//    val ret = Flow(cloneOf(dataIn))
//    ret.payload := dataIn
//    ret.valid := valid
//    ret
//  }

  val mem = new memBank(g)
  val mem_rd_IF = Array.fill(g.BI)(new myRamReadOnly(config = myRamConfig(g.width, g.BankAddrWidth)))
  val mem_wr_IF = Array.fill(g.BI)(new myRamWriteOnly(config = myRamConfig(g.width, g.BankAddrWidth)))

  // Input: Addr 10bit
  // Output: Mem Read Addr(7bit) & idx(3bit)
  val rdCal = new Area {
    val rdMemPreCal = new memPreCal(g, false)
    rdMemPreCal.io.oriAddr := io.RdAddrOri.payload
    val rdAddrDec = rdMemPreCal.io.AddrBus
    val rdIdxTrans = rdMemPreCal.io.idxTrans
    val rdIdxDelay = Delay(Cat(rdIdxTrans), g.ramLatency).addAttribute("srl_style", "srl")
    val re = io.RdAddrOri.valid
  }
  val tw = new Area {
    val rom = new twRom(g)
    rom.io.twBus := io.twBus
//    io.NttPayload.toSeq.zip(rom.io.twData).foreach{case(t1,t2) => t1.payload.Tw := t2}
  }

  mem_rd_IF.toSeq.zip(rdCal.rdAddrDec).foreach { case (t1, t2) =>
    t1.rAddr := t2; t1.re := Delay(rdCal.re, g.DecodeLatency)
  }
  val ValidDelaySeq = Reg(Bits(g.BfuInttDelay + g.ramLatency bits)) init (0)
  ValidDelaySeq := ValidDelaySeq(g.BfuInttDelay + g.ramLatency - 1 downto 1) ## io.RdAddrOri.valid
  val deMuxPayload = Vec(mem_rd_IF.map(item => item.rData).toSeq)
  val dataDeMux = DataDeMux(deMuxPayload, Vec(rdCal.rdIdxDelay.subdivideIn(g.BI slices).map(_.asUInt)))
  val rdValidDelay = Delay(io.RdAddrOri.valid, (g.ramLatency + g.DecodeLatency))
  io.RdDataArray.payload.zip(dataDeMux).foreach { case (t1, t2) => t1 := t2.asUInt }
  io.RdDataArray.valid := rdValidDelay && io.isOutSideRead

  io.NttPayload.toSeq.zip(dataDeMux.grouped(2).toSeq).foreach { case (t1, t2) =>
    t1.A := t2(0).asUInt; t1.B := t2(1).asUInt
    t1.valid := rdValidDelay && !io.isOutSideRead
  }
  io.NttPayload.toSeq.zip(tw.rom.io.twData).foreach { case (t1, t2) => t1.payload.Tw := t2 }

  val wrCal = new Area {
    val wrMemPreCal = new memPreCal(g, true)
    when(io.isOutSideWrite) {
      wrMemPreCal.io.oriAddr := io.WrBus.payload.Addr
      wrMemPreCal.io.dataIn := io.WrBus.payload.Data
    } otherwise {
      wrMemPreCal.io.oriAddr := io.wrAddrOri.payload
      val dutPayload = Vec(io.NttWriteBack.flatMap { item => Seq(item.payload.A, item.payload.B) }.toSeq)
      wrMemPreCal.io.dataIn.zip(dutPayload) foreach ({ case (t1, t2) => t1 := t2.asBits })
    }
  }

  mem_wr_IF.toSeq.zip(wrCal.wrMemPreCal.io.dataBus).foreach { case (t1, t2) => t1.wData := t2 }
  mem_wr_IF.toSeq.zip(wrCal.wrMemPreCal.io.AddrBus).foreach { case (t1, t2) => t1.wAddr := t2 }
  mem_wr_IF.foreach(item => item.we := Delay(io.isOutSideWrite ? io.WrBus.valid | io.wrAddrOri.valid, g.DecodeLatency))
  mem.io.memIf.zip(mem_rd_IF.toSeq).foreach { case (t1, t2) => t1 << t2 }
  mem.io.memIf.zip(mem_wr_IF.toSeq).foreach { case (t1, t2) => t1 << t2 }
}

object MemTopOptGenV extends App {
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "./rtl/Ntt/DataPath/Mem"
  ).generate(new MemTopOpt(NttCfg2414()))
}

object memTopOptSim extends App {
  val period = 10
  val dut = SimConfig.withXSim.withWave.compile(new MemTopOpt(NttCfg2414(nttPoint = 64)))
  dut.doSim("test") { dut =>
    SimTimeout(1000 * period)
    import dut._
    clockDomain.forkStimulus(period)
    dut.io.isOutSideRead #= false
    io.isOutSideWrite #= true
    io.RdAddrOri.valid #= false
    io.WrBus.valid #= false
    io.twBus.valid #= false
    io.wrAddrOri.valid #= false
    clockDomain.waitSampling(10)
    for (i <- 0 until g.nttPoint / g.BI) {
      io.WrBus.valid #= true
      io.isOutSideWrite #= true
      (0 until g.BI).map { j => g.BI * i + j }.zip(io.WrBus.payload.Addr).foreach { case (t1, t2) => t2 #= t1 }
      (0 until g.BI).map { j => g.BI * i + j }.zip(io.WrBus.payload.Data).foreach { case (t1, t2) => t2 #= t1 }
      clockDomain.waitSampling()
      io.WrBus.valid #= false
    }
    clockDomain.waitSampling(10)
    for (i <- 0 until g.nttPoint / g.BI) {
      io.isOutSideRead #= true
      io.RdAddrOri.valid #= true
      (0 until g.BI).map { j => g.BI * i + j }.zip(io.RdAddrOri.payload).foreach { case (t1, t2) => t2 #= t1 }
      clockDomain.waitSampling()
      io.RdAddrOri.valid #= false
    }
    clockDomain.waitSampling(10)
    for (i <- 0 until g.nttPoint / g.BI) {
      io.isOutSideRead #= false
      io.RdAddrOri.valid #= true
      (0 until g.BI).map { j => g.BI * i + j }.zip(io.RdAddrOri.payload).foreach { case (t1, t2) => t2 #= t1 }
      clockDomain.waitSampling()
      io.RdAddrOri.valid #= false
    }
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


object MemTopOptVivadoFlow extends App {
  val workspace = "./vivado_prj/Ntt/DataPath/Mem/MemTop"
  val vivadopath = "/opt/Xilinx/Vivado/2023.1/bin"
  val family = "Zynq UltraScale+ MPSoCS"
  val device = "xczu9eg-ffvb1156-2-i"
  val frequency = 300 MHz
  val cpu = 12
  val rtl = new Rtl {

    /** Name */
    override def getName(): String = "MemTopOpt"
    override def getRtlPath(): String = "/PRJ/SpinalHDL-prj/PRJ/myTest/test/rtl/Ntt/DataPath/Mem/MemTopOpt.v"
  }

  val flow = VivadoFlow(vivadopath, workspace, rtl, family, device, frequency, cpu)
  println(s"${family} -> ${(flow.getFMax / 1e6).toInt} MHz ${flow.getArea} ")
}
