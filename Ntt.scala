package Ntt
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.eda.bench.Rtl
import spinal.lib.eda.xilinx.VivadoFlow
import test._
import NttCfg._
import CTRL._
import DataPath._
import myRam._

case class addrDecodeTest(g: NttCfg2414) extends Component {
  val io = new Bundle {
    val start = in Bool ()
    val isNtt = in Bool ()
    val idle = out Bool ()
    val BankBus = out Vec (DecodeBus(idx = g.BankIndexWidth, addr = g.BankAddrWidth), g.BI)
  }
  val ctrl = new Ctrl(g)
  val decode = new AddrDecode(g)
  ctrl.io.isNtt := io.isNtt
  ctrl.io.start := io.start
  io.idle := ctrl.io.idle
  decode.io.addrOri := ctrl.io.RdAddrOri.payload
  io.BankBus := decode.io.BankBus
}

object addrDecodeTestSim extends App {
  val period = 10
  val dut = SimConfig.withXSim.withWave.compile(new addrDecodeTest(NttCfg2414(nttPoint = 128)))
  dut.doSim("test") { dut =>
    import dut._
    SimTimeout(1000 * period)
    clockDomain.forkStimulus(period)
    io.start #= false
    io.isNtt #= true
    clockDomain.waitSampling(10)
    io.start #= true
    clockDomain.waitSampling()
    io.start #= false
    clockDomain.waitActiveEdgeWhere(io.idle.toBoolean)
    clockDomain.waitSampling(10)
  }
}

case class ctrlMem(g: NttCfg2414) extends Component {
  val io = new Bundle {
    val start = in Bool ()
    val wctrlStart = in Bool ()
    val isNtt = in Bool ()
    val idle = out Bool ()
    val isOutSideRead = in Bool ()
    val isOutSideWrite = in Bool ()
//    val RdAddrOri = slave Flow Vec(UInt(g.Log2NttPoints bits), g.BI)
    val RdDataArray = master Flow Vec(UInt(g.width bits), g.BI)
//    val WrBus = slave Flow ParaWriteBus(DataWidth = g.width, AddrWidth = g.Log2NttPoints, para = g.BI)
    val NttPayload = Array.fill(g.paraNum)(master Flow BfuPayload(g))
//    val wrAddrOri = slave Flow Vec(UInt(g.Log2NttPoints bits), g.BI)
    val wrData = in Vec (Bits(g.width bits), g.BI)
    val NttWriteBack = Array.fill(g.paraNum)(slave Flow DataPayload(g))
  }

  val ctrl = new Ctrl(g)
  val writeback_ctrl = new writeBackCtrl(g)
  ctrl.io.start := io.start
  ctrl.io.isNtt := io.isNtt
  writeback_ctrl.io.wctrlStart := io.wctrlStart
  writeback_ctrl.io.isNtt := io.isNtt
  writeback_ctrl.io.isOutside := io.isOutSideWrite | io.isOutSideRead

  val mem = new MemTopOpt(g)
  // ctrl bus--------------------------------------------------
  mem.io.isOutSideRead := io.isOutSideRead
  mem.io.isOutSideWrite := io.isOutSideWrite
  io.idle := ctrl.io.idle && writeback_ctrl.io.idle
  // -----------------------------------------------------------

  // outside read-----------------------------------------------
  mem.io.RdAddrOri := ctrl.io.RdAddrOri
  io.RdDataArray := mem.io.RdDataArray
  // -----------------------------------------------------------

  // outside write----------------------------------------------
  mem.io.WrBus.payload.Addr := writeback_ctrl.io.wrAddrOri.payload
  mem.io.WrBus.payload.Data := io.wrData
  mem.io.WrBus.valid := writeback_ctrl.io.wrAddrOri.valid && io.isOutSideWrite
  // -----------------------------------------------------------

  // mem -> bfu------------------------------------------------
  mem.io.twBus := ctrl.io.TwBus
  io.NttPayload.toSeq.zip(mem.io.NttPayload.toSeq).foreach { case (t1, t2) => t1 := t2 }
  // -----------------------------------------------------------

  // bfu -> mem-------------------------------------------------
  mem.io.NttWriteBack.toSeq.zip(io.NttWriteBack.toSeq).foreach { case (t1, t2) => t1 := t2 }
  mem.io.wrAddrOri.payload := writeback_ctrl.io.wrAddrOri.payload
  mem.io.wrAddrOri.valid := writeback_ctrl.io.wrAddrOri.valid && (!io.isOutSideWrite)
  // -----------------------------------------------------------

}

case class ctrlMemOpt1(g: NttCfg2414) extends Component {
  val io = new Bundle {
    val start = in Bool ()
    val isNtt = in Bool ()
    val idle = out Bool ()
    val isCal = in Bool ()
    val isOutSideRead = in Bool ()
    val isOutSideWrite = in Bool ()

    val outsideAddrOri = slave Flow Vec(UInt(g.Log2NttPoints bits), g.BI)
    val outsideRdDataArray = master Flow Vec(Bits(g.width bits), g.BI)
    val outsideWrDataArray = slave Flow Vec(Bits(g.width bits), g.BI)

    val NttPayload = Array.fill(g.paraNum)(master Flow BfuPayload(g))
    //    val wrAddrOri = slave Flow Vec(UInt(g.Log2NttPoints bits), g.BI)

    val NttWriteBack = Array.fill(g.paraNum)(slave Flow DataPayload(g))
  }

  val ctrl = new Ctrl(g)

  ctrl.io.start := io.start
  ctrl.io.isNtt := io.isNtt

  val mem = new MemTopOpt1(g)
  // ctrl bus--------------------------------------------------
  mem.io.isOutSideRead := io.isOutSideRead
  mem.io.isOutSideWrite := io.isOutSideWrite
  mem.io.isCal := io.isCal
  mem.io.isNtt := io.isNtt
  io.idle := ctrl.io.idle
  // -----------------------------------------------------------

  // outside read&write -----------------------------------------
  mem.io.outsideAddrOri := io.outsideAddrOri
  io.outsideRdDataArray := mem.io.outsideRdDataArray
  mem.io.outsideWrDataArray := io.outsideWrDataArray
  // -----------------------------------------------------------

  // mem -> bfu------------------------------------------------
  mem.io.bfuRdAddrOri := ctrl.io.RdAddrOri
  mem.io.twBus := ctrl.io.TwBus
  io.NttPayload.toSeq.zip(mem.io.NttPayload.toSeq).foreach { case (t1, t2) => t1 := t2 }
  // -----------------------------------------------------------

  // bfu -> mem-------------------------------------------------
  mem.io.NttWriteBack.toSeq.zip(io.NttWriteBack.toSeq).foreach { case (t1, t2) => t1 := t2 }
  // -----------------------------------------------------------

}

object ctrlMemGenV extends App {
  SpinalConfig(mode = Verilog, nameWhenByFile = false, anonymSignalPrefix = "tmp", targetDirectory = "./rtl/Ntt/Top/")
    .generate(new ctrlMem(NttCfg2414()))
}

object ctrlMemSim extends App {
  val period = 10
  val dut = SimConfig.withXSim.withWave.compile(new ctrlMem(NttCfg2414(nttPoint = 128)))
  dut.doSim("test") { dut =>
    import dut._
    SimTimeout(1000 * period)
    clockDomain.forkStimulus(period)
    io.isNtt #= false
    io.start #= false
    io.isOutSideRead #= false
    io.isOutSideWrite #= false
    clockDomain.waitSampling(10)
    io.isOutSideWrite #= true
    clockDomain.waitSampling()
    io.wctrlStart #= true
    for (i <- 0 until g.nttPoint / g.BI) {
      clockDomain.waitSampling()
      io.wctrlStart #= false
      (0 until g.BI).map { j => g.BI * i + j }.zip(io.wrData).foreach { case (t1, t2) => t2 #= t1 }
    }
    clockDomain.waitSampling(10)
    io.isNtt #= true
    clockDomain.waitSampling()
    io.start #= true
    clockDomain.waitSampling()
    io.start #= false
    clockDomain.waitActiveEdgeWhere(io.idle.toBoolean)
    clockDomain.waitSampling(10)
  }
}

object ctrlMemOpt1Sim extends App {
  val period = 10
  val dut = SimConfig.withXSim.withWave.compile(new ctrlMemOpt1(NttCfg2414(nttPoint = 128)))
  dut.doSim("test") { dut =>
    import dut._
    SimTimeout(1000 * period)
    clockDomain.forkStimulus(period)
    io.isNtt #= false
    io.start #= false
    io.isCal #= false
    io.isOutSideRead #= false
    io.isOutSideWrite #= false
    clockDomain.waitSampling(10)

    io.isOutSideWrite #= true
    clockDomain.waitSampling()
    for (i <- 0 until g.nttPoint / g.BI) {
      io.outsideAddrOri.valid #= true
      (0 until g.BI).map { j => g.BI * i + j }.zip(io.outsideWrDataArray.payload).foreach { case (t1, t2) => t2 #= t1 }
      (0 until g.BI).map { j => g.BI * i + j }.zip(io.outsideAddrOri.payload).foreach { case (t1, t2) => t2 #= t1 }
      clockDomain.waitSampling()
      io.outsideAddrOri.valid #= false
    }
    clockDomain.waitSampling(10)
    io.isOutSideWrite #= false
    clockDomain.waitSampling(10)

    io.isOutSideRead #= true
    clockDomain.waitSampling()
    for (i <- 0 until g.nttPoint / g.BI) {
      io.outsideAddrOri.valid #= true
      (0 until g.BI).map { j => g.BI * i + j }.zip(io.outsideAddrOri.payload).foreach { case (t1, t2) => t2 #= t1 }
      (0 until g.BI).map { j => g.BI * i + j }.zip(io.outsideWrDataArray.payload).foreach { case (t1, t2) => t2 #= t1 }
      clockDomain.waitSampling()
      io.outsideAddrOri.valid #= false
    }
    clockDomain.waitSampling(10)
    io.isOutSideRead #= false
    clockDomain.waitSampling(10)

    io.isNtt #= true
    io.isCal #= true
    clockDomain.waitSampling()
    io.start #= true
    clockDomain.waitSampling()
    io.start #= false
    clockDomain.waitActiveEdgeWhere(io.idle.toBoolean)
    clockDomain.waitSampling(10)
  }
}
