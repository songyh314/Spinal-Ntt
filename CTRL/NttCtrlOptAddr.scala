package Ntt.CTRL

import Ntt.DataPath.AddrDecode
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import Ntt.NttCfg._
import spinal.core.sim.SimConfigLegacy
import spinal.lib.eda.bench.Rtl
import spinal.lib.eda.xilinx.VivadoFlow
import spinal.lib.fsm._


case class CtrlOptAddr(g: NttCfg2414) extends Component {
  val io = new Bundle {
    val start = in Bool ()
    val isNtt = in Bool ()
    val idle = out Bool ()
    val RdAddrOri = master Flow Vec(UInt(g.BankAddrWidth bits), g.radix)
    val RdLsbOri = master Flow Vec(UInt(g.BankIndexWidth bits), g.BI)
    val TwBus = master Flow (twPayload(
      addrWidth = g.twAddrWidth,
      muxWidth = log2Up(g.paraNum),
      para = g.paraNum
    ))
  }
  val loopCnt = Reg(UInt(log2Up(g.nttPoint / g.BI) bits)) init U(0)
  val stageCnt = Reg(UInt(log2Up(g.Log2NttPoints) bits)) init U(0)
  val stageCntCop = g.Log2NttPoints - 1 - stageCnt

  val OH_shifter = Reg(Bits(g.Log2NttPoints bits)) init 0
  val Thermal_shifter = Reg(Bits(g.Log2NttPoints bits)) init 0

  val constSeq = (0 until g.paraNum).map { U(_, log2Up(g.paraNum) bits) }
  val mask = Bits(log2Up(g.nttPoint / g.paraNum) bits)
  val fsm = new StateMachine {
    //    setEncoding(binaryOneHot)
    val IDLE = makeInstantEntry()
    val LOOP = new State
    IDLE
      .whenIsActive {
        when(io.start)(goto(LOOP))
      }
    LOOP
      .onEntry {
        when(io.isNtt) {
          OH_shifter.msb := True
          Thermal_shifter.msb := True
        } otherwise {
          OH_shifter.lsb := True
          Thermal_shifter.lsb := True
        }
      }
      .whenIsActive {
        when(loopCnt === loopCnt.maxValue) {
          OH_shifter := io.isNtt ? (OH_shifter |>> 1) | (OH_shifter |<< 1)
          Thermal_shifter := io.isNtt ? (B"1'b1" ## Thermal_shifter(Thermal_shifter.high downto 1)) | (Thermal_shifter(
            Thermal_shifter.high - 1 downto 0
          ) ## B"1'b1")
        }
        when(loopCnt === loopCnt.maxValue) {
          loopCnt := U(0)
        } otherwise (loopCnt := loopCnt + 1)
        when(loopCnt === loopCnt.maxValue) {
          when(stageCnt === (g.Log2NttPoints - 1)) { goto(IDLE) } otherwise { stageCnt := stageCnt + 1 }
        }
      }
    when(isNext(IDLE)) {
      loopCnt := 0; stageCnt := 0;
      OH_shifter := 0; Thermal_shifter := 0
    }
  }

  val subDut = new Area {
    val MsbPadConstSeq = constSeq.map { item => Cat(loopCnt.lsb, item) }
    val MsbPadLoopCnt = (B"1'b0" ## loopCnt(loopCnt.high downto 1)).asBits
    val LsbPadConstSeq = constSeq.map { item => Cat(item, B"1'b0") }
    val LsbPadLoopCnt = loopCnt.asBits
    val lsbMuxArray = Array.fill(g.paraNum)(new SeqMux(log2Up(g.BI)))
    lsbMuxArray.toSeq.zip(MsbPadConstSeq.zip(LsbPadConstSeq)).foreach { case (dut, (t1, t2)) =>
      dut.io.SeqA := t1
      dut.io.SeqB := t2
      dut.io.isNtt := io.isNtt
      dut.io.ohSeq := OH_shifter(log2Up(g.paraNum) downto 0)
      dut.io.thermalSeq := Thermal_shifter(log2Up(g.paraNum) downto 0)
    }
    val msbMux = new SeqMux(g.Log2NttPoints - log2Up(g.BI))
    msbMux.io.SeqA := MsbPadLoopCnt
    msbMux.io.SeqB := LsbPadLoopCnt
    msbMux.io.isNtt := io.isNtt
    msbMux.io.ohSeq := OH_shifter(OH_shifter.high downto (log2Up(g.paraNum) + 1))
    msbMux.io.thermalSeq := Thermal_shifter(OH_shifter.high downto (log2Up(g.paraNum) + 1))

    val ins0_Msb = msbMux.io.ins0_seq
    val ins1_Msb = msbMux.io.ins1_seq
    val ins0_Lsb = Vec(Bits(g.BankIndexWidth bits), g.paraNum)
    val ins1_Lsb = Vec(Bits(g.BankIndexWidth bits), g.paraNum)
    ins0_Lsb.zip(ins1_Lsb).zip(lsbMuxArray.toSeq).foreach{case((t1,t2),dut) =>
      t1 := dut.io.ins0_seq
      t2 := dut.io.ins1_seq
    }
//    val ins0_subSeq = Vec(Bits(g.Log2NttPoints bits), g.paraNum)
//    val ins1_subSeq = Vec(Bits(g.Log2NttPoints bits), g.paraNum)
//    (ins0_subSeq.zip(ins1_subSeq)).zip(lsbMuxArray.toSeq).foreach { case ((ins0, ins1), dut) =>
//      ins0 := msbMux.io.ins0_seq ## dut.io.ins0_seq
//      ins1 := msbMux.io.ins1_seq ## dut.io.ins1_seq
//    }
  }
  import subDut._
  val twGen = new Area {
    val twLoopCntMsbPadNtt = Cat((B"1'b1"), loopCnt).asUInt
    val twLoopCntMsbPadIntt = Cat((B"1'b1"), ~(loopCnt.asBits)).asUInt
    val loopCntShiftNtt = UInt(loopCnt.getWidth + 1 bits)
    val loopCntShiftIntt = UInt(loopCnt.getWidth + 1 bits)
    loopCntShiftIntt := (Cat((B"1'b0" #* log2Up(g.BI)), loopCnt).asUInt |>> stageCnt).resized
    loopCntShiftNtt := (Cat((B"1'b1" #* log2Up(g.BI)), loopCnt).asUInt |>> stageCntCop).resized

    mask := io.isNtt ? Thermal_shifter(0, log2Up(g.nttPoint / g.paraNum) bits).reversed | Thermal_shifter(
      1,
      log2Up(g.nttPoint / g.paraNum) bits
    ).reversed
    val twAddr = io.isNtt ? (mask & loopCntShiftNtt.asBits) | ~(mask | loopCntShiftIntt.asBits)
    val stageOverflow = io.isNtt ? (stageCntCop >= log2Up(g.paraNum)) | (stageCnt >= log2Up(g.paraNum))
    val shiftCnt = io.isNtt ? { stageOverflow ? (stageCntCop - log2Up(g.paraNum)) | U(0) } | {
      stageOverflow ? (stageCnt - log2Up(g.paraNum)) | U(0)
    }
    val loopSeqNtt = Bits(log2Up(g.paraNum) bits)
    val loopSeqIntt = Bits(log2Up(g.paraNum) bits)
    loopSeqNtt := (twLoopCntMsbPadNtt >> shiftCnt).resize(log2Up(g.paraNum) bits)(0, log2Up(g.paraNum) bits).asBits
    loopSeqIntt := (twLoopCntMsbPadIntt >> shiftCnt).resize(log2Up(g.paraNum) bits)(0, log2Up(g.paraNum) bits).asBits
    val twMuxArray = Array.fill(g.paraNum)(new twMux(g.paraNum))
    twMuxArray.toSeq.zip(constSeq).foreach {
      case (t1, t2) => {
        t1.io.loopSeq := io.isNtt ? loopSeqNtt | loopSeqIntt
        t1.io.constSeq := io.isNtt ? t2.asBits | ~(t2.asBits)
        t1.io.stageCntLsb := io.isNtt ? stageCntCop(0, log2Up(log2Up(g.paraNum)) bits).asBits |
          (stageCnt(0, log2Up(log2Up(g.paraNum)) bits).asBits)
        t1.io.stageCntoverflow := stageOverflow
      }
    }
  }
  import twGen._

  val flatIdxSeq = Vec((ins0_Lsb.zip(ins1_Lsb)).flatMap { case (t1, t2) => Seq(t1.asUInt, t2.asUInt) })
  io.idle := fsm.isActive(fsm.IDLE)
  io.RdAddrOri.payload := Vec(ins0_Msb.asUInt,ins1_Msb.asUInt)
  io.RdAddrOri.valid := fsm.isActive(fsm.LOOP)
  io.RdLsbOri.payload := flatIdxSeq
  io.RdLsbOri.valid := fsm.isActive(fsm.LOOP)
//  if (g.debug) {
//    val uDec = AddrDecode(g)
//    uDec.io.addrOri := io.RdAddrOri.payload
//    val probe = uDec.io.BankBus.simPublic()
//  }

  io.TwBus.payload.twAddr := twAddr.asUInt
  //  io.TwBus.payload.twAddr := Delay(twAddr.asUInt,g.DecodeLatency)
  io.TwBus.payload.twMux.zip(twMuxArray.toSeq).foreach { case (t1, t2) => t1 := t2.io.twMuxUnit }
  //  io.TwBus.payload.twMux.zip(twMuxArray.toSeq).foreach { case (t1, t2) => t1 := Delay(t2.io.twMuxUnit,g.DecodeLatency) }
  io.TwBus.valid := fsm.isActive(fsm.LOOP)
  //  io.TwBus.valid := Delay(fsm.isActive(fsm.LOOP),g.DecodeLatency)
}


object CtrlOptAddrGenV extends App {
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "NttOpt/rtl/Ctrl",
    genLineComments = true
  ).generate(new CtrlOptAddr(NttCfg2414(paraNum = 32, debug = false)))
}



object CtrlOptAddrSim extends App {
  val period = 10
  val dut = SimConfig.withXSim.withWave.workspacePath("./NttOpt/sim").compile(new CtrlOptAddr(NttCfg2414(nttPoint = 128, paraNum = 4)))
  dut.doSim("test") { dut =>
    import dut._
    SimTimeout(5000 * period)
    clockDomain.forkStimulus(period)
    clockDomain.waitSampling(10)
    io.start #= true
    io.isNtt #= true
    clockDomain.waitSampling()
    io.start #= false
    clockDomain.waitSampling(10)
    clockDomain.waitActiveEdgeWhere(io.idle.toBoolean)
    clockDomain.waitSampling(10)
    io.isNtt #= false
    io.start #= true
    clockDomain.waitSampling()
    io.start #= false
    clockDomain.waitSampling(10)
    clockDomain.waitActiveEdgeWhere(io.idle.toBoolean)
    clockDomain.waitSampling(10)
    simSuccess()
  }
}


object CtrlOptAddrVivadoFlow extends App {
  val workspace = "NttOpt/fpga/CtrlOptAddr"
  val vivadopath = "/opt/Xilinx/Vivado/2023.1/bin"
  val family = "Zynq UltraScale+ MPSoCS"
  val device = "xczu9eg-ffvb1156-2-i"
  val frequency = 300 MHz
  val cpu = 16
  val rtl = new Rtl {
    override def getName(): String = "CtrlOptAddr"
    override def getRtlPath(): String = "/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/rtl/Ctrl/CtrlOptAddr.v"
  }
  val flow = VivadoFlow(vivadopath, workspace, rtl, family, device, frequency, cpu)
  println(s"${family} -> ${(flow.getFMax / 1e6).toInt} MHz ${flow.getArea}")
}

