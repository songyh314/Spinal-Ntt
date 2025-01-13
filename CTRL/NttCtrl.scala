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

case class SeqMux(width: Int) extends Component {
  this.setDefinitionName(s"SeqMux_w${width}")
  val io = new Bundle {
    val SeqA = in Bits (width bits)
    val SeqB = in Bits (width bits)
    val ohSeq = in Bits (width bits)
    val isNtt = in Bool ()
    val thermalSeq = in Bits (width bits)
    val ins0_seq = out Bits (width bits)
    val ins1_seq = out Bits (width bits)
  }
  val cmpSeq = Vec(Bool(), width)
  cmpSeq.zip(io.SeqA.asBools.zip(io.SeqB.asBools).zipWithIndex).foreach { case (cmp, ((t1, t2), index)) =>
    switch(Cat(io.ohSeq(index), io.thermalSeq(index) ^ io.isNtt).asBits) {
      is(B"00") { cmp := t2 }
      is(B"01")(cmp := t1)
      is(B"10")(cmp := False)
      default(cmp := False)
    }
  }
  io.ins0_seq := (cmpSeq.asBits)
  io.ins1_seq := (cmpSeq.asBits | io.ohSeq)
}

case class twMux(num: Int) extends Component {
  assert(isPow2(num) && (num != 1), "tw num should be power of 2 and should not be 1")
  val width = log2Up(num)
  val io = new Bundle {
    val constSeq = in Bits (width bits)
    val loopSeq = in Bits (width bits)
    val stageCntLsb = in Bits (log2Up(width) bits)
    val stageCntoverflow = in Bool ()
    val twMuxUnit = out UInt (width bits)
  }
  val muxTmp = Vec(Bits(width + 1 bits), width)
  val jointSeq = Cat(io.loopSeq, io.constSeq)
  for (i <- 0 until width) {
    muxTmp(i) := jointSeq(i, width + 1 bits)
  }
  val muxRes = Vec(Bool(), width)
  muxRes.zip(muxTmp).foreach { case (t1, t2) =>
    when(io.stageCntoverflow) {
      t1 := t2.msb
    } otherwise {
      t1 := (t2(t2.high - 1 downto 0).asBools).read(io.stageCntLsb.asUInt.resized)
    }
  }
  io.twMuxUnit := muxRes.asBits.asUInt
}
object twMuxSim extends App {
  val period = 10
  val n = 4
  val dut = SimConfig.withWave.withXSim.compile(new twMux(n))
  dut.doSim("test") { dut =>
    import dut._
    SimTimeout(1000 * period)
    clockDomain.forkStimulus(period)
    clockDomain.waitSampling(10)
    io.constSeq #= BigInt("01", 2)
    var flag = true
    val loopSeq = fork {
      while (flag) {
        for (i <- 0 until n) {
          io.loopSeq #= i
          clockDomain.waitSampling()
        }
      }
    }
    val ctrl = fork(
      while (flag) {
        io.stageCntoverflow.randomize()
        io.stageCntLsb.randomize()
        clockDomain.waitSampling()
      }
    )
    clockDomain.waitSampling(100)
    flag = false
    loopSeq.join()
    ctrl.join()
  }
}

case class Ctrl(g: NttCfg2414) extends Component {
  val io = new Bundle {
    val start = in Bool ()
    val isNtt = in Bool ()
    val idle = out Bool ()
    val RdAddrOri = master Flow Vec(UInt(g.Log2NttPoints bits), g.BI)
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
    val MsbPadSeq = constSeq.map { item => Cat(loopCnt.lsb, item) }
    val MsbPad = (B"1'b0" ## loopCnt(loopCnt.high downto 1)).asBits
    val LsbPadSeq = constSeq.map { item => Cat(item, B"1'b0") }
    val LsbPad = loopCnt.asBits
    val lsbMuxArray = Array.fill(g.paraNum)(new SeqMux(log2Up(g.paraNum) + 1))
    lsbMuxArray.toSeq.zip(MsbPadSeq.zip(LsbPadSeq)).foreach { case (dut, (t1, t2)) =>
      dut.io.SeqA := t1
      dut.io.SeqB := t2
      dut.io.isNtt := io.isNtt
      dut.io.ohSeq := OH_shifter(log2Up(g.paraNum) downto 0)
      dut.io.thermalSeq := Thermal_shifter(log2Up(g.paraNum) downto 0)
    }
    val msbMux = new SeqMux(g.Log2NttPoints - (log2Up(g.paraNum) + 1))
    msbMux.io.SeqA := MsbPad
    msbMux.io.SeqB := LsbPad
    msbMux.io.isNtt := io.isNtt
    msbMux.io.ohSeq := OH_shifter(OH_shifter.high downto (log2Up(g.paraNum) + 1))
    msbMux.io.thermalSeq := Thermal_shifter(OH_shifter.high downto (log2Up(g.paraNum) + 1))
    val ins0_subSeq = Vec(Bits(g.Log2NttPoints bits), g.paraNum)
    val ins1_subSeq = Vec(Bits(g.Log2NttPoints bits), g.paraNum)
    (ins0_subSeq.zip(ins1_subSeq)).zip(lsbMuxArray.toSeq).foreach { case ((ins0, ins1), dut) =>
      ins0 := msbMux.io.ins0_seq ## dut.io.ins0_seq
      ins1 := msbMux.io.ins1_seq ## dut.io.ins1_seq
    }
  }
  import subDut._
  val twGen = new Area {
    val twLoopCntMsbPad = Cat((B"1'b1"), loopCnt).asUInt
    val loopCntShiftNtt = UInt(loopCnt.getWidth + 1 bits)
    val loopCntShiftIntt = UInt(loopCnt.getWidth + 1 bits)
    loopCntShiftIntt := (Cat((B"1'b0" #* log2Up(g.BI)), loopCnt).asUInt |>> stageCnt).resized
    loopCntShiftNtt := (Cat((B"1'b1" #* log2Up(g.BI)), loopCnt).asUInt |>> stageCntCop).resized
//    mask := io.isNtt ? Thermal_shifter(0, log2Up(g.nttPoint / g.paraNum) bits).reversed | Thermal_shifter(
//      Thermal_shifter.high - (log2Up(log2Up(g.paraNum))) downto (Thermal_shifter.high - (log2Up(
//        log2Up(g.paraNum)
//      )) - log2Up(g.nttPoint / g.paraNum) + 1)
//    ).reversed
    mask := io.isNtt ? Thermal_shifter(0, log2Up(g.nttPoint / g.paraNum) bits).reversed | Thermal_shifter(
      1,
      log2Up(g.nttPoint / g.paraNum) bits
    ).reversed
    val twAddr = io.isNtt ? (mask & loopCntShiftNtt.asBits) | (mask | loopCntShiftIntt.asBits)
    val stageOverflow = io.isNtt ? (stageCntCop >= log2Up(g.paraNum)) | (stageCnt >= log2Up(g.paraNum))
    val shiftCnt = io.isNtt ? { stageOverflow ? (stageCntCop - log2Up(g.paraNum)) | U(0) } | {
      stageOverflow ? (stageCnt - log2Up(g.paraNum)) | U(0)
    }
    val loopSeq = Bits(log2Up(g.paraNum) bits)
    loopSeq := (twLoopCntMsbPad >> shiftCnt).resize(log2Up(g.paraNum) bits)(0, log2Up(g.paraNum) bits).asBits
    val twMuxArray = Array.fill(g.paraNum)(new twMux(g.paraNum))
    twMuxArray.toSeq.zip(constSeq).foreach {
      case (t1, t2) => {
        t1.io.loopSeq := loopSeq
        t1.io.constSeq := t2.asBits
        t1.io.stageCntLsb := io.isNtt ? stageCntCop(0, log2Up(log2Up(g.paraNum)) bits).asBits | stageCnt(
          0,
          log2Up(log2Up(g.paraNum)) bits
        ).asBits
        t1.io.stageCntoverflow := stageOverflow
      }
    }
  }
  import twGen._

  val flatSeq = Vec((ins0_subSeq.zip(ins1_subSeq)).flatMap { case (t1, t2) => Seq(t1.asUInt, t2.asUInt) })
  io.idle := fsm.isActive(fsm.IDLE)
  io.RdAddrOri.payload := flatSeq
  io.RdAddrOri.valid := fsm.isActive(fsm.LOOP)
  if (g.debug) {
    val uDec = AddrDecode(g)
    uDec.io.addrOri := io.RdAddrOri.payload
    val probe = uDec.io.BankBus.simPublic()
  }

  io.TwBus.payload.twAddr := twAddr.asUInt
//  io.TwBus.payload.twAddr := Delay(twAddr.asUInt,g.DecodeLatency)
  io.TwBus.payload.twMux.zip(twMuxArray.toSeq).foreach { case (t1, t2) => t1 := t2.io.twMuxUnit }
//  io.TwBus.payload.twMux.zip(twMuxArray.toSeq).foreach { case (t1, t2) => t1 := Delay(t2.io.twMuxUnit,g.DecodeLatency) }
  io.TwBus.valid := fsm.isActive(fsm.LOOP)
//  io.TwBus.valid := Delay(fsm.isActive(fsm.LOOP),g.DecodeLatency)
}

case class CtrlOpt1(g: NttCfg2414) extends Component {
  val io = new Bundle {
    val start = in Bool ()
    val isNtt = in Bool ()
    val idle = out Bool ()
    val RdAddrOri = master Flow Vec(UInt(g.Log2NttPoints bits), g.BI)
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
    val MsbPadSeq = constSeq.map { item => Cat(loopCnt.lsb, item) }
    val MsbPad = (B"1'b0" ## loopCnt(loopCnt.high downto 1)).asBits
    val LsbPadSeq = constSeq.map { item => Cat(item, B"1'b0") }
    val LsbPad = loopCnt.asBits
    val lsbMuxArray = Array.fill(g.paraNum)(new SeqMux(log2Up(g.paraNum) + 1))
    lsbMuxArray.toSeq.zip(MsbPadSeq.zip(LsbPadSeq)).foreach { case (dut, (t1, t2)) =>
      dut.io.SeqA := t1
      dut.io.SeqB := t2
      dut.io.isNtt := io.isNtt
      dut.io.ohSeq := OH_shifter(log2Up(g.paraNum) downto 0)
      dut.io.thermalSeq := Thermal_shifter(log2Up(g.paraNum) downto 0)
    }
    val msbMux = new SeqMux(g.Log2NttPoints - (log2Up(g.paraNum) + 1))
    msbMux.io.SeqA := MsbPad
    msbMux.io.SeqB := LsbPad
    msbMux.io.isNtt := io.isNtt
    msbMux.io.ohSeq := OH_shifter(OH_shifter.high downto (log2Up(g.paraNum) + 1))
    msbMux.io.thermalSeq := Thermal_shifter(OH_shifter.high downto (log2Up(g.paraNum) + 1))
    val ins0_subSeq = Vec(Bits(g.Log2NttPoints bits), g.paraNum)
    val ins1_subSeq = Vec(Bits(g.Log2NttPoints bits), g.paraNum)
    (ins0_subSeq.zip(ins1_subSeq)).zip(lsbMuxArray.toSeq).foreach { case ((ins0, ins1), dut) =>
      ins0 := msbMux.io.ins0_seq ## dut.io.ins0_seq
      ins1 := msbMux.io.ins1_seq ## dut.io.ins1_seq
    }
  }
  import subDut._
  val twGen = new Area {
    val twLoopCntMsbPadNtt = Cat((B"1'b1"), loopCnt).asUInt
    val twLoopCntMsbPadIntt = Cat((B"1'b1"), ~(loopCnt.asBits)).asUInt
    val loopCntShiftNtt = UInt(loopCnt.getWidth + 1 bits)
    val loopCntShiftIntt = UInt(loopCnt.getWidth + 1 bits)
    loopCntShiftIntt := (Cat((B"1'b0" #* log2Up(g.BI)), loopCnt).asUInt |>> stageCnt).resized
    loopCntShiftNtt := (Cat((B"1'b1" #* log2Up(g.BI)), loopCnt).asUInt |>> stageCntCop).resized
    //    mask := io.isNtt ? Thermal_shifter(0, log2Up(g.nttPoint / g.paraNum) bits).reversed | Thermal_shifter(
    //      Thermal_shifter.high - (log2Up(log2Up(g.paraNum))) downto (Thermal_shifter.high - (log2Up(
    //        log2Up(g.paraNum)
    //      )) - log2Up(g.nttPoint / g.paraNum) + 1)
    //    ).reversed
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

  val flatSeq = Vec((ins0_subSeq.zip(ins1_subSeq)).flatMap { case (t1, t2) => Seq(t1.asUInt, t2.asUInt) })
  io.idle := fsm.isActive(fsm.IDLE)
  io.RdAddrOri.payload := flatSeq
  io.RdAddrOri.valid := fsm.isActive(fsm.LOOP)
  if (g.debug) {
    val uDec = AddrDecode(g)
    uDec.io.addrOri := io.RdAddrOri.payload
    val probe = uDec.io.BankBus.simPublic()
  }

  io.TwBus.payload.twAddr := twAddr.asUInt
  //  io.TwBus.payload.twAddr := Delay(twAddr.asUInt,g.DecodeLatency)
  io.TwBus.payload.twMux.zip(twMuxArray.toSeq).foreach { case (t1, t2) => t1 := t2.io.twMuxUnit }
  //  io.TwBus.payload.twMux.zip(twMuxArray.toSeq).foreach { case (t1, t2) => t1 := Delay(t2.io.twMuxUnit,g.DecodeLatency) }
  io.TwBus.valid := fsm.isActive(fsm.LOOP)
  //  io.TwBus.valid := Delay(fsm.isActive(fsm.LOOP),g.DecodeLatency)
}

object CtrlGenV extends App {
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "./rtl/Ntt/CtrlPath/",
    genLineComments = true
  ).generate(new Ctrl(NttCfg2414(paraNum = 32, debug = false)))
}
object CtrlOpt1GenV extends App {
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "./rtl/Ntt/CtrlPath/",
    genLineComments = true
  ).generate(new CtrlOpt1(NttCfg2414(paraNum = 32, debug = false)))
}
object SeqMuxGenV extends App {
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "./rtl/Ntt/CtrlPath/",
    genLineComments = true
  ).generate(new SeqMux(8))
}

object TwMuxGenV extends App {
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "./rtl/Ntt/CtrlPath/",
    genLineComments = true
  ).generate(new twMux(4))
}

object CtrlSimOpt1 extends App {
  val period = 10
  val dut = SimConfig.withXSim.withWave.compile(new CtrlOpt1(NttCfg2414(nttPoint = 32, paraNum = 4)))
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

object SeqMuxVivadoFlow extends App {
  val workspace = "./vivado_prj/Ntt/Ctrl/SeqMux"
  val vivadopath = "/opt/Xilinx/Vivado/2023.1/bin"
  val family = "Zynq UltraScale+ MPSoCS"
  val device = "xczu9eg-ffvb1156-2-i"
  val frequency = 300 MHz
  val cpu = 12
  val rtl = new Rtl {
    override def getName(): String = "SeqMux"
    override def getRtlPath(): String = "/PRJ/SpinalHDL-prj/PRJ/myTest/test/rtl/Ntt/CtrlPath/SeqMux.v"
  }
  val flow = VivadoFlow(vivadopath, workspace, rtl, family, device, frequency, cpu)
  println(s"${family} -> ${(flow.getFMax / 1e6).toInt} MHz ${flow.getArea}")
}
object CtrlVivadoFlow extends App {
  val workspace = "./vivado_prj/Ntt/Ctrl/Ctrl"
  val vivadopath = "/opt/Xilinx/Vivado/2023.1/bin"
  val family = "Zynq UltraScale+ MPSoCS"
  val device = "xczu9eg-ffvb1156-2-i"
  val frequency = 300 MHz
  val cpu = 12
  val rtl = new Rtl {
    override def getName(): String = "Ctrl"
    override def getRtlPath(): String = "/PRJ/SpinalHDL-prj/PRJ/myTest/test/rtl/Ntt/CtrlPath/Ctrl.v"
  }
  val flow = VivadoFlow(vivadopath, workspace, rtl, family, device, frequency, cpu)
  println(s"${family} -> ${(flow.getFMax / 1e6).toInt} MHz ${flow.getArea}")
}

object CtrlOpt1VivadoFlow extends App {
  val workspace = "./vivado_prj/Ntt/Ctrl/CtrlOpt1"
  val vivadopath = "/opt/Xilinx/Vivado/2023.1/bin"
  val family = "Zynq UltraScale+ MPSoCS"
  val device = "xczu9eg-ffvb1156-2-i"
  val frequency = 300 MHz
  val cpu = 12
  val rtl = new Rtl {
    override def getName(): String = "CtrlOpt1"
    override def getRtlPath(): String = "/PRJ/SpinalHDL-prj/PRJ/myTest/test/rtl/Ntt/CtrlPath/CtrlOpt1.v"
  }
  val flow = VivadoFlow(vivadopath, workspace, rtl, family, device, frequency, cpu)
  println(s"${family} -> ${(flow.getFMax / 1e6).toInt} MHz ${flow.getArea}")
}
