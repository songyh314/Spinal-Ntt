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
    val stageCntoverflw = in Bool ()
    val twMuxUnit = out UInt (width bits)
  }
  val muxTmp = Vec(Bits(width + 1 bits), width)
  val jointSeq = Cat(io.loopSeq, io.constSeq)
  for (i <- 0 until width) {
    muxTmp(i) := jointSeq(i, width + 1 bits)
  }
  val muxRes = Vec(Bool(), width)
  muxRes.zip(muxTmp).foreach { case (t1, t2) =>
    when(io.stageCntoverflw) {
      t1 := t2.msb
    } otherwise {
      t1 := (t2(t2.high - 1 downto 0).asBools).read(io.stageCntLsb.asUInt.resized)
    }
  }
  io.twMuxUnit := muxRes.asBits.asUInt
}

object twMuxSim extends App {
  val period = 10
  val dut = SimConfig.withXSim.withWave.compile(new twMux(4))

}

//只对位反转得到的INTT地址在译码时会产生冲突
//case class CtrlFail(g: NttCfg2414) extends Component {
//  val io = new Bundle {
//    val start = in Bool ()
//    val isNtt = in Bool ()
//    val idle = out Bool ()
//    val RdAddrOri = master Flow Vec(UInt(g.Log2NttPoints bits), g.BI)
//  }
//  val loopCnt = Reg(UInt(log2Up(g.nttPoint / g.BI) bits)) init U(0)
//  val stageCnt = Reg(UInt(log2Up(g.Log2NttPoints) bits)) init U(0)
//  val OH_shifter = Reg(Bits(g.Log2NttPoints bits)) init 0
//  val Thermal_shifter = Reg(Bits(g.Log2NttPoints bits)) init 0
//  val constSeq = (0 until g.paraNum).map { U(_, log2Up(g.paraNum) bits) }
//
//  val fsm = new StateMachine {
////    setEncoding(binaryOneHot)
//    val IDLE = makeInstantEntry()
//    val LOOP = new State
//    IDLE
//      .whenIsActive {
//        when(io.start)(goto(LOOP))
//      }
//    LOOP
//      .onEntry {
//        OH_shifter.msb := True
//        Thermal_shifter.msb := True
//      }
//      .whenIsActive {
//        when(isNext(IDLE)) {
//          loopCnt := 0; stageCnt := 0
//          OH_shifter := 0; Thermal_shifter := 0
//        } elsewhen (loopCnt === loopCnt.maxValue) {
//          OH_shifter := OH_shifter |>> 1
//          Thermal_shifter := (B"1'b1" ## Thermal_shifter(Thermal_shifter.high downto 1))
//        }
//        when(loopCnt === loopCnt.maxValue) {
//          loopCnt := U(0)
//        } otherwise (loopCnt := loopCnt + 1)
//        when(loopCnt === loopCnt.maxValue) {
//          when(stageCnt === (g.Log2NttPoints - 1)) { goto(IDLE) } otherwise { stageCnt := stageCnt + 1 }
//        }
//      }
//  }
//
//  val subDut = new Area {
//    val MsbPadSeq = constSeq.map { item => Cat(loopCnt.lsb, item) }
//    val MsbPad = (B"1'b0" ## loopCnt(loopCnt.high downto 1)).asBits
//    val LsbPadSeq = constSeq.map { item => Cat(item, B"1'b0") }
//    val LsbPad = loopCnt.asBits
//    val lsbMuxArray = Array.fill(g.paraNum)(new SeqMux(log2Up(g.paraNum) + 1))
//    lsbMuxArray.toSeq.zip(MsbPadSeq.zip(LsbPadSeq)).foreach { case (dut, (t1, t2)) =>
//      dut.io.SeqA := t1
//      dut.io.SeqB := t2
//      dut.io.ohSeq := OH_shifter(log2Up(g.paraNum) downto 0)
//      dut.io.thermalSeq := Thermal_shifter(log2Up(g.paraNum) downto 0)
//    }
//    val msbMux = new SeqMux(g.Log2NttPoints - (log2Up(g.paraNum) + 1))
//    msbMux.io.SeqA := MsbPad
//    msbMux.io.SeqB := LsbPad
//    msbMux.io.ohSeq := OH_shifter(OH_shifter.high downto (log2Up(g.paraNum) + 1))
//    msbMux.io.thermalSeq := Thermal_shifter(OH_shifter.high downto (log2Up(g.paraNum) + 1))
//    val ins0_subSeq = Vec(Bits(g.Log2NttPoints bits), g.paraNum)
//    val ins1_subSeq = Vec(Bits(g.Log2NttPoints bits), g.paraNum)
//    (ins0_subSeq.zip(ins1_subSeq)).zip(lsbMuxArray.toSeq).foreach { case ((ins0, ins1), dut) =>
//      ins0 := msbMux.io.ins0_seq ## dut.io.ins0_seq
//      ins1 := msbMux.io.ins1_seq ## dut.io.ins1_seq
//    }
//  }
//  import subDut._
//
//  val flatSeq = Vec((ins0_subSeq.zip(ins1_subSeq)).flatMap { case (t1, t2) => Seq(t1.asUInt, t2.asUInt) })
//  val flatSeqRev = Vec(
//    for (i <- 0 until g.BI) yield {
//      flatSeq.read(U(i, log2Up(g.BI) bits)).asBits.reversed.asUInt
//    }
//  )
//  println(flatSeq)
//  println(flatSeqRev)
////  val flatSeqRev = flatSeq.map(_.asBits).map(_.reversed).map(_.asUInt)
//  io.idle := fsm.isActive(fsm.IDLE)
//  io.RdAddrOri.payload := io.isNtt ? flatSeq | flatSeqRev
//  io.RdAddrOri.valid := fsm.isActive(fsm.LOOP)
//  val uDec = AddrDecode(g)
//  uDec.io.addrOri := io.RdAddrOri.payload
//  val probe = uDec.io.BankBus.simPublic()
//}

//case class twAddr(loopWidth:Int, stageWidth:Int,n:Int) extends App{
//  val io = new Bundle{
//    val loopCnt = in UInt(loopWidth bits)
//    val stageCnt = in UInt(stageWidth bits)
//    val musk = in Bits((stageWidth - log2Up(n)) bits)
//    val isNtt = Bool()
//    val twAddr = out UInt((stageWidth - log2Up(n)) bits)
//  }
//
//}
case class Ctrl(g: NttCfg2414) extends Component {
  val io = new Bundle {
    val start = in Bool ()
    val isNtt = in Bool ()
    val idle = out Bool ()
    val RdAddrOri = master Flow Vec(UInt(g.Log2NttPoints bits), g.BI)
    val TwBus = master Flow (twPayload(
      addrWidth = log2Up(g.nttPoint / g.paraNum),
      muxWidth = log2Up(g.paraNum),
      para = g.paraNum
    ))
  }
  val loopCnt = Reg(UInt(log2Up(g.nttPoint / g.BI) bits)) init U(0)
//  val loopCntShift = io.isNtt ? (loopCnt |>> stageCnt) | (loopCnt |>> stageCntCop)
  val stageCnt = Reg(UInt(log2Up(g.Log2NttPoints) bits)) init U(0)
  val OH_shifter = Reg(Bits(g.Log2NttPoints bits)) init 0
  val Thermal_shifter = Reg(Bits(g.Log2NttPoints bits)) init 0
  val constSeq = (0 until g.paraNum).map { U(_, log2Up(g.paraNum) bits) }
  val stageCntCop = g.Log2NttPoints - 1 - stageCnt
  val twAddrReg = Reg(UInt(log2Up(g.nttPoint / g.paraNum) bits)) init (U(0))
  val twMask = Reg(Bits(g.Log2NttPoints - log2Up(g.paraNum) bits)) init 0
  val twMuskInc = io.isNtt ? (stageCntCop + 1 >= log2Up(g.paraNum)) | (stageCnt + 1 >= log2Up(g.paraNum))
  val fsm = new StateMachine {
    //    setEncoding(binaryOneHot)
//    val twMuskInc = io.isNtt ? (stageCntCop + 1 >= log2Up(g.paraNum)) | (stageCnt + 1 >= log2Up(g.paraNum))
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
          when(twMuskInc) {
            twMask := io.isNtt ? (B"1'b1" ## twMask(twMask.high downto 1)) | (twMask(
              twMask.high - 1 downto 0
            ) ## B"1'b1")
          }
        }
        when(loopCnt === loopCnt.maxValue) {
          loopCnt := U(0)
        } otherwise (loopCnt := loopCnt + 1)
        when(loopCnt === loopCnt.maxValue) {
          when(stageCnt === (g.Log2NttPoints - 1)) { goto(IDLE) } otherwise { stageCnt := stageCnt + 1 }
        }
      }
    when(isNext(IDLE)) {
      loopCnt := 0; stageCnt := 0; twAddrReg := U(0)
      OH_shifter := 0; Thermal_shifter := 0
      twMask := 0
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
  val twMux = new Area {
    val twLoopCntMsbPad = Cat(B"1'b1", loopCnt).asUInt
    val loopCntShiftNtt = UInt(loopCnt.getWidth bits)
    val loopCntShiftIntt = UInt(loopCnt.getWidth bits)
    loopCntShiftNtt := loopCnt |>> stageCnt
    loopCntShiftIntt := (Cat((B"1'b1" #* log2Up(g.BI)), loopCnt).asUInt |>> stageCntCop).resized
//    val twLoopShift = io.isNtt ? ()|()
//    val loopMuxSel = twLoopCntMsbPad |>> ()
    val twAddr = Cat(
      twMask.msb,
      io.isNtt ? (twMask(twMask.high - 1 downto 0) | loopCntShiftNtt.asBits) | (twMask(
        twMask.high - 1 downto 0
      ) & loopCntShiftIntt.asBits)
    ).asUInt
  }
  import twMux._

  val flatSeq = Vec((ins0_subSeq.zip(ins1_subSeq)).flatMap { case (t1, t2) => Seq(t1.asUInt, t2.asUInt) })
  io.idle := fsm.isActive(fsm.IDLE)
  io.RdAddrOri.payload := flatSeq
  io.RdAddrOri.valid := fsm.isActive(fsm.LOOP)
  if (g.debug) {
    val uDec = AddrDecode(g)
    uDec.io.addrOri := io.RdAddrOri.payload
    val probe = uDec.io.BankBus.simPublic()
  }

  io.TwBus.payload.twAddr := twAddr
  io.TwBus.payload.twMux.assignDontCare()
  io.TwBus.valid := fsm.isActive(fsm.LOOP)
}

object CtrlGenV extends App {
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "./rtl/Ntt/CtrlPath/",
    genLineComments = true
  ).generate(new Ctrl(NttCfg2414(debug = false)))
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

object CtrlSim extends App {
  val period = 10
  val dut = SimConfig.withXSim.withWave.compile(new Ctrl(NttCfg2414(nttPoint = 64)))
  dut.doSim("test") { dut =>
    import dut._
    SimTimeout(1000 * period)
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
