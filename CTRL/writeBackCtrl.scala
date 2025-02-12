package Ntt.CTRL

import Ntt.NttCfg.NttCfgParam
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.eda.bench.Rtl
import spinal.lib.eda.xilinx.VivadoFlow
import spinal.lib.fsm.{State, StateMachine}

case class writeBackCtrl(g: NttCfgParam) extends Component {
  val io = new Bundle {
    val wctrlStart = in Bool ()
    val isNtt = in Bool ()
    val idle = out Bool ()
    val isOutside = in Bool ()
    val wrAddrOri = master Flow Vec(UInt(g.Log2NttPoints bits), g.BI)
  }
  val loopCnt = Reg(UInt(log2Up(g.nttPoint / g.BI) bits)) init U(0)
  val stageCnt = Reg(UInt(log2Up(g.Log2NttPoints) bits)) init U(0)

  val OH_shifter = Reg(Bits(g.Log2NttPoints bits)) init 0
  val Thermal_shifter = Reg(Bits(g.Log2NttPoints bits)) init 0

  val constSeq = (0 until g.paraNum).map { U(_, log2Up(g.paraNum) bits) }
  val outsideWriteSeq = (0 until g.BI).map { U(_, log2Up(g.BI) bits) }
  val fsm = new StateMachine {
    //    setEncoding(binaryOneHot)
    val IDLE = makeInstantEntry()
    val LOOP = new State
    IDLE
      .whenIsActive {
        when(io.wctrlStart)(goto(LOOP))
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
          when(io.isOutside) { goto(IDLE) }
          OH_shifter := io.isNtt ? (OH_shifter |>> 1) | (OH_shifter |<< 1)
          Thermal_shifter := io.isNtt ? (B"1'b1" ## Thermal_shifter(Thermal_shifter.high downto 1)) | (Thermal_shifter(
            Thermal_shifter.high - 1 downto 0
          ) ## B"1'b1")
        }
        when(loopCnt === loopCnt.maxValue) {
          loopCnt := U(0)
        } otherwise (loopCnt := loopCnt + 1)
        when(loopCnt === loopCnt.maxValue) {
          when(stageCnt === (g.Log2NttPoints - 1) && !(io.isOutside)) { goto(IDLE) } otherwise {
            stageCnt := stageCnt + 1
          }
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
    val msbMuxWriteBack = new SeqMux(g.Log2NttPoints - (log2Up(g.paraNum) + 1))
    msbMuxWriteBack.io.SeqA := MsbPad
    msbMuxWriteBack.io.SeqB := LsbPad
    msbMuxWriteBack.io.isNtt := io.isNtt
    msbMuxWriteBack.io.ohSeq := OH_shifter(OH_shifter.high downto (log2Up(g.paraNum) + 1))
    msbMuxWriteBack.io.thermalSeq := Thermal_shifter(OH_shifter.high downto (log2Up(g.paraNum) + 1))
    val Seq0 = Vec(Bits(g.Log2NttPoints bits), g.paraNum)
    val Seq1 = Vec(Bits(g.Log2NttPoints bits), g.paraNum)
    (Seq0.zip(Seq1)).zip(lsbMuxArray.toSeq).foreach { case ((ins0, ins1), dut) =>
      ins0 := msbMuxWriteBack.io.ins0_seq ## dut.io.ins0_seq
      ins1 := msbMuxWriteBack.io.ins1_seq ## dut.io.ins1_seq
    }
  }
  import subDut._
  val flatWrSeq = Vec((Seq0.zip(Seq1)).flatMap { case (t1, t2) => Seq(t1.asUInt, t2.asUInt) })
  val flatOutsideWriteSeq = Vec(outsideWriteSeq.map { item => Cat(loopCnt, item).asUInt })
  io.idle := fsm.isActive(fsm.IDLE)
  io.wrAddrOri.payload := io.isOutside ? flatOutsideWriteSeq | flatWrSeq
  io.wrAddrOri.valid := fsm.isActive(fsm.LOOP)
}

object writeBackCtrlGenV extends App {
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "./rtl/Ntt/CtrlPath/",
    genLineComments = true
  ).generate(new writeBackCtrl(NttCfgParam(paraNum = 4, debug = false)))
}
object writeBackCtrlVivadoFlow extends App {
  val workspace = "./vivado_prj/Ntt/Ctrl/writeBackCtrl"
  val vivadopath = "/opt/Xilinx/Vivado/2023.1/bin"
  val family = "Zynq UltraScale+ MPSoCS"
  val device = "xczu9eg-ffvb1156-2-i"
  val frequency = 300 MHz
  val cpu = 12
  val rtl = new Rtl {
    override def getName(): String = "writeBackCtrl"
    override def getRtlPath(): String = "/PRJ/SpinalHDL-prj/PRJ/myTest/test/rtl/Ntt/CtrlPath/writeBackCtrl.v"
  }
  val flow = VivadoFlow(vivadopath, workspace, rtl, family, device, frequency, cpu)
  println(s"${family} -> ${(flow.getFMax / 1e6).toInt} MHz ${flow.getArea}")
}

object writeBackCtrlSim extends App {
  val period = 10
  val dut = SimConfig.withXSim.withWave.compile(new writeBackCtrl(NttCfgParam(nttPoint = 128, paraNum = 4)))
  dut.doSim("test") { dut =>
    import dut._
    SimTimeout(5000 * period)
    io.isOutside #= false
    clockDomain.forkStimulus(period)
    clockDomain.waitSampling(10)
    io.wctrlStart #= true
    io.isNtt #= true
    clockDomain.waitSampling()
    io.wctrlStart #= false
    clockDomain.waitSampling(10)
    clockDomain.waitActiveEdgeWhere(io.idle.toBoolean)
    clockDomain.waitSampling(10)
    io.isNtt #= false
    io.wctrlStart #= true
    clockDomain.waitSampling()
    io.wctrlStart #= false
    clockDomain.waitSampling(10)
    clockDomain.waitActiveEdgeWhere(io.idle.toBoolean)
    clockDomain.waitSampling(10)
    io.isOutside #= true
    io.wctrlStart #= true
    clockDomain.waitSampling()
    io.wctrlStart #= false
    clockDomain.waitActiveEdgeWhere(io.idle.toBoolean)
    clockDomain.waitSampling(10)

    simSuccess()
  }
}
