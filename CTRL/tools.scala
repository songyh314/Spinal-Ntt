package Ntt.CTRL

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.eda.bench.Rtl
import spinal.lib.eda.xilinx.VivadoFlow

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

object SeqMuxGenV extends App {
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "./rtl/Ntt/CtrlPath/",
    genLineComments = true
  ).generate(new SeqMux(8))
}


case class twMux(num: Int) extends Component {
//  assert(isPow2(num) && (num != 1), "tw num should be power of 2 and should not be 1")
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

object TwMuxGenV extends App {
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "./rtl/Ntt/CtrlPath/",
    genLineComments = true
  ).generate(new twMux(4))
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

