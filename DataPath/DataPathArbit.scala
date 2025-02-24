package Ntt.DataPath
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import Ntt.NttCfg._
import myRam._
import myTools._
import spinal.lib.eda.bench.Rtl
import spinal.lib.eda.xilinx.VivadoFlow

import scala.collection.mutable

object shuffleMux {
  // 0,1,4 -> 0/4
  case class sfMuxp4ch0_4(sw: Int = 3, expect: Int, id: Seq[Int]) extends Component {
    val io = new Bundle {
      val muxIn = in Vec (UInt(sw bits), 3)
      val muxOut = out UInt (sw bits)
    }
//    val const = Vec(UInt(sw bits))
    val const = id.map(U(_, sw bits))
    val flag = io.muxIn.map { item => item === expect }
    io.muxOut := MuxOH(flag, const)
  }

  // 1,2,3,5,6 -> 1/5
  case class sfMuxp4ch1_5(sw: Int = 3, expect: Int, id: Seq[Int]) extends Component {
    val io = new Bundle {
      val muxIn = in Vec (UInt(sw bits), 5)
      val muxOut = out UInt (sw bits)
    }
    val const = id.map(U(_, sw bits))
    val flag = io.muxIn.map { item => item === expect }
    io.muxOut := MuxOH(flag, const)
  }

  // 1,2,4,5,6 -> 2/6
  case class sfMuxp4ch2_6(sw: Int = 3, expect: Int, id: Seq[Int]) extends Component {
    val io = new Bundle {
      val muxIn = in Vec (UInt(sw bits), 5)
      val muxOut = out UInt (sw bits)
    }
    val const = id.map(U(_, sw bits))
    val flag = io.muxIn.map { item => item === expect }
    io.muxOut := MuxOH(flag, const)
  }

  // 3,6,7 -> 3/7
  case class sfMuxp4ch3_7(sw: Int = 3, expect: Int, id: Seq[Int]) extends Component {
    val io = new Bundle {
      val muxIn = in Vec (UInt(sw bits), 3)
      val muxOut = out UInt (sw bits)
    }
    val const = id.map(U(_, sw bits))
    val flag = io.muxIn.map { item => item === expect }
    io.muxOut := MuxOH(flag, const)
  }

//0,1,8 -> 0/8
  case class sfMuxp8ch0_8(sw: Int = 4, expect: Int, id: Seq[Int]) extends Component {
    require(sw == 4)
    val io = new Bundle {
      val muxIn = in Vec (UInt(sw bits), 3)
      val muxOut = out UInt (sw bits)
    }
    val const = id.map(U(_, sw bits))
    val flag = io.muxIn.map { item => item === expect }
    io.muxOut := MuxOH(flag, const)
  }

  // 1,2,3,9,10 -> 1/9
  case class sfMuxp8ch1_9(sw: Int = 4, expect: Int, id: Seq[Int]) extends Component {
    require(sw == 4)
    val io = new Bundle {
      val muxIn = in Vec (UInt(sw bits), 5)
      val muxOut = out UInt (sw bits)
    }
    val const = id.map(U(_, sw bits))
    val flag = io.muxIn.map { item => item === expect }
    io.muxOut := MuxOH(flag, const)
  }

  // 1,3,4,5,9,10,12 -> 2/10
  // bug
  // new -> 1,2,4,5,9,10,12 -> 2/10
  case class sfMuxp8ch2_10(sw: Int = 4, expect: Int, id: Seq[Int]) extends Component {
    require(sw == 4)
    val io = new Bundle {
      val muxIn = in Vec (UInt(sw bits), 7)
      val muxOut = out UInt (sw bits)
    }
    val const = id.map(U(_, sw bits))
    val flag = io.muxIn.map { item => item === expect }
    io.muxOut := MuxOH(flag, const)
  }

  // 3,6,7,11,14 -> 3/11
  case class sfMuxp8ch3_11(sw: Int = 4, expect: Int, id: Seq[Int]) extends Component {
    require(sw == 4)
    val io = new Bundle {
      val muxIn = in Vec (UInt(sw bits), 5)
      val muxOut = out UInt (sw bits)
    }
    val const = id.map(U(_, sw bits))
    val flag = io.muxIn.map { item => item === expect }
    io.muxOut := MuxOH(flag, const)
  }
  // 1,4,8,9,12 -> 4/12
  case class sfMuxp8ch4_12(sw: Int = 4, expect: Int, id: Seq[Int]) extends Component {
    require(sw == 4)
    val io = new Bundle {
      val muxIn = in Vec (UInt(sw bits), 5)
      val muxOut = out UInt (sw bits)
    }
    val const = id.map(U(_, sw bits))
    val flag = io.muxIn.map { item => item === expect }
    io.muxOut := MuxOH(flag, const)
  }

  // 3,5,6,10,11,13,14 -> 5/13
  case class sfMuxp8ch5_13(sw: Int = 4, expect: Int, id: Seq[Int]) extends Component {
    require(sw == 4)
    val io = new Bundle {
      val muxIn = in Vec (UInt(sw bits), 7)
      val muxOut = out UInt (sw bits)
    }
    val const = id.map(U(_, sw bits))
    val flag = io.muxIn.map { item => item === expect }
    io.muxOut := MuxOH(flag, const)
  }

  // 5,6,12,13,14 -> 6/14
  case class sfMuxp8ch6_14(sw: Int = 4, expect: Int, id: Seq[Int]) extends Component {
    require(sw == 4)
    val io = new Bundle {
      val muxIn = in Vec (UInt(sw bits), 5)
      val muxOut = out UInt (sw bits)
    }
    val const = id.map(U(_, sw bits))
    val flag = io.muxIn.map { item => item === expect }
    io.muxOut := MuxOH(flag, const)
  }

  // 7,14,15 -> 7/15
  case class sfMuxp8ch7_15(sw: Int = 4, expect: Int, id: Seq[Int]) extends Component {
    require(sw == 4)
    val io = new Bundle {
      val muxIn = in Vec (UInt(sw bits), 3)
      val muxOut = out UInt (sw bits)
    }
    val const = id.map(U(_, sw bits))
    val flag = io.muxIn.map { item => item === expect }
    io.muxOut := MuxOH(flag, const)
  }

}
object sfMuxp4ch0_4Genv extends App {
  import shuffleMux._
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "NttOpt/rtl/DataPath",
    genLineComments = true
  ).generate(new sfMuxp4ch0_4(3, 0, Seq(0, 1, 4)))
}

case class idxDecodeUnitOpt(g: NttCfgParam) extends Component {
  val io = new Bundle {
    val addrOri = in UInt (g.BankAddrWidth bits)
    val idxOri = in UInt (g.BankIndexWidth bits)
    val idxDec = out UInt (g.BankIndexWidth bits)
  }
  noIoPrefix()
//  val bankIdx = Reg(UInt(g.BankIndexWidth bits)) init U(0)
  val sn = io.addrOri.asBits.xorR ^ io.idxOri.msb
  //  val bi = io.idxOri +^ (sn(log2Up(g.radix) - 1 downto 0) ## (B"1'b0" #* (log2Up(g.paraNum)))).asUInt
  //  bankIdx := bi(g.BankIndexWidth - 1 downto 0)
  io.idxDec := Cat(sn, io.idxOri(0, g.BankIndexWidth - 1 bits)).asUInt
}
object idxDecodeUnitOpt {
  def apply(addr: UInt, idx: UInt, g: NttCfgParam): UInt = {
    val dut = new idxDecodeUnitOpt(g)
    dut.io.addrOri := addr
    dut.io.idxOri := idx
    dut.io.idxDec
  }
}

case class shuffleOpt(g: NttCfgParam) extends Component {
  val io = new Bundle {
    val addrOri = in Vec (UInt(g.BankAddrWidth bits), g.radix)
    val idxOri = in Vec (UInt(g.BankIndexWidth bits), g.BI)
    val idxDec = out Vec (UInt(g.BankIndexWidth bits), g.BI)
    val idxShuffle = out Vec (UInt(g.BankIndexWidth bits), g.BI)
  }

  val addrFlat = Vec(UInt(g.BankAddrWidth bits), g.BI)
  for (i <- 0 until g.BI) {
    addrFlat(i) := io.addrOri(i % g.radix)
  }
  val decTmp = Vec(UInt(g.BankIndexWidth bits), g.BI)
  decTmp.zip((io.idxOri).zip(addrFlat)).foreach { case (port, (idx, addr)) =>
    port := idxDecodeUnitOpt(addr = addr, idx = idx, g = g)
  }
  io.idxDec := RegNext(decTmp)

  import shuffleMux._
  import DataPathTools._
  if (g.paraNum == 4) {
    val ch0 = sfMuxp4ch0_4(g.BankIndexWidth, 0, Seq(0, 1, 4))
    val ch4 = sfMuxp4ch0_4(g.BankIndexWidth, 4, Seq(0, 1, 4))
    ch0.io.muxIn := muxDrive(Seq(0, 1, 4), decTmp)
    ch4.io.muxIn := muxDrive(Seq(0, 1, 4), decTmp)
    io.idxShuffle(0) := RegNext(ch0.io.muxOut); io.idxShuffle(4) := RegNext(ch4.io.muxOut)

    val ch1 = sfMuxp4ch1_5(g.BankIndexWidth, 1, Seq(1, 2, 3, 5, 6))
    val ch5 = sfMuxp4ch1_5(g.BankIndexWidth, 5, Seq(1, 2, 3, 5, 6))
    ch1.io.muxIn := muxDrive(Seq(1, 2, 3, 5, 6), decTmp)
    ch5.io.muxIn := muxDrive(Seq(1, 2, 3, 5, 6), decTmp)
    io.idxShuffle(1) := RegNext(ch1.io.muxOut); io.idxShuffle(5) := RegNext(ch5.io.muxOut)

    val ch2 = sfMuxp4ch2_6(g.BankIndexWidth, 2, Seq(1, 2, 4, 5, 6))
    val ch6 = sfMuxp4ch2_6(g.BankIndexWidth, 6, Seq(1, 2, 4, 5, 6))
    ch2.io.muxIn := muxDrive(Seq(1, 2, 4, 5, 6), decTmp)
    ch6.io.muxIn := muxDrive(Seq(1, 2, 4, 5, 6), decTmp)
    io.idxShuffle(2) := RegNext(ch2.io.muxOut); io.idxShuffle(6) := RegNext(ch6.io.muxOut)

    val ch3 = sfMuxp4ch3_7(g.BankIndexWidth, 3, Seq(3, 6, 7))
    val ch7 = sfMuxp4ch3_7(g.BankIndexWidth, 7, Seq(3, 6, 7))
    ch3.io.muxIn := muxDrive(Seq(3, 6, 7), decTmp)
    ch7.io.muxIn := muxDrive(Seq(3, 6, 7), decTmp)
    io.idxShuffle(3) := RegNext(ch3.io.muxOut); io.idxShuffle(7) := RegNext(ch7.io.muxOut)
  }
  else if (g.paraNum == 8) {
    val ch0 = new sfMuxp8ch0_8(g.BankIndexWidth, 0, Seq(0, 1, 8))
    val ch8 = new sfMuxp8ch0_8(g.BankIndexWidth, 8, Seq(0, 1, 8))
    ch0.io.muxIn := muxDrive(Seq(0, 1, 8), decTmp)
    ch8.io.muxIn := muxDrive(Seq(0, 1, 8), decTmp)
    io.idxShuffle(0) := RegNext(ch0.io.muxOut)
    io.idxShuffle(8) := RegNext(ch8.io.muxOut)

    val ch1 = new sfMuxp8ch1_9(g.BankIndexWidth, 1, Seq(1, 2, 3, 9, 10))
    val ch9 = new sfMuxp8ch1_9(g.BankIndexWidth, 9, Seq(1, 2, 3, 9, 10))
    ch1.io.muxIn := muxDrive(Seq(1, 2, 3, 9, 10), decTmp)
    ch9.io.muxIn := muxDrive(Seq(1, 2, 3, 9, 10), decTmp)
    io.idxShuffle(1) := RegNext(ch1.io.muxOut)
    io.idxShuffle(9) := RegNext(ch9.io.muxOut)

    val ch2 = new sfMuxp8ch2_10(g.BankIndexWidth, 2, Seq(1, 2, 4, 5, 9, 10, 12))
    val ch10 = new sfMuxp8ch2_10(g.BankIndexWidth, 10, Seq(1, 2, 4, 5, 9, 10, 12))
    ch2.io.muxIn := muxDrive(Seq(1, 2, 4, 5, 9, 10, 12), decTmp)
    ch10.io.muxIn := muxDrive(Seq(1, 2, 4, 5, 9, 10, 12), decTmp)
    io.idxShuffle(2) := RegNext(ch2.io.muxOut)
    io.idxShuffle(10) := RegNext(ch10.io.muxOut)

    val ch3 = new sfMuxp8ch3_11(g.BankIndexWidth, 3, Seq(3, 6, 7, 11, 14))
    val ch11 = new sfMuxp8ch3_11(g.BankIndexWidth, 11, Seq(3, 6, 7, 11, 14))
    ch3.io.muxIn := muxDrive(Seq(3, 6, 7, 11, 14), decTmp)
    ch11.io.muxIn := muxDrive(Seq(3, 6, 7, 11, 14), decTmp)
    io.idxShuffle(3) := RegNext(ch3.io.muxOut)
    io.idxShuffle(11) := RegNext(ch11.io.muxOut)

    val ch4 = new sfMuxp8ch4_12(g.BankIndexWidth, 4, Seq(1, 4, 8, 9, 12))
    val ch12 = new sfMuxp8ch4_12(g.BankIndexWidth, 12, Seq(1, 4, 8, 9, 12))
    ch4.io.muxIn := muxDrive(Seq(1, 4, 8, 9, 12), decTmp)
    ch12.io.muxIn := muxDrive(Seq(1, 4, 8, 9, 12), decTmp)
    io.idxShuffle(4) := RegNext(ch4.io.muxOut)
    io.idxShuffle(12) := RegNext(ch12.io.muxOut)

    val ch5 = new sfMuxp8ch5_13(g.BankIndexWidth, 5, Seq(3, 5, 6, 10, 11, 13, 14))
    val ch13 = new sfMuxp8ch5_13(g.BankIndexWidth, 13, Seq(3, 5, 6, 10, 11, 13, 14))
    ch5.io.muxIn := muxDrive(Seq(3, 5, 6, 10, 11, 13, 14), decTmp)
    ch13.io.muxIn := muxDrive(Seq(3, 5, 6, 10, 11, 13, 14), decTmp)
    io.idxShuffle(5) := RegNext(ch5.io.muxOut)
    io.idxShuffle(13) := RegNext(ch13.io.muxOut)

    val ch6 = new sfMuxp8ch6_14(g.BankIndexWidth, 6, Seq(5, 6, 12, 13, 14))
    val ch14 = new sfMuxp8ch6_14(g.BankIndexWidth, 14, Seq(5, 6, 12, 13, 14))
    ch6.io.muxIn := muxDrive(Seq(5, 6, 12, 13, 14), decTmp)
    ch14.io.muxIn := muxDrive(Seq(5, 6, 12, 13, 14), decTmp)
    io.idxShuffle(6) := RegNext(ch6.io.muxOut)
    io.idxShuffle(14) := RegNext(ch14.io.muxOut)

    val ch7 = new sfMuxp8ch7_15(g.BankIndexWidth, 7, Seq(7, 14, 15))
    val ch15 = new sfMuxp8ch7_15(g.BankIndexWidth, 15, Seq(7, 14, 15))
    ch7.io.muxIn := muxDrive(Seq(7, 14, 15), decTmp)
    ch15.io.muxIn := muxDrive(Seq(7, 14, 15), decTmp)
    io.idxShuffle(7) := RegNext(ch7.io.muxOut)
    io.idxShuffle(15) := RegNext(ch15.io.muxOut)
  }
  else {
    val sfSeq = genSfSeq(g.paraNum)
    val tmpSeq = Vec (UInt(g.BankIndexWidth bits), g.BI)
    tmpSeq.zip(sfSeq.zipWithIndex).map{ case(t1,(muxSeq,id)) =>
      t1 := applySf(decTmp,muxSeq,id)
    }
    io.idxShuffle := RegNext(tmpSeq)
  }


}

object shuffleOptGenv extends App {
  import shuffleMux._
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "NttOpt/rtl/DataPath",
    genLineComments = true
  ).generate(new shuffleOpt(NttCfgParam(nttPoint = 4096,paraNum = 16)))
}

case class idxDecodeUnit(g: NttCfgParam) extends Component {
  val io = new Bundle {
    val addrOri = in UInt (g.BankAddrWidth bits)
    val idxOri = in UInt (g.BankIndexWidth bits)
    val idxDec = out UInt (g.BankIndexWidth bits)
  }
  noIoPrefix()
  val bankIdx = Reg(UInt(g.BankIndexWidth bits)) init U(0)
  val sn = io.addrOri.subdivideIn(1 bits).reduceBalancedTree(_ +^ _).resize(log2Up(g.BankAddrWidth) + 1 bits)
  val bi = io.idxOri +^ (sn(log2Up(g.radix) - 1 downto 0) ## (B"1'b0" #* (log2Up(g.paraNum)))).asUInt
  bankIdx := bi(g.BankIndexWidth - 1 downto 0)
  io.idxDec := bankIdx
}
object idxDecodeUnit {
  def apply(addr: UInt, idx: UInt, g: NttCfgParam): UInt = {
    val dut = new idxDecodeUnit(g)
    dut.io.addrOri := addr
    dut.io.idxOri := idx
    dut.io.idxDec
  }
}

case class idxDecode(g: NttCfgParam) extends Component {
  val io = new Bundle {
    val addrOri = in Vec (UInt(g.BankAddrWidth bits), g.radix)
    val idxOri = in Vec (UInt(g.BankIndexWidth bits), g.BI)
    val bankIdx = out Vec (UInt(g.BankIndexWidth bits), g.BI)
  }
  noIoPrefix()

  val addrFlat = Vec(UInt(g.BankAddrWidth bits), g.BI)
  for (i <- 0 until g.BI) {
    addrFlat(i) := io.addrOri(i % g.radix)
  }
  io.bankIdx.zip((io.idxOri).zip(addrFlat)).foreach { case (port, (idx, addr)) =>
    port := idxDecodeUnit(addr = addr, idx = idx, g = g)
  }
}
object idxDecode {
  def apply(addr: Vec[UInt], idx: Vec[UInt], g: NttCfgParam): Vec[UInt] = {
    val dut = new idxDecode(g)
    dut.io.addrOri := addr
    dut.io.idxOri := idx
    dut.io.bankIdx
  }
}
object idxDecodeGenV extends App {
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "NttOpt/rtl/DataPath",
    genLineComments = true
  ).generate(new idxDecode(NttCfgParam()))
}

case class memInMux(g: NttCfgParam,prefix:String = "") extends Component {
  val io = new Bundle {
    val dataIn = in Vec (Bits(g.width bits), g.BI)
    val idxIn = in Vec (UInt(g.BankIndexWidth bits), g.BI)
    val dataOut = out Vec (Bits(g.width bits), g.BI)
  }
  import io._
  import DataPathTools._

  if (g.paraNum == 4) {

    val ch0 = Mux3ch0_4p4(g.width, g.BankIndexWidth)
    ch0.io.muxIn := muxDrive(Seq(0, 1, 4), dataIn)
    ch0.io.sel := idxIn(0)
    io.dataOut(0) := ch0.io.muxOut.setName("ch0")

    val ch1 = Mux5ch1_5p4(g.width, g.BankIndexWidth)
    ch1.io.muxIn := muxDrive(Seq(1, 2, 3, 5, 6), dataIn)
    ch1.io.sel := idxIn(1)
    io.dataOut(1) := ch1.io.muxOut.setName("ch1")

    val ch2 = Mux5ch2_6p4(g.width, g.BankIndexWidth)
    ch2.io.muxIn := muxDrive(Seq(1, 2, 4, 5, 6), dataIn)
    ch2.io.sel := idxIn(2)
    io.dataOut(2) := ch2.io.muxOut.setName("ch2")

    val ch3 = Mux3ch3_7p4(g.width, g.BankIndexWidth)
    ch3.io.muxIn := muxDrive(Seq(3, 6, 7), dataIn)
    ch3.io.sel := idxIn(3)
    io.dataOut(3) := ch3.io.muxOut.setName("ch3")

    val ch4 = Mux3ch0_4p4(g.width, g.BankIndexWidth)
    ch4.io.muxIn := muxDrive(Seq(0, 1, 4), dataIn)
    ch4.io.sel := idxIn(4)
    io.dataOut(4) := ch4.io.muxOut.setName("ch4")

    val ch5 = Mux5ch1_5p4(g.width, g.BankIndexWidth)
    ch5.io.muxIn := muxDrive(Seq(1, 2, 3, 5, 6), dataIn)
    ch5.io.sel := idxIn(5)
    io.dataOut(5) := ch5.io.muxOut.setName("ch5")

    val ch6 = Mux5ch2_6p4(g.width, g.BankIndexWidth)
    ch6.io.muxIn := muxDrive(Seq(1, 2, 4, 5, 6), dataIn)
    ch6.io.sel := idxIn(6)
    io.dataOut(6) := ch6.io.muxOut.setName("ch6")

    val ch7 = Mux3ch3_7p4(g.width, g.BankIndexWidth)
    ch7.io.muxIn := muxDrive(Seq(3, 6, 7), dataIn)
    ch7.io.sel := idxIn(7)
    io.dataOut(7) := ch7.io.muxOut.setName("ch7")

  }
  else if (g.paraNum == 8) {

    val ch0 = Mux3ch0_8p8(g.width)
    val ch8 = Mux3ch0_8p8(g.width)
    ch0.io.sel := idxIn(0)
    ch8.io.sel := idxIn(8)
    ch0.io.muxIn := muxDrive(Seq(0, 1, 8), dataIn)
    ch8.io.muxIn := muxDrive(Seq(0, 1, 8), dataIn)
    dataOut(0) := ch0.io.muxOut; dataOut(8) := ch8.io.muxOut

    val ch1 = Mux5ch1_9p8(g.width)
    val ch9 = Mux5ch1_9p8(g.width)
    ch1.io.sel := idxIn(1)
    ch9.io.sel := idxIn(9)
    ch1.io.muxIn := muxDrive(Seq(1, 2, 3, 9, 10), dataIn)
    ch9.io.muxIn := muxDrive(Seq(1, 2, 3, 9, 10), dataIn)
    dataOut(1) := ch1.io.muxOut; dataOut(9) := ch9.io.muxOut

    val ch2 = Mux7ch2_10p8(g.width)
    val ch10 = Mux7ch2_10p8(g.width)
    ch2.io.sel := idxIn(2)
    ch10.io.sel := idxIn(10)
    ch2.io.muxIn := muxDrive(Seq(1, 2, 4, 5, 9, 10, 12), dataIn)
    ch10.io.muxIn := muxDrive(Seq(1, 2, 4, 5, 9, 10, 12), dataIn)
    dataOut(2) := ch2.io.muxOut; dataOut(10) := ch10.io.muxOut

    val ch3 = Mux5ch3_11p8(g.width)
    val ch11 = Mux5ch3_11p8(g.width)
    ch3.io.sel := idxIn(3)
    ch11.io.sel := idxIn(11)
    ch3.io.muxIn := muxDrive(Seq(3, 6, 7, 11, 14), dataIn)
    ch11.io.muxIn := muxDrive(Seq(3, 6, 7, 11, 14), dataIn)
    dataOut(3) := ch3.io.muxOut; dataOut(11) := ch11.io.muxOut

    val ch4 = Mux5ch4_12p8(g.width)
    val ch12 = Mux5ch4_12p8(g.width)
    ch4.io.sel := idxIn(4)
    ch12.io.sel := idxIn(12)
    ch4.io.muxIn := muxDrive(Seq(1, 4, 8, 9, 12), dataIn)
    ch12.io.muxIn := muxDrive(Seq(1, 4, 8, 9, 12), dataIn)
    dataOut(4) := ch4.io.muxOut; dataOut(12) := ch12.io.muxOut

    val ch5 = Mux7ch5_13p8(g.width)
    val ch13 = Mux7ch5_13p8(g.width)
    ch5.io.sel := idxIn(5)
    ch13.io.sel := idxIn(13)
    ch5.io.muxIn := muxDrive(Seq(3, 5, 6, 10, 11, 13, 14), dataIn)
    ch13.io.muxIn := muxDrive(Seq(3, 5, 6, 10, 11, 13, 14), dataIn)
    dataOut(5) := ch5.io.muxOut; dataOut(13) := ch13.io.muxOut

    val ch6 = Mux5ch6_14p8(g.width)
    val ch14 = Mux5ch6_14p8(g.width)
    ch6.io.sel := idxIn(6)
    ch14.io.sel := idxIn(14)
    ch6.io.muxIn := muxDrive(Seq(5, 6, 12, 13, 14), dataIn)
    ch14.io.muxIn := muxDrive(Seq(5, 6, 12, 13, 14), dataIn)
    dataOut(6) := ch6.io.muxOut; dataOut(14) := ch14.io.muxOut

    val ch7 = Mux3ch7_15p8(g.width)
    val ch15 = Mux3ch7_15p8(g.width)
    ch7.io.sel := idxIn(7)
    ch15.io.sel := idxIn(15)
    ch7.io.muxIn := muxDrive(Seq(7, 14, 15), dataIn)
    ch15.io.muxIn := muxDrive(Seq(7, 14, 15), dataIn)
    dataOut(7) := ch7.io.muxOut; dataOut(15) := ch15.io.muxOut

  }
  else {
    val sfSeq = genSfSeq(g.paraNum)
    val tmpMux = Vec (Bits(g.width bits), g.BI)
    tmpMux.zip(sfSeq.zipWithIndex).map {case(t1,(mSeq,id)) =>
      t1 := applyMux(io.dataIn,mSeq,io.idxIn(id),id,prefix)
    }
    io.dataOut := (tmpMux)
  }
}
object memInMux {
  def apply(dataIn: Vec[Bits], idx: Vec[UInt], g: NttCfgParam,prefix:String): Vec[Bits] = {
    val dut = new memInMux(g,prefix)
    dut.io.dataIn := dataIn
    dut.io.idxIn := idx
    dut.io.dataOut
  }
}
object memInMuxGenV extends App {
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "NttOpt/rtl/DataPath",
    genLineComments = true
  ).generate(new memInMux(NttCfgParam(paraNum = 16)))
}

case class memInArb(g: NttCfgParam) extends Component {
  val io = new Bundle {
    val addrOri = in Vec (UInt(g.BankAddrWidth bits), g.radix)
    val idxOri = in Vec (UInt(g.BankIndexWidth bits), g.BI)
    val dataOri = in Vec (Bits(g.width bits), g.BI)

    val bankIdxTrans = out Vec (UInt(g.BankIndexWidth bits), g.BI)
    val shuffleIdxTrans = out Vec (UInt(g.BankIndexWidth bits), g.BI)
//    val addrMem = out Vec (UInt(g.BankAddrWidth bits), g.BI)
    val addrSel = out Vec (Bool(), g.BI)
    val addrOri_r1 = out Vec (UInt(g.BankAddrWidth bits), g.radix)
    val dataMem = out Vec (Bits(g.width bits), g.BI)
  }

  val sf = new shuffleOpt(g)
  sf.io.idxOri := io.idxOri; sf.io.addrOri := io.addrOri
  val shuffleIdx = sf.io.idxShuffle
  val bankIdx = sf.io.idxDec // Register 1 cyc

//  val bankIdx = idxDecode(addr = io.addrOri, idx = io.idxOri, g = g) // Register 1 cyc
//  val shuffleIdx = idxShuffle(dataIn = bankIdx)
//  io.dataMem := RegNext(memInMux(dataIn = RegNext(io.dataOri), idx = shuffleIdx, g = g)) // Register 1 cyc, total 2 cyc in -> out
  io.dataMem := (memInMux(dataIn = RegNext(io.dataOri), idx = shuffleIdx, g = g,prefix = "memIn_"))
  io.bankIdxTrans := RegNext(bankIdx) // 2cyc idxori -> bankidxtrans, assign with datamem
  io.shuffleIdxTrans := RegNext(shuffleIdx) // 2cyc idxori -> shuffleidxtrans, assign with datamem

  io.addrSel.zip(shuffleIdx).foreach { case (t1, t2) => t1 := t2.lsb } // 1 cyc earlier than dataMem
  io.addrOri_r1 := RegNext(io.addrOri) // 1 cyc earlier than dataMem

}
object memInArbGenV extends App {
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "NttOpt/rtl/DataPath",
    genLineComments = true
  ).generate(new memInArb(NttCfgParam()))
}

case class memWritebackArb(g: NttCfgParam) extends Component {
  val io = new Bundle {
    val isNtt = in Bool ()
    val dataWb = in Vec (Bits(g.width bits), g.BI) // form BFU
    val idxWb = in Vec (UInt(g.BankIndexWidth bits), g.BI) // from memInArb
    val addrWb = in Vec (UInt(g.BankAddrWidth bits), g.radix) // from memInArb

    val dataWbMem = out Vec (Bits(g.width bits), g.BI)
    val addrWbMem = out Vec (UInt(g.BankAddrWidth bits), g.radix) //  1 cyc earlier than data
    val addrWbSelMem = out Vec (Bool(), g.BI) //  1 cyc earlier than data
  }

  val idxDelaySt1 =
    Delay(io.idxWb, g.BfuNttDelay + g.Arbit.ramLatency + g.Arbit.DatDeMuxLatency + g.BfuRegisterIoDelay)
      .addAttribute("srl_style", "srl")
  val idxDelaySt2 = Delay(idxDelaySt1, g.BfuInttDelay - g.BfuNttDelay)
  val addrDelaySt1 =
    Delay(io.addrWb, g.BfuNttDelay + g.Arbit.ramLatency + g.Arbit.DatDeMuxLatency + g.BfuRegisterIoDelay)
      .addAttribute("srl_style", "srl")
  val addrDelaySt2 = Delay(addrDelaySt1, g.BfuInttDelay - g.BfuNttDelay)

  val idx = io.isNtt ? idxDelaySt1 | idxDelaySt2
  io.addrWbMem := io.isNtt ? addrDelaySt1 | addrDelaySt2

  io.dataWbMem := (memInMux(dataIn = (io.dataWb), idx = idx, g = g,prefix = "memWb_"))
//  io.dataWbMem := RegNext(memInMux(dataIn = (io.dataWb), idx = idx, g = g))
  io.addrWbSelMem.zip(idx).foreach { case (t1, t2) => t1 := t2.lsb }
}
object memWritebackArbGenV extends App {
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "NttOpt/rtl/DataPath",
    genLineComments = true
  ).generate(new memWritebackArb(NttCfgParam()))
}

case class memOutArb(g: NttCfgParam) extends Component {
  val io = new Bundle {
    val dataMem = in Vec (Bits(g.width bits), g.BI)
    val idx = in Vec (UInt(g.BankIndexWidth bits), g.BI)
    val dataOrder = out Vec (Bits(g.width bits), g.BI)
  }
  import io._
  import DataPathTools._

  if (g.paraNum == 4) {
    val ch0 = new dataMux2p4(g.width)
    ch0.io.muxIn := muxDrive(Seq(0, 4), dataMem)
    ch0.io.sel := idx(0).msb
    dataOrder(0) := ch0.io.muxOut

    val ch1 = new dataMux6ch1p4(g.width)
    ch1.io.muxIn := muxDrive(Seq(0, 1, 2, 4, 5, 6), dataMem)
    ch1.io.sel := idx(1).asBits
    dataOrder(1) := ch1.io.muxOut

    val ch2 = new dataMux4p4(g.width)
    ch2.io.muxIn := muxDrive(Seq(1, 2, 5, 6), dataMem)
    ch2.io.sel := idx(2)(g.BankIndexWidth - 1 downto 1)
    dataOrder(2) := ch2.io.muxOut

    val ch3 = new dataMux4p4(g.width)
    ch3.io.muxIn := muxDrive(Seq(1, 3, 5, 7), dataMem)
    ch3.io.sel := idx(3)(g.BankIndexWidth - 1 downto 1)
    dataOrder(3) := ch3.io.muxOut

    val ch4 = new dataMux4p4(g.width)
    ch4.io.muxIn := muxDrive(Seq(0, 2, 4, 6), dataMem)
    ch4.io.sel := idx(4)(g.BankIndexWidth - 1 downto 1)
    dataOrder(4) := ch4.io.muxOut

    val ch5 = new dataMux4p4(g.width)
    ch5.io.muxIn := muxDrive(Seq(1, 2, 5, 6), dataMem)
    ch5.io.sel := idx(5)(g.BankIndexWidth - 1 downto 1)
    dataOrder(5) := ch5.io.muxOut

    val ch6 = new dataMux6ch6p4(g.width)
    ch6.io.muxIn := muxDrive(Seq(1, 2, 3, 5, 6, 7), dataMem)
    ch6.io.sel := idx(6).asBits
    dataOrder(6) := ch6.io.muxOut

    val ch7 = new dataMux2p4(g.width)
    ch7.io.muxIn := muxDrive(Seq(3, 7), dataMem)
    ch7.io.sel := idx(7).msb
    dataOrder(7) := ch7.io.muxOut
  }
  else if (g.paraNum == 8) {

    val ch0 = new dataMux2ch0_15p8(g.width)
    ch0.io.sel := idx(0)
    ch0.io.muxIn := muxDrive(Seq(0, 8), dataMem)
    io.dataOrder(0) := ch0.io.muxOut

    val ch1 = new dataMux8ch1p8(g.width)
    ch1.io.sel := idx(1)
    ch1.io.muxIn := muxDrive(Seq(0, 1, 2, 4, 8, 9, 10, 12), io.dataMem)
    io.dataOrder(1) := ch1.io.muxOut

    val ch2 = new dataMux4ch2_13p8(g.width)
    ch2.io.sel := idx(2)
    ch2.io.muxIn := muxDrive(Seq(1, 2, 9, 10), dataMem)
    io.dataOrder(2) := ch2.io.muxOut

    val ch3 = new dataMux6ch3_9_10p8(g.width)
    ch3.io.sel := idx(3)
    ch3.io.muxIn := muxDrive(Seq(1, 3, 5, 9, 11, 13), dataMem)
    io.dataOrder(3) := ch3.io.muxOut

    val ch4 = new dataMux4ch4_7_8_11p8(g.width)
    ch4.io.sel := idx(4)
    ch4.io.muxIn := muxDrive(Seq(2, 4, 10, 12), dataMem)
    io.dataOrder(4) := ch4.io.muxOut

    val ch5 = new dataMux6ch5_6_12p8(g.width)
    ch5.io.sel := idx(5)
    ch5.io.muxIn := muxDrive(Seq(2, 5, 6, 10, 13, 14), dataMem)
    io.dataOrder(5) := ch5.io.muxOut

    val ch6 = new dataMux6ch5_6_12p8(g.width)
    ch6.io.sel := idx(6)
    ch6.io.muxIn := muxDrive(Seq(3, 5, 6, 11, 13, 14), dataMem)
    io.dataOrder(6) := ch6.io.muxOut

    val ch7 = new dataMux4ch4_7_8_11p8(g.width)
    ch7.io.sel := idx(7)
    ch7.io.muxIn := muxDrive(Seq(3, 7, 11, 15), dataMem)
    io.dataOrder(7) := ch7.io.muxOut

    val ch8 = new dataMux4ch4_7_8_11p8(g.width)
    ch8.io.sel := idx(8)
    ch8.io.muxIn := muxDrive(Seq(0, 4, 8, 12), dataMem)
    io.dataOrder(8) := ch8.io.muxOut

    val ch9 = new dataMux6ch3_9_10p8(g.width)
    ch9.io.sel := idx(9)
    ch9.io.muxIn := muxDrive(Seq(1, 2, 4, 9, 10, 12), dataMem)
    io.dataOrder(9) := ch9.io.muxOut

    val ch10 = new dataMux6ch3_9_10p8(g.width)
    ch10.io.sel := idx(10)
    ch10.io.muxIn := muxDrive(Seq(1, 2, 5, 9, 10, 13), dataMem)
    io.dataOrder(10) := ch10.io.muxOut

    val ch11 = new dataMux4ch4_7_8_11p8(g.width)
    ch11.io.sel := idx(11)
    ch11.io.muxIn := muxDrive(Seq(3, 5, 11, 13), dataMem)
    io.dataOrder(11) := ch11.io.muxOut

    val ch12 = new dataMux6ch5_6_12p8(g.width)
    ch12.io.sel := idx(12)
    ch12.io.muxIn := muxDrive(Seq(2, 4, 6, 10, 12, 14), dataMem)
    io.dataOrder(12) := ch12.io.muxOut

    val ch13 = new dataMux4ch2_13p8(g.width)
    ch13.io.sel := idx(13)
    ch13.io.muxIn := muxDrive(Seq(5, 6, 13, 14), dataMem)
    io.dataOrder(13) := ch13.io.muxOut

    val ch14 = new dataMux8ch14p8(g.width)
    ch14.io.sel := idx(14)
    ch14.io.muxIn := muxDrive(Seq(3, 5, 6, 7, 11, 13, 14, 15), dataMem)
    io.dataOrder(14) := ch14.io.muxOut

    val ch15 = new dataMux2ch0_15p8(g.width)
    ch15.io.sel := idx(15)
    ch15.io.muxIn := muxDrive(Seq(7, 15), dataMem)
    io.dataOrder(15) := ch15.io.muxOut

  }
  else {
    val dmuxSeq = genMuxSeq(g.paraNum)
    val tmpSeq = cloneOf(io.dataOrder)
    tmpSeq.zip(dmuxSeq.zipWithIndex).map{case(t1,(mseq,id)) =>
      t1 := applyMux(io.dataMem,mseq,io.idx(id),id,"data")
    }
    io.dataOrder := tmpSeq
  }
}
object memOutArb {
  def apply(dataIn: Vec[Bits], idx: Vec[UInt], g: NttCfgParam): Vec[Bits] = {
    val dut = new memOutArb(g)
    dut.io.dataMem := dataIn
    dut.io.idx := idx
    dut.io.dataOrder
  }
}

object memOutArbGenV extends App {
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "NttOpt/rtl/DataPath",
    genLineComments = true
  ).generate(new memOutArb(NttCfgParam(paraNum = 16)))
}

case class memForwardCtrl(g: NttCfgParam) extends Component {
  val io = new Bundle {
    val ctrl = in(CtrlBus())
    val outsideAddrOri = slave Flow Vec(UInt(g.BankAddrWidth bits), g.radix)
    val outsideIdxOri = slave Flow Vec(UInt(g.BankIndexWidth bits), g.BI)
    val outsideWrDataArray = slave Flow Vec(Bits(g.width bits), g.BI)

    val bfuRdAddrOri = slave Flow Vec(UInt(g.BankAddrWidth bits), g.radix)
    val bfuRdIdxOri = slave Flow Vec(UInt(g.BankIndexWidth bits), g.BI)
    val NttWriteBack = slave Flow (Vec(Bits(g.width bits), g.BI))

    val MemIfRe = out Bool ()
    val MemIfRdAddr = out Vec (UInt(g.BankAddrWidth bits), g.BI)
    val bankIdxTrans = out Vec (UInt(g.BankIndexWidth bits), g.BI)
    val MemIfWe = out Bool ()
    val MemIfWrAddr = out Vec (UInt(g.BankAddrWidth bits), g.BI)
    val MemIfWrData = out Vec (Bits(g.width bits), g.BI)
  }

  val memInArb = new memInArb(g)
  val memWbArb = new memWritebackArb(g)

  memInArb.io.addrOri := io.ctrl.isCal ? io.bfuRdAddrOri.payload | io.outsideAddrOri.payload
  memInArb.io.idxOri := io.ctrl.isCal ? io.bfuRdIdxOri.payload | io.outsideIdxOri.payload
  memInArb.io.dataOri := io.outsideWrDataArray.payload

  io.bankIdxTrans := memInArb.io.bankIdxTrans

  memWbArb.io.isNtt := io.ctrl.isNtt
  memWbArb.io.dataWb := io.NttWriteBack.payload
  memWbArb.io.addrWb := RegNext(memInArb.io.addrOri_r1)
  memWbArb.io.idxWb := memInArb.io.shuffleIdxTrans

  for (i <- 0 until g.BI) {
    io.MemIfWrAddr(i) := RegNext(
      io.ctrl.isCal ? (memWbArb.io
        .addrWbSelMem(i) ? (memWbArb.io.addrWbMem(1)) | (memWbArb.io.addrWbMem(0))) |
        (memInArb.io.addrSel(i) ? (memInArb.io.addrOri_r1(1)) | (memInArb.io.addrOri_r1(0)))
    )
    io.MemIfRdAddr(i) := RegNext(
      memInArb.io.addrSel(i) ? (memInArb.io.addrOri_r1(1)) | (memInArb.io.addrOri_r1(0))
    )
  }

  io.MemIfRe := io.ctrl.isCal ? { Delay(io.bfuRdAddrOri.valid, g.Arbit.DecodeLatency) } | {
    io.ctrl.isOutSideRead ? Delay(io.outsideAddrOri.valid, g.Arbit.DecodeLatency) | False
  }
  io.MemIfWe := io.ctrl.isCal ? Delay(io.NttWriteBack.valid, g.Arbit.DecodeMuxRegLatency) | {
    io.ctrl.isOutSideWrite ? Delay(io.outsideAddrOri.valid, g.Arbit.DecodeLatency) | False
  }

  io.MemIfWrData := RegNext(io.ctrl.isCal ? memWbArb.io.dataWbMem | memInArb.io.dataMem)
}
object memForwardCtrlGenV extends App {
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "NttOpt/rtl/DataPath",
    genLineComments = true
  ).generate(new memForwardCtrl(NttCfgParam(paraNum = 16)))
}

case class memBackwardCtrl(g: NttCfgParam) extends Component {
  val io = new Bundle {
    val ctrl = in(CtrlBus())
    val memIfRdData = slave Flow Vec(Bits(g.width bits), g.BI)
    val idx = in Vec (UInt(g.BankIndexWidth bits), g.BI)
    val outsideRdDataArray = master Flow Vec(Bits(g.width bits), g.BI)
    val nttPayload = master Flow Vec(Bits(g.width bits), g.BI)
  }
  val dataMux = memOutArb(dataIn = io.memIfRdData.payload, idx = io.idx, g = g)
  io.nttPayload.payload := RegNext(dataMux)
  io.nttPayload.valid := RegNext(io.memIfRdData.valid && io.ctrl.isCal)
  io.outsideRdDataArray.payload := RegNext(dataMux)
  io.outsideRdDataArray.valid := RegNext(io.memIfRdData.valid && io.ctrl.isOutSideRead)
}
object memBackwardCtrlGenV extends App {
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "NttOpt/rtl/DataPath",
    genLineComments = true
  ).generate(new memBackwardCtrl(NttCfgParam(paraNum = 16)))
}

case class DataPathTop(g: NttCfgParam) extends Component {
  val io = new Bundle {
    val ctrl = in(CtrlBus())

    val outsideAddrOri = slave Flow Vec(UInt(g.BankAddrWidth bits), g.radix)
    val outsideIdxOri = slave Flow Vec(UInt(g.BankIndexWidth bits), g.BI)
    val outsideRdDataArray = master Flow Vec(Bits(g.width bits), g.BI)
    val outsideWrDataArray = slave Flow Vec(Bits(g.width bits), g.BI)

    val bfuRdAddrOri = slave Flow Vec(UInt(g.BankAddrWidth bits), g.radix)
    val bfuRdIdxOri = slave Flow Vec(UInt(g.BankIndexWidth bits), g.BI)
    val NttWriteBack = Array.fill(g.paraNum)(slave Flow DataPayload(g))
    val NttPayload = Array.fill(g.paraNum)(master Flow BfuPayload(g))
    val twBus = slave Flow twPayload(addrWidth = g.twAddrWidth, muxWidth = log2Up(g.paraNum), para = g.paraNum)
  }

  val mem = new memBank(g)
  val mem_rd_IF = Array.fill(g.BI)(new myRamReadOnly(config = myRamConfig(g.width, g.BankAddrWidth)))
  val mem_wr_IF = Array.fill(g.BI)(new myRamWriteOnly(config = myRamConfig(g.width, g.BankAddrWidth)))

  val tw = new Area {
    val rom = new twRom(g)
    rom.io.twBus := Delay(io.twBus, (g.Arbit.DecodeLatency + g.Arbit.DatDeMuxLatency - g.Arbit.romMuxLatency))
      .addAttribute("srl_style", "srl")
  }

  val fc = memForwardCtrl(g)
  val bc = memBackwardCtrl(g)
  fc.io.ctrl := io.ctrl
  fc.io.outsideAddrOri := io.outsideAddrOri
  fc.io.outsideIdxOri := io.outsideIdxOri
  fc.io.outsideWrDataArray := io.outsideWrDataArray
  fc.io.bfuRdAddrOri := io.bfuRdAddrOri
  fc.io.bfuRdIdxOri := io.bfuRdIdxOri
  val NttWbConvert = Vec(io.NttWriteBack.flatMap { item => Seq(item.payload.A.asBits, item.payload.B.asBits) }.toSeq)
  fc.io.NttWriteBack.payload := NttWbConvert
  fc.io.NttWriteBack.valid := io.NttWriteBack(0).valid

  mem_rd_IF.toSeq.zip(fc.io.MemIfRdAddr).foreach { case (t1, t2) => { t1.rAddr := t2; t1.re := fc.io.MemIfRe } }
  bc.io.memIfRdData.payload.zip(mem_rd_IF.toSeq).foreach { case (t1, t2) => t1 := t2.rData }
  mem_wr_IF.toSeq.zip(fc.io.MemIfWrAddr.zip(fc.io.MemIfWrData)).foreach { case (interface, (addr, data)) =>
    interface.wData := data; interface.wAddr := addr; interface.we := fc.io.MemIfWe
  }

  bc.io.memIfRdData.valid := Delay(fc.io.MemIfRe, g.Arbit.ramLatency)
  val idxTrans = Delay(fc.io.bankIdxTrans, g.Arbit.ramLatency)
  bc.io.ctrl := io.ctrl
  bc.io.idx := idxTrans
  io.outsideRdDataArray := bc.io.outsideRdDataArray
  io.NttPayload.toSeq.zip(bc.io.nttPayload.payload.grouped(2).toSeq).foreach { case (t1, t2) =>
    t1.A := t2(0).asUInt; t1.B := t2(1).asUInt
    t1.valid := bc.io.nttPayload.valid
  }
  io.NttPayload.toSeq.zip(tw.rom.io.twData).foreach { case (t1, t2) => t1.payload.Tw := t2 }
  mem.io.memIf.zip(mem_rd_IF.toSeq).foreach { case (t1, t2) => t1 << t2 }
  mem.io.memIf.zip(mem_wr_IF.toSeq).foreach { case (t1, t2) => t1 << t2 }
}
