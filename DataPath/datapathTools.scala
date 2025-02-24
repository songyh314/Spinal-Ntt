package Ntt.DataPath

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import Ntt.NttCfg._
import myRam._
import myTools._
import spinal.sim.DataType

object DataPathTools {
  def muxDrive[T <: Data](num: Seq[Int], dataIn: Vec[T]): Vec[T] = {
    val size = num.size
    val ret = Vec(cloneOf(dataIn.head), size)
    for (i <- 0 until num.size) {
      ret(i) := dataIn(num(i))
    }
    ret
  }
  def genSeq(num: Int): Seq[String] = {
    val len = log2Up(num)
    val ret = (0 until (num)).map { item =>
      val tmp = item.toBinaryString
      if (tmp.length < len) { "0" * (len - tmp.length) + tmp }
      else tmp
    }
    ret
  }
  def flipMsb(s: String): String = {
    if (s.isEmpty) { s }
    else {
      val firstChar = s.head
      val flippedChar = if (firstChar == '0') { '1' }
      else { '0' }
      val ret = flippedChar + s.tail
      ret
    }
  }
  def insertString(s: String, insert: String = "0"): Seq[String] = {
    val ret = (0 to s.length).map { i => s.substring(0, i) + insert + s.substring(i) }.toSeq
//    println(s"$s insert $insert : $ret")
    ret
  }
  def bin2Int(s: String): Int = {
    Integer.parseInt(s, 2)
  }
  def insertAndFlip(s: String): Seq[Seq[Int]] = {
    val insertedZero = insertString(s, "0")
    val insertedOne = insertString(s, "1")
    val flippedZero: Seq[String] = insertedZero ++ (insertedZero map flipMsb)
    val flippedOne: Seq[String] = insertedOne ++ (insertedOne map flipMsb)
    val ret: Seq[Seq[Int]] = Seq((flippedZero.distinct map bin2Int).sorted, (flippedOne.distinct map bin2Int).sorted)
    ret
  }
  def findIndex(A: Seq[Seq[Int]]): Seq[Seq[Int]] = {
    val outerSize = A.size
    val ret = Array.fill(outerSize)(Seq.empty[Int])
    A.zipWithIndex.foreach { case (innerSeq, j) =>
      innerSeq.foreach { i =>
        ret(i) = ret(i) :+ j
      }
    }
    ret.toSeq
  }
  def genMuxSeq(num: Int): Seq[Seq[Int]] = {
    val strSeq = genSeq(num)
    val ret = (strSeq map insertAndFlip).flatten
    ret
  }
  def genSfSeq(num: Int): Seq[Seq[Int]] = {
    val strSeq = genSeq(num)
    val mux = (strSeq map insertAndFlip).flatten
    val ret = findIndex(mux)
    ret
  }

  case class patternMux(width: Int, selWidth: Int = 3, muxSeq: Seq[Int]) extends Component {
    val muxSize = muxSeq.size
    val patternWidth = log2Up(muxSize)
    val tmpSeq = muxSeq.map { item => U(item, selWidth bits) }
    val io = new Bundle {
      val muxIn = in Vec (Bits(width bits), muxSize)
      val sel = in UInt (selWidth bits)
      val muxOut = out Bits (width bits)
    }
    noIoPrefix()
    val idx = UInt(patternWidth bits)
    switch(io.sel) {
      for (i <- 0 until muxSize) {
        is(tmpSeq(i)) {
          io.muxOut := io.muxIn(i)
        }
      }
      default { io.muxOut := io.muxIn(0) }
    }
  }

  case class patternSf(sw: Int, expect: Int, id: Seq[Int]) extends Component {
    val io = new Bundle {
      val muxIn = in Vec (UInt(sw bits), id.size)
      val muxOut = out UInt (sw bits)
    }
    val const = id.map(U(_, sw bits))
    val flag = io.muxIn.map { item => item === expect }
    io.muxOut := MuxOH(flag, const)
  }

  def applySf(muxOri: Vec[UInt], muxSeq: Seq[Int], expect: Int): UInt = {
    val dut =
      new patternSf(muxOri.head.getWidth, expect, muxSeq).setDefinitionName(s"sf_p${(muxOri.size) / 2}_ch${expect}")
    val chIn = muxDrive(muxSeq, muxOri)
    dut.io.muxIn := chIn
    dut.io.muxOut
  }
  def applyMux(muxOri: Vec[Bits], muxSeq: Seq[Int], idx: UInt, id: Int, prefix: String = ""): Bits = {
    val dut =
      new patternMux(muxOri.head.getWidth, idx.getWidth, muxSeq).setDefinitionName(s"${prefix}mux_p${(muxOri.size) / 2}_ch$id")
    val chIn = muxDrive(muxSeq, muxOri)
    dut.io.muxIn := chIn
    dut.io.sel := idx
    dut.io.muxOut
  }

}

object Main extends App {
  import DataPathTools._
  val constSeq = genSeq(8)
  val ret1 = (constSeq map insertAndFlip).flatten
  val ret2 = findIndex(ret1)
  println(ret1.mkString("\n"))
  println(ret2.mkString("\n"))
}

object patternMuxGenv extends App {
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "NttOpt/rtl/DataPath",
    genLineComments = true
  ).generate(new DataPathTools.patternMux(width = 24, selWidth = 4, muxSeq = Seq(2, 4, 10, 12)))
}

// mux with 3 input used for ch0/ch4 while para is 4
case class Mux3ch0_4p4(width: Int, selWidth: Int = 3) extends Component {
  require(selWidth == 3)
  val io = new Bundle {
    val muxIn = in Vec (Bits(width bits), 3)
    val sel = in UInt (selWidth bits)
    val muxOut = out Bits (width bits)
  }
  import io._
  switch(io.sel.asBits) {
    is(B"000") { io.muxOut := io.muxIn(0) }
    is(B"001") { io.muxOut := io.muxIn(1) }
    is(B"100") { io.muxOut := io.muxIn(2) }
    default { muxOut := B(0) }
  }
}
case class Mux5ch1_5p4(width: Int, selWidth: Int = 3) extends Component {
  require(selWidth == 3)
  val io = new Bundle {
    val muxIn = in Vec (Bits(width bits), 5)
    val sel = in UInt (selWidth bits)
    val muxOut = out Bits (width bits)
  }
  import io._
  switch(sel.asBits) {
    is(B"001") { muxOut := muxIn(0) }
    is(B"010") { muxOut := muxIn(1) }
    is(B"011") { muxOut := muxIn(2) }
    is(B"101") { muxOut := muxIn(3) }
    is(B"110") { muxOut := muxIn(4) }
    default { muxOut := B(0) }
  }
}

case class Mux5ch2_6p4(width: Int, selWidth: Int = 3) extends Component {
  require(selWidth == 3)
  val io = new Bundle {
    val muxIn = in Vec (Bits(width bits), 5)
    val sel = in UInt (selWidth bits)
    val muxOut = out Bits (width bits)
  }
  import io._
  switch(sel.asBits) {
    is(B"001") { muxOut := muxIn(0) }
    is(B"010") { muxOut := muxIn(1) }
    is(B"100") { muxOut := muxIn(2) }
    is(B"101") { muxOut := muxIn(3) }
    is(B"110") { muxOut := muxIn(4) }
    default { muxOut := B(0) }
  }
}

case class Mux3ch3_7p4(width: Int, selWidth: Int = 3) extends Component {
  require(selWidth == 3)
  val io = new Bundle {
    val muxIn = in Vec (Bits(width bits), 3)
    val sel = in UInt (selWidth bits)
    val muxOut = out Bits (width bits)
  }
  import io._
  switch(io.sel.asBits) {
    is(B"011") { io.muxOut := io.muxIn(0) }
    is(B"110") { io.muxOut := io.muxIn(1) }
    is(B"111") { io.muxOut := io.muxIn(2) }
    default { muxOut := B(0) }
  }
}

case class Mux3ch0_8p8(width: Int, selWidth: Int = 4) extends Component {
  require(selWidth == 4)
  val io = new Bundle {
    val muxIn = in Vec (Bits(width bits), 3)
    val sel = in UInt (selWidth bits)
    val muxOut = out Bits (width bits)
  }
  import io._
  switch(io.sel.asBits) {
    is(B"0000") { muxOut := muxIn(0) }
    is(B"0001") { muxOut := muxIn(1) }
    is(B"1000") { muxOut := muxIn(2) }
    default { muxOut := 0 }
  }
}

case class Mux5ch1_9p8(width: Int, selWidth: Int = 4) extends Component {
  require(selWidth == 4)
  val io = new Bundle {
    val muxIn = in Vec (Bits(width bits), 5)
    val sel = in UInt (selWidth bits)
    val muxOut = out Bits (width bits)
  }
  import io._
  switch(io.sel.asBits) {
    is(B"0001") { muxOut := muxIn(0) }
    is(B"0010") { muxOut := muxIn(1) }
    is(B"0011") { muxOut := muxIn(2) }
    is(B"1001") { muxOut := muxIn(3) }
    is(B"1010") { muxOut := muxIn(4) }
    default { muxOut := 0 }
  }
}

case class Mux7ch2_10p8(width: Int, selWidth: Int = 4) extends Component {
  require(selWidth == 4)
  val io = new Bundle {
    val muxIn = in Vec (Bits(width bits), 7)
    val sel = in UInt (selWidth bits)
    val muxOut = out Bits (width bits)
  }
  import io._
  switch(io.sel) {
    is(1) { muxOut := muxIn(0) }
    is(2) { muxOut := muxIn(1) }
    is(4) { muxOut := muxIn(2) }
    is(5) { muxOut := muxIn(3) }
    is(9) { muxOut := muxIn(4) }
    is(10) { muxOut := muxIn(5) }
    is(12) { muxOut := muxIn(6) }
    default { muxOut := 0 }
  }
}

case class Mux5ch3_11p8(width: Int, selWidth: Int = 4) extends Component {
  require(selWidth == 4)
  val io = new Bundle {
    val muxIn = in Vec (Bits(width bits), 5)
    val sel = in UInt (selWidth bits)
    val muxOut = out Bits (width bits)
  }
  import io._
  switch(io.sel) {
    is(3) { muxOut := muxIn(0) }
    is(6) { muxOut := muxIn(1) }
    is(7) { muxOut := muxIn(2) }
    is(11) { muxOut := muxIn(3) }
    is(14) { muxOut := muxIn(4) }
    default { muxOut := 0 }
  }
}

case class Mux5ch4_12p8(width: Int, selWidth: Int = 4) extends Component {
  require(selWidth == 4)
  val io = new Bundle {
    val muxIn = in Vec (Bits(width bits), 5)
    val sel = in UInt (selWidth bits)
    val muxOut = out Bits (width bits)
  }
  import io._
  switch(io.sel) {
    is(1) { muxOut := muxIn(0) }
    is(4) { muxOut := muxIn(1) }
    is(8) { muxOut := muxIn(2) }
    is(9) { muxOut := muxIn(3) }
    is(12) { muxOut := muxIn(4) }
    default { muxOut := 0 }
  }
}

case class Mux7ch5_13p8(width: Int, selWidth: Int = 4) extends Component {
  require(selWidth == 4)
  val io = new Bundle {
    val muxIn = in Vec (Bits(width bits), 7)
    val sel = in UInt (selWidth bits)
    val muxOut = out Bits (width bits)
  }
  import io._
  switch(io.sel) {
    is(3) { muxOut := muxIn(0) }
    is(5) { muxOut := muxIn(1) }
    is(6) { muxOut := muxIn(2) }
    is(10) { muxOut := muxIn(3) }
    is(11) { muxOut := muxIn(4) }
    is(13) { muxOut := muxIn(5) }
    is(14) { muxOut := muxIn(6) }
    default { muxOut := 0 }
  }
}

case class Mux5ch6_14p8(width: Int, selWidth: Int = 4) extends Component {
  require(selWidth == 4)
  val io = new Bundle {
    val muxIn = in Vec (Bits(width bits), 5)
    val sel = in UInt (selWidth bits)
    val muxOut = out Bits (width bits)
  }
  import io._
  switch(io.sel) {
    is(5) { muxOut := muxIn(0) }
    is(6) { muxOut := muxIn(1) }
    is(12) { muxOut := muxIn(2) }
    is(13) { muxOut := muxIn(3) }
    is(14) { muxOut := muxIn(4) }
    default { muxOut := 0 }
  }
}

case class Mux3ch7_15p8(width: Int, selWidth: Int = 4) extends Component {
  require(selWidth == 4)
  val io = new Bundle {
    val muxIn = in Vec (Bits(width bits), 3)
    val sel = in UInt (selWidth bits)
    val muxOut = out Bits (width bits)
  }
  import io._
  switch(io.sel) {
    is(7) { muxOut := muxIn(0) }
    is(14) { muxOut := muxIn(1) }
    is(15) { muxOut := muxIn(2) }
    default { muxOut := 0 }
  }
}

case class dataMux2p4(width: Int) extends Component {
  val io = new Bundle {
    val muxIn = in Vec (Bits(width bits), 2)
    val sel = in Bool ()
    val muxOut = out Bits (width bits)
  }
  import io._
  muxOut := sel ? muxIn(1) | muxIn(0)
}

case class dataMux4p4(width: Int) extends Component {
  val io = new Bundle {
    val muxIn = in Vec (Bits(width bits), 4)
    val sel = in UInt (2 bits)
    val muxOut = out Bits (width bits)
  }
  import io._
  muxOut := muxIn.read(sel)
}

case class dataMux6ch1p4(width: Int) extends Component {
  val io = new Bundle {
    val muxIn = in Vec (Bits(width bits), 6)
    val sel = in Bits (3 bits)
    val muxOut = out Bits (width bits)
  }
  import io._
  switch(sel) {
    is(B"000") { muxOut := muxIn(0) }
    is(B"001") { muxOut := muxIn(1) }
    is(B"010") { muxOut := muxIn(2) }
    is(B"100") { muxOut := muxIn(3) }
    is(B"101") { muxOut := muxIn(4) }
    is(B"110") { muxOut := muxIn(5) }
    default { muxOut := B(0) }
  }
}

case class dataMux6ch6p4(width: Int) extends Component {
  val io = new Bundle {
    val muxIn = in Vec (Bits(width bits), 6)
    val sel = in Bits (3 bits)
    val muxOut = out Bits (width bits)
  }
  import io._
  switch(sel) {
    is(B"001") { muxOut := muxIn(0) }
    is(B"010") { muxOut := muxIn(1) }
    is(B"011") { muxOut := muxIn(2) }
    is(B"101") { muxOut := muxIn(3) }
    is(B"110") { muxOut := muxIn(4) }
    is(B"111") { muxOut := muxIn(5) }
    default { muxOut := B(0) }
  }
}

case class dataMux2ch0_15p8(width: Int) extends Component {
  val io = new Bundle {
    val muxIn = in Vec (Bits(width bits), 2)
    val sel = in UInt (4 bits)
    val muxOut = out Bits (width bits)
  }
  import io._
  muxOut := sel.msb ? muxIn(1) | muxIn(0)
}

case class dataMux8ch1p8(width: Int) extends Component {
  val io = new Bundle {
    val muxIn = in Vec (Bits(width bits), 8)
    val sel = in UInt (4 bits)
    val muxOut = out Bits (width bits)
  }
  import io._
  val muxGroup = muxIn.splitAt(muxIn.size / 2)
  val muxSelLsb = Bits(width bits)
  val muxSelMsb = Bits(width bits)
  switch(sel(2 downto 0).asBits) {
    is(B"000") { muxSelLsb := muxGroup._1(0); muxSelMsb := muxGroup._2(0) }
    is(B"001") { muxSelLsb := muxGroup._1(1); muxSelMsb := muxGroup._2(1) }
    is(B"010") { muxSelLsb := muxGroup._1(2); muxSelMsb := muxGroup._2(2) }
    is(B"100") { muxSelLsb := muxGroup._1(3); muxSelMsb := muxGroup._2(3) }
    default { muxSelLsb := 0; muxSelMsb := 0 }
  }
  muxOut := sel.msb ? muxSelMsb | muxSelLsb
}
object dataMuxGenv extends App {
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "NttOpt/rtl/DataPath",
    genLineComments = true
  ).generate(new dataMux8ch1p8(24))
}

case class dataMux8ch14p8(width: Int) extends Component {
  val io = new Bundle {
    val muxIn = in Vec (Bits(width bits), 8)
    val sel = in UInt (4 bits)
    val muxOut = out Bits (width bits)
  }
  import io._
  val muxGroup = muxIn.splitAt(muxIn.size / 2)
  val muxSelLsb = Bits(width bits)
  val muxSelMsb = Bits(width bits)
  switch(sel(2 downto 0).asBits) {
    is(B"011") { muxSelLsb := muxGroup._1(0); muxSelMsb := muxGroup._2(0) }
    is(B"101") { muxSelLsb := muxGroup._1(1); muxSelMsb := muxGroup._2(1) }
    is(B"110") { muxSelLsb := muxGroup._1(2); muxSelMsb := muxGroup._2(2) }
    is(B"111") { muxSelLsb := muxGroup._1(3); muxSelMsb := muxGroup._2(3) }
    default { muxSelLsb := 0; muxSelMsb := 0 }
  }
  muxOut := sel.msb ? muxSelMsb | muxSelLsb
}

case class dataMux4ch2_13p8(width: Int) extends Component {
  val io = new Bundle {
    val muxIn = in Vec (Bits(width bits), 4)
    val sel = in UInt (4 bits)
    val muxOut = out Bits (width bits)
  }
  import io._
  switch(Cat(sel.msb, sel(1))) {
    is(B"00") { muxOut := muxIn(0) }
    is(B"01") { muxOut := muxIn(1) }
    is(B"10") { muxOut := muxIn(2) }
    is(B"11") { muxOut := muxIn(3) }
  }
}
object dataMux4ch2_13p8Genv extends App {
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "NttOpt/rtl/DataPath",
    genLineComments = true
  ).generate(new dataMux4ch2_13p8(24))
}

case class dataMux4ch4_7_8_11p8(width: Int) extends Component {
  val io = new Bundle {
    val muxIn = in Vec (Bits(width bits), 4)
    val sel = in UInt (4 bits)
    val muxOut = out Bits (width bits)
  }
  import io._
  switch(sel(3 downto 2).asBits) {
    is(B"00") { muxOut := muxIn(0) }
    is(B"01") { muxOut := muxIn(1) }
    is(B"10") { muxOut := muxIn(2) }
    is(B"11") { muxOut := muxIn(3) }
  }
}

case class dataMux6ch3_9_10p8(width: Int) extends Component {
  val io = new Bundle {
    val muxIn = in Vec (Bits(width bits), 6)
    val sel = in UInt (4 bits)
    val muxOut = out Bits (width bits)
  }
  import io._
  val muxGroup = muxIn.splitAt(muxIn.size / 2)
  val muxSelLsb = Bits(width bits)
  val muxSelMsb = Bits(width bits)
  switch(sel(2 downto 1).asBits) {
    is(B"00") { muxSelLsb := muxGroup._1(0); muxSelMsb := muxGroup._2(0) }
    is(B"01") { muxSelLsb := muxGroup._1(1); muxSelMsb := muxGroup._2(1) }
    is(B"10") { muxSelLsb := muxGroup._1(2); muxSelMsb := muxGroup._2(2) }
    default { muxSelLsb := 0; muxSelMsb := 0 }
  }
  muxOut := sel.msb ? muxSelMsb | muxSelLsb
}

case class dataMux6ch5_6_12p8(width: Int) extends Component {
  val io = new Bundle {
    val muxIn = in Vec (Bits(width bits), 6)
    val sel = in UInt (4 bits)
    val muxOut = out Bits (width bits)
  }
  import io._
  val muxGroup = muxIn.splitAt(muxIn.size / 2)
  val muxSelLsb = Bits(width bits)
  val muxSelMsb = Bits(width bits)
  switch(sel(2 downto 1).asBits) {
    is(B"01") { muxSelLsb := muxGroup._1(0); muxSelMsb := muxGroup._2(0) }
    is(B"10") { muxSelLsb := muxGroup._1(1); muxSelMsb := muxGroup._2(1) }
    is(B"11") { muxSelLsb := muxGroup._1(2); muxSelMsb := muxGroup._2(2) }
    default { muxSelLsb := 0; muxSelMsb := 0 }
  }
  muxOut := sel.msb ? muxSelMsb | muxSelLsb
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
