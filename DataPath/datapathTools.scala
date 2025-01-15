package Ntt.DataPath

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import Ntt.NttCfg._
import myRam._
import myTools._

object DataPathTools {
  def muxDrive(num: Seq[Int], dataIn: Vec[Bits]): Vec[Bits] = {
    val size = num.size
    val ret = Vec(Bits(dataIn.head.getWidth bits), size)
    for (i <- 0 until num.size) {
      ret(i) := dataIn(num(i))
    }
    ret
  }
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
