package Ntt.BFU

import Ntt.NttCfg.{NttCfgParam, PrimeCfg}
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.eda.bench.Rtl
import spinal.lib.eda.xilinx.VivadoFlow

import scala.collection.mutable
import scala.util.Random

case class FastMod2414(g: NttCfgParam) extends Component {
  val prime = g.Prime
  val io = new Bundle {
    val dataIn = slave Flow (UInt(2 * g.width bits))
    val dataOut = master Flow (UInt(g.width bits))
  }

  val aPart = Vec(for (i <- 0 until (g.pGroup)) yield {
    if ((i + 1) * g.delta > g.width) {
      io.dataIn.payload(2 * g.width - 1 downto i * g.delta + g.width)
    } else io.dataIn.payload(g.width + (i + 1) * g.delta - 1 downto i * g.delta + g.width)
  })
  val bPart = Vec(for (i <- 0 until (g.pGroup)) yield {
    io.dataIn.payload(2 * g.width - 1 downto i * g.delta + g.width)
  })
  val A = RegNextWhen(aPart.reduce(_ +^ _), io.dataIn.valid) init U(0)
  val B_1 = RegNextWhen(bPart(2), io.dataIn.valid) init U(0)
  val B_2 = RegNextWhen(bPart(1) +^ bPart(0), io.dataIn.valid) init U(0)
  val B = RegNext(B_1 +^ B_2)
  val C = RegNextWhen(io.dataIn.payload(g.width - 1 downto 0), io.dataIn.valid)
  val Aslice = A._data.resize(2 * g.delta).subdivideIn(2 slices)
  val D = Aslice.reduce(_ +^ _)
  val Dslice = D.resize(2 * g.delta).subdivideIn(2 slices)
  val E = Dslice.reduce(_ + _)
  val Fsub = Dslice(1) + Aslice(1)
  val F = (E << g.P.N) - Fsub
  val Freg = RegNext(F)
  val Cdelay = RegNext(C)
  val Res1_tmp1 = Freg +^ Cdelay - g.Prime
  val Res1_tmp2 = Freg +^ Cdelay
  val Res1_value = (Freg +^ Cdelay >= g.Prime) ? Res1_tmp1 | Res1_tmp2
  val Res1 = RegNext(Res1_value)
  val Bdelay = RegNext(B)
  val Res2_tmp1 = Res1 - Bdelay
  val Res2_tmp2 = (Res1 +^ g.Prime) - Bdelay
  val Res2_value = (Res1 >= Bdelay) ? Res2_tmp1 | Res2_tmp2
  val Res2 = RegNext(Res2_value.resize(g.width bits))
  val valid = Delay(io.dataIn.valid, LatencyAnalysis(io.dataIn.payload, Res2))
  io.dataOut.payload := Res2
  io.dataOut.valid := valid
}
object FastMod2414 {
  def apply(dataIn: Flow[UInt], g: NttCfgParam): Flow[UInt] = {
    val dut = new FastMod2414(g)
    dut.io.dataIn := dataIn
    val ret = dut.io.dataOut
    ret
  }
}
case class FastMod2414_preOpt(g: NttCfgParam) extends Component {
  val prime = g.Prime
  val io = new Bundle {
    val dataIn = slave Flow (UInt(2 * g.width bits))
    val dataOut = master Flow (UInt(g.width bits))
  }

  val aPart = Vec(for (i <- 0 until (g.pGroup)) yield {
    if ((i + 1) * g.delta > g.width) {
      io.dataIn.payload(2 * g.width - 1 downto i * g.delta + g.width)
    } else io.dataIn.payload(g.width + (i + 1) * g.delta - 1 downto i * g.delta + g.width)
  })
  val bPart = Vec(for (i <- 0 until (g.pGroup)) yield {
    io.dataIn.payload(2 * g.width - 1 downto i * g.delta + g.width)
  })
  val A = RegNextWhen(aPart.reduce(_ +^ _), io.dataIn.valid) init U(0)
  val B_1 = RegNextWhen(bPart(2), io.dataIn.valid) init U(0)
  val B_2 = RegNextWhen(bPart(1) +^ bPart(0), io.dataIn.valid) init U(0)
  val B = RegNext(B_1 +^ B_2)
  val C = RegNextWhen(io.dataIn.payload(g.width - 1 downto 0), io.dataIn.valid)
  val Aslice = A._data.resize(2 * g.delta).subdivideIn(2 slices)
  val D = Aslice.reduce(_ +^ _)
  val Dslice = D.resize(2 * g.delta).subdivideIn(2 slices)
  val E = Dslice.reduce(_ + _)
  val Fsub = Dslice(1) + Aslice(1)
  val F = (E << g.P.N) - Fsub
  val Res1_tmp1 = F +^ C._data - g.Prime
  val Res1_tmp2 = F +^ C._data
  val Res1_value = (F +^ C >= g.Prime) ? Res1_tmp1 | Res1_tmp2
  val Res1 = RegNext(Res1_value)
  val Res2_tmp1 = Res1 - B
  val Res2_tmp2 = (Res1 +^ g.Prime) - B
  val Res2_value = (Res1 >= B) ? Res2_tmp1 | Res2_tmp2
  val Res2 = RegNext(Res2_value.resize(g.width bits))
  val valid = Delay(io.dataIn.valid, LatencyAnalysis(io.dataIn.payload, Res2))
  io.dataOut.payload := Res2
  io.dataOut.valid := valid
}

case class FastMod1412(g: NttCfgParam = NttCfgParam(P = PrimeCfg(14, 12))) extends Component {
  val prime = g.Prime
  val io = new Bundle {
    val dataIn = slave Flow (UInt(2 * g.width bits))
    val dataOut = master Flow (UInt(g.width bits))
  }
  val aPart = Vec(for (i <- 0 until (g.pGroup)) yield {
    if ((i + 1) * g.delta > g.width) {
      io.dataIn.payload(2 * g.width - 1 downto i * g.delta + g.width)
    } else io.dataIn.payload(g.width + (i + 1) * g.delta - 1 downto i * g.delta + g.width)
  })
  val bPart = Vec(for (i <- 0 until (g.pGroup)) yield {
    io.dataIn.payload(2 * g.width - 1 downto i * g.delta + g.width)
  })
  val A = RegNextWhen(aPart.reduceBalancedTree(_ +^ _).resize(5 bits), io.dataIn.valid) init U(0)

//  val bPart_1 = bPart.zipWithIndex.filter { case (_, idx) => idx % 2 == 0 }.map(_._1).reduceBalancedTree(_ +^ _)
//  val bPart_2 = bPart.zipWithIndex.filter { case (_, idx) => idx % 2 != 0 }.map(_._1).reduceBalancedTree(_ +^ _)
//  val B_1 = RegNextWhen(bPart_1,io.dataIn.valid) init U(0)
//  val B_2 = RegNextWhen(bPart_2, io.dataIn.valid) init U(0)
//  val B = RegNext(B_1 +^ B_2)
  val B = RegNextWhen(bPart.reduceBalancedTree(_ +^ _).resize(g.P.M + 1 bits), io.dataIn.valid)
  val Bdelay = Delay(B, 2)
  val C = RegNextWhen(io.dataIn.payload(g.width - 1 downto 0), io.dataIn.valid)
//  print(s"*******${A.getWidth} ${g.P.delta} ${scala.math.ceil(A.getWidth.toDouble/g.P.delta).toInt} *******")
  val Aslice = A._data
    .resize(g.P.delta * scala.math.ceil(A.getWidth.toDouble / g.P.delta).toInt bits)
    .subdivideIn(scala.math.ceil(A.getWidth.toDouble / g.P.delta).toInt slices)
  val D = Aslice.reduceBalancedTree(_ +^ _).resize(4 bits)
  val Dslice = D.subdivideIn(2 slices)
  val E = Dslice.reduce(_ +^ _).resize(2 bits)
  val Fsub = Dslice(1) +^ A(A.getWidth - 1 downto 2) +^ A.msb.asUInt
  val F = (E << g.P.N) - Fsub
  val Freg = RegNext(F)
  val Csub = C._data - g.Prime
  val Cst2 = (C._data >= g.Prime) ? Csub | C
  val Cdelay = RegNext(Cst2)
  val Res1_tmp1 = Freg +^ Cdelay - g.Prime
  val Res1_tmp2 = Freg +^ Cdelay
  val Res1_value = (Freg +^ Cdelay >= g.Prime) ? Res1_tmp1 | Res1_tmp2
  val Res1 = RegNext(Res1_value)

  val Res2_tmp1 = Res1 - Bdelay
  val Res2_tmp2 = (Res1 +^ g.Prime) - Bdelay
  val Res2_value = (Res1 >= Bdelay) ? Res2_tmp1 | Res2_tmp2
  val Res2 = RegNext(Res2_value.resize(g.width bits))
  val valid = Delay(io.dataIn.valid, LatencyAnalysis(io.dataIn.payload, Res2))
  io.dataOut.payload := Res2
  io.dataOut.valid := valid
}

object FastMod1412 {
  def apply(dataIn: Flow[UInt], g: NttCfgParam): Flow[UInt] = {
    val dut = new FastMod1412(g)
    dut.io.dataIn := dataIn
    val ret = dut.io.dataOut
    ret
  }
}

case class FastMod6432(g: NttCfgParam) extends Component {
  val prime = g.Prime
  require(g.P.M == 64 & g.P.N == 32)
  val io = new Bundle {
    val dataIn = slave Flow (UInt(2 * g.width bits))
    val dataOut = master Flow (UInt(g.width bits))
  }
  val sliceData = io.dataIn.payload.subdivideIn(4 slices)
  val st1_res1_tmp = sliceData(2) +^ sliceData(1)
  val st1_res2_tmp = (sliceData(0) -^ (sliceData(3) +^ sliceData(2))).asSInt
  val st1_res1 = RegNextWhen(st1_res1_tmp, io.dataIn.valid)
  val st1_res2 = RegNextWhen(st1_res2_tmp, io.dataIn.valid)
  val st2_res_tmp = (st1_res1 << g.P.N).asSInt +^ st1_res2
  val st2_res = RegNext(st2_res_tmp)
  val of_flag = st2_res.asUInt >= g.Prime
  val uf_flag = st2_res < 0
  val st3_of = (st2_res - g.Prime).asUInt.resize(g.width bits)
  val st3_uf = (st2_res + g.Prime).asUInt.resize(g.width bits)
  val st3_res_tmp = of_flag ? (st3_of) | (uf_flag ? (st3_uf) | (st2_res.asUInt.resize(g.width bits)))
  val st3_res = RegNext(st3_res_tmp)
  io.dataOut.payload := st3_res
  io.dataOut.valid := Delay(io.dataIn.valid, LatencyAnalysis(io.dataIn.payload, io.dataOut.payload))
}
object FastMod6432 {
  def apply(dataIn: Flow[UInt], g: NttCfgParam): Flow[UInt] = {
    val dut = new FastMod6432(g)
    dut.io.dataIn := dataIn
    val ret = dut.io.dataOut
    ret
  }
}

case class FastMod3220(g: NttCfgParam) extends Component {
  val prime = g.Prime
  require(g.P.M == 32 & g.P.N == 20)
  val io = new Bundle {
    val dataIn = slave Flow (UInt(2 * g.width bits))
    val dataOut = master Flow (UInt(g.width bits))
  }
  val a0 = io.dataIn.payload(43 downto 32)
  val a1 = io.dataIn.payload(55 downto 44)
  val a2 = io.dataIn.payload(63 downto 56)
  val b0 = io.dataIn.payload(63 downto 32)
  val b1 = io.dataIn.payload(63 downto 44)
  val b2 = io.dataIn.payload(63 downto 56)
  val c = io.dataIn.payload(31 downto 0)
  // st0
  val A_sum = a0._data +^ a1._data +^ a2._data
  val Areg = RegNextWhen(A_sum.resize(14 bits), io.dataIn.valid)
  val Breg_0 = RegNextWhen(b0._data +^ b1._data, io.dataIn.valid)
  val Breg_1 = RegNextWhen(b2._data, io.dataIn.valid)
  val CregSt0 = RegNextWhen(c, io.dataIn.valid)
  // st1
  val BregSt1 = RegNext((Breg_0 +^ Breg_1).resize(33 bits))
  val A_slice0 = Areg(11 downto 0)
  val A_slice1 = Areg(13 downto 12)
  val D_sum = (A_slice0 +^ A_slice1).resize(13 bits)
  val D_slice0 = D_sum(11 downto 0)
  val D_slice1 = D_sum.msb.asUInt
  val E_sum = (D_slice0 + D_slice1).resize(12 bits)
  val E_sub = A_slice1 +^ D_slice1
  val E_cb = (E_sum << g.P.N) - E_sub
  val Ereg = RegNext(E_cb)
  val CregSt1 = RegNext(CregSt0)
  // st2
  val BregSt2 = RegNext(BregSt1)
  val Res1_tmp1 = Ereg +^ CregSt1 - g.Prime
  val Res1_tmp2 = Ereg +^ CregSt1
  val Res1_value = (Ereg +^ CregSt1 >= g.Prime) ? Res1_tmp1 | Res1_tmp2
  val Res1 = RegNext(Res1_value)
  val Res2_tmp1 = Res1 - BregSt2
  val Res2_tmp2 = (Res1 +^ g.Prime) - BregSt2
  val Res2_value = (Res1 >= BregSt2) ? Res2_tmp1 | Res2_tmp2
  val Res2 = RegNext(Res2_value.resize(g.width bits))
  io.dataOut.payload := Res2
  io.dataOut.valid := Delay(io.dataIn.valid, LatencyAnalysis(io.dataIn.payload, io.dataOut.payload))
}
object FastMod3220 {
  def apply(dataIn: Flow[UInt], g: NttCfgParam): Flow[UInt] = {
    val dut = new FastMod3220(g)
    dut.io.dataIn := dataIn
    val ret = dut.io.dataOut
    ret
  }
}

case class BarretMod2414(g: NttCfgParam) extends Component {
  val io = new Bundle {
    val dataIn = slave Flow (UInt(2 * g.width bits))
    val dataOut = master Flow (UInt(g.width bits))
  }
  val dataReg = RegNextWhen(io.dataIn.payload, io.dataIn.valid)
  val st1_tmp =
    ((dataReg << 24) +^ (dataReg << 14) +^ (dataReg << 4) - dataReg).resize(72 bits)
  val st1_reg = RegNext(st1_tmp(71 downto 48))
  val dataDelay = Delay(dataReg, g.Bfu.MultLatency + 1).addAttribute("srl_style", "srl_reg")
  val mulRes = mulBlackBox(st1_reg, g.Prime, g)
  val st2_sub_tmp = RegNext(dataDelay - mulRes).resize(g.width bits)
  val st2_sub_tmp_of = RegNext(dataDelay - mulRes - g.Prime).resize(g.width bits)
  val of = dataDelay - mulRes >= g.Prime
  val res = RegNext(of ? st2_sub_tmp_of | st2_sub_tmp).resize(g.width bits)
  io.dataOut.payload := res
  io.dataOut.valid := Delay(io.dataIn.valid, g.Bfu.MultLatency + 3)
}
