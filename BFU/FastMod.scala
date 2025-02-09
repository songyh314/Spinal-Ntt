package Ntt.BFU

import Ntt.NttCfg.NttCfg2414
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.eda.bench.Rtl
import spinal.lib.eda.xilinx.VivadoFlow

import scala.collection.mutable
import scala.util.Random

//class FastMod2420(g: NttConfig2420) extends Component {
//  val prime = g.Prime
//  val io = new Bundle {
//    val dataIn = slave Flow (UInt(2 * g.width bits))
//    val dataOut = master Flow (UInt(g.width bits))
//  }
//  val A = Vec(for (i <- 0 until g.pGroup) yield {
//    RegNextWhen(
//      io.dataIn.payload((2 * g.width - i * g.delta - 1) downto (2 * g.width - (i + 1) * g.delta)),
//      io.dataIn.valid
//    )
//  }).setName("storeA")
//  val B = Vec(for (i <- 0 until (g.pGroup)) yield {
//    RegNextWhen(
//      io.dataIn.payload((2 * g.width - 1) downto (2 * g.width - (i + 1) * g.delta)),
//      io.dataIn.valid
//    )
//  }).setName("storeB")
//
//  val C = RegNext(RegNextWhen(io.dataIn.payload(g.width - 1 downto 0), io.dataIn.valid))
//  val Asum = RegNext(A.reduceBalancedTree(_ +^ _)).setName("sum_A")
//  val Bsum1 = RegNext(
//    B.zipWithIndex.filter(_._2 % 2 == 0).map(_._1).reduceBalancedTree(_ +^ _)
//  )
//  val Bsum2 = RegNext(
//    B.zipWithIndex.filter(_._2 % 2 == 1).map(_._1).reduceBalancedTree(_ +^ _)
//  )
//  val Bsum = RegNext(Bsum1._data + Bsum2._data).setName("sum_B")
//  val slicesNum = scala.math.ceil(widthOf(Asum).toDouble / g.delta).toInt
//  val E =
//    Asum._data.resize(g.delta * slicesNum).subdivideIn(slicesNum slices).reduceBalancedTree(_ +^ _).setName("sum_A_bit")
//  val F1 = E(widthOf(E) - 1 downto g.delta) + E(g.delta - 1 downto 0)
//  val F2 = E(widthOf(E) - 1 downto g.delta) +^ Asum._data(Asum._data.high downto g.delta)
//  val F = ((F1 << 20) - F2).setName("sum_F")
//  val AsumAddC = RegNext((F + C >= prime) ? (F + C - prime) | (F + C)).setName("RES1")
//  val ACsubB = RegNext(
//    (AsumAddC._data >= Bsum._data) ? (AsumAddC._data - Bsum._data) | ((prime - Bsum._data) + AsumAddC._data)
//  ).setName("RES2")
//  val validDealy = Delay(io.dataIn.valid, LatencyAnalysis(io.dataIn.payload, ACsubB) + 1)
//  io.dataOut.payload := ACsubB.resized
//  io.dataOut.valid := validDealy
//}

case class FastMod2414(g: NttCfg2414) extends Component {
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
  val F = (E << g.N) - Fsub
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
case class FastMod2414_preOpt(g: NttCfg2414) extends Component {
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
  val F = (E << g.N) - Fsub
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

