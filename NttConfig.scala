package Ntt
import Ntt.NttCfg.NttCfgParam
import spinal.core._
import spinal.lib._

import scala.io.Source
import scala.util._
import scala.collection.mutable.ArrayBuffer

object tools {
  def wordCat(data: Seq[BigInt], n: Int, width: Int): Seq[BigInt] = {
    data
      .grouped(n)
      .map { item =>
        {
          var ret = BigInt(0)
          item.zipWithIndex.foreach { case (t1, idx) =>
            ret = ret + (t1 << (width * (idx)))
          }
          ret
        }
      }
      .toSeq
  }
  def readData(path: String): Seq[BigInt] = {
    val source = Source.fromFile(path)
    val ret: Seq[BigInt] = source.getLines().map(_.trim).filter(_.nonEmpty).map(BigInt(_)).toSeq
    ret
  }

}

object NttCfg {

  case class PrimeCfg(M: Int = 24, N: Int = 14) {
    val Prime = BigInt(2).pow(M) - BigInt(2).pow(N) + 1
    val HalfPrime = (Prime + 1) / 2
    val delta = M - N
    val pGroup = scala.math.ceil((M.toDouble / delta)).toInt
    def show(): Unit = {
      println(s"The prime is 2^$M - 2^$N + 1")
      println(s"group size is $pGroup")
    }
  }

  case class BfuParamCfg(M:Int = 24,device:String = "9eg") {
    val AddSubLatencyIntt = 3 // add&sub + rescale
    val AddSubLatencyNtt = 2 // add&sub
    val MultLatency = M match {
      case 14 => 3
      case 24 => 4
      case 64 => 18
    }
    val FastModLatency = M match {
      case 14 => 4
      case 24 => 4
      case 64 => 3
    }
//    val FastModLatency = 4
    val BfuRegisterInput = 1
    val BfuRegisterOutput = 1
    val pathMultIP = s"/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/IP/mul/mult_w${M}_${device}.xcix"
  }

  case class ArbitParamCfg() {
    val DecodeCalLatency = 1 // addrori -> bankidx & bankaddr
    val DecodeMuxRegLatency = 1 // mux -> register -> out
    val DecodeLatency = DecodeCalLatency + DecodeMuxRegLatency
    val ramLatency = 1
    val romMuxLatency = 1
    val DatDeMuxLatency = 1 // addrdecode -> mem -> datademux -> bfu
  }


  case class NttCfgParam(
      Bfu: BfuParamCfg = BfuParamCfg(),
      Arbit: ArbitParamCfg = ArbitParamCfg(),
      P: PrimeCfg = PrimeCfg(24, 14),
      nttPoint: Int = 1024,
      paraNum: Int = 4,
      debug: Boolean = true
  ) {
    val radix = 2

    val useBramIP = false
    val useMulIP = true
    val useTwFile = true
    val nttSimPublic = true

//    val AddSubLatencyIntt = Bfu.AddSubLatencyIntt // add&sub + rescale
//    val AddSubLatencyNtt = Bfu.AddSubLatencyNtt // add&sub
//    val MultLatency = Bfu.MultLatency
//    val FastModLatency = Bfu.FastModLatency
//    val BfuRegisterInput = Bfu.BfuRegisterInput
//    val BfuRegisterOutput = Bfu.BfuRegisterOutput

    val BfuLatencySt1 = Bfu.AddSubLatencyIntt
    val BfuLatencySt2 = Bfu.MultLatency + Bfu.FastModLatency
    val BfuNttDelay = Bfu.AddSubLatencyNtt + Bfu.MultLatency + Bfu.FastModLatency
    val BfuInttDelay = Bfu.AddSubLatencyIntt + Bfu.MultLatency + Bfu.FastModLatency
    val BfuRegisterIoDelay = Bfu.BfuRegisterInput + Bfu.BfuRegisterOutput

//    val DecodeCalLatency = 1 // addrori -> bankidx & bankaddr
//    val DecodeMuxRegLatency = 1 // mux -> register -> out
//    val DecodeLatency = DecodeCalLatency + DecodeMuxRegLatency
//    val ramLatency = 1
//    val romMuxLatency = 1
//    val DatDeMuxLatency = 1 // addrdecode -> mem -> datademux -> bfu
    val addrNttLoopLatency = Arbit.ramLatency + BfuNttDelay + BfuRegisterIoDelay
    val addrInttLoopLatency = Arbit.ramLatency + BfuInttDelay + BfuRegisterIoDelay
    val bfuValidLoopNttLatency = addrNttLoopLatency + Arbit.DecodeLatency + Arbit.DatDeMuxLatency
    val bfuValidLoopInttLatency = addrInttLoopLatency + Arbit.DecodeLatency + Arbit.DatDeMuxLatency
    val bfuValidLoopLatency = if (bfuValidLoopInttLatency > bfuValidLoopNttLatency) {
      bfuValidLoopInttLatency
    } else {
      bfuValidLoopNttLatency
    }

    val width = P.M
    val Prime = P.Prime
    val HalfPrime = P.HalfPrime
    val delta = P.delta
    val pGroup = P.pGroup
    val BI = 2 * paraNum
    val Log2NttPoints = log2Up(nttPoint)
    val BankIndexWidth = log2Up(BI)
    val BankAddrWidth = Log2NttPoints - BankIndexWidth
    val twNum = nttPoint / paraNum
    val twAddrWidth = log2Up(nttPoint / paraNum)
    val twWidth = width * paraNum

    val initTable: Seq[BigInt] = Seq.range(0, nttPoint).map(item => BigInt(item))
    val twFilePath = s"/PRJ/SpinalHDL-prj/PRJ/py/nwc_ntt_python/data/tw${nttPoint}p${paraNum}q${P.M}_${P.N}.txt"
    val twData: Seq[BigInt] = tools.readData(twFilePath)
    val initTableCompress = tools.wordCat(initTable, paraNum, width)
  }

  case class CtrlBus() extends Bundle {
    val isNtt = Bool()
    val isCal = Bool()
    val isOutSideRead = Bool()
    val isOutSideWrite = Bool()
  }

  case class BfuPayload(g: NttCfgParam) extends Bundle {
    val A = UInt(g.width bits)
    val B = UInt(g.width bits)
    val Tw = UInt(g.width bits)
  }

  case class twPayload(addrWidth: Int, muxWidth: Int, para: Int) extends Bundle {
    val twAddr = UInt(addrWidth bits)
    val twMux = Vec(UInt(muxWidth bits), para)
  }

  case class ParaWriteBus(DataWidth: Int, AddrWidth: Int, para: Int) extends Bundle with IMasterSlave {
    val Addr = Vec(UInt(AddrWidth bits), para)
    val Data = Vec(Bits(DataWidth bits), para)

    override def asMaster(): Unit = {
      out(Addr, Data)
    }
  }
  case class DataPayload(g: NttCfgParam) extends Bundle {
    val A = UInt(g.width bits)
    val B = UInt(g.width bits)
  }

  case class multPayload(g: NttCfgParam) extends Bundle {
    val data = UInt(g.width bits)
    val tw = UInt(g.width bits)
  }

}

object test {
  def main(args: Array[String]): Unit = {
    try {
      val tmp: Seq[BigInt] = tools.readData("/PRJ/SpinalHDL-prj/PRJ/py/nwc_ntt_python/data/tw1024.txt")
      println(tmp(10))
    } catch {
      case e: Exception =>
        println(s"Error reading : ${e.getMessage}")
    }
    val g = new NttCfgParam(nttPoint = 1024, paraNum = 4)
    print(g.twFilePath)
  }
}
