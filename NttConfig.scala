package Ntt
import Ntt.DataPath.DataDeMux
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
    val primeList = Seq((14,12),(24,14),(64,32))
    require(primeList.contains((M,N)),s"prime is not illegal")
    val Prime = BigInt(2).pow(M) - BigInt(2).pow(N) + 1
    val HalfPrime = (Prime + 1) / 2
    val delta = M - N
    val pGroup = scala.math.ceil((M.toDouble / delta)).toInt
    def show(): Unit = {
      println(s"The prime is 2^$M - 2^$N + 1")
      println(s"group size is $pGroup")
    }
  }

  case class BfuParamCfg(M: Int = 24, device: String = "9eg", spiltMul: Boolean = false) {
    val widthList = Seq(14,24,64)
    val deviceList = Seq("9eg","v7")
    require(widthList.contains(M),"mul's width is illegal")
    require(deviceList.contains(device),"illegal device")
    val AddSubLatencyIntt = 3 // add&sub + rescale
    val AddSubLatencyNtt = 2 // add&sub
    val dspWidth = if (spiltMul) { M/2 }
    else { M }
    val MultLatency = M match {
      case 14 => 3
      case 24 => 4
      case 64 => if (spiltMul) {8} else {18}
    }
    val FastModLatency = M match {
      case 14 => 4
      case 24 => 4
      case 64 => 3
    }
//    val FastModLatency = 4
    val BfuRegisterInput = 1
    val BfuRegisterOutput = 1
    val pathMultIP = s"/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/IP/mul/mult_w${dspWidth}_${device}.xcix"
  }

  case class ArbitParamCfg(para:Int = 4) {
    val DecodeCalLatency = 1 // addrori -> bankidx & bankaddr
    val DecodeMuxRegLatency = 1 // mux -> register -> out
    val DecodeLatency = DecodeCalLatency + DecodeMuxRegLatency
    val ramLatency = 1
    val romLatency = if(para >= 16){2} else {1}
    val romMuxLatency = 1
    val DatDeMuxLatency = 1 // addrdecode -> mem -> datademux -> bfu
    val romDealyLatency = DecodeLatency + DatDeMuxLatency + (ramLatency - romLatency) - romMuxLatency
  }

  case class modeCfg(
      debug:Boolean = false,
      nttSimPublic: Boolean = true,
      useTwFile:Boolean = true
                    ){}

  case class NttCfgParam(
      Bfu: BfuParamCfg = BfuParamCfg(),
      Arbit: ArbitParamCfg = ArbitParamCfg(),
      P: PrimeCfg = PrimeCfg(24, 14),
      mode:modeCfg = modeCfg(debug = false, nttSimPublic = true, useTwFile = true),
      nttPoint: Int = 1024,
      paraNum: Int = 4,
//      debug: Boolean = false,
//      nttSimPublic: Boolean = true,
//      useTwFile:Boolean = true
  ) {
    val nttPointList = Seq(512,1024,4096,8192)
    if (mode.useTwFile) {require(nttPointList.contains(nttPoint),s"only support 512/1024/4096/8192 points")}
    if (P.M == 14){require(nttPoint <= 1024,"for q=12289, only support 512/1024")}
    val radix = 2
    val useBramIP = false
    val useMulIP = true
//    val useTwFile = true

    val BfuLatencySt1 = Bfu.AddSubLatencyIntt
    val BfuLatencySt2 = Bfu.MultLatency + Bfu.FastModLatency
    val BfuNttDelay = Bfu.AddSubLatencyNtt + Bfu.MultLatency + Bfu.FastModLatency
    val BfuInttDelay = Bfu.AddSubLatencyIntt + Bfu.MultLatency + Bfu.FastModLatency
    val BfuRegisterIoDelay = Bfu.BfuRegisterInput + Bfu.BfuRegisterOutput

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
    val counter = nttPoint / BI
    val Log2NttPoints = log2Up(nttPoint)
    val BankIndexWidth = log2Up(BI)
    val BankAddrWidth = Log2NttPoints - BankIndexWidth
    val twNum = nttPoint / paraNum
    val twAddrWidth = log2Up(nttPoint / paraNum)
    val twWidth = width * paraNum

    val initTable: Seq[BigInt] = Seq.range(0, nttPoint).map(item => BigInt(item))

    val twFilePath = s"/PRJ/SpinalHDL-prj/PRJ/py/nwc_ntt_python/data/tw${nttPoint}p${paraNum}q${P.M}_${P.N}.txt"
    lazy val twData: Seq[BigInt] = tools.readData(twFilePath)
    val initTableCompress = tools.wordCat(initTable, paraNum, width)



    val family = Bfu.device match {
      case "v7"  => "Virtex 7"
      case "9eg" => "Zynq UltraScale+ MPSoCS"
    }
    val device = Bfu.device match {
      case "9eg" => "xczu9eg-ffvb1156-2-i"
      case "v7"  => "xc7vx485tffg1157-2"
    }

  }

  case class CtrlBus() extends Bundle {
    val isNtt = Bool()
    val isCal = Bool()
    val isOutSideRead = Bool()
    val isOutSideWrite = Bool()
  }

  def driveCtrl(that: CtrlBus,cmd:Bits): Unit = {


    require(cmd.getWidth == 4)
    that.isNtt := cmd(3)
    that.isCal := cmd(2)
    that.isOutSideRead := cmd(1)
    that.isOutSideWrite := cmd(0)
  }


//
//  case class ctrlTest() extends Component{
//    val ctrlBus = CtrlBus()
//    val ctrlBits = Bits(4 bits)
//    ctrlBits := B"4'b1010"
//    ctrlBus.assign(ctrlBits)
//    println(s"isNtt : ${if (ctrlBus.isNtt == True){1} else {0}}")
//    println(s"isCal : ${if (ctrlBus.isCal == True){1} else {0}}")
//    println(s"isOutSideRead : ${if (ctrlBus.isOutSideRead == True){1} else {0}}")
//    println(s"isOutSideWrite : ${if (ctrlBus.isOutSideWrite == True){1} else {0}}")
//  }


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
  }
}
