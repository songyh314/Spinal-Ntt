package Ntt
import spinal.core._
import spinal.lib._

object NttCfg {

  case class NttCfg2414(nttPoint: Int = 1024, paraNum: Int = 4, debug: Boolean = true) {
    val M = 24
    val N = 14
    val useBramIP = false
    val ramLatency = 2
    val AddSubLatencyIntt = 2
    val AddSubLatencyNtt = 1
    val MultLatency = 4
    val DecodeLatency = 1
    val FastModLatency = 3
    val BfuLatencySt1 = AddSubLatencyIntt
    val BfuLatencySt2 = MultLatency + FastModLatency
    val width = M
    val useMulIP = true
    val radix = 2
    val Prime = BigInt(2).pow(M) - BigInt(2).pow(N) + 1
    val HalfPrime = (Prime + 1) / 2
    val delta = M - N
    val pGroup = scala.math.ceil((M.toDouble / delta)).toInt
    val BI = 2 * paraNum
    val Log2NttPoints = log2Up(nttPoint)
    val BankIndexWidth = log2Up(BI)
    val BankAddrWidth = Log2NttPoints - BankIndexWidth
    val twNum = nttPoint / paraNum
    val twAddrWidth = log2Up(nttPoint / paraNum)
    val twWidth = width * paraNum
//    val twInitSeq = (0 until nttPoint).map(B(_,width bits)).grouped(paraNum).map(item => Cat(item)).toSeq
    val BfuNttDelay = AddSubLatencyNtt + MultLatency + FastModLatency
    val BfuInttDelay = AddSubLatencyIntt + MultLatency + FastModLatency
    def show(): Unit = {
      println(s"The prime is 2^$M - 2^$N + 1")
      println(s"group size is $pGroup")
    }

  }
//  case class NttCtrl(g:NttCfg2414) extends Bundle{
//    val isNtt
//  }
  case class BfuPayload(g: NttCfg2414) extends Bundle {
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
  case class DataPayload(g: NttCfg2414) extends Bundle {
    val A = UInt(g.width bits)
    val B = UInt(g.width bits)
  }

  case class multPayload(g: NttCfg2414) extends Bundle {
    val data = UInt(g.width bits)
    val tw = UInt(g.width bits)
  }

}
