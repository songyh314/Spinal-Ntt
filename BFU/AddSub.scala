package Ntt.BFU

import Ntt.NttCfg.{DataPayload, NttCfg2414}
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.eda.bench.Rtl
import spinal.lib.eda.xilinx.VivadoFlow

import scala.collection.mutable
import scala.util.Random

//isRescale作为控制信号在该模式下的计算全部完成之前不能改变
//可以扩展module输出Mux,但是两种模式切换时必须空一个周期
//原因是两种模式Latency不一致,不能无缝切换.实际功能也不需要,故不做扩展

class AddSub(g: NttCfg2414) extends Component {
  val io = new Bundle {
    val isRescale = in Bool ()
    val dataIn = slave Flow (DataPayload(g))
    val dataOut = master Flow (DataPayload(g))
  }
  noIoPrefix()
  import io._

//  val Add_tmp1 = dataIn.A +^ dataIn.B - g.Prime
//  val Add_tmp2 = (dataIn.A + dataIn.B)
//  val Add_tmp = ((dataIn.A +^ dataIn.B) >= g.Prime) ? Add_tmp1 | Add_tmp2
//val Sub_tmp1 = dataIn.A - dataIn.B
//  val Sub_tmp2 = dataIn.A +^ g.Prime - dataIn.B
//  val Sub_tmp = (dataIn.A >= dataIn.B) ? Sub_tmp1 | Sub_tmp2
  val Areg = RegNextWhen(dataIn.A, dataIn.valid) init 0
  val Breg = RegNextWhen(dataIn.B, dataIn.valid) init 0
  val Add_tmp1 = Areg +^ Breg - g.Prime
  val Add_tmp2 = (Areg + Breg)
  val flagOf = Reg(Bool())
  val flagUf = Reg(Bool())
  flagOf := (dataIn.A +^ dataIn.B) >= g.Prime
  flagUf := dataIn.A >= dataIn.B
  val Add_tmp = (flagOf) ? Add_tmp1 | Add_tmp2
  val Sub_tmp1 = Areg - Breg
  val Sub_tmp2 = Areg +^ g.Prime - Breg
  val Sub_tmp = (flagUf) ? Sub_tmp1 | Sub_tmp2
  val addReg = Reg(UInt(g.width bits))
  val subReg = Reg(UInt(g.width bits))
  def rescale(A: UInt, sel: Bool = False, valid: Bool): UInt = {
    val ret = UInt(g.width bits)
    ret := (A |>> 1)
    val resTmp = A.lsb ? (ret + g.HalfPrime) | ret
    val res = RegNextWhen(resTmp, valid && sel)
    res
  }
  val validLat1 = Delay(io.dataIn.valid,g.AddSubLatencyNtt)
  val validLat2 = RegNext(validLat1)

  addReg := Add_tmp.resized
  subReg := Sub_tmp.resized
  io.dataOut.A := io.isRescale ? rescale(addReg, io.isRescale, validLat1).setName("addRescale") | addReg
  io.dataOut.B := io.isRescale ? rescale(subReg, io.isRescale, validLat1).setName("subRescale") | subReg
  io.dataOut.valid := io.isRescale ? validLat2 | validLat1
}
object AddSub {
  def apply(dataIn: Flow[DataPayload], mode: Bool, config: NttCfg2414): AddSub = {
    val uAddSub = new AddSub(config)
    uAddSub.io.dataIn := dataIn
    uAddSub.io.isRescale := mode
    uAddSub
  }
}

