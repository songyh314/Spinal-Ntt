package Ntt.BFU

import Ntt.NttCfg.{BfuPayload, DataPayload, NttCfg2414}
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.eda.bench.Rtl
import spinal.lib.eda.xilinx.VivadoFlow

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

case class invTwConvert(g:NttCfg2414) extends Component{
  val io = new Bundle{
    val tw = in UInt(g.width bits)
    val invTw = out UInt(g.width bits)
  }
  val tmp = g.Prime - io.tw
  io.invTw := RegNext(tmp)
}

class Bfu(g: NttCfg2414, debug: Boolean = false) extends Component {
  val io = new Bundle {
    val isNtt = in Bool ()
    val dataIn = slave Flow (BfuPayload(g))
    val dataOut = master Flow (DataPayload(g))
  }

  def genInvTw(tw:UInt):UInt = {
    val ret = RegNext(g.Prime - tw)
    ret
  }
  import io._

  val debugArea = if (debug) {
    new Area {
      dataOut.A := io.isNtt ? Delay(io.dataIn.payload.A, g.BfuNttDelay) | Delay(io.dataIn.payload.A, g.BfuInttDelay)
      dataOut.B := io.isNtt ? Delay(io.dataIn.payload.B, g.BfuNttDelay) | Delay(io.dataIn.payload.B, g.BfuInttDelay)
      dataOut.valid := io.isNtt ? Delay(io.dataIn.valid, g.BfuNttDelay) | Delay(io.dataIn.valid, g.BfuInttDelay)
    }
  } else { null }

  val funcArea = if (!debug) {
    new Area {

      val uAddSub = new AddSub(g)
      val uModMult = new ModMult(g)
      val invTw = genInvTw(io.dataIn.payload.Tw)
      val DelayOutSt1 = Delay(invTw, g.BfuLatencySt1 - 1).addAttribute("SRL_STYLE", "SRL")
      val DelayOutSt2 =
        Delay(io.isNtt ? io.dataIn.A | uAddSub.io.dataOut.A, g.BfuLatencySt2).addAttribute("SRL_STYLE", "SRL_REG")

      uModMult.io.dataIn.valid := isNtt ? io.dataIn.valid | uAddSub.io.dataOut.valid
      uModMult.io.dataIn.payload.data := isNtt ? io.dataIn.payload.B | uAddSub.io.dataOut.B
      uModMult.io.dataIn.payload.tw := isNtt ? io.dataIn.payload.Tw | DelayOutSt1

      uAddSub.io.isRescale := !io.isNtt
      uAddSub.io.dataIn.valid := isNtt ? uModMult.io.dataOut.valid | io.dataIn.valid
      uAddSub.io.dataIn.A := isNtt ? DelayOutSt2 | io.dataIn.A
      uAddSub.io.dataIn.B := isNtt ? uModMult.io.dataOut.payload | io.dataIn.B

      io.dataOut.A := isNtt ? uAddSub.io.dataOut.A | DelayOutSt2
      io.dataOut.B := isNtt ? uAddSub.io.dataOut.B | uModMult.io.dataOut.payload
      io.dataOut.valid := isNtt ? uAddSub.io.dataOut.valid | uModMult.io.dataOut.valid
    }
  } else { null }

}
