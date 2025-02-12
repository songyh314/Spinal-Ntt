package Ntt.BFU

import Ntt.NttCfg.{BfuPayload, DataPayload, NttCfgParam}
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.eda.bench.Rtl
import spinal.lib.eda.xilinx.VivadoFlow

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

case class BfuArray(g: NttCfgParam, debug: Boolean = false) extends Component {
  val io = new Bundle {
    val isNtt = in Bool ()
    val dataIn = Array.fill(g.paraNum)(slave Flow (BfuPayload(g)))
    val dataOut = Array.fill(g.paraNum)(master Flow (DataPayload(g)))
  }
  noIoPrefix()
  val bfu = Array.fill(g.paraNum)(new Bfu(g, debug))
  bfu.toSeq.zip(io.dataIn.toSeq).foreach { case (t1, t2) => t1.io.dataIn := RegNext(t2) }
  io.dataOut.toSeq.zip(bfu.toSeq).foreach { case (t1, t2) => t1 := RegNext(t2.io.dataOut) }
  bfu.toSeq.foreach { item => item.io.isNtt := io.isNtt }
}

object BfuArrayGenV extends App {
  SpinalConfig(mode = Verilog, nameWhenByFile = false, anonymSignalPrefix = "tmp", targetDirectory = "./rtl/Ntt/Bfu/")
    .generate(new BfuArray(NttCfgParam()))
}
