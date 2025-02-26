package Ntt
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.eda.bench.Rtl
import spinal.lib.eda.xilinx.VivadoFlow
import test._
import NttCfg._
import BFU._
import CTRL._
import DataPath._

import java.io.PrintWriter
import scala.collection.mutable.ArrayBuffer

case class CtrlMemTop(g: NttCfgParam) extends Component {
  val io = new Bundle {
    val start = in Bool ()
    val idle = out Bool ()
    val ctrl = in(CtrlBus())
    val outsideAddrOri = slave Flow Vec(UInt(g.BankAddrWidth bits), g.radix)
    val outsideIdxOri = slave Flow Vec(UInt(g.BankIndexWidth bits), g.BI)
    val outsideRdDataArray = master Flow Vec(Bits(g.width bits), g.BI)
    val outsideWrDataArray = slave Flow Vec(Bits(g.width bits), g.BI)

    val NttWriteBack = Array.fill(g.paraNum)(slave Flow DataPayload(g))
    val NttPayload = Array.fill(g.paraNum)(master Flow BfuPayload(g))
  }
  val ctrl = new CtrlOptAddr(g)
  val dut = new DataPathTop(g)
  ctrl.io.start := io.start
  ctrl.io.isNtt := io.ctrl.isNtt

  io.idle := ctrl.io.idle
  dut.io.ctrl := io.ctrl
  dut.io.bfuRdAddrOri := ctrl.io.RdAddrOri
  dut.io.bfuRdIdxOri := ctrl.io.RdLsbOri
  dut.io.twBus := ctrl.io.TwBus

  dut.io.outsideAddrOri := io.outsideAddrOri
  dut.io.outsideIdxOri := io.outsideIdxOri
  io.outsideRdDataArray := dut.io.outsideRdDataArray
  dut.io.outsideWrDataArray := io.outsideWrDataArray

  dut.io.NttWriteBack.zip(io.NttWriteBack).foreach { case (t1, t2) => t1 := t2 }
  io.NttPayload.zip(dut.io.NttPayload).foreach { case (t1, t2) => t1 := t2 }
}

//case class CtrlMemTopP1(g: NttCfgParam) extends Component {
//  val io = new Bundle {
//    val start = in Bool ()
//    val idle = out Bool ()
//    val ctrl = in(CtrlBus())
//    val outsideAddrOri = slave Flow Vec(UInt(g.BankAddrWidth bits), g.radix)
//    val outsideRdDataArray = master Flow Vec(Bits(g.width bits), g.BI)
//    val outsideWrDataArray = slave Flow Vec(Bits(g.width bits), g.BI)
//
//    val NttWriteBack = Array.fill(g.paraNum)(slave Flow DataPayload(g))
//    val NttPayload = Array.fill(g.paraNum)(master Flow BfuPayload(g))
//  }
//  val ctrl = new CtrlOptAddrp1(g)
//  val dut = new DataPathTopP1(g)
//  ctrl.io.start := io.start
//  ctrl.io.isNtt := io.ctrl.isNtt
//
//  io.idle := ctrl.io.idle
//  dut.io.ctrl := io.ctrl
//  dut.io.bfuRdAddrOri := ctrl.io.RdAddrOri
//  dut.io.twBus := ctrl.io.TwBus
//
//  dut.io.outsideAddrOri := io.outsideAddrOri
//  io.outsideRdDataArray := dut.io.outsideRdDataArray
//  dut.io.outsideWrDataArray := io.outsideWrDataArray
//
//  dut.io.NttWriteBack.zip(io.NttWriteBack).foreach { case (t1, t2) => t1 := t2 }
//  io.NttPayload.zip(dut.io.NttPayload).foreach { case (t1, t2) => t1 := t2 }
//}

case class NttTop(g: NttCfgParam, debug: Boolean = false) extends Component {
  val io = new Bundle {
    val start = in Bool ()

    val idle = out Bool ()
    val ctrl = in(CtrlBus())

    val outsideAddrOri = slave Flow Vec(UInt(g.BankAddrWidth bits), g.radix)
    val outsideIdxOri = slave Flow Vec(UInt(g.BankIndexWidth bits), g.BI)
    val outsideRdDataArray = master Flow Vec(Bits(g.width bits), g.BI)
    val outsideWrDataArray = slave Flow Vec(Bits(g.width bits), g.BI)

    val bfuOut = if (g.mode.nttSimPublic) { Array.fill(g.paraNum)(master Flow (DataPayload(g))) }
    else { null }
    val bfuIn = if (g.mode.nttSimPublic) { Array.fill(g.paraNum)(master Flow (BfuPayload(g))) }
    else { null }
  }
  noIoPrefix()
  val ctrlMem = new CtrlMemTop(g)
  val bfuArray = new BfuArray(g, debug)
  io.idle := ctrlMem.io.idle
  ctrlMem.io.start := io.start
  ctrlMem.io.ctrl := io.ctrl
  ctrlMem.io.outsideAddrOri := io.outsideAddrOri
  ctrlMem.io.outsideIdxOri := io.outsideIdxOri
  io.outsideRdDataArray := ctrlMem.io.outsideRdDataArray
  ctrlMem.io.outsideWrDataArray := io.outsideWrDataArray
  bfuArray.io.isNtt := io.ctrl.isNtt
  bfuArray.io.dataIn.toSeq.zip(ctrlMem.io.NttPayload.toSeq).foreach { case (t1, t2) => t1 := (t2) }
  ctrlMem.io.NttWriteBack.toSeq.zip(bfuArray.io.dataOut.toSeq).foreach { case (t1, t2) => t1 := (t2) }
  if (g.mode.nttSimPublic) {
    io.bfuOut.toSeq.zip(bfuArray.io.dataOut.toSeq).foreach { case (t1, t2) => t1 := t2 }
    io.bfuIn.toSeq.zip(ctrlMem.io.NttPayload.toSeq).foreach { case (t1, t2) => t1 := t2 }
  }
}


//case class NttTopP1(g: NttCfgParam, debug: Boolean = false) extends Component {
//  val io = new Bundle {
//    val start = in Bool ()
//
//    val idle = out Bool ()
//    val ctrl = in(CtrlBus())
//
//    val outsideAddrOri = slave Flow Vec(UInt(g.BankAddrWidth bits), g.radix)
//    val outsideIdxOri = slave Flow Vec(UInt(g.BankIndexWidth bits), g.BI)
//    val outsideRdDataArray = master Flow Vec(Bits(g.width bits), g.BI)
//    val outsideWrDataArray = slave Flow Vec(Bits(g.width bits), g.BI)
//
//    val bfuOut = if (g.mode.nttSimPublic) { Array.fill(g.paraNum)(master Flow (DataPayload(g))) }
//    else { null }
//    val bfuIn = if (g.mode.nttSimPublic) { Array.fill(g.paraNum)(master Flow (BfuPayload(g))) }
//    else { null }
//  }
//  noIoPrefix()
//  val ctrlMem = new CtrlMemTopP1(g)
//  val bfuArray = new BfuArray(g, debug)
//  io.idle := ctrlMem.io.idle
//  ctrlMem.io.start := io.start
//  ctrlMem.io.ctrl := io.ctrl
//  ctrlMem.io.outsideAddrOri := io.outsideAddrOri
//  io.outsideRdDataArray := ctrlMem.io.outsideRdDataArray
//  ctrlMem.io.outsideWrDataArray := io.outsideWrDataArray
//  bfuArray.io.isNtt := io.ctrl.isNtt
//  bfuArray.io.dataIn.toSeq.zip(ctrlMem.io.NttPayload.toSeq).foreach { case (t1, t2) => t1 := (t2) }
//  ctrlMem.io.NttWriteBack.toSeq.zip(bfuArray.io.dataOut.toSeq).foreach { case (t1, t2) => t1 := (t2) }
//  if (g.mode.nttSimPublic) {
//    io.bfuOut.toSeq.zip(bfuArray.io.dataOut.toSeq).foreach { case (t1, t2) => t1 := t2 }
//    io.bfuIn.toSeq.zip(ctrlMem.io.NttPayload.toSeq).foreach { case (t1, t2) => t1 := t2 }
//  }
//}

