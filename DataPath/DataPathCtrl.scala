package Ntt.DataPath

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import Ntt.NttCfg._
import myRam._
import myTools._
import spinal.core
import spinal.lib.eda.bench.Rtl
import spinal.lib.eda.xilinx.VivadoFlow

import scala.collection.mutable

//case class memForwardCtrlOpt(g: NttCfgParam) extends Component {
//  val io = new Bundle {
//    val ctrl = in(CtrlBus())
//    val outsideAddrOri = slave Flow Vec(UInt(g.BankAddrWidth bits), g.radix)
//    val outsideIdxOri = slave Flow Vec(UInt(g.BankIndexWidth bits), g.BI)
//    val outsideWrDataArray = slave Flow Vec(Bits(g.width bits), g.BI)
//
//    val bfuRdAddrOri = slave Flow Vec(UInt(g.BankAddrWidth bits), g.radix)
//    val bfuRdIdxOri = slave Flow Vec(UInt(g.BankIndexWidth bits), g.BI)
//    val NttWriteBack = slave Flow (Vec(Bits(g.width bits), g.BI))
//
//    val MemIfRe = out Bool ()
//    val MemIfRdAddr = out Vec (UInt(g.BankAddrWidth bits), g.BI)
//    val bankIdxTrans = out Vec (UInt(g.BankIndexWidth bits), g.BI)
//    val MemIfWe = out Vec(Bool (),g.BI)
//    val MemIfWrAddr = out Vec (UInt(g.BankAddrWidth bits), g.BI)
//    val MemIfWrData = out Vec (Bits(g.width bits), g.BI)
//  }
//
//  val memInArb = new memInArbOpt(g)
//  val memWbArb = new memWritebackArb(g)
//
//  memInArb.io.addrOri := io.ctrl.isCal ? io.bfuRdAddrOri.payload | io.outsideAddrOri.payload
//  memInArb.io.idxOri := io.ctrl.isCal ? io.bfuRdIdxOri.payload | io.outsideIdxOri.payload
//  memInArb.io.dataOri := io.outsideWrDataArray.payload
//
//  io.bankIdxTrans := memInArb.io.bankIdxTrans
//
//  memWbArb.io.isNtt := io.ctrl.isNtt
//  memWbArb.io.dataWb := io.NttWriteBack.payload
//  memWbArb.io.addrWb := RegNext(memInArb.io.addrOri_r1)
//  memWbArb.io.idxWb := memInArb.io.shuffleIdxTrans
//
//  for (i <- 0 until g.BI) {
//    io.MemIfWrAddr(i) := RegNext(
//      io.ctrl.isCal ? (memWbArb.io
//        .addrWbSelMem(i) ? (memWbArb.io.addrWbMem(1)) | (memWbArb.io.addrWbMem(0))) |
//        (memInArb.io.addrSel(i) ? (memInArb.io.addrOri_r1(1)) | (memInArb.io.addrOri_r1(0)))
//    )
//    io.MemIfRdAddr(i) := RegNext(
//      memInArb.io.addrSel(i) ? (memInArb.io.addrOri_r1(1)) | (memInArb.io.addrOri_r1(0))
//    )
//  }
//
//  io.MemIfRe := io.ctrl.isCal ? { Delay(io.bfuRdAddrOri.valid, g.Arbit.DecodeLatency) } | {
//    io.ctrl.isOutSideRead ? Delay(io.outsideAddrOri.valid, g.Arbit.DecodeLatency) | False
//  }
//  io.MemIfWe := io.ctrl.isCal ? Delay(io.NttWriteBack.valid, g.Arbit.DecodeMuxRegLatency) | {
//    io.ctrl.isOutSideWrite ? Delay(io.outsideAddrOri.valid, g.Arbit.DecodeLatency) | False
//  }
//  io.MemIfWrData := RegNext(io.ctrl.isCal ? memWbArb.io.dataWbMem | memInArb.io.dataMem)
//
//}
case class memForwardCtrl(g: NttCfgParam) extends Component {
  val io = new Bundle {
    val ctrl = in(CtrlBus())
    val outsideAddrOri = slave Flow Vec(UInt(g.BankAddrWidth bits), g.radix)
    val outsideIdxOri = slave Flow Vec(UInt(g.BankIndexWidth bits), g.BI)
    val outsideWrDataArray = slave Flow Vec(Bits(g.width bits), g.BI)

    val bfuRdAddrOri = slave Flow Vec(UInt(g.BankAddrWidth bits), g.radix)
    val bfuRdIdxOri = slave Flow Vec(UInt(g.BankIndexWidth bits), g.BI)
    val NttWriteBack = slave Flow (Vec(Bits(g.width bits), g.BI))

    val MemIfRe = out Bool ()
    val MemIfRdAddr = out Vec (UInt(g.BankAddrWidth bits), g.BI)
    val bankIdxTrans = out Vec (UInt(g.BankIndexWidth bits), g.BI)
    val MemIfWe = out Bool ()
    val MemIfWrAddr = out Vec (UInt(g.BankAddrWidth bits), g.BI)
    val MemIfWrData = out Vec (Bits(g.width bits), g.BI)
  }

  val memInArb = new memInArb(g)
  val memWbArb = new memWritebackArb(g)

  memInArb.io.addrOri := io.ctrl.isCal ? io.bfuRdAddrOri.payload | io.outsideAddrOri.payload
  memInArb.io.idxOri := io.ctrl.isCal ? io.bfuRdIdxOri.payload | io.outsideIdxOri.payload
  memInArb.io.dataOri := io.outsideWrDataArray.payload

  io.bankIdxTrans := memInArb.io.bankIdxTrans

  memWbArb.io.isNtt := io.ctrl.isNtt
  memWbArb.io.dataWb := io.NttWriteBack.payload
  memWbArb.io.addrWb := RegNext(memInArb.io.addrOri_r1)
  memWbArb.io.idxWb := memInArb.io.shuffleIdxTrans

  for (i <- 0 until g.BI) {
    io.MemIfWrAddr(i) := RegNext(
      io.ctrl.isCal ? (memWbArb.io
        .addrWbSelMem(i) ? (memWbArb.io.addrWbMem(1)) | (memWbArb.io.addrWbMem(0))) |
        (memInArb.io.addrSel(i) ? (memInArb.io.addrOri_r1(1)) | (memInArb.io.addrOri_r1(0)))
    )
    io.MemIfRdAddr(i) := RegNext(
      memInArb.io.addrSel(i) ? (memInArb.io.addrOri_r1(1)) | (memInArb.io.addrOri_r1(0))
    )
  }

  io.MemIfRe := io.ctrl.isCal ? { Delay(io.bfuRdAddrOri.valid, g.Arbit.DecodeLatency) } | {
    io.ctrl.isOutSideRead ? Delay(io.outsideAddrOri.valid, g.Arbit.DecodeLatency) | False
  }
  io.MemIfWe := io.ctrl.isCal ? Delay(io.NttWriteBack.valid, g.Arbit.DecodeMuxRegLatency) | {
    io.ctrl.isOutSideWrite ? Delay(io.outsideAddrOri.valid, g.Arbit.DecodeLatency) | False
  }
  io.MemIfWrData := RegNext(io.ctrl.isCal ? memWbArb.io.dataWbMem | memInArb.io.dataMem)

}
object memForwardCtrl {
  def apply(
      ctrl: CtrlBus,
      outsideAddrOri: Flow[Vec[UInt]],
      outsideIdxOri: Flow[Vec[UInt]],
      outsideWrDataArray: Flow[Vec[Bits]],
      bfuRdAddrOri: Flow[Vec[UInt]],
      bfuRdIdxOri: Flow[Vec[UInt]],
      NttWriteBack: Flow[Vec[Bits]],
      g: NttCfgParam
  ): memForwardCtrl = {
    val inst = new memForwardCtrl(g)
    import inst._
    io.ctrl := ctrl
    io.outsideAddrOri := outsideAddrOri
    io.outsideIdxOri := outsideIdxOri
    io.outsideWrDataArray := outsideWrDataArray
    io.bfuRdAddrOri := bfuRdAddrOri
    io.bfuRdIdxOri := bfuRdIdxOri
    io.NttWriteBack := NttWriteBack
    inst
  }

}

case class memForwardCtrlOpt(g: NttCfgParam) extends Component {
  val io = new Bundle {
    val ctrl = in(CtrlBus())
    val outsideAddrOri = slave Flow Vec(UInt(g.BankAddrWidth bits), g.radix)
    val outsideIdxOri = slave Flow Vec(UInt(g.BankIndexWidth bits), g.BI)
    val outsideWrDataArray = slave Flow (Bits(g.width bits))

    val bfuRdAddrOri = slave Flow Vec(UInt(g.BankAddrWidth bits), g.radix)
    val bfuRdIdxOri = slave Flow Vec(UInt(g.BankIndexWidth bits), g.BI)
    val NttWriteBack = slave Flow (Vec(Bits(g.width bits), g.BI))

    val MemIfRe = out Vec (Bool(), g.BI)
    val MemIfRdAddr = out Vec (UInt(g.BankAddrWidth bits), g.BI)
    val bankIdxTrans = out Vec (UInt(g.BankIndexWidth bits), g.BI)
    val MemIfWe = out Vec (Bool(), g.BI)
    val MemIfWrAddr = out Vec (UInt(g.BankAddrWidth bits), g.BI)
    val MemIfWrData = out Vec (Bits(g.width bits), g.BI)
  }

  val memInArb = new memInArbOpt(g)
  val memWbArb = new memWritebackArb(g)

  memInArb.io.addrOri := io.ctrl.isCal ? io.bfuRdAddrOri.payload | io.outsideAddrOri.payload
  memInArb.io.idxOri := io.ctrl.isCal ? io.bfuRdIdxOri.payload | io.outsideIdxOri.payload
  memInArb.io.dataOri := io.outsideWrDataArray.payload

  io.bankIdxTrans := memInArb.io.bankIdxTrans

  memWbArb.io.isNtt := io.ctrl.isNtt
  memWbArb.io.dataWb := io.NttWriteBack.payload
  memWbArb.io.addrWb := RegNext(memInArb.io.addrOri_r1)
  memWbArb.io.idxWb := memInArb.io.shuffleIdxTrans

  for (i <- 0 until g.BI) {
    io.MemIfWrAddr(i) := RegNext(
      io.ctrl.isCal ? (memWbArb.io
        .addrWbSelMem(i) ? (memWbArb.io.addrWbMem(1)) | (memWbArb.io.addrWbMem(0))) | memInArb.io.addrOri_r1(0)
    )
    io.MemIfRdAddr(i) := RegNext(
      memInArb.io.addrSel(i) ? (memInArb.io.addrOri_r1(1)) | (memInArb.io.addrOri_r1(0))
    )
  }

  io.MemIfRe.foreach { item =>
    item := io.ctrl.isCal ? { Delay(io.bfuRdAddrOri.valid, g.Arbit.DecodeLatency) } | {
      io.ctrl.isOutSideRead ? Delay(io.outsideAddrOri.valid, g.Arbit.DecodeLatency) | False
    }
  }
  io.MemIfWe := io.ctrl.isCal ? (Delay(io.NttWriteBack.valid, g.Arbit.DecodeMuxRegLatency) #* g.BI).asBools | {
    io.ctrl.isOutSideWrite ? RegNext(memInArb.io.memWe) | ((False #* g.BI).asBools)
  }
  val owMem = Vec(Bits(g.width bits),g.BI)
  owMem.foreach{ item => item := memInArb.io.dataMem}
  io.MemIfWrData := RegNext(io.ctrl.isCal ? memWbArb.io.dataWbMem | owMem)

}
object memForwardCtrlOpt {
  def apply(
      ctrl: CtrlBus,
      outsideAddrOri: Flow[Vec[UInt]],
      outsideIdxOri: Flow[Vec[UInt]],
      outsideWrDataArray: Flow[Bits],
      bfuRdAddrOri: Flow[Vec[UInt]],
      bfuRdIdxOri: Flow[Vec[UInt]],
      NttWriteBack: Flow[Vec[Bits]],
      g: NttCfgParam
  ): memForwardCtrlOpt = {
    val inst = new memForwardCtrlOpt(g)
    import inst._
    io.ctrl := ctrl
    io.outsideAddrOri := outsideAddrOri
    io.outsideIdxOri := outsideIdxOri
    io.outsideWrDataArray := outsideWrDataArray
    io.bfuRdAddrOri := bfuRdAddrOri
    io.bfuRdIdxOri := bfuRdIdxOri
    io.NttWriteBack := NttWriteBack
    inst
  }

}

object memForwardCtrlGenV extends App {
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "NttOpt/rtl/DataPath",
    genLineComments = true
  ).generate(new memForwardCtrl(NttCfgParam(paraNum = 1)))
}

case class memBackwardCtrl(g: NttCfgParam) extends Component {
  val io = new Bundle {
    val ctrl = in(CtrlBus())
    val memIfRdData = slave Flow Vec(Bits(g.width bits), g.BI)
    val idx = in Vec (UInt(g.BankIndexWidth bits), g.BI)
    val outsideRdDataArray = master Flow Vec(Bits(g.width bits), g.BI)
    val nttPayload = master Flow Vec(Bits(g.width bits), g.BI)
  }

  val dataMux = memOutArb(dataIn = io.memIfRdData.payload, idx = io.idx, g = g)
  io.nttPayload.payload := RegNext(dataMux)
  io.nttPayload.valid := RegNext(io.memIfRdData.valid && io.ctrl.isCal)
  io.outsideRdDataArray.payload := RegNext(dataMux)
  io.outsideRdDataArray.valid := RegNext(io.memIfRdData.valid && io.ctrl.isOutSideRead)

}
object memBackwardCtrl {
  def apply(
      ctrl: CtrlBus,
      memIfRdData: Flow[Vec[Bits]],
      idx: Vec[UInt],
      g: NttCfgParam
  ): memBackwardCtrl = {
    val inst = new memBackwardCtrl(g)
    import inst._
    io.ctrl := ctrl
    io.memIfRdData := memIfRdData
    io.idx := idx
    inst
  }

}

object memBackwardCtrlGenV extends App {
  SpinalConfig(
    mode = Verilog,
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp",
    targetDirectory = "NttOpt/rtl/DataPath",
    genLineComments = true
  ).generate(new memBackwardCtrl(NttCfgParam(paraNum = 16)))
}

case class DataPathTop(g: NttCfgParam) extends Component {
  val io = new Bundle {
    val ctrl = in(CtrlBus())
    val outsideAddrOri = slave Flow Vec(UInt(g.BankAddrWidth bits), g.radix)
    val outsideIdxOri = slave Flow Vec(UInt(g.BankIndexWidth bits), g.BI)
    val outsideRdDataArray = master Flow Vec(Bits(g.width bits), g.BI)
    val outsideWrDataArray = slave Flow Vec(Bits(g.width bits), g.BI)

    val bfuRdAddrOri = slave Flow Vec(UInt(g.BankAddrWidth bits), g.radix)
    val bfuRdIdxOri = slave Flow Vec(UInt(g.BankIndexWidth bits), g.BI)
    val NttWriteBack = Array.fill(g.paraNum)(slave Flow DataPayload(g))
    val NttPayload = Array.fill(g.paraNum)(master Flow BfuPayload(g))
    val twBus = slave Flow twPayload(addrWidth = g.twAddrWidth, muxWidth = log2Up(g.paraNum), para = g.paraNum)
  }

  val mem = new memBank(g)
  val mem_rd_IF = Array.fill(g.BI)(new myRamReadOnly(config = myRamConfig(g.width, g.BankAddrWidth)))
  val mem_wr_IF = Array.fill(g.BI)(new myRamWriteOnly(config = myRamConfig(g.width, g.BankAddrWidth)))

  val tw = new Area {
    val rom = new twRom(g)
    rom.io.twBus := Delay(io.twBus, (g.romDealyLatency))
      //    rom.io.twBus := Delay(io.twBus, (g.Arbit.DecodeLatency + g.Arbit.DatDeMuxLatency - g.Arbit.romMuxLatency))
      .addAttribute("srl_style", "srl")
  }

  val fc = memForwardCtrl(g)
  val bc = memBackwardCtrl(g)
  fc.io.ctrl := io.ctrl
  fc.io.outsideAddrOri := io.outsideAddrOri
  fc.io.outsideIdxOri := io.outsideIdxOri
  fc.io.outsideWrDataArray := io.outsideWrDataArray
  fc.io.bfuRdAddrOri := io.bfuRdAddrOri
  fc.io.bfuRdIdxOri := io.bfuRdIdxOri
  val NttWbConvert = Vec(io.NttWriteBack.flatMap { item => Seq(item.payload.A.asBits, item.payload.B.asBits) }.toSeq)
  fc.io.NttWriteBack.payload := NttWbConvert
  fc.io.NttWriteBack.valid := io.NttWriteBack(0).valid

  mem_rd_IF.toSeq.zip(fc.io.MemIfRdAddr).foreach { case (t1, t2) => { t1.rAddr := t2; t1.re := fc.io.MemIfRe } }
  bc.io.memIfRdData.payload.zip(mem_rd_IF.toSeq).foreach { case (t1, t2) => t1 := t2.rData }
  mem_wr_IF.toSeq.zip(fc.io.MemIfWrAddr.zip(fc.io.MemIfWrData)).foreach { case (interface, (addr, data)) =>
    interface.wData := data; interface.wAddr := addr; interface.we := fc.io.MemIfWe
  }

  bc.io.memIfRdData.valid := Delay(fc.io.MemIfRe, g.Arbit.ramLatency)
  val idxTrans = Delay(fc.io.bankIdxTrans, g.Arbit.ramLatency)
  bc.io.ctrl := io.ctrl
  bc.io.idx := idxTrans
  io.outsideRdDataArray := bc.io.outsideRdDataArray
  io.NttPayload.toSeq.zip(bc.io.nttPayload.payload.grouped(2).toSeq).foreach { case (t1, t2) =>
    t1.A := t2(0).asUInt; t1.B := t2(1).asUInt
    t1.valid := bc.io.nttPayload.valid
  }
  io.NttPayload.toSeq.zip(tw.rom.io.twData).foreach { case (t1, t2) => t1.payload.Tw := t2 }
  mem.io.memIf.zip(mem_rd_IF.toSeq).foreach { case (t1, t2) => t1 << t2 }
  mem.io.memIf.zip(mem_wr_IF.toSeq).foreach { case (t1, t2) => t1 << t2 }
}

case class DataPathTopOpt(g: NttCfgParam) extends Component {
  val io = new Bundle {
    val ctrl = in(CtrlBus())
    val outsideAddrOri = slave Flow Vec(UInt(g.BankAddrWidth bits), g.radix)
    val outsideIdxOri = slave Flow Vec(UInt(g.BankIndexWidth bits), g.BI)
    val outsideRdDataArray = master Flow Vec(Bits(g.width bits), g.BI)
    val outsideWrDataArray = slave Flow Bits(g.width bits)

    val bfuRdAddrOri = slave Flow Vec(UInt(g.BankAddrWidth bits), g.radix)
    val bfuRdIdxOri = slave Flow Vec(UInt(g.BankIndexWidth bits), g.BI)
    val NttWriteBack = Array.fill(g.paraNum)(slave Flow DataPayload(g))
    val NttPayload = Array.fill(g.paraNum)(master Flow BfuPayload(g))
    val twBus = slave Flow twPayload(addrWidth = g.twAddrWidth, muxWidth = log2Up(g.paraNum), para = g.paraNum)
  }

  val mem = new memBank(g)
  val mem_rd_IF = Array.fill(g.BI)(new myRamReadOnly(config = myRamConfig(g.width, g.BankAddrWidth)))
  val mem_wr_IF = Array.fill(g.BI)(new myRamWriteOnly(config = myRamConfig(g.width, g.BankAddrWidth)))

  val tw = new Area {
    val rom = new twRom(g)
    rom.io.twBus := Delay(io.twBus, (g.romDealyLatency))
      //    rom.io.twBus := Delay(io.twBus, (g.Arbit.DecodeLatency + g.Arbit.DatDeMuxLatency - g.Arbit.romMuxLatency))
      .addAttribute("srl_style", "srl")
  }

  val fc = memForwardCtrlOpt(g)
  val bc = memBackwardCtrl(g)
  fc.io.ctrl := io.ctrl
  fc.io.outsideAddrOri := io.outsideAddrOri
  fc.io.outsideIdxOri := io.outsideIdxOri
  fc.io.outsideWrDataArray := io.outsideWrDataArray
  fc.io.bfuRdAddrOri := io.bfuRdAddrOri
  fc.io.bfuRdIdxOri := io.bfuRdIdxOri
  val NttWbConvert = Vec(io.NttWriteBack.flatMap { item => Seq(item.payload.A.asBits, item.payload.B.asBits) }.toSeq)
  fc.io.NttWriteBack.payload := NttWbConvert
  fc.io.NttWriteBack.valid := io.NttWriteBack(0).valid

  mem_rd_IF.toSeq.zip(fc.io.MemIfRdAddr).foreach { case (t1, t2) => t1.rAddr := t2 }
  mem_rd_IF.toSeq.zip(fc.io.MemIfRe).foreach { case (t1, t2) => t1.re := t2 }
  bc.io.memIfRdData.payload.zip(mem_rd_IF.toSeq).foreach { case (t1, t2) => t1 := t2.rData }
//  mem_wr_IF.toSeq.zip(fc.io.MemIfWrAddr.zip(fc.io.MemIfWrData)).foreach { case (interface, (addr, data)) =>
//    interface.wData := data; interface.wAddr := addr; interface.we := fc.io.MemIfWe
//  }
  mem_wr_IF.toSeq.zip((fc.io.MemIfWrAddr).zip(fc.io.MemIfWrData).zip(fc.io.MemIfWe)).foreach {
    case (interface, ((addr, data), we)) =>
      interface.wData := data; interface.wAddr := addr; interface.we := we
  }

  bc.io.memIfRdData.valid := Delay(fc.io.MemIfRe(0), g.Arbit.ramLatency)
  val idxTrans = Delay(fc.io.bankIdxTrans, g.Arbit.ramLatency)
  bc.io.ctrl := io.ctrl
  bc.io.idx := idxTrans
  io.outsideRdDataArray := bc.io.outsideRdDataArray
  io.NttPayload.toSeq.zip(bc.io.nttPayload.payload.grouped(2).toSeq).foreach { case (t1, t2) =>
    t1.A := t2(0).asUInt; t1.B := t2(1).asUInt
    t1.valid := bc.io.nttPayload.valid
  }
  io.NttPayload.toSeq.zip(tw.rom.io.twData).foreach { case (t1, t2) => t1.payload.Tw := t2 }
  mem.io.memIf.zip(mem_rd_IF.toSeq).foreach { case (t1, t2) => t1 << t2 }
  mem.io.memIf.zip(mem_wr_IF.toSeq).foreach { case (t1, t2) => t1 << t2 }
}
