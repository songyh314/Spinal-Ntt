package Ntt

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.eda.bench.Rtl
import spinal.lib.eda.xilinx.VivadoFlow
import test._
import NttCfg._
import BFU._
import CTRL._
import DataPath._
import spinal.core.Component.push

case class simpleDut(g: NttCfgParam) extends Component {
  val io = new Bundle {
    val sig = in Bool ()
    val idle = out Bool () setAsReg () init False
  }
  io.idle := Delay(io.sig, g.nttPoint)
}
object simpleDutGenV extends App {
  SpinalConfig(mode = Verilog, nameWhenByFile = false, anonymSignalPrefix = "tmp", targetDirectory = "NttOpt/rtl/Top")
    .generate(new simpleDut(NttCfgParam()))
}

class TestTop(g: NttCfgParam) extends Component {
  val io = new Bundle {
    val test_start = in Bool ()
    val fin = out Bool ()
    val error = out Bool ()
    val pass = out Bool ()
    val flush = in Bool ()
  }
  val constSeq = (0 until g.BI).map { U(_, log2Up(g.BI) bits) }
//  val dut = new simpleDut(g)
  val dut = new NttTop(g)
  val ctrlReg = Reg(CtrlBus())
  ctrlReg.isNtt.setName("isNtt") init False
  ctrlReg.isCal.setName("isCal") init False
  ctrlReg.isOutSideRead.setName("isOutSideRead") init False
  ctrlReg.isOutSideWrite.setName("isOutSideWrite") init False
  val fsm = new StateMachine {
    val cnt_reg = Reg(UInt(log2Up(g.nttPoint / g.BI) bits)) init U(0)
    val delay_reg = Reg(UInt(log2Up(32) bits)) init U(0)
    val cntEn = Reg(Bool()) init False
    val cntClr = Reg(Bool()) init False
    val IDLE = makeInstantEntry()
    val WRITE_CMD = new State()
    val WRITE = new State()
    val READ_CMD = new State()
    val READ = new State()
    val NTT_CMD = new State()
    val NTT_CAL = new State()
    val INTT_CMD = new State()
    val INTT_CAL = new State()
    val Fin = new State()
    val DELAY1: State = new StateDelay(32) { whenCompleted(goto(NTT_CMD)) }
    val DELAY2: State = new StateDelay(32) { whenCompleted(goto(INTT_CMD)) }
    val DELAY3: State = new StateDelay(32) { whenCompleted(goto(READ_CMD)) }
    val DELAY4: State = new StateDelay(32) { whenCompleted(goto(Fin)) }
    val cnt_limit = (g.nttPoint / g.BI) - 1
    when(cntClr) {
      cnt_reg := 0
    } elsewhen (cnt_reg =/= cnt_limit) {
      when(cntEn) { cnt_reg := cnt_reg + 1 }
    }

    IDLE
      .whenIsActive {
        driveCtrl(ctrlReg, B"0000")
        when(io.test_start) { goto(WRITE_CMD) }
      }
    WRITE_CMD
      .whenIsActive {
        driveCtrl(ctrlReg, B"0001")
        goto(WRITE)
      }
    WRITE
      .onEntry {
        cntEn := True
        cntClr := False
      }
      .whenIsActive {
        when(cnt_reg === cnt_limit) { goto(DELAY1) }
      }
      .onExit {
        cntEn := False
        cntClr := True
      }

    NTT_CMD
      .onEntry(driveCtrl(ctrlReg, B"1100"))
      .whenIsActive {
        goto(NTT_CAL)
      }
    NTT_CAL
      .whenIsActive {
        when(dut.io.idle && ctrlReg.isCal) { goto(DELAY2) }
      }

    INTT_CMD
      .onEntry(driveCtrl(ctrlReg, B"0100"))
      .whenIsActive {
        goto(INTT_CAL)
      }
    INTT_CAL
      .whenIsActive {
        when(dut.io.idle && ctrlReg.isCal) { goto(DELAY3) }
      }

    READ_CMD
      .whenIsActive {
        driveCtrl(ctrlReg, B"0010")
        goto(READ)
      }
    READ
      .onEntry {
        cntEn := True
        cntClr := False
      }
      .whenIsActive {
        when(cnt_reg === cnt_limit) { goto(DELAY4) }
      }
      .onExit {
        cntEn := False
        cntClr := True
      }
    Fin.whenIsActive(goto(IDLE))
  }

  val rw_valid = fsm.isActive(fsm.WRITE) || fsm.isActive(fsm.READ)
  io.fin := fsm.isActive(fsm.Fin)
  dut.io.ctrl := ctrlReg
  dut.io.start := fsm.isEntering(fsm.NTT_CAL) || fsm.isEntering(fsm.INTT_CAL)
  dut.io.outsideAddrOri.payload.foreach { item => item := fsm.cnt_reg }
  dut.io.outsideAddrOri.valid := rw_valid
  dut.io.outsideIdxOri.payload.zip(constSeq).foreach { case (t1, t2) => t1 := t2 }
  dut.io.outsideIdxOri.valid := rw_valid
  dut.io.outsideWrDataArray.payload.zip(constSeq).foreach { case (t1, t2) => t1 := Cat(fsm.cnt_reg, t2).resized }
  dut.io.outsideWrDataArray.valid := fsm.isActive(fsm.WRITE)

  val verify = new Area {
//    val resMem = Mem(Bits(g.width * g.BI bits), (g.nttPoint / g.BI))
//    val refMem = Mem(Bits(g.width * g.BI bits), (g.nttPoint / g.BI))
//    val refVec = Cat(dut.io.outsideWrDataArray.payload)
//    val resVec = Cat(dut.io.outsideRdDataArray.payload)
    val resStream = Stream(cloneOf(dut.io.outsideRdDataArray.payload))
    resStream.valid := dut.io.outsideRdDataArray.valid
    resStream.payload := dut.io.outsideRdDataArray.payload
    val refStream = Stream(cloneOf(dut.io.outsideWrDataArray.payload))
    refStream.valid := dut.io.outsideWrDataArray.valid
    refStream.payload := dut.io.outsideWrDataArray.payload
    val resFifo = StreamFifo(dataType = cloneOf(dut.io.outsideRdDataArray.payload), depth = g.counter + 32)
    val refFifo = StreamFifo(dataType = cloneOf(dut.io.outsideWrDataArray.payload), depth = g.counter + 32)
    resFifo.io.push << resStream
    refFifo.io.push << refStream

    val resPop = Stream(cloneOf(dut.io.outsideRdDataArray.payload))
    val refPop = Stream(cloneOf(dut.io.outsideRdDataArray.payload))

    val vfsm = new StateMachine {
      val IDLE = makeInstantEntry()
      val Fill = new State()
      val Comp = new State()
      val Error = new State()
      val Pass = new State()
      val pop_rdy = isActive(Comp)
      val pop_cnt = Reg(UInt(log2Up(g.counter) bits)) init U(0)
      resPop.ready := pop_rdy
      refPop.ready := pop_rdy
      resPop << resFifo.io.pop
      refPop << refFifo.io.pop
      IDLE.whenIsActive {
        when(io.test_start) { goto(Fill) }
      }
      Fill.whenIsActive { when(fsm.isActive(fsm.Fin)) { goto(Comp) } }
      Comp.whenIsActive {
        when(resFifo.io.pop.fire && refFifo.io.pop.fire) {
          when(resPop.payload =/= refPop.payload) { goto(Error) } elsewhen (pop_cnt === pop_cnt.maxValue) { goto(Pass) }
          pop_cnt := pop_cnt + 1
        }
      }
      Error.whenIsActive(when(io.flush) { goto(IDLE) })
      Pass.whenIsActive(when(io.flush) { goto(IDLE) })
    }
  }
  io.error := verify.vfsm.isActive(verify.vfsm.Error)
  io.pass := verify.vfsm.isActive(verify.vfsm.Pass)
}

object TestTopGenV extends App {
  SpinalConfig(mode = Verilog, nameWhenByFile = false, anonymSignalPrefix = "tmp", targetDirectory = "NttOpt/rtl/Top")
    .generate(new TestTop(NttCfgParam()))
}
