package Ntt.BFU

import Ntt.NttCfg._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.eda.bench.Rtl
import spinal.lib.eda.xilinx.VivadoFlow

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

case class mul() extends BlackBox {
  val io = new Bundle {
    val CLK = in Bool ()
    val A = in UInt (24 bits)
    val B = in UInt (24 bits)
    val P = out UInt (48 bits)
  }
  noIoPrefix()
  mapClockDomain(clock = io.CLK)
  addRTLPath("/PRJ/SpinalHDL-prj/PRJ/myTest/test/hw/spinal/Ntt/xilinx_ip/mul.v")
}
object mul {
  def apply(A: UInt, B: UInt): mul = {
    val umul = new mul()
    umul.io.A := A
    umul.io.B := B
    umul
  }
}
case class mulBlackBox(width: Int = 24, device: String = "9eg") extends BlackBox {
  addGeneric("width", width)
  val io = new Bundle {
    val CLK = in Bool ()
    val A = in UInt (width bits)
    val B = in UInt (width bits)
    val P = out UInt (2 * width bits)
  }
  noIoPrefix()
  mapClockDomain(clock = io.CLK)
  setInlineVerilog(s"""
                      |module mulBlackBox  #(
                      |   parameter width = $width
                      |)(
                      |    input wire [${width - 1}:0] A,
                      |    input wire [${width - 1}:0] B,
                      |    input wire CLK,
                      |    output wire [${(2 * width) - 1}:0] P
                      |);
                      |mult_w${width}_${device} mul_inst (
                      |  .CLK(CLK),
                      |  .A(A),
                      |  .B(B),
                      |  .P(P)
                      |);
                      |endmodule
                      |""".stripMargin)
}
object mulBlackBox {
  def apply(A: UInt, B: UInt, g: NttCfgParam): UInt = {
    val umul = new mulBlackBox(g.Bfu.dspWidth, g.Bfu.device)
    umul.io.A := A
    umul.io.B := B
    umul.io.P
  }
}
case class signedMulBlackBox(width: Int = 24, device: String = "9eg") extends BlackBox {
  addGeneric("width", width)
  val io = new Bundle {
    val CLK = in Bool ()
    val A = in SInt (width bits)
    val B = in SInt (width bits)
    val P = out SInt (2 * width bits)
  }
  noIoPrefix()
  mapClockDomain(clock = io.CLK)
  setInlineVerilog(s"""
                      |module signedMulBlackBox  #(
                      |   parameter width = $width
                      |)(
                      |    input wire [${width - 1}:0] A,
                      |    input wire [${width - 1}:0] B,
                      |    input wire CLK,
                      |    output wire [${(2 * width) - 1}:0] P
                      |);
                      |mult_w${width}_${device}_s mul_inst (
                      |  .CLK(CLK),
                      |  .A(A),
                      |  .B(B),
                      |  .P(P)
                      |);
                      |endmodule
                      |""".stripMargin)
}
object signedMulBlackBox {
  def apply(A: SInt, B: SInt, g: NttCfgParam): SInt = {
    val umul = new signedMulBlackBox(g.Bfu.dspWidth + 1, g.Bfu.device)
    umul.io.A := A
    umul.io.B := B
    umul.io.P
  }
}

class xMul(g: NttCfgParam) extends Component {
//  this.setDefinitionName(s"mul")
  val io = new Bundle {
    val dataIn = slave Flow (multPayload(g))
//    val dataA = in UInt (g.width bits)
//    val dataB = in UInt (g.width bits)
//    val dataP = out UInt (2 * g.width bits)
    val dataOut = master Flow (UInt(2 * g.width bits))
  }
  io.dataOut.valid := Delay(io.dataIn.valid, 4)
  io.dataOut.payload := mul(io.dataIn.data, io.dataIn.tw).io.P
}

case class spiltMult(g: NttCfgParam) extends Component {
  val io = new Bundle {
    val dataIn = slave Flow (multPayload(g))
    val dataOut = master Flow (UInt(2 * g.width bits))
  }
  val A = io.dataIn.payload.data.subdivideIn(2 slices)
  val B = io.dataIn.payload.tw.subdivideIn(2 slices)
  val mul_A1B1 = mulBlackBox(A(1), B(1), g)
  val mul_A0B0 = mulBlackBox(A(0), B(0), g)
  val deltaA = Reg(SInt(g.Bfu.dspWidth + 1 bits))
  val deltaB = Reg(SInt(g.Bfu.dspWidth + 1 bits))
  deltaA := (A(1) -^ A(0)).asSInt.resized
  deltaB := (B(1) -^ B(0)).asSInt.resized
  val deltaMul = signedMulBlackBox(deltaA, deltaB, g)
  val mulSumSt1 = Reg(UInt(2 * (g.Bfu.dspWidth) + 1 bits))
  val mulSubSt2 = Reg(UInt(2 * (g.Bfu.dspWidth) + 1 bits))
  mulSumSt1 := (mul_A1B1 +^ mul_A0B0)
  mulSubSt2 := (mulSumSt1.asSInt - deltaMul).asUInt.resized
  val Res = Reg(cloneOf(io.dataOut.payload))
  Res := (Delay(mul_A1B1, 2) << g.width) + (mulSubSt2 << g.Bfu.dspWidth) + Delay(mul_A0B0, 2)
  io.dataOut.payload := Res
  io.dataOut.valid := Delay(io.dataIn.valid, g.Bfu.MultLatency)
}
object spiltMult {
  def apply(dataIn: Flow[multPayload], g: NttCfgParam): Flow[UInt] = {
    val dut = new spiltMult(g)
    dut.io.dataIn := dataIn
    dut.io.dataOut
  }
}

class Mult(g: NttCfgParam) extends Component {
  val io = new Bundle {
    val dataIn = slave Flow (multPayload(g))
    val dataOut = master Flow (UInt(2 * g.width bits))
  }
  if (g.useMulIP) {
    if (g.Bfu.spiltMul) {
      io.dataOut := spiltMult(io.dataIn, g)
    } else {
      io.dataOut.valid := Delay(io.dataIn.valid, g.Bfu.MultLatency)
      io.dataOut.payload := mulBlackBox(io.dataIn.data, io.dataIn.tw, g)
    }
//    io.dataOut.payload := mul(io.dataIn.data, io.dataIn.tw).io.P
  } else {
    val dataReg = RegNext(io.dataIn.payload.data) init U(0)
    val twReg = RegNext(io.dataIn.payload.tw) init U(0)
    val Mreg = Reg(UInt(2 * g.width bits))
    Mreg := dataReg * twReg
    val Preg = RegNext(Mreg)
    val Valid = Delay(io.dataIn.valid, LatencyAnalysis(io.dataIn.payload.data, Preg))
    io.dataOut.payload := Preg
    io.dataOut.valid := Valid
  }
}
