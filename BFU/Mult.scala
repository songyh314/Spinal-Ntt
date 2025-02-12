package Ntt.BFU

import Ntt.NttCfg.{NttCfgParam, multPayload}
import spinal.core._
import spinal.lib._
import spinal.lib.eda.bench.Rtl
import spinal.lib.eda.xilinx.VivadoFlow


case class mul() extends BlackBox {
  val io = new Bundle {
    val CLK = in Bool ()
    val A = in UInt  (24 bits)
    val B = in UInt  (24 bits)
    val P = out UInt  (48 bits)
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
case class mulBlackBox(width:Int = 24,device:String = "9eg") extends BlackBox {
  addGeneric("width",width)
  val io = new Bundle {
    val CLK = in Bool ()
    val A = in UInt  (width bits)
    val B = in UInt  (width bits)
    val P = out UInt  (2*width bits)
  }
  noIoPrefix()
  mapClockDomain(clock = io.CLK)
  setInlineVerilog(s"""
                      |module mulBlackBox  #(
                      |   parameter width = $width
                      |)(
                      |    input wire [${width-1}:0] A,
                      |    input wire [${width-1}:0] B,
                      |    input wire CLK,
                      |    output wire [${(2*width)-1}:0] P
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
  def apply(A: UInt, B: UInt, g:NttCfgParam): UInt = {
    val umul = new mulBlackBox(g.width, g.Bfu.device)
    umul.io.A := A
    umul.io.B := B
    umul.io.P
  }
}

class xMul(g: NttCfgParam) extends Component {
//  this.setDefinitionName(s"mul")
  val io = new Bundle {
    val dataIn = slave Flow(multPayload(g))
//    val dataA = in UInt (g.width bits)
//    val dataB = in UInt (g.width bits)
//    val dataP = out UInt (2 * g.width bits)
    val dataOut = master Flow(UInt (2 * g.width bits))
  }
  io.dataOut.valid := Delay(io.dataIn.valid,4)
  io.dataOut.payload := mul(io.dataIn.data, io.dataIn.tw).io.P
}

case class Mult(g: NttCfgParam) extends Component {
  val io = new Bundle {
    val dataIn = slave Flow (multPayload(g))
    val dataOut = master Flow (UInt(2 * g.width bits))
  }
  if (g.useMulIP == true){
    io.dataOut.valid := Delay(io.dataIn.valid,4)
    io.dataOut.payload := mulBlackBox(io.dataIn.data, io.dataIn.tw,g)
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


object MultGenVerilog extends App {
  SpinalConfig(
    mode = Verilog,
    targetDirectory = "./rtl/Ntt/Mult",
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp"
  ).generate(new xMul(NttCfgParam()))
}

object MultVivadoFlow extends App {
  val workspace = "./vivado_prj/Ntt/FastMod"
  val vivadopath = "/opt/Xilinx/Vivado/2023.1/bin"
  val family = "Zynq UltraScale+ MPSoCS"
  val device = "xczu9eg-ffvb1156-2-i"
  val frequency = 300 MHz
  val cpu = 8

  val xcix = "/PRJ/SpinalHDL-prj/PRJ/myTest/test/hw/spinal/Ntt/xilinx_ip/mult_gen_0.xcix"
  val top = "xMul"
  val paths = Seq("/PRJ/SpinalHDL-prj/PRJ/myTest/test/rtl/Ntt/Mult/xMul.v",
    "/PRJ/SpinalHDL-prj/PRJ/myTest/test/hw/spinal/Ntt/xilinx_ip/mul.v"
  )

  val rtl = new Rtl {
    /** Name */
    override def getName(): String = top
    override def getRtlPaths(): Seq[String] = paths
  }

  val flow = VivadoFlow(vivadopath, workspace, rtl, family, device, frequency, cpu, xcix)
  println(s"${family} -> ${(flow.getFMax / 1e6).toInt} MHz ${flow.getArea} ")
}
