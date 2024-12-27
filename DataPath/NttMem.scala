package Ntt.DataPath

import Ntt.NttCfg.NttCfg2414
import myRam._
import spinal.core._
import spinal.lib._
import spinal.lib.eda.bench.Rtl
import spinal.lib.eda.xilinx.VivadoFlow

case class DataMem(width:Int, addrWidth: Int, latency:Int=1) extends Component {
  val io = new Bundle {
    val memIf = slave(myRamIF(config = myRamConfig(width,addrWidth,latency)))
  }
  val mem = Mem(Bits(width bits), 1<<(addrWidth)).addAttribute("ram_style","block")
  if (latency == 1){
    mem.write(io.memIf.wAddr, io.memIf.wData, io.memIf.we)
    io.memIf.rData := mem.readSync(io.memIf.rAddr, io.memIf.re)
  } else if (latency == 2){
    val wrAddr = RegNext(io.memIf.wAddr)
    val wData = RegNext(io.memIf.wData)
    val we = RegNext(io.memIf.we)
    val rdAddr = RegNext(io.memIf.rAddr)
    val re = RegNext(io.memIf.re)
    mem.write(wrAddr, wData, we)
    io.memIf.rData := mem.readSync(rdAddr, re)
  }

}

//case class memTry(g: NttCfg2414) extends Component {
//  val io = new Bundle {
//    val memIf = slave(myRamIF(config = myRamConfig(g.width, g.BankAddrWidth)))
//  }
//  val memInst = new DataMem(g)
//  io.memIf >> memInst.io.memIf
//}

case class memBank(g: NttCfg2414) extends Component {
  val io = new Bundle {
    val memIf = Array.fill(g.BI)(slave(myRamIF(config = myRamConfig(g.width, g.BankAddrWidth))))
  }
  val memArray = Array.fill(g.BI)(new DataMem(g.width, g.BankAddrWidth, g.ramLatency))
  io.memIf.zip(memArray.map(_.io.memIf)).foreach { case (t, t1) => t >> t1 }
}
//case class memCluster(g: NttCfg2414) extends Component {
//  val io = new Bundle{
//    val
//  }
//}

object memBankGenV extends App {
  SpinalConfig(mode = Verilog, targetDirectory = "./rtl/Ntt/memBank").generate(new memBank(NttCfg2414()))
}

object MemBankVivadoFlow extends App{
  val workspace = "./vivado_prj/Ntt/DataPath/Mem/MemBank"
  val vivadopath = "/opt/Xilinx/Vivado/2023.1/bin"
  val family = "Zynq UltraScale+ MPSoCS"
  val device = "xczu9eg-ffvb1156-2-i"
  val frequency = 300 MHz
  val cpu = 12
  val rtl = new Rtl {

    /** Name */
    override def getName(): String = "memBank"
    override def getRtlPath(): String = "/PRJ/SpinalHDL-prj/PRJ/myTest/test/rtl/Ntt/memBank/memBank.v"
  }

  val flow = VivadoFlow(vivadopath, workspace, rtl, family, device, frequency, cpu)
  println(s"${family} -> ${(flow.getFMax / 1e6).toInt} MHz ${flow.getArea} ")
}
