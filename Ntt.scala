package Ntt
import spinal.core._
import spinal.lib._
import spinal.lib.eda.bench.Rtl
import spinal.lib.eda.xilinx.VivadoFlow
import test._
import NttCfg._
import myRam._


//case class nttPayload(g: nttConfig) extends Bundle {
//  val data0 = Bits(g.dataWidth bits)
//  val data1 = Bits(g.dataWidth bits)
//  val tw = Bits(g.dataWidth bits)
//}

//case class nttPayload(g:nttConfig) extends Bundle {
//  val data0 = Bits(g.dataWidth bits)
//  val data1 = Bits(g.dataWidth bits)
//}



case class addrBus(g: NttCfg2414) extends Bundle {
  val waddr = Vec(UInt(g.BankAddrWidth bits), g.paraNum * 2)
  val raddr = Vec(UInt(g.BankAddrWidth bits), g.paraNum * 2)
}

case class ctrl(g: NttCfg2414) extends Component {
  val io = new Bundle {
    val memIf = Array.fill(g.paraNum*2)(master(myRamIF(config = myRamConfig(g.width,g.BankAddrWidth))))
    val idle = out Bool()
  }

}



