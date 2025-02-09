//package Ntt.DataPath
//
//import Ntt.NttCfg._
//import myRam._
//import spinal.core._
//import spinal.core.sim._
//import spinal.lib._
//import spinal.core.BlackBox
//import spinal.lib.eda.bench.Rtl
//import spinal.lib.eda.xilinx.VivadoFlow
//
//import scala.collection.mutable.ArrayBuffer
//
//case class romIP(nttPoints: Int, depth: Int, romWidth: Int, paraNum: Int) extends BlackBox {
//  val addrWidth = log2Up(depth)
//  val io = new Bundle {
//    val clk = in Bool ()
//    val addr = in UInt (addrWidth bits)
//    val dout = out Bits (romWidth bits)
//  }
//  noIoPrefix()
//  val moduleName = s"twRom${nttPoints}p${paraNum}"
//  setInlineVerilog(s"""
//       |module romIP (
//       |    input clk,
//       |    input [${addrWidth - 1}:0] addr,
//       |    output [${romWidth - 1}:0] dout
//       |);
//       |${moduleName} inst (
//       |    .clka(clk),
//       |    .addra(addr),
//       |    .douta(dout)
//       |  );
//       |endmodule
//       |""".stripMargin)
//  mapClockDomain(clock = io.clk)
//}
//case class romIPWrap(nttPoints: Int, width: Int, paraNum: Int) extends Component {
//  val depth = nttPoints / paraNum
//  val romWidth = width * paraNum
//  val addrWidth = log2Up(depth)
//  val io = new Bundle {
//    val addr = in UInt (addrWidth bits)
//    val dout = out Bits (romWidth bits)
//  }
//  val rom = new romIP(nttPoints, depth, romWidth, paraNum)
//  rom.io.addr := io.addr
//  io.dout := rom.io.dout
//}
//
//object romIP_Wrap_GenV extends App {
//  SpinalConfig(
//    mode = Verilog,
//    nameWhenByFile = false,
//    anonymSignalPrefix = "tmp",
//    targetDirectory = "NttOpt/rtl/DataPath/rom"
//  )
//    .generate(new romIPWrap(nttPoints = 1024, width = 24, paraNum = 4))
//}
//
//object romIP_Wrap_Sim extends App {
//  val path = ArrayBuffer("/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/IP/rom")
//  val coePath = ArrayBuffer("/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/IP/rom/tw1024.coe")
//  val dut = SimConfig.withXSim.withWave
//    .withXilinxDevice("xc7vx485tffg1157-1")
//    .withXSimSourcesPaths(path, path)
//    .workspacePath("NttOpt/sim/rom")
//
//    .compile(new romIPWrap(nttPoints = 1024, width = 24, paraNum = 4))
//  val period = 10
//  dut.doSim("test") { dut =>
//    SimTimeout(1000 * period)
//    import dut._
//    io.addr #= 0
//    clockDomain.forkStimulus(period)
//    clockDomain.waitSampling(10)
//    for (i <- 0 until 256) {
//      io.addr #= i
//      clockDomain.waitSampling()
//    }
//    clockDomain.waitSampling(10)
//    simSuccess()
//  }
//
//}
//
//case class romBlackBox(addrW:Int, W:Int, depth:Int, path:String) extends BlackBox {
//  addGeneric("width",W)
//  addGeneric("depth",depth)
//  val io = new Bundle{
//    val clk = in Bool()
//    val addr = in UInt(addrW bits)
//    val data = out Bits(W bits)
//  }
//  noIoPrefix()
//  //  val path = "/PRJ/SpinalHDL-prj/PRJ/myTest/test/NttOpt/rtl/NttTop/NttTop.v_toplevel_ctrlMem_dut_tw_rom_rom.bin"
//  setInlineVerilog(
//    s"""
//       |module romBlackBox #(
//       |    parameter width = $W,
//       |    parameter depth = $depth
//       |) (
//       |    input  wire                     clk,
//       |    input  wire [$$clog2(depth)-1:0] addr,
//       |    output wire [        width-1:0] data
//       |);
//       |  (* ram_style = "block" *) reg [width-1:0] mem[depth-1:0];
//       |  initial begin
//       |    $$readmemb("$path", mem);
//       |  end
//       |  reg [width-1:0] data_reg = 'd0;
//       |  always @(posedge clk) begin
//       |    data_reg <= mem[addr];
//       |  end
//       |endmodule
//       |""".stripMargin)
//  mapClockDomain(clock = io.clk)
//}
//case class romWrap(g:NttCfg2414) extends Component{
//  val io = new Bundle{
//    val addr = in UInt(g.twAddrWidth bits)
//    val data = out Bits(g.twWidth bits)
//  }
//  val m = new romBlackBox(addrW = g.twAddrWidth, W = g.twWidth, depth = g.twNum,path = {
//    if (g.nttPoint == 1024 && g.paraNum == 8){g.tw1024p8_path} else {null}
//  })
//  m.io.addr := io.addr
//  io.data := m.io.data
//}
//
//object romWrapGenV extends App {
//  SpinalConfig(mode = Verilog, targetDirectory = "./rtl/Ntt/memBank").generate(new romWrap(NttCfg2414()))
//}
//
//case class twRomWrap(g: NttCfg2414) extends Component {
//  val io = new Bundle {
//    val twBus = slave Flow twPayload(
//      addrWidth = g.twAddrWidth,
//      muxWidth = log2Up(g.paraNum),
//      para = g.paraNum
//    )
//    val twData = out Vec (UInt(g.width bits), g.paraNum)
//  }
//  val rom = new romWrap(g)
//  rom.io.addr := io.twBus.twAddr
//  val muxReg = RegNextWhen(io.twBus.payload.twMux, io.twBus.valid)
//  val readSeq = rom.io.data
//  val sliceSeq = readSeq.subdivideIn(g.paraNum slices).map(_.asUInt)
//  def readMux(sel: UInt): UInt = {
//    val ret = RegNext(sliceSeq.read(sel))
//    ret
//  }
//  io.twData.zip(muxReg).foreach { case (t1, t2) => t1 := readMux(t2) }
//}
//
//
//object twRomWrapGenV extends App {
//  SpinalConfig(
//    mode = Verilog,
//    nameWhenByFile = false,
//    anonymSignalPrefix = "tmp",
//    targetDirectory = "NttOpt/rtl/DataPath/rom",
//    genLineComments = true
//  ).generate(new twRomWrap(NttCfg2414(nttPoint = 1024, paraNum = 4)))
//}