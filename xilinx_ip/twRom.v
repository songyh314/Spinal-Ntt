module twRom (
    input clk,
    input [7:0] addr,
    output [95:0] dout
);
twRom1024p4 inst (
    .clka(clk),    // input wire clka
    .addra(addr),  // input wire [7 : 0] addra
    .douta(dout)  // output wire [95 : 0] douta
  );
endmodule