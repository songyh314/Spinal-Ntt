module bram (
    input  clk,
    input wea,
    input wire [6 : 0] addra,
    input wire [23 : 0] dina,
    input wire enb,
    input wire [6 : 0] addrb,
    output wire [23 : 0] doutb
);


mem mem_inst (
  .clka(clk),    // input wire clka
  .wea(wea),      // input wire [0 : 0] wea
  .addra(addra),  // input wire [6 : 0] addra
  .dina(dina),    // input wire [23 : 0] dina
  .clkb(clk),    // input wire clkb
  .enb(enb),      // input wire enb
  .addrb(addrb),  // input wire [6 : 0] addrb
  .doutb(doutb)  // output wire [23 : 0] doutb
);
endmodule