module mul (
    input wire [23:0] A,
    input wire [23:0] B,
    input wire CLK,
    output wire [47:0] P
);
mult_gen_0 mul_inst (
  .CLK(CLK),  // input wire CLK
  .A(A),      // input wire [23 : 0] A
  .B(B),      // input wire [23 : 0] B
  .P(P)      // output wire [47 : 0] P
);
endmodule
