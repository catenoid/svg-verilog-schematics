
`timescale 1ns / 1ns


module comparator0 (in0, out0);
input [7:0] in0;
output out0;
assign out0 = (in0 >5) ? 1 : 0;
endmodule

module source (count, out0, out1);
input [7:0] count;
output [7:0] out0;
output [7:0] out1;
assign out0 = count;
assign out1 = count;
endmodule


module comparator1 (in0, out0);
input [7:0] in0;
output out0;
assign out0 = (in0 <=7) ? 1 : 0;
endmodule

module test_counter;

reg clk, reset;
wire [7:0] count;

wire [7:0] wire0;
wire [7:0] wire1;
wire wire2;
wire wire3;
wire wire4;
wire result;

counter dut (count, clk, reset);

comparator0 comp1 (wire0, wire3);
assign result = wire4;
source source0 (count, wire0, wire1);
comparator1 comp2 (wire1, wire2);
assign wire4 = wire3&wire2;

initial // Clock generator
  begin
    clk = 0;
    forever #10 clk = !clk;
  end
  
initial	// Test stimulus
  begin
    reset = 0;
    #5 reset = 1;
    #4 reset = 0;
  end
  
initial
    $monitor($stime,, reset,, clk,,, count); 
    
endmodule    

