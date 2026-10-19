`timescale 1ns/1ps
// riscv-arch-test runner for the BRAM image (simple_risc), in simulation.
//
// run_arch_tests.py patches a simulation copy of the generated Verilog: the
// boot ROM's contents come from the stub ROM (stub_rom.S), and the 64 KiB
// RAM preloads +IMAGE and, when the stub ROM stores its marker to the last
// RAM word, writes the signature words and ends the run (bram_ram_hook.vh).
// This module only clocks the core and enforces the cycle limit.
module bram_tb;
 reg clk=0;always #5 clk=~clk;
 reg reset=1;
 wire uart_tx,sd_sck,sd_mosi,sd_cs;
 simple_risc dut(.clk(clk),.reset(reset),.enable(1'b1),.uart_rx(1'b1),.sd_miso(1'b1),
   .uart_tx(uart_tx),.sd_sck(sd_sck),.sd_mosi(sd_mosi),.sd_cs(sd_cs));
 reg [63:0] limit,cycles=0;
 always @(posedge clk)cycles<=cycles+1;
 initial begin
   if(!$value$plusargs("LIMIT=%d",limit))limit=64'd50000000;
   repeat(5)@(negedge clk);reset=0;
   wait(cycles>=limit);
   $display("ARCH TIMEOUT cycles=%0d",cycles);
   $finish;
 end
endmodule
