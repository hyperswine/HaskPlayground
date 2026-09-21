// iverilog testbench for the Clash-generated simple_risc core: program a tiny
// "Hi" program over the UART, run it, and expect "HiDONE" back.
//
//   iverilog -o tb.vvp boards/tangnano20k/tb_simple_risc.v \
//     output/tangnano20k/clash/SimpleRisc.topEntity/simple_risc.v && vvp tb.vvp
`timescale 1ns/1ps
`default_nettype none

module tb_simple_risc;
  localparam integer CPB = 868;  // clocks per UART bit (uartClocksPerBit)

  reg clk = 1'b0;
  reg reset = 1'b1;
  reg rx = 1'b1;
  wire tx;

  always #10 clk = ~clk;

  simple_risc dut (
      .clk(clk),
      .reset(reset),
      .enable(1'b1),
      .uart_rx(rx),
      .uart_tx(tx)
  );

  task send_byte(input [7:0] value);
    integer i;
    begin
      rx = 1'b0;
      repeat (CPB) @(posedge clk);
      for (i = 0; i < 8; i = i + 1) begin
        rx = value[i];
        repeat (CPB) @(posedge clk);
      end
      rx = 1'b1;
      repeat (CPB + 20) @(posedge clk);
    end
  endtask

  task send_word(input [31:0] value);
    begin
      send_byte(value[7:0]);
      send_byte(value[15:8]);
      send_byte(value[23:16]);
      send_byte(value[31:24]);
    end
  endtask

  // Decode what the core transmits.
  reg [7:0] received [0:15];
  integer count = 0;
  reg monitor_on = 1'b0;

  initial begin : monitor
    reg [7:0] value;
    integer i;
    wait (monitor_on);
    forever begin
      @(negedge tx);
      repeat (CPB / 2) @(posedge clk);
      for (i = 0; i < 8; i = i + 1) begin
        repeat (CPB) @(posedge clk);
        value[i] = tx;
      end
      repeat (CPB) @(posedge clk);
      received[count] = value;
      count = count + 1;
      $display("  tx byte %0d: 0x%02h '%c'", count, value, value);
    end
  end

  initial begin : main
    integer timeout;
    repeat (5) @(posedge clk);
    reset = 1'b0;
    repeat (20) @(posedge clk);
    monitor_on = 1'b1;

    send_byte("P");
    send_byte(8'd5);
    send_byte(8'd0);
    send_word(32'h1000_0137);  // lui  x2, 0x10000
    send_word(32'h0480_0093);  // addi x1, x0, 'H'
    send_word(32'h0011_2023);  // sw   x1, 0(x2)
    send_word(32'h0690_0093);  // addi x1, x0, 'i'
    send_word(32'h0011_2023);  // sw   x1, 0(x2)
    send_byte("R");

    timeout = 0;
    while (count < 6 && timeout < 8 * 10 * CPB) begin
      @(posedge clk);
      timeout = timeout + 1;
    end

    if (count == 6 && received[0] == "H" && received[1] == "i" && received[2] == "D" &&
        received[3] == "O" && received[4] == "N" && received[5] == "E")
      $display("PASS: received HiDONE");
    else
      $display("FAIL: received %0d bytes", count);
    $finish;
  end
endmodule
