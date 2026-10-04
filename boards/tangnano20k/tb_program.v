// iverilog testbench: run any program on the Clash-generated simple_risc core
// through its real UART loader, and print what it transmits.
//
//   boards/tangnano20k/run_program.py IMAGE.bin --input TEXT --frame frame.hex
//   iverilog -o tb.vvp boards/tangnano20k/tb_program.v \
//     output/tangnano20k/clash/SimpleRisc.topEntity/simple_risc.v
//   vvp -n tb.vvp +frame=frame.hex [+max_cycles=N]
//
// The frame file holds the host's bytes, one hex byte per line ('P', count,
// words, 'R', then any input).  The run stops when the core sends "DONE".
`timescale 1ns/1ps
`default_nettype none

module tb_program;
  localparam integer CPB = 868;  // clocks per UART bit (uartClocksPerBit)

  reg clk = 1'b0;
  reg reset = 1'b1;
  reg rx = 1'b1;
  wire tx;

  always #5 clk = ~clk;

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

  // Decode and print what the core transmits; finish after "DONE".
  reg [31:0] last4 = 0;
  reg monitor_on = 1'b0;
  integer received = 0;

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
      $write("%c", value);
      $fflush;
      received = received + 1;
      last4 = {last4[23:0], value};
      if (last4 == "DONE") begin
        $display("\n[tb_program: DONE after %0d bytes, %0t ns]", received, $time);
        $finish;
      end
    end
  end

  integer max_cycles;
  initial begin : timeout
    if (!$value$plusargs("max_cycles=%d", max_cycles)) max_cycles = 50_000_000;
    repeat (max_cycles) @(posedge clk);
    $display("\n[tb_program: TIMEOUT after %0d cycles, %0d bytes received]", max_cycles, received);
    $finish;
  end

  reg [8*512-1:0] frame_path;
  integer file, count, status;
  reg [7:0] value;
  initial begin : host
    if (!$value$plusargs("frame=%s", frame_path)) begin
      $display("tb_program: pass +frame=FILE");
      $finish;
    end
    file = $fopen(frame_path, "r");
    if (file == 0) begin
      $display("tb_program: cannot open the frame file");
      $finish;
    end
    repeat (5) @(posedge clk);
    reset = 1'b0;
    repeat (20) @(posedge clk);
    monitor_on = 1'b1;
    count = 0;
    while (!$feof(file)) begin
      status = $fscanf(file, "%h\n", value);
      if (status == 1) begin
        send_byte(value);
        count = count + 1;
      end
    end
    $fclose(file);
  end
endmodule
