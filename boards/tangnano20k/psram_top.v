// Tang Nano 20K (GW2AR-LV18QN88C8/I7) wrapper for the Clash-generated
// psram_regs core (src/PsramRegs.hs), which build_psram.sh generates into
// output/tangnano20k/psram/clash/PsramRegs.topEntity/psram_regs.v.
//
// The core runs straight from the on-board 27 MHz oscillator: its UART uses
// 234 clocks per bit, 115385 baud (+0.16% from 115200).
`default_nettype none

module top (
    input  wire clk_27m,  // on-board 27 MHz oscillator
    input  wire btn_s1,   // S1, active high: resets the core
    input  wire uart_rx,  // from the BL616 USB-UART
    output wire uart_tx   // to the BL616 USB-UART
);
  // Assert reset asynchronously, release it synchronously to the clock.
  reg [1:0] reset_sync = 2'b11;
  always @(posedge clk_27m or posedge btn_s1)
    if (btn_s1) reset_sync <= 2'b11;
    else reset_sync <= {reset_sync[0], 1'b0};

  psram_regs core (
      .clk(clk_27m),
      .reset(reset_sync[1]),
      .enable(1'b1),
      .uart_rx(uart_rx),
      .uart_tx(uart_tx)
  );
endmodule
