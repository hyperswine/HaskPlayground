// Tang Nano 20K (GW2AR-LV18QN88C8/I7) wrapper for the Clash-generated
// simple_risc core, which build.sh generates into
// output/tangnano20k/clash/SimpleRisc.topEntity/simple_risc.v.
//
// An rPLL turns the on-board 27 MHz oscillator into the core clock:
//   f = 27 / (IDIV_SEL + 1) * (FBDIV_SEL + 1) = 3 * (FBDIV_SEL + 1) MHz
// with the VCO (f * ODIV_SEL) kept within 500-1250 MHz.  build.sh sets
// FBDIV_SEL and ODIV_SEL from FREQ_MHZ.  The UART runs at f / 868 baud.
`default_nettype none

module top #(
    parameter integer FBDIV_SEL = 31,  // 96 MHz
    parameter integer ODIV_SEL  = 8    // VCO = 768 MHz
) (
    input  wire clk_27m,  // on-board 27 MHz oscillator
    input  wire btn_s1,   // S1, active high: resets the core
    input  wire uart_rx,  // from the BL616 USB-UART
    output wire uart_tx   // to the BL616 USB-UART
);
  wire clk_core;
  wire pll_lock;

  rPLL #(
      .FCLKIN("27"),
      .DYN_IDIV_SEL("false"),
      .IDIV_SEL(8),    // divide by 9 -> 3 MHz PFD
      .DYN_FBDIV_SEL("false"),
      .FBDIV_SEL(FBDIV_SEL),
      .DYN_ODIV_SEL("false"),
      .ODIV_SEL(ODIV_SEL),
      .PSDA_SEL("0000"),
      .DYN_DA_EN("true"),
      .DUTYDA_SEL("1000"),
      .CLKOUT_FT_DIR(1'b1),
      .CLKOUTP_FT_DIR(1'b1),
      .CLKOUT_DLY_STEP(0),
      .CLKOUTP_DLY_STEP(0),
      .CLKFB_SEL("internal"),
      .CLKOUT_BYPASS("false"),
      .CLKOUTP_BYPASS("false"),
      .CLKOUTD_BYPASS("false"),
      .DYN_SDIV_SEL(2),
      .CLKOUTD_SRC("CLKOUT"),
      .CLKOUTD3_SRC("CLKOUT"),
      .DEVICE("GW2A-18C")
  ) pll (
      .CLKOUT(clk_core),
      .LOCK(pll_lock),
      .CLKOUTP(),
      .CLKOUTD(),
      .CLKOUTD3(),
      .RESET(1'b0),
      .RESET_P(1'b0),
      .CLKIN(clk_27m),
      .CLKFB(1'b0),
      .FBDSEL(6'b0),
      .IDSEL(6'b0),
      .ODSEL(6'b0),
      .PSDA(4'b0),
      .DUTYDA(4'b0),
      .FDLY(4'b0)
  );

  // Assert reset asynchronously, release it synchronously to the core clock.
  wire reset_request = btn_s1 | ~pll_lock;
  reg [1:0] reset_sync = 2'b11;
  always @(posedge clk_core or posedge reset_request)
    if (reset_request) reset_sync <= 2'b11;
    else reset_sync <= {reset_sync[0], 1'b0};

  simple_risc core (
      .clk(clk_core),
      .reset(reset_sync[1]),
      .enable(1'b1),
      .uart_rx(uart_rx),
      .uart_tx(uart_tx)
  );
endmodule
