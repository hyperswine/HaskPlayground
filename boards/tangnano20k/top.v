// Tang Nano 20K (GW2AR-LV18QN88C8/I7) wrapper for the Clash-generated
// simple_risc core, which build.sh generates into
// output/tangnano20k/clash/SimpleRisc.topEntity/simple_risc.v.
//
// The core's UART divider assumes a 50 MHz clock.  The board has a 27 MHz
// oscillator, so an rPLL makes 27 / 9 * 17 = 51 MHz, giving roughly 117.5k
// baud on the host side (about 2% fast, within 8-N-1 tolerance).
`default_nettype none

module top (
    input  wire clk_27m,  // on-board 27 MHz oscillator
    input  wire btn_s1,   // S1, active high: resets the core
    input  wire uart_rx,  // from the BL616 USB-UART
    output wire uart_tx   // to the BL616 USB-UART
);
  wire clk_51m;
  wire pll_lock;

  rPLL #(
      .FCLKIN("27"),
      .DYN_IDIV_SEL("false"),
      .IDIV_SEL(8),    // divide by 9  -> 3 MHz PFD
      .DYN_FBDIV_SEL("false"),
      .FBDIV_SEL(16),  // multiply by 17 -> 51 MHz
      .DYN_ODIV_SEL("false"),
      .ODIV_SEL(16),   // VCO = 816 MHz
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
      .CLKOUT(clk_51m),
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

  // Assert reset asynchronously, release it synchronously to the PLL clock.
  wire reset_request = btn_s1 | ~pll_lock;
  reg [1:0] reset_sync = 2'b11;
  always @(posedge clk_51m or posedge reset_request)
    if (reset_request) reset_sync <= 2'b11;
    else reset_sync <= {reset_sync[0], 1'b0};

  simple_risc core (
      .clk(clk_51m),
      .reset(reset_sync[1]),
      .enable(1'b1),
      .uart_rx(uart_rx),
      .uart_tx(uart_tx)
  );
endmodule
