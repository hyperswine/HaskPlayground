module sdram_pll #(parameter FBDIV_SEL=17, ODIV_SEL=16)(input clk_27m, output clk_core,clk_sdram,pll_lock);
  rPLL #(
      .FCLKIN("27"),
      .DYN_IDIV_SEL("false"),
      .IDIV_SEL(8),    // divide by 9 -> 3 MHz PFD
      .DYN_FBDIV_SEL("false"),
      .FBDIV_SEL(FBDIV_SEL),
      .DYN_ODIV_SEL("false"),
      .ODIV_SEL(ODIV_SEL),
      .PSDA_SEL("1010"),
      .DYN_DA_EN("false"),
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
      .DEVICE("GW2AR-18C")
  ) pll (
      .CLKOUT(clk_core),
      .LOCK(pll_lock),
      .CLKOUTP(clk_sdram),
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

endmodule
