// Independent PLLs default to the tested 54 MHz controller / 135-degree SDRAM
// clock relationship while running the CPU/cache at 108 MHz, or 72 MHz for
// a lower-frequency integration checkpoint. The CPU VCO is 864 or 576 MHz;
// the memory VCO stays at 864 MHz. build.sh can generate another SDRAM phase
// for controlled experiments. The mailbox treats PLL phase as unrelated.
module sdram_dual_pll #(parameter CPU_MHZ=108)(
 input clk_27m,output clk_core,clk_memory,clk_sdram,pll_lock);
 wire cpu_lock,memory_lock;
 sdram_pll #(.FBDIV_SEL(CPU_MHZ/3-1),.ODIV_SEL(8)) cpu(
  .clk_27m(clk_27m),.clk_core(clk_core),.clk_sdram(),.pll_lock(cpu_lock));
 sdram_pll #(.FBDIV_SEL(17),.ODIV_SEL(16)) memory(
  .clk_27m(clk_27m),.clk_core(clk_memory),.clk_sdram(clk_sdram),.pll_lock(memory_lock));
 assign pll_lock=cpu_lock && memory_lock;
endmodule
