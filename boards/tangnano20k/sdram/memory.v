// One outstanding word transaction; refresh has priority before accepting work.
module sdram_memory #(parameter FREQ=54000000)(
 input clk, clk_sdram, reset,
 input req, write, input [20:0] address, input [31:0] wdata, input [3:0] mask,
 output ready, output reg done, output [31:0] rdata,
 output O_sdram_clk,O_sdram_cke,O_sdram_cs_n,O_sdram_cas_n,O_sdram_ras_n,O_sdram_wen_n,
 output [3:0] O_sdram_dqm, output [10:0] O_sdram_addr, output [1:0] O_sdram_ba,
 inout [31:0] IO_sdram_dq
);
 wire busy; reg active, seen_busy; reg [15:0] refresh_count;
 localparam REFRESH_INTERVAL=FREQ/1000000*10; // 10 us, earlier than the 15 us limit
 wire refresh_due=refresh_count>=REFRESH_INTERVAL;
 assign ready=!reset && !busy && !active && !refresh_due;
 wire accept=req && ready;
 wire refresh=!reset && !busy && !active && refresh_due;
 always @(posedge clk) begin
   done<=0;
   if (reset) begin active<=0;seen_busy<=0;refresh_count<=0; end
   else begin
     if(refresh) refresh_count<=0;
     else if(refresh_count!=65535) refresh_count<=refresh_count+1'b1;
     if(accept) begin active<=1;seen_busy<=0;end
     if(active && busy) seen_busy<=1;
     if(active && seen_busy && !busy) begin active<=0;done<=1;end
   end
 end
 sdram #(.FREQ(FREQ)) ram(.clk(clk),.clk_sdram(clk_sdram),.resetn(!reset),
 .rd(accept&&!write),.wr(accept&&write),.refresh(refresh),.addr({address,2'b00}),
 .din(wdata),.mask(mask),.dout(),.dout32(rdata),.busy(busy),.data_ready(),
 .SDRAM_DQ(IO_sdram_dq),.SDRAM_A(O_sdram_addr),.SDRAM_BA(O_sdram_ba),
 .SDRAM_nCS(O_sdram_cs_n),.SDRAM_nWE(O_sdram_wen_n),.SDRAM_nRAS(O_sdram_ras_n),
 .SDRAM_nCAS(O_sdram_cas_n),.SDRAM_CLK(O_sdram_clk),.SDRAM_CKE(O_sdram_cke),.SDRAM_DQM(O_sdram_dqm));
endmodule
