module top(input clk_27m,btn_s1,uart_rx,output uart_tx,
 output O_sdram_clk,O_sdram_cke,O_sdram_cs_n,O_sdram_cas_n,O_sdram_ras_n,O_sdram_wen_n,
 output [3:0] O_sdram_dqm,output [10:0] O_sdram_addr,output [1:0] O_sdram_ba,inout [31:0] IO_sdram_dq);
 wire clk,clk_sdram,lock;
 sdram_pll pll(.clk_27m(clk_27m),.clk_core(clk),.clk_sdram(clk_sdram),.pll_lock(lock));
 reg [1:0] reset_sync=3; wire reset_request=btn_s1|!lock;
 always @(posedge clk or posedge reset_request)
 if(reset_request) reset_sync<=3;else reset_sync<={reset_sync[0],1'b0};
 wire reset=reset_sync[1];
 wire rx_valid,rx_pop,tx_ready,tx_send;wire [7:0] rx_data,tx_data;
 rv64_uart uart(clk,reset,uart_rx,rx_pop,rx_valid,rx_data,tx_send,tx_data,tx_ready,uart_tx);
 wire req,wr,ready,done;wire [20:0] address;wire [31:0] wdata,rdata;
 rv64_core core(clk,reset,rx_valid,rx_data,rx_pop,tx_ready,tx_send,tx_data,
   req,wr,address,wdata,ready,done,rdata);
 wire mr,mw,mready,mdone;wire [20:0] ma;wire [31:0] md,mdata;
 sdram_cache cache(clk,reset,req,wr,address,wdata,ready,done,rdata,
   mr,mw,ma,md,mready,mdone,mdata);
 sdram_memory mem(.clk(clk),.clk_sdram(clk_sdram),.reset(reset),.req(mr),.write(mw),
 .address(ma),.wdata(md),.mask(4'b1111),.ready(mready),.done(mdone),.rdata(mdata),
 .O_sdram_clk(O_sdram_clk),.O_sdram_cke(O_sdram_cke),.O_sdram_cs_n(O_sdram_cs_n),
 .O_sdram_cas_n(O_sdram_cas_n),.O_sdram_ras_n(O_sdram_ras_n),.O_sdram_wen_n(O_sdram_wen_n),
 .O_sdram_dqm(O_sdram_dqm),.O_sdram_addr(O_sdram_addr),.O_sdram_ba(O_sdram_ba),.IO_sdram_dq(IO_sdram_dq));
endmodule
