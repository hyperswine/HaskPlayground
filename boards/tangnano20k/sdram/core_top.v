module top(input clk_27m,btn_s1,uart_rx,output uart_tx,
 output O_sdram_clk,O_sdram_cke,O_sdram_cs_n,O_sdram_cas_n,O_sdram_ras_n,O_sdram_wen_n,
 output [3:0] O_sdram_dqm,output [10:0] O_sdram_addr,output [1:0] O_sdram_ba,inout [31:0] IO_sdram_dq);
 wire clk,clk_sdram,lock;
 sdram_pll pll(.clk_27m(clk_27m),.clk_core(clk),.clk_sdram(clk_sdram),.pll_lock(lock));
 reg [1:0] reset_sync=3; wire reset_request=btn_s1|!lock;
 always @(posedge clk or posedge reset_request)
 if(reset_request) reset_sync<=3;else reset_sync<={reset_sync[0],1'b0};
 wire reset=reset_sync[1];
 wire req,write,ready,done; wire [20:0] address;wire [31:0] wdata,rdata;
 sdram_simple_risc core(.clk(clk),.reset(reset),.enable(1'b1),.uart_rx(uart_rx),.uart_tx(uart_tx),
 .memory_data(rdata),.memory_ready(ready),.memory_done(done),.memory_req(req),
 .memory_write(write),.memory_address(address),.memory_wdata(wdata));
 sdram_memory mem(.clk(clk),.clk_sdram(clk_sdram),.reset(reset),.req(req),.write(write),
 .address(address),.wdata(wdata),.mask(4'b1111),.ready(ready),.done(done),.rdata(rdata),
 .O_sdram_clk(O_sdram_clk),.O_sdram_cke(O_sdram_cke),.O_sdram_cs_n(O_sdram_cs_n),
 .O_sdram_cas_n(O_sdram_cas_n),.O_sdram_ras_n(O_sdram_ras_n),.O_sdram_wen_n(O_sdram_wen_n),
 .O_sdram_dqm(O_sdram_dqm),.O_sdram_addr(O_sdram_addr),.O_sdram_ba(O_sdram_ba),.IO_sdram_dq(IO_sdram_dq));
endmodule
