module top(input clk_27m,btn_s1,uart_rx,output uart_tx,
 input sd_dat0,output sd_clk,sd_cmd,sd_dat1,sd_dat2,sd_dat3,
 output O_sdram_clk,O_sdram_cke,O_sdram_cs_n,O_sdram_cas_n,O_sdram_ras_n,O_sdram_wen_n,
 output [3:0] O_sdram_dqm,output [10:0] O_sdram_addr,output [1:0] O_sdram_ba,inout [31:0] IO_sdram_dq);
 wire clk,clk_memory,clk_sdram,lock;
 sdram_pll pll(.clk_27m(clk_27m),.clk_core(clk),.clk_sdram(clk_sdram),.pll_lock(lock));
 assign clk_memory=clk; // SINGLE_CLOCK_MEMORY
 reg [1:0] reset_sync=3;wire reset_request=btn_s1|!lock;
 always @(posedge clk or posedge reset_request)
 if(reset_request)reset_sync<=3;else reset_sync<={reset_sync[0],1'b0};
 wire reset=reset_sync[1];
 reg [1:0] memory_reset_sync=3;
 always @(posedge clk_memory or posedge reset_request)
 if(reset_request)memory_reset_sync<=3;else memory_reset_sync<={memory_reset_sync[0],1'b0};
 wire memory_reset=memory_reset_sync[1];
 assign sd_dat1=1'b1;assign sd_dat2=1'b1;
 wire [21:0] address;wire write,hold_cpu;wire [31:0] wdata,rdata;wire [3:0] mask;
 sdram_pipeline_risc core(.clk(clk),.reset(reset),.enable(1'b1),.uart_rx(uart_rx),.uart_tx(uart_tx),
   .memory_data(rdata),.memory_hold(hold_cpu),.memory_address(address),.memory_write(write),.memory_wdata(wdata),.memory_mask(mask),
   .sd_miso(sd_dat0),.sd_sck(sd_clk),.sd_mosi(sd_cmd),.sd_cs(sd_dat3));
 wire req,mw,ready,done,word_valid;wire [20:0] ma;wire [31:0] md,mdata;wire [3:0] mm;wire [1:0] word_index;
 sdram_pipeline_cache #(.LINE_BITS(CACHE_LINE_BITS),.ROM_FILE("BOOT_ROM_FILE")) cache(clk,reset,address,write,wdata,mask,hold_cpu,rdata,
   req,mw,ma,md,mm,ready,done,mdata,word_valid,word_index);
 wire memory_req,memory_write,memory_ready,memory_done,memory_word_valid;
 wire [20:0] memory_address;wire [31:0] memory_wdata,memory_rdata;
 wire [3:0] memory_mask;wire [1:0] memory_word_index;
 generate if(CPU_FREQUENCY==CORE_FREQUENCY)begin: single_clock
   assign memory_req=req;assign memory_write=mw;assign memory_address=ma;
   assign memory_wdata=md;assign memory_mask=mm;assign ready=memory_ready;
   assign done=memory_done;assign mdata=memory_rdata;
   assign word_valid=memory_word_valid;assign word_index=memory_word_index;
 end else begin: dual_clock
   sdram_clock_bridge bridge(
    .cpu_clk(clk),.cpu_reset(reset),.cpu_req(req),.cpu_write(mw),.cpu_address(ma),
    .cpu_wdata(md),.cpu_mask(mm),.cpu_ready(ready),.cpu_done(done),.cpu_rdata(mdata),
    .cpu_word_valid(word_valid),.cpu_word_index(word_index),
    .mem_clk(clk_memory),.mem_reset(memory_reset),.mem_req(memory_req),.mem_write(memory_write),
    .mem_address(memory_address),.mem_wdata(memory_wdata),.mem_mask(memory_mask),
    .mem_ready(memory_ready),.mem_done(memory_done),.mem_rdata(memory_rdata),
    .mem_word_valid(memory_word_valid),.mem_word_index(memory_word_index));
 end endgenerate
 sdram_burst_memory #(.FREQ(CORE_FREQUENCY)) mem(clk_memory,clk_sdram,memory_reset,
   memory_req,memory_write,memory_address,memory_wdata,memory_mask,
   memory_ready,memory_done,memory_rdata,memory_word_valid,memory_word_index,
   O_sdram_clk,O_sdram_cke,O_sdram_cs_n,O_sdram_cas_n,O_sdram_ras_n,O_sdram_wen_n,
   O_sdram_dqm,O_sdram_addr,O_sdram_ba,IO_sdram_dq);
endmodule
