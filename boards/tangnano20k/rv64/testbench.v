`timescale 1ns/1ps
module rv64_tb;
 reg clk=0;always #5 clk=~clk;
 reg reset=1,rx_valid=0;reg [7:0] rx_data;
 wire rx_pop,tx_send;wire [7:0] tx_data;
 wire req,wr,ready,done;wire [20:0] address;wire [31:0] wdata,rdata;
 rv64_core core(clk,reset,rx_valid,rx_data,rx_pop,1'b1,tx_send,tx_data,req,wr,address,wdata,ready,done,rdata);
 wire mr,mw;wire [20:0] ma;wire [31:0] md;
 wire mready;reg mdone=0;reg [31:0] mdata=0;
 sdram_cache cache(clk,reset,req,wr,address,wdata,ready,done,rdata,mr,mw,ma,md,mready,mdone,mdata);
 reg [31:0] ram[0:2097151];integer delay=0,ticks=0,count=0;
 reg saved_write;reg [20:0] saved_address;reg [31:0] saved_data;
 assign mready=delay==0 && ticks%7!=0;
 always @(posedge clk) begin
   ticks<=ticks+1;mdone<=0;
   if(mr && mready) begin saved_write<=mw;saved_address<=ma;saved_data<=md;delay<=8;end
   else if(delay>0) begin delay<=delay-1;if(delay==1) begin
     if(saved_write) ram[saved_address]<=saved_data;else mdata<=ram[saved_address];mdone<=1;
   end end
   if(tx_send) begin $write("%c",tx_data);if(tx_data=="E" && !core.running) begin $display("\nTRAP cause=%h pc=%h value=%h a0=%h",core.mcause,core.mepc,core.mtval,core.regs[10]);$display("COUNTS %h %h %h",core.regs[8],core.regs[9],core.regs[18]);$finish;end end
 end
 reg [1023:0] image;
 initial begin
   if(!$value$plusargs("IMAGE=%s",image)) $fatal(1,"missing image");
   $readmemh(image,ram);
   repeat(4) @(negedge clk);reset=0;
   repeat(300) @(negedge clk);rx_valid=1;rx_data="H";
   wait(rx_pop);@(negedge clk);rx_valid=0;
 end
 initial begin repeat(20000000) @(posedge clk);$fatal(1,"timeout pc=%h state=%d instruction=%h",core.pc,core.state,core.instruction);end
endmodule
