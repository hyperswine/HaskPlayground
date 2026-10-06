`timescale 1ns/1ps
module cache_tb;
 reg clk=0;always #5 clk=~clk;
 reg reset=1,req=0,write=0;reg [20:0] address=0;
 reg [31:0] wdata=0;wire ready,done;wire [31:0] rdata;
 wire mr,mw;wire [20:0] ma;wire [31:0] md;
 reg mready=0,mdone=0;reg [31:0] mdata=0;
 reg [31:0] backing[0:511];integer reads=0,writes=0,ticks=0,delay=0;
 reg pending_write;reg [20:0] pending_address;reg [31:0] pending_data;
 sdram_cache dut(clk,reset,req,write,address,wdata,ready,done,rdata,
   mr,mw,ma,md,mready,mdone,mdata);
 always @(posedge clk) begin
   ticks<=ticks+1;mdone<=0;mready<=delay==0 && ticks%7!=0;
   if(mr && mready) begin
     if(delay!=0) $fatal(1,"duplicate memory request");
     pending_write<=mw;pending_address<=ma;pending_data<=md;delay<=6;
     if(mw) writes<=writes+1;else reads<=reads+1;
   end else if(delay!=0) begin
     delay<=delay-1;
     if(delay==1) begin
       if(pending_write) backing[pending_address]<=pending_data;
       else mdata<=backing[pending_address];
       mdone<=1;
     end
   end
 end
 task access(input wr,input [20:0] addr,input [31:0] value,input [31:0] expected);
 integer guard;
 begin
   @(negedge clk);while(!ready) @(negedge clk);
   req=1;write=wr;address=addr;wdata=value;
   @(negedge clk);req=0;address=511;wdata=32'hdeadbeef;
   guard=0;
   while(!done) begin @(negedge clk);guard=guard+1;if(guard>100) $fatal(1,"timeout");end
   if(!wr && rdata!==expected) $fatal(1,"bad data %h expected %h",rdata,expected);
   if(wr && backing[addr]!==value) $fatal(1,"early write acknowledgment");
 end endtask
 integer i;
 initial begin
   for(i=0;i<512;i=i+1) backing[i]=i^32'habcdef00;
   repeat(3) @(negedge clk);reset=0;
   access(0,3,0,32'habcdef03);
   access(0,3,0,32'habcdef03);
   if(reads!=1) $fatal(1,"read did not hit");
   access(0,259,0,32'habcdee03);
   access(0,3,0,32'habcdef03);
   if(reads!=3) $fatal(1,"tag collision");
   access(1,3,32'h12345678,0);
   access(0,3,0,32'h12345678);
   access(0,3,0,32'h12345678);
   if(reads!=4 || writes!=1) $fatal(1,"write coherence");
   @(negedge clk);reset=1;@(negedge clk);reset=0;
   access(0,3,0,32'h12345678);
   if(reads!=5) $fatal(1,"reset retained valid entry");
   $display("PASS: hits, collisions, write completion, reset, delayed readiness");$finish;
 end
 initial begin #100000;$fatal(1,"global timeout");end
endmodule
