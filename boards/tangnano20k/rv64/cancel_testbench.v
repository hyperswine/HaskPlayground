`timescale 1ns/1ps
module cancel_tb;
 reg clk=0;always #5 clk=~clk;
 reg reset=1,rx_valid=0;reg [7:0] rx_data=0;
 wire rx_pop,tx_send;wire [7:0] tx_data;
 wire req,wr;wire [20:0] address;wire [31:0] wdata;
 reg ready=1,done=0;reg [31:0] rdata=32'h0000006f; // JAL zero,0
 rv64_core core(clk,reset,rx_valid,rx_data,rx_pop,1'b1,tx_send,tx_data,
   req,wr,address,wdata,ready,done,rdata);
 task start;
 begin
   @(negedge clk);rx_valid=1;rx_data="H";
   @(negedge clk);rx_valid=0;
   wait(core.state==5); // accepted memory request awaiting its response
 end endtask
 initial begin
   repeat(3)@(negedge clk);reset=0;
   start;
   @(negedge clk);rx_valid=1;rx_data=3;done=1;
   @(negedge clk);rx_valid=0;done=0;
   if(core.state!==0 || core.abort_pending!==0 || core.running!==0)
     $fatal(1,"coincident Ctrl-C/completion did not return to host");
   start;
   @(negedge clk);rx_valid=1;rx_data=3;
   @(negedge clk);rx_valid=0;
   if(core.state!==5 || core.abort_pending!==1 || req!==0)
     $fatal(1,"pending response was abandoned");
   repeat(5)@(negedge clk);done=1;
   @(negedge clk);done=0;
   if(core.state!==0 || core.abort_pending!==0)
     $fatal(1,"accepted response was not drained");
   start;
   @(negedge clk);done=1;
   @(negedge clk);done=0;
   repeat(15)@(negedge clk);
   if(!core.running)$fatal(1,"host could not restart after cancellation");
   $display("PASS: coincident cancellation/completion, delayed draining, restart");$finish;
 end
 initial begin repeat(200)@(posedge clk);$fatal(1,"timeout");end
endmodule
