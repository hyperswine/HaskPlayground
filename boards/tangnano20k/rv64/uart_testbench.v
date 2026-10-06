`timescale 1ns/1ps
module uart_tb;
 reg clk=0;always #5 clk=~clk;
 reg reset=1,rx=1;wire tx,rx_valid,rx_pop,tx_ready,tx_send;wire [7:0] rx_data,tx_data;
 rv64_uart uart(clk,reset,rx,rx_pop,rx_valid,rx_data,tx_send,tx_data,tx_ready,tx);
 wire req,wr;wire [20:0] address;wire [31:0] wdata;
 reg done=0;reg [31:0] rdata;
 rv64_core core(clk,reset,rx_valid,rx_data,rx_pop,tx_ready,tx_send,tx_data,req,wr,address,wdata,1'b1,done,rdata);
 reg [31:0] ram[0:7];integer delay=0;reg saved_write;reg [20:0] saved_addr;reg [31:0] saved_data;
 always @(posedge clk) begin
   done<=0;
   if(req && delay==0) begin delay<=4;saved_write<=wr;saved_addr<=address;saved_data<=wdata;end
   else if(delay>0) begin delay<=delay-1;if(delay==1) begin
     if(saved_write)ram[saved_addr]<=saved_data;else rdata<=ram[saved_addr];done<=1;
   end end
 end
 task send(input [7:0] b);integer i;
 begin
   @(negedge clk);rx=0;repeat(868)@(negedge clk);
   for(i=0;i<8;i=i+1)begin rx=b[i];repeat(868)@(negedge clk);end
   rx=1;repeat(1736)@(negedge clk);
 end endtask
 task receive(input [7:0] expected);integer j;reg [7:0] got;
 begin
   @(negedge tx);repeat(1302)@(negedge clk);
   for(j=0;j<8;j=j+1)begin got[j]=tx;if(j!=7)repeat(868)@(negedge clk);end
   repeat(868)@(negedge clk);
   if(got!==expected || tx!==1) $fatal(1,"TX expected %h got %h",expected,got);
 end endtask
 initial begin
   repeat(4)@(negedge clk);reset=0;
   send("P");send(1);send(0);send(8'h73);send(0);send(8'h10);send(0);
   if(ram[0]!==32'h00100073) $fatal(1,"upload byte handshake %h",ram[0]);
   fork
     send("H");
     begin receive("D");receive("O");receive("N");receive("E");end
   join
   $display("PASS: real UART framing, image upload, instruction fetch, DONE");$finish;
 end
 initial begin repeat(300000)@(posedge clk);$fatal(1,"timeout state=%d RX=%b",core.state,rx_valid);end
endmodule
