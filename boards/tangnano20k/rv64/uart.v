// 8N1, clock/868 baud. A holding byte is consumed explicitly by the CPU.
module rv64_uart(input clk,reset,rx,input rx_pop,output reg rx_valid,
 output reg [7:0] rx_data,input tx_send,input [7:0] tx_data,
 output tx_ready,output tx);
 reg [1:0] sync=3;
 reg [9:0] rx_count,tx_count;
 reg [3:0] rx_bit,tx_bit;
 reg [7:0] rx_shift;
 reg [9:0] tx_shift;
 reg rx_busy,tx_busy;
 assign tx_ready=!tx_busy;assign tx=tx_busy?tx_shift[0]:1'b1;
 always @(posedge clk) begin
   sync<={sync[0],rx};
   if(reset) begin rx_busy<=0;rx_valid<=0;tx_busy<=0;tx_shift<=1023;end
   else begin
     if(rx_pop) rx_valid<=0;
     if(!rx_busy) begin
       if(!sync[1]) begin rx_busy<=1;rx_count<=433;rx_bit<=0;end
     end else if(rx_count!=0) rx_count<=rx_count-1'b1;
     else begin
       rx_count<=867;
       if(rx_bit==0) begin
         if(sync[1]) rx_busy<=0;else rx_bit<=1;
       end else if(rx_bit==9) begin
         rx_busy<=0;
         if(sync[1] && (!rx_valid || rx_pop)) begin rx_data<=rx_shift;rx_valid<=1;end
       end else begin rx_shift<={sync[1],rx_shift[7:1]};rx_bit<=rx_bit+1'b1;end
     end
     if(!tx_busy) begin
       if(tx_send) begin tx_shift<={1'b1,tx_data,1'b0};tx_busy<=1;tx_count<=867;tx_bit<=0;end
     end else if(tx_count!=0) tx_count<=tx_count-1'b1;
     else if(tx_bit==9) tx_busy<=0;
     else begin tx_shift<={1'b1,tx_shift[9:1]};tx_bit<=tx_bit+1'b1;tx_count<=867;end
   end
 end
endmodule
