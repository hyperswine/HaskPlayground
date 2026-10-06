// Unified physically addressed 1 KiB direct-mapped word cache.
// Single outstanding transaction. Writes invalidate the indexed line and are
// acknowledged only after SDRAM completion. Reset sweeps valid bits in BRAM.
module sdram_cache(input clk,reset,req,write,input [20:0] address,
 input [31:0] wdata,output ready,output reg done,output reg [31:0] rdata,
 output memory_req,output memory_write,output [20:0] memory_address,
 output [31:0] memory_wdata,input memory_ready,memory_done,input [31:0] memory_rdata);
 localparam CLEAR=0,IDLE=1,LOOKUP=2,CHECK=3,SEND=4,WAIT=5;
 reg [2:0] state;
 reg [7:0] clear_index;
 reg [20:0] saved_address;
 reg saved_write;
 reg [31:0] saved_data;
 // Packed valid, physical tag, data; synchronous read and one write port.
 reg [45:0] lines [0:255];
 reg [45:0] entry;
 wire hit=entry[45] && entry[44:32]==saved_address[20:8];
 assign ready=!reset && state==IDLE;
 assign memory_req=!reset && state==SEND;
 assign memory_write=saved_write;
 assign memory_address=saved_address;
 assign memory_wdata=saved_data;
 always @(posedge clk) begin
   done<=0;
   if(reset) begin state<=CLEAR;clear_index<=0;end
   else case(state)
     CLEAR: begin
       lines[clear_index]<=0;
       clear_index<=clear_index+1'b1;
       if(clear_index==255) state<=IDLE;
     end
     IDLE: if(req) begin
       saved_address<=address;saved_data<=wdata;saved_write<=write;
       state<=LOOKUP;
     end
     LOOKUP: begin entry<=lines[saved_address[7:0]];state<=CHECK;end
     CHECK: if(saved_write) begin
       lines[saved_address[7:0]]<=0;state<=SEND;
     end else if(hit) begin rdata<=entry[31:0];done<=1;state<=IDLE;end
     else state<=SEND;
     SEND: if(memory_ready) state<=WAIT;
     WAIT: if(memory_done) begin
       if(!saved_write) begin
         lines[saved_address[7:0]]<={1'b1,saved_address[20:8],memory_rdata};
         rdata<=memory_rdata;
       end
       done<=1;state<=IDLE;
     end
     default: state<=CLEAR;
   endcase
 end
endmodule
