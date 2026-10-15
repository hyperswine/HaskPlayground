// Fixed three-logical-clock CPU word port, with a blocking line refill.
// A miss freezes the CPU and this port's three stages together. Physical
// clocks, SDRAM refresh, and the ordered masked-write queue keep advancing.
// Address bit21 selects boot ROM; bits20:0 are full SDRAM word addresses.
module sdram_pipeline_cache #(
 parameter LINE_BITS=7, // 128 x 16-byte lines; 2 KiB by default
 parameter ROM_FILE="boards/tangnano20k/bootrom/bootrom.hex"
)(input clk,reset,input [21:0] cpu_address,input cpu_write,
 input [31:0] cpu_wdata,input [3:0] cpu_mask,
 output hold_cpu,output reg [31:0] cpu_rdata,
 output memory_req,output memory_write,output [20:0] memory_address,
 output [31:0] memory_wdata,output [3:0] memory_mask,
 input memory_ready,memory_done,input [31:0] memory_rdata,
 input memory_word_valid,input [1:0] memory_word_index);
 localparam IDLE=0,WRITE_SEND=1,WRITE_WAIT=2,READ_SEND=3,READ_WAIT=4;
 reg [2:0] state;
 reg clearing;reg [LINE_BITS-1:0] clear_index;
 reg [31:0] words[0:(1<<(LINE_BITS+2))-1];
 reg [19-LINE_BITS:0] tags[0:(1<<LINE_BITS)-1]; // valid + physical tag
 reg [31:0] rom[0:511];
 initial $readmemh(ROM_FILE,rom);
 reg v0,v1,v2,w0,w1,w2,hit2;
 reg [21:0] a0,a1,a2;
 reg [31:0] d0,d1,d2,entry_word;
 reg [3:0] m0,m1,m2;
 reg [19-LINE_BITS:0] entry_tag;
 reg [20:0] write_address[0:3];
 reg [31:0] write_data[0:3];reg [3:0] write_mask[0:3];
 reg [1:0] write_head,write_tail;reg [2:0] write_count;
 reg last_write;reg [21:0] last_address;reg [31:0] last_data;reg [3:0] last_mask;
 reg fill_forward;reg [18:0] fill_line;
 reg [31:0] fill_data[0:3],fill_target;
 wire read_miss=v2 && !w2 && !hit2;
 wire write_full=v2 && w2 && !a2[21] && write_count==4;
 assign hold_cpu=reset || clearing || read_miss || write_full;
 wire advance=!hold_cpu;
 wire push=advance && v2 && w2 && !a2[21];
 wire pop=state==WRITE_WAIT && memory_done;
 assign memory_req=!reset && (state==WRITE_SEND || state==READ_SEND);
 assign memory_write=state==WRITE_SEND || state==WRITE_WAIT;
 assign memory_address=memory_write ? write_address[write_head]:{a2[20:2],2'b00};
 assign memory_wdata=write_data[write_head];
 assign memory_mask=memory_write ? write_mask[write_head]:4'b1111;
 wire filled=fill_forward && !a1[21] && a1[20:2]==fill_line;
 wire hit1=a1[21] || filled || (entry_tag[19-LINE_BITS] && entry_tag[18-LINE_BITS:0]==a1[20:LINE_BITS+2]);
 function [31:0] merged(input [31:0] before_word,input [31:0] data,input [3:0] lanes);
 integer k;
 begin
   merged=before_word;
   for(k=0;k<4;k=k+1)if(lanes[k])merged[k*8+:8]=data[k*8+:8];
 end endfunction
 reg [31:0] forwarded;
 always @* begin
   forwarded=filled ? fill_data[a1[1:0]]:entry_word;
   // A synchronous lookup can predate either of the preceding stores.
   if(last_write && a1==last_address)forwarded=merged(forwarded,last_data,last_mask);
   if(v2 && w2 && a1==a2)forwarded=merged(forwarded,d2,m2);
 end
 always @(posedge clk) begin
   if(reset)begin
     clearing<=1;clear_index<=0;state<=IDLE;write_count<=0;
     write_head<=0;write_tail<=0;v0<=0;v1<=0;v2<=0;
     hit2<=0;last_write<=0;fill_forward<=0;cpu_rdata<=0;
   end else begin
     if(clearing)begin
       tags[clear_index]<=0;clear_index<=clear_index+1'b1;
       if(clear_index==((1<<LINE_BITS)-1))clearing<=0;
     end
     if(advance)begin
       a0<=cpu_address;w0<=cpu_write;d0<=cpu_wdata;m0<=cpu_mask;v0<=1;
       a1<=a0;w1<=w0;d1<=d0;m1<=m0;v1<=v0;
       entry_word<=a0[21] ? rom[a0[8:0]]:words[a0[LINE_BITS+1:0]];
       entry_tag<=tags[a0[LINE_BITS+1:2]];
       a2<=a1;w2<=w1;d2<=d1;m2<=m1;v2<=v1;hit2<=hit1;
       cpu_rdata<=forwarded;
       fill_forward<=0;
       last_write<=push;last_address<=a2;last_data<=d2;last_mask<=m2;
     end
     if(push)begin
       write_address[write_tail]<=a2[20:0];write_data[write_tail]<=d2;write_mask[write_tail]<=m2;
       write_tail<=write_tail+1'b1;
       // Unified physical tags keep loader stores and instruction fetches
       // coherent. Misses drain older stores before reading a new line.
       tags[a2[LINE_BITS+1:2]]<=0;
     end
     if(pop)write_head<=write_head+1'b1;
     case({push,pop})
       2'b10:write_count<=write_count+1'b1;
       2'b01:write_count<=write_count-1'b1;
     endcase
     case(state)
       IDLE:if(write_count!=0)state<=WRITE_SEND;
         else if(read_miss)state<=READ_SEND;
       WRITE_SEND:if(memory_ready)state<=WRITE_WAIT;
       WRITE_WAIT:if(memory_done)state<=IDLE;
       READ_SEND:if(memory_ready)begin state<=READ_WAIT;fill_line<=a2[20:2];end
       READ_WAIT:begin
         if(memory_word_valid)begin
           words[{a2[LINE_BITS+1:2],memory_word_index}]<=memory_rdata;
           fill_data[memory_word_index]<=memory_rdata;
           if(memory_word_index==a2[1:0])fill_target<=memory_rdata;
         end
         if(memory_done)begin
           tags[a2[LINE_BITS+1:2]]<={1'b1,a2[20:LINE_BITS+2]};
           cpu_rdata<=fill_target;hit2<=1;fill_forward<=1;state<=IDLE;
         end
       end
     endcase
   end
 end
endmodule
