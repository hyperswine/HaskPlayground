// Fixed three-logical-clock CPU word port, with a blocking line refill.
// The CPU wrapper owns the request register; this cache owns its two lookup
// stages. A miss freezes the CPU and all three stages together. Physical
// clocks, SDRAM refresh, and the ordered masked-write queue keep advancing.
// Address bit21 selects boot ROM; bits20:0 are full SDRAM word addresses.
module sdram_pipeline_cache #(
 parameter LINE_BITS=7, // 128 x 16-byte lines; 2 KiB by default
 // 1: a store that hits updates its cached word and keeps the line valid
 // (board-verified with the banked data array, 2026-10-19).
 // 0: every store invalidates its line (the original policy).
 parameter WRITE_UPDATE=1,
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
 // The data array is banks of 512 x 32 words, one wide BSRAM each (so
 // LINE_BITS>=7). Left to itself Gowin maps a deep array to narrow slices
 // (16Kx1, 8Kx2), and on the board a single-word write to those sometimes
 // changed a bit of another word in the slice, which write-update's isolated
 // stores hit; the 512x32 mode did not (WRITE-UPDATE-2026-10-19.md).
 localparam BANKS=(1<<(LINE_BITS+2))/512,BANK_BITS=BANKS>1 ? $clog2(BANKS):1;
 // One synchronous read and one write port map the small tag array to
 // BSRAM. Distributed tags create a broad asynchronous address mux which
 // otherwise dominates timing even though the CPU request is registered.
 reg [19-LINE_BITS:0] tags[0:(1<<LINE_BITS)-1] /* synthesis syn_ramstyle = "block_ram" */;
 reg [31:0] rom[0:511];
 initial $readmemh(ROM_FILE,rom);
 reg v1,v2,w1,w2,hit2,keep2;
 reg [21:0] a1,a2;
 reg [31:0] d1,d2;
 reg [3:0] m1,m2;
 reg [19-LINE_BITS:0] entry_tag;
 reg tag_conflict;
 reg [20:0] write_address[0:3];
 reg [31:0] write_data[0:3];reg [3:0] write_mask[0:3];
 reg [1:0] write_head,write_tail;reg [2:0] write_count;
 reg last_write,last_update;reg [21:0] last_address;reg [31:0] last_data,last_word;reg [3:0] last_mask;
 reg fill_forward;reg [18:0] fill_line;
 reg [31:0] fill_data[0:3],fill_target;
 wire read_miss=v2 && !w2 && !hit2;
 wire write_full=v2 && w2 && !a2[21] && write_count==4;
 assign hold_cpu=reset || clearing || read_miss || write_full;
 wire advance=!hold_cpu;
 wire push=advance && v2 && w2 && !a2[21];
 // A store keeps its line only when its lookup is certain: a hit whose line
 // no refill replaced in between. Anything else invalidates, as before.
 wire update=WRITE_UPDATE!=0 && push && keep2;
 wire pop=state==WRITE_WAIT && memory_done;
 wire tag_fill=state==READ_WAIT && memory_done;
 wire tag_write=clearing || (push && !update) || tag_fill;
 wire [LINE_BITS-1:0] tag_write_index=(tag_fill || push) ? a2[LINE_BITS+1:2]:clear_index;
 wire [19-LINE_BITS:0] tag_write_value=tag_fill ? {1'b1,a2[20:LINE_BITS+2]}:0;
 assign memory_req=!reset && (state==WRITE_SEND || state==READ_SEND);
 assign memory_write=state==WRITE_SEND || state==WRITE_WAIT;
 assign memory_address=memory_write ? write_address[write_head]:{a2[20:2],2'b00};
 assign memory_wdata=write_data[write_head];
 assign memory_mask=memory_write ? write_mask[write_head]:4'b1111;
 // A refill must not read SDRAM under a queued store to its own line. Other
 // queued stores may complete after the refill: they touch other lines.
 wire [3:0] same_line;
 genvar q;
 generate for(q=0;q<4;q=q+1)begin: queued_lines
   localparam [1:0] SLOT=q;
   wire [1:0] age=SLOT-write_head; // position in the queue, modulo four
   assign same_line[q]=age<write_count && write_address[q][20:2]==a2[20:2];
 end endgenerate
 wire fill_conflict=|same_line;
 wire filled=fill_forward && !a1[21] && a1[20:2]==fill_line;
 wire hit1=a1[21] || filled || (!tag_conflict && entry_tag[19-LINE_BITS] && entry_tag[18-LINE_BITS:0]==a1[20:LINE_BITS+2]);
 // The refill that just completed overwrote a1's line: its tag lookup is stale.
 wire evicted=fill_forward && !a1[21] && !filled && a1[LINE_BITS+1:2]==fill_line[LINE_BITS-1:0];
 // A store invalidating the line being read may return any BRAM value.
 // Ignore that tag, preserving coherence independently of device-specific
 // read-during-write behavior. Refill completion already freezes advance.
 always @(posedge clk)begin
   if(!reset && tag_write)tags[tag_write_index]<=tag_write_value;
   if(advance)entry_tag<=tags[cpu_address[LINE_BITS+1:2]];
 end
 function [31:0] merged(input [31:0] before_word,input [31:0] data,input [3:0] lanes);
 integer k;
 begin
   merged=before_word;
   for(k=0;k<4;k=k+1)if(lanes[k])merged[k*8+:8]=data[k*8+:8];
 end endfunction
 // cpu_rdata holds the store's own lookup (with older stores merged), so
 // the updated word is complete even for a partial store.
 wire [31:0] store_word=merged(cpu_rdata,d2,m2);
 wire words_write=(state==READ_WAIT && memory_word_valid) || update;
 wire [LINE_BITS+1:0] words_index=update ? a2[LINE_BITS+1:0]:{a2[LINE_BITS+1:2],memory_word_index};
 wire [31:0] words_data=update ? store_word:memory_rdata;
 // Every bank reads cpu_address on every edge; entry_word is the word read on
 // the last advancing edge (the held copy otherwise), as the read enable
 // advance would give, with a single copy rather than one per bank.
 wire [32*BANKS-1:0] bank_word;
 genvar b;
 generate for(b=0;b<BANKS;b=b+1)begin: banks
   reg [31:0] mem[0:511] /* synthesis syn_ramstyle = "block_ram" */;
   reg [31:0] read_word;
   always @(posedge clk)begin
     if(!reset && words_write && (words_index>>9)==b)mem[words_index[8:0]]<=words_data;
     read_word<=mem[cpu_address[8:0]];
   end
   assign bank_word[32*b+:32]=read_word;
 end endgenerate
 reg advanced,read_rom;reg [BANK_BITS-1:0] read_bank;reg [31:0] rom_word,held_word;
 wire [31:0] entry_word=!advanced ? held_word:read_rom ? rom_word:bank_word[32*read_bank+:32];
 always @(posedge clk)begin
   advanced<=advance;held_word<=entry_word;
   read_rom<=cpu_address[21];read_bank<=cpu_address[LINE_BITS+1:0]>>9;rom_word<=rom[cpu_address[8:0]];
 end
 reg [31:0] forwarded;
 always @* begin
   forwarded=filled ? fill_data[a1[1:0]]:entry_word;
   // A synchronous lookup can predate either of the preceding stores. An
   // updating store wrote this word on the lookup's own edge, so the BSRAM
   // read may be undefined there: take the whole written word instead.
   if(last_write && a1==last_address)forwarded=last_update ? last_word:merged(forwarded,last_data,last_mask);
   if(v2 && w2 && a1==a2)forwarded=merged(forwarded,d2,m2);
 end
 always @(posedge clk) begin
   if(reset)begin
     clearing<=1;clear_index<=0;state<=IDLE;write_count<=0;
     write_head<=0;write_tail<=0;v1<=0;v2<=0;
     hit2<=0;keep2<=0;last_write<=0;last_update<=0;fill_forward<=0;cpu_rdata<=0;tag_conflict<=0;
   end else begin
     if(clearing)begin
       clear_index<=clear_index+1'b1;
       if(clear_index==((1<<LINE_BITS)-1))clearing<=0;
     end
     if(advance)begin
       a1<=cpu_address;w1<=cpu_write;d1<=cpu_wdata;m1<=cpu_mask;v1<=1;
       tag_conflict<=tag_write && tag_write_index==cpu_address[LINE_BITS+1:2];
       a2<=a1;w2<=w1;d2<=d1;m2<=m1;v2<=v1;hit2<=hit1;keep2<=hit1 && !evicted;
       cpu_rdata<=forwarded;
       fill_forward<=0;
       last_write<=push;last_update<=update;last_address<=a2;last_data<=d2;last_mask<=m2;last_word<=store_word;
     end
     if(push)begin
       write_address[write_tail]<=a2[20:0];write_data[write_tail]<=d2;write_mask[write_tail]<=m2;
       write_tail<=write_tail+1'b1;
       // Unified physical tags keep loader stores and instruction fetches
       // coherent. A miss drains older stores to its own line first.
     end
     if(pop)write_head<=write_head+1'b1;
     case({push,pop})
       2'b10:write_count<=write_count+1'b1;
       2'b01:write_count<=write_count-1'b1;
     endcase
     case(state)
       IDLE:if(read_miss && !fill_conflict)state<=READ_SEND;
         else if(write_count!=0)state<=WRITE_SEND;
         else if(read_miss)state<=READ_SEND;
       WRITE_SEND:if(memory_ready)state<=WRITE_WAIT;
       WRITE_WAIT:if(memory_done)state<=IDLE;
       READ_SEND:if(memory_ready)begin state<=READ_WAIT;fill_line<=a2[20:2];end
       READ_WAIT:begin
         if(memory_word_valid)begin
           fill_data[memory_word_index]<=memory_rdata;
           if(memory_word_index==a2[1:0])fill_target<=memory_rdata;
         end
         if(memory_done)begin
           // Completion may arrive with the final beat.
           cpu_rdata<=(memory_word_valid && memory_word_index==a2[1:0]) ? memory_rdata:fill_target;
           hit2<=1;fill_forward<=1;state<=IDLE;
         end
       end
     endcase
   end
 end
endmodule
