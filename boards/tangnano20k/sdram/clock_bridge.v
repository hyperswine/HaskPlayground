// An in-order request FIFO between unrelated CPU and SDRAM clocks.
//
// Requests (masked word writes and four-word line reads) enter a four-entry
// asynchronous FIFO. Only Gray-coded pointers cross domains, each through a
// two-flop synchronizer; a payload slot is written before its pointer
// advances and is not reused until the memory side has completed it. The
// memory side executes requests strictly in order, so a read never overtakes
// an earlier write. Writes are posted: the CPU side completes them as soon as
// they are queued, so consecutive writes stream at SDRAM speed instead of
// paying a round trip each. A read waits for its line: the memory side
// collects all four words, toggles the response, and the CPU side replays
// them, completing with the final word. One read is outstanding at a time.
// Both resets must assert together; their release may be synchronized locally.
module sdram_clock_bridge(
 input cpu_clk,cpu_reset,input cpu_req,cpu_write,input [20:0] cpu_address,
 input [31:0] cpu_wdata,input [3:0] cpu_mask,
 output cpu_ready,output reg cpu_done,output reg [31:0] cpu_rdata,
 output reg cpu_word_valid,output reg [1:0] cpu_word_index,
 input mem_clk,mem_reset,output mem_req,mem_write,output [20:0] mem_address,
 output [31:0] mem_wdata,output [3:0] mem_mask,
 input mem_ready,mem_done,input [31:0] mem_rdata,
 input mem_word_valid,input [1:0] mem_word_index);
 localparam CPU_IDLE=0,CPU_WAIT=1,CPU_CAPTURE=2,CPU_REPLAY=3;
 localparam MEM_IDLE=0,MEM_SEND=1,MEM_WAIT=2;
 function [2:0] gray(input [2:0] binary);gray=binary^(binary>>1);endfunction
 reg [1:0] cpu_state,mem_state;
 // Pointers: two index bits plus a wrap bit.
 reg [2:0] put,put_gray,take,take_gray;
 (* syn_preserve=1, ASYNC_REG="TRUE" *) reg [2:0] put_meta,put_sync;
 (* syn_preserve=1, ASYNC_REG="TRUE" *) reg [2:0] take_meta,take_sync;
 reg read_toggle,response_toggle;
 (* syn_preserve=1, ASYNC_REG="TRUE" *) reg response_meta,response_sync;
 reg slot_write[0:3];reg [20:0] slot_address[0:3];
 reg [31:0] slot_wdata[0:3];reg [3:0] slot_mask[0:3];
 reg operation_write;reg [20:0] operation_address;
 reg [31:0] operation_wdata;reg [3:0] operation_mask;
 reg [31:0] response_words[0:3];
 reg [31:0] replay_words[0:3];
 reg [1:0] replay_index;
 reg [3:0] received_words;
 wire full=put_gray=={~take_sync[2:1],take_sync[0]};
 wire empty=take_gray==put_sync;
 assign cpu_ready=!cpu_reset && cpu_state==CPU_IDLE && !full;
 wire accept=cpu_req && cpu_ready;
 assign mem_req=!mem_reset && mem_state==MEM_SEND;
 assign mem_write=operation_write;
 assign mem_address=operation_address;
 assign mem_wdata=operation_wdata;
 assign mem_mask=operation_mask;
 always @(posedge cpu_clk or posedge cpu_reset)begin
  if(cpu_reset)begin take_meta<=0;take_sync<=0;response_meta<=0;response_sync<=0;end
  else begin take_meta<=take_gray;take_sync<=take_meta;response_meta<=response_toggle;response_sync<=response_meta;end
 end
 always @(posedge mem_clk or posedge mem_reset)begin
  if(mem_reset)begin put_meta<=0;put_sync<=0;end
  else begin put_meta<=put_gray;put_sync<=put_meta;end
 end
 // Payload slots have no reset: a slot is read only after its pointer has
 // crossed, and it is always written before that pointer advances.
 always @(posedge cpu_clk)if(accept)begin
  slot_write[put[1:0]]<=cpu_write;slot_address[put[1:0]]<=cpu_address;
  slot_wdata[put[1:0]]<=cpu_wdata;slot_mask[put[1:0]]<=cpu_mask;
 end
 integer c;
 always @(posedge cpu_clk or posedge cpu_reset)begin
  if(cpu_reset)begin
   cpu_state<=CPU_IDLE;put<=0;put_gray<=0;read_toggle<=0;
   cpu_done<=0;cpu_word_valid<=0;cpu_word_index<=0;cpu_rdata<=0;replay_index<=0;
   for(c=0;c<4;c=c+1)replay_words[c]<=0;
  end else begin
   cpu_done<=0;cpu_word_valid<=0;
   case(cpu_state)
    CPU_IDLE:if(accept)begin
     put<=put+1'b1;put_gray<=gray(put+1'b1);
     if(cpu_write)cpu_done<=1;
     else begin read_toggle<=!read_toggle;cpu_state<=CPU_WAIT;end
    end
    CPU_WAIT:if(response_sync==read_toggle)cpu_state<=CPU_CAPTURE;
    CPU_CAPTURE:begin
     for(c=0;c<4;c=c+1)replay_words[c]<=response_words[c];
     replay_index<=0;cpu_state<=CPU_REPLAY;
    end
    CPU_REPLAY:begin
     cpu_rdata<=replay_words[replay_index];cpu_word_index<=replay_index;cpu_word_valid<=1;
     replay_index<=replay_index+1'b1;
     if(replay_index==3)begin cpu_done<=1;cpu_state<=CPU_IDLE;end
    end
   endcase
  end
 end
 integer m;
 always @(posedge mem_clk or posedge mem_reset)begin
  if(mem_reset)begin
   mem_state<=MEM_IDLE;take<=0;take_gray<=0;response_toggle<=0;operation_write<=0;
   operation_address<=0;operation_wdata<=0;operation_mask<=0;received_words<=0;
   for(m=0;m<4;m=m+1)response_words[m]<=0;
  end else begin
   case(mem_state)
    MEM_IDLE:if(!empty)begin
     operation_write<=slot_write[take[1:0]];operation_address<=slot_address[take[1:0]];
     operation_wdata<=slot_wdata[take[1:0]];operation_mask<=slot_mask[take[1:0]];
     received_words<=0;mem_state<=MEM_SEND;
    end
    MEM_SEND:if(mem_ready)mem_state<=MEM_WAIT;
    MEM_WAIT:begin
     if(mem_word_valid)begin
      response_words[mem_word_index]<=mem_rdata;
      received_words[mem_word_index]<=1;
     end
     if(mem_done)begin
      // Free the slot only now: the CPU side cannot overwrite it earlier.
      take<=take+1'b1;take_gray<=gray(take+1'b1);
      if(!operation_write)response_toggle<=!response_toggle;
      mem_state<=MEM_IDLE;
     end
    end
    default:mem_state<=MEM_IDLE;
   endcase
  end
 end
 // Functional assertions supplement the independent testbench. They are not
 // synthesized into the FPGA and do not alter the hardware protocol.
 // synthesis translate_off
 always @(posedge mem_clk)if(!mem_reset && mem_state==MEM_WAIT)begin
  if(mem_word_valid && (operation_write || received_words[mem_word_index]))
   $fatal(1,"CDC backend duplicate/unexpected read word");
  if(mem_done && !operation_write &&
    (received_words | (mem_word_valid ? (4'b1<<mem_word_index):4'b0))!=4'b1111)
   $fatal(1,"CDC backend completed an incomplete read line");
 end
 // synthesis translate_on
endmodule
