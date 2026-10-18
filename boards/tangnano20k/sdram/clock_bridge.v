// One outstanding ordered request between unrelated CPU and SDRAM clocks.
//
// Each mailbox payload is registered and held until the remote transaction
// completes. Only the request/response toggle enters a two-flop synchronizer.
// The receiver captures the stable bundled payload after observing stage two;
// it never samples changing address/data buses. A complete four-word response
// is captured before the response toggle changes, then replayed on CPU clocks.
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
 localparam CPU_IDLE=0,CPU_WAIT=1,CPU_CAPTURE=2,CPU_REPLAY=3,CPU_FINISH=4;
 localparam MEM_IDLE=0,MEM_SEND=1,MEM_WAIT=2;
 reg [2:0] cpu_state;
 reg [1:0] mem_state;
 reg request_toggle,response_toggle;
 (* syn_preserve=1, ASYNC_REG="TRUE" *) reg request_meta,request_sync;
 (* syn_preserve=1, ASYNC_REG="TRUE" *) reg response_meta,response_sync;
 reg request_write;
 reg [20:0] request_address;
 reg [31:0] request_wdata;
 reg [3:0] request_mask;
 reg operation_write;
 reg [20:0] operation_address;
 reg [31:0] operation_wdata;
 reg [3:0] operation_mask;
 reg [31:0] response_words[0:3];
 reg [31:0] replay_words[0:3];
 reg [1:0] replay_index;
 reg [3:0] received_words;
 assign cpu_ready=!cpu_reset && cpu_state==CPU_IDLE;
 assign mem_req=!mem_reset && mem_state==MEM_SEND;
 assign mem_write=operation_write;
 assign mem_address=operation_address;
 assign mem_wdata=operation_wdata;
 assign mem_mask=operation_mask;
 always @(posedge cpu_clk or posedge cpu_reset)begin
  if(cpu_reset)begin response_meta<=0;response_sync<=0;end
  else begin response_meta<=response_toggle;response_sync<=response_meta;end
 end
 always @(posedge mem_clk or posedge mem_reset)begin
  if(mem_reset)begin request_meta<=0;request_sync<=0;end
  else begin request_meta<=request_toggle;request_sync<=request_meta;end
 end
 integer c;
 always @(posedge cpu_clk or posedge cpu_reset)begin
  if(cpu_reset)begin
   cpu_state<=CPU_IDLE;request_toggle<=0;request_write<=0;
   request_address<=0;request_wdata<=0;request_mask<=0;
   cpu_done<=0;cpu_word_valid<=0;cpu_word_index<=0;cpu_rdata<=0;replay_index<=0;
   for(c=0;c<4;c=c+1)replay_words[c]<=0;
  end else begin
   cpu_done<=0;cpu_word_valid<=0;
   case(cpu_state)
    CPU_IDLE:if(cpu_req)begin
     request_write<=cpu_write;request_address<=cpu_address;
     request_wdata<=cpu_wdata;request_mask<=cpu_mask;
     request_toggle<=!request_toggle;cpu_state<=CPU_WAIT;
    end
    CPU_WAIT:if(response_sync==request_toggle)cpu_state<=CPU_CAPTURE;
    CPU_CAPTURE:begin
     for(c=0;c<4;c=c+1)replay_words[c]<=response_words[c];
     replay_index<=0;cpu_state<=request_write ? CPU_FINISH:CPU_REPLAY;
    end
    CPU_REPLAY:begin
     cpu_rdata<=replay_words[replay_index];cpu_word_index<=replay_index;cpu_word_valid<=1;
     replay_index<=replay_index+1'b1;
     if(replay_index==3)cpu_state<=CPU_FINISH;
    end
    CPU_FINISH:begin cpu_done<=1;cpu_state<=CPU_IDLE;end
    default:cpu_state<=CPU_IDLE;
   endcase
  end
 end
 integer m;
 always @(posedge mem_clk or posedge mem_reset)begin
  if(mem_reset)begin
   mem_state<=MEM_IDLE;response_toggle<=0;operation_write<=0;
   operation_address<=0;operation_wdata<=0;operation_mask<=0;received_words<=0;
   for(m=0;m<4;m=m+1)response_words[m]<=0;
  end else begin
   case(mem_state)
    MEM_IDLE:if(request_sync!=response_toggle)begin
     operation_write<=request_write;operation_address<=request_address;
     operation_wdata<=request_wdata;operation_mask<=request_mask;
     received_words<=0;mem_state<=MEM_SEND;
    end
    MEM_SEND:if(mem_ready)mem_state<=MEM_WAIT;
    MEM_WAIT:begin
     if(mem_word_valid)begin
      response_words[mem_word_index]<=mem_rdata;
      received_words[mem_word_index]<=1;
     end
     if(mem_done)begin response_toggle<=request_sync;mem_state<=MEM_IDLE;end
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
