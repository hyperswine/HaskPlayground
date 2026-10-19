`timescale 1ns/1ps
// Where the SDRAM pipeline's cycles go, in simulation.
//
// The generated CPU, the real cache, the optional clock bridge, the real
// burst controller and a functional SDRAM pin model run a preloaded image.
// A two-instruction ROM jumps to 0x8000_0000, so there is no UART upload.
// UART output is decoded and echoed; the run ends at END_MARKER or the cycle
// limit. Counters come from the cache's own state and are printed at the end.
// This measures cycles, not electrical timing; it is not an I/O signoff.
module perf_tb #(
 parameter ROM_FILE="rom.hex",
 parameter CDC_BRIDGE=1,
 parameter real CPU_MHZ=72.0,
 parameter real MEM_MHZ=54.0,
 parameter PHASE_DEGREES=135,
 parameter UART_BIT=32,
 parameter LINE_BITS=12,
 parameter WRITE_UPDATE=1
);
 localparam real CPU_NS=1000.0/CPU_MHZ;
 localparam real MEM_NS=1000.0/MEM_MHZ;
 reg clk=0;always #(CPU_NS/2.0) clk=~clk;
 reg memory_clk=0;
 initial begin #(CPU_NS*0.313);forever #(MEM_NS/2.0) memory_clk=~memory_clk;end
 wire backend_clk=CDC_BRIDGE ? memory_clk:clk;
 reg reset=1;
 reg [1:0] cpu_reset_pipe=3,mem_reset_pipe=3;
 always @(posedge clk or posedge reset)if(reset)cpu_reset_pipe<=3;else cpu_reset_pipe<={cpu_reset_pipe[0],1'b0};
 always @(posedge backend_clk or posedge reset)if(reset)mem_reset_pipe<=3;else mem_reset_pipe<={mem_reset_pipe[0],1'b0};
 wire core_reset=cpu_reset_pipe[1];
 wire backend_reset=CDC_BRIDGE ? mem_reset_pipe[1]:core_reset;

 wire uart_tx,hold_cpu,cpu_write,sd_sck,sd_mosi,sd_cs;
 wire [21:0] cpu_address;wire [31:0] cpu_wdata,cpu_rdata;wire [3:0] cpu_mask;
 sdram_pipeline_risc cpu(.clk(clk),.reset(core_reset),.enable(1'b1),
   .uart_rx(1'b1),.uart_tx(uart_tx),.memory_data(cpu_rdata),.memory_hold(hold_cpu),
   .memory_address(cpu_address),.memory_write(cpu_write),.memory_wdata(cpu_wdata),.memory_mask(cpu_mask),
   .sd_miso(1'b1),.sd_sck(sd_sck),.sd_mosi(sd_mosi),.sd_cs(sd_cs));
 wire c_req,c_write,c_ready,c_done,c_valid;
 wire [20:0] c_address;wire [31:0] c_wdata,c_rdata;wire [3:0] c_mask;wire [1:0] c_word_index;
 sdram_pipeline_cache #(.LINE_BITS(LINE_BITS),.WRITE_UPDATE(WRITE_UPDATE),.ROM_FILE(ROM_FILE)) cache(clk,core_reset,
   cpu_address,cpu_write,cpu_wdata,cpu_mask,hold_cpu,cpu_rdata,
   c_req,c_write,c_address,c_wdata,c_mask,c_ready,c_done,c_rdata,c_valid,c_word_index);
 wire req,write,ready,done,valid;wire [20:0] address;wire [31:0] wdata,rdata;
 wire [3:0] mask;wire [1:0] word_index;
 generate if(CDC_BRIDGE)begin: clock_crossing
   sdram_clock_bridge bridge(.cpu_clk(clk),.cpu_reset(core_reset),
     .cpu_req(c_req),.cpu_write(c_write),.cpu_address(c_address),.cpu_wdata(c_wdata),.cpu_mask(c_mask),
     .cpu_ready(c_ready),.cpu_done(c_done),.cpu_rdata(c_rdata),.cpu_word_valid(c_valid),.cpu_word_index(c_word_index),
     .mem_clk(backend_clk),.mem_reset(backend_reset),
     .mem_req(req),.mem_write(write),.mem_address(address),.mem_wdata(wdata),.mem_mask(mask),
     .mem_ready(ready),.mem_done(done),.mem_rdata(rdata),.mem_word_valid(valid),.mem_word_index(word_index));
 end else begin: common_clock
   assign req=c_req;assign write=c_write;assign address=c_address;assign wdata=c_wdata;assign mask=c_mask;
   assign c_ready=ready;assign c_done=done;assign c_rdata=rdata;assign c_valid=valid;assign c_word_index=word_index;
 end endgenerate

 // Functional SDRAM pins, as in pipeline_cpu_tb.v (CAS2, burst 4).
 reg [31:0] ram[0:2097151];
 wire sclk,cke,cs_n,cas_n,ras_n,we_n;
 wire [3:0] dqm;wire [10:0] sa;wire [1:0] ba;tri [31:0] dq;
 reg phase_clk=0;
 always @(backend_clk)phase_clk<=#((CDC_BRIDGE ? MEM_NS:CPU_NS)*PHASE_DEGREES/360.0)backend_clk;
 sdram_burst_memory #(.FREQ(CDC_BRIDGE ? MEM_MHZ*1000000:CPU_MHZ*1000000)) mem(backend_clk,phase_clk,backend_reset,
   req,write,address,wdata,mask,ready,done,rdata,valid,word_index,sclk,cke,cs_n,cas_n,ras_n,we_n,dqm,sa,ba,dq);
 reg [10:0] open_row[0:3];reg [3:0] bank_open=0;reg [10:0] mode=0;reg configured=0;
 reg drive=0;reg [31:0] pin_data=0;
 assign dq=drive ? pin_data:32'bz;
 integer read_delay=0,read_beat=0,pin_lane,physical_word;
 reg active_read=0;reg [20:0] read_base;reg [1:0] read_bank;
 always @(posedge sclk)begin
   if(backend_reset)begin active_read=0;read_delay=0;read_beat=0;drive=0;bank_open=0;configured=0;mode=0;end
   else begin
     if(active_read)begin
       if(read_delay>1)begin read_delay=read_delay-1;drive=0;end
       else begin
         if(read_delay==1)read_delay=0;
         drive=1;pin_data=ram[read_base+read_beat];
         if(read_beat==3)begin active_read=0;bank_open[read_bank]=0;end
         else read_beat=read_beat+1;
       end
     end else drive=0;
     case({ras_n,cas_n,we_n})
       3'b111:begin end
       3'b010:if(sa[10])bank_open=0;else bank_open[ba]=0;
       3'b001:begin end
       3'b000:begin mode=sa;configured=1;end
       3'b011:begin open_row[ba]=sa;bank_open[ba]=1;end
       3'b101:begin read_base={ba,open_row[ba],sa[7:0]};read_bank=ba;read_delay=mode[6:4]-1;read_beat=0;active_read=1;end
       3'b100:begin
         physical_word={ba,open_row[ba],sa[7:0]};
         for(pin_lane=0;pin_lane<4;pin_lane=pin_lane+1)if(!dqm[pin_lane])
           ram[physical_word][pin_lane*8+:8]=dq[pin_lane*8+:8];
         bank_open[ba]=0;
       end
       default:$fatal(1,"unsupported SDRAM pin command");
     endcase
   end
 end

 // Counters, all in CPU clocks.
 localparam IDLE=0,WRITE_SEND=1,WRITE_WAIT=2,READ_SEND=3,READ_WAIT=4;
 integer text_end_word=0;
 reg [63:0] cycles=0,held=0,held_read=0,held_write_full=0,held_clear=0;
 reg [63:0] miss_draining=0,miss_filling=0,fills=0,code_fills=0,writes_sent=0,stores=0;
 reg [63:0] store_invalidated_fills=0,store_hit_invalidations=0,store_updates=0;
 reg [63:0] window_start=0,window_cycles=0,window_held=0,window_fills=0,window_writes=0;
 reg [19-LINE_BITS:0] killed_tag[0:(1<<LINE_BITS)-1];
 reg killed[0:(1<<LINE_BITS)-1];
 integer k;
 initial for(k=0;k<(1<<LINE_BITS);k=k+1)begin killed[k]=0;killed_tag[k]=0;end
 wire [LINE_BITS-1:0] line2=cache.a2[LINE_BITS+1:2];
 wire [18-LINE_BITS:0] tag2=cache.a2[20:LINE_BITS+2];
 reg measuring=0;
 always @(posedge clk)if(!core_reset)begin
   cycles<=cycles+1;
   if(hold_cpu)held<=held+1;
   if(cache.read_miss)held_read<=held_read+1;
   else if(cache.write_full)held_write_full<=held_write_full+1;
   if(cache.clearing)held_clear<=held_clear+1;
   if(cache.read_miss && (cache.state==WRITE_SEND || cache.state==WRITE_WAIT ||
      (cache.state==IDLE && cache.write_count!=0)))miss_draining<=miss_draining+1;
   if(cache.read_miss && (cache.state==READ_SEND || cache.state==READ_WAIT))miss_filling<=miss_filling+1;
   if(cache.state==READ_SEND && c_ready)begin
     fills<=fills+1;
     if(cache.a2[20:0]<text_end_word)code_fills<=code_fills+1;
     if(killed[line2] && killed_tag[line2]==tag2)store_invalidated_fills<=store_invalidated_fills+1;
     killed[line2]<=0;
   end
   if(cache.state==WRITE_SEND && c_ready)writes_sent<=writes_sent+1;
   if(cache.push)begin
     stores<=stores+1;
     if(cache.update)store_updates<=store_updates+1;
     // The store's lookup hit but it drops the line anyway.
     else if(cache.hit2)begin
       store_hit_invalidations<=store_hit_invalidations+1;
       killed[line2]<=1;killed_tag[line2]<=tag2;
     end
   end
   if(measuring)begin
     window_cycles<=window_cycles+1;
     if(hold_cpu)window_held<=window_held+1;
     if(cache.state==READ_SEND && c_ready)window_fills<=window_fills+1;
     if(cache.state==WRITE_SEND && c_ready)window_writes<=window_writes+1;
   end
 end

 // UART receiver for the CPU's transmitter (UART_BIT clocks per bit).
 reg [8*64-1:0] line_text=0;integer line_length=0;
 reg [8*64-1:0] end_marker,start_marker;
 integer bit_index;reg [7:0] received;
 task finish_run;
 begin
   $display("PERF cycles=%0d held=%0d held_read_miss=%0d held_write_full=%0d held_clear=%0d",
     cycles,held,held_read,held_write_full,held_clear);
   $display("PERF miss_draining=%0d miss_filling=%0d fills=%0d code_fills=%0d writes=%0d stores=%0d",
     miss_draining,miss_filling,fills,code_fills,writes_sent,stores);
   $display("PERF store_updates=%0d store_hit_invalidations=%0d store_invalidated_fills=%0d",
     store_updates,store_hit_invalidations,store_invalidated_fills);
   $display("PERF window_cycles=%0d window_held=%0d window_fills=%0d window_writes=%0d",
     window_cycles,window_held,window_fills,window_writes);
   $finish;
 end endtask
 initial begin
   if(!$value$plusargs("END=%s",end_marker))end_marker="FPR EXIT 0";
   if(!$value$plusargs("START=%s",start_marker))start_marker="DTREE DATA READY";
   forever begin
     @(negedge uart_tx);
     repeat(UART_BIT/2)@(posedge clk);
     for(bit_index=0;bit_index<8;bit_index=bit_index+1)begin
       repeat(UART_BIT)@(posedge clk);received[bit_index]=uart_tx;
     end
     repeat(UART_BIT)@(posedge clk);
     $write("%c",received);$fflush;
     if(received==10)begin
       // Whole-line matches: line_text is zero above the line's characters.
       if(line_text==start_marker && !measuring)begin measuring=1;window_start=cycles;end
       if(line_text==end_marker)finish_run;
       line_text=0;line_length=0;
     end else begin line_text={line_text[8*63-1:0],received};line_length=line_length+1;end
   end
 end

 reg [4095:0] image_file;integer words,i;reg [63:0] limit;
 initial begin
   if(!$value$plusargs("IMAGE=%s",image_file) || !$value$plusargs("WORDS=%d",words))$fatal(1,"missing image/word count");
   if(!$value$plusargs("TEXT_END=%d",text_end_word))text_end_word=0;
   if(!$value$plusargs("LIMIT=%d",limit))limit=64'd400000000;
   for(i=0;i<2097152;i=i+1)ram[i]=0;
   $readmemh(image_file,ram,0,words-1);
   repeat(5)@(negedge clk);reset=0;
   wait(cycles>=limit);
   $display("\nPERF LIMIT reached");
   finish_run;
 end
endmodule
