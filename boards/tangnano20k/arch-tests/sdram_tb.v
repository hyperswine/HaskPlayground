`timescale 1ns/1ps
// riscv-arch-test runner for the SDRAM pipeline image, in simulation.
//
// As perf_tb.v: the generated CPU, the real cache, the clock bridge, the real
// burst controller and a functional SDRAM pin model run a preloaded image,
// and the stub ROM (stub_rom.S, given to the cache as its ROM) jumps to
// 0x8000_0000. When the test halts through the finisher the stub ROM stores a
// marker to the last RAM word; posted writes reach the pins in order, so at
// that point every earlier store is in the pin model and the signature words
// are written out, one 32-bit word per line as Spike does.
module sdram_tb #(
 parameter ROM_FILE="rom.hex",
 parameter CDC_BRIDGE=1,
 parameter real CPU_MHZ=72.0,
 parameter real MEM_MHZ=54.0,
 parameter PHASE_DEGREES=135,
 parameter LINE_BITS=12,
 parameter WRITE_UPDATE=0
);
 localparam real CPU_NS=1000.0/CPU_MHZ;
 localparam real MEM_NS=1000.0/MEM_MHZ;
 localparam [20:0] MARKER_WORD=21'h1fffff;
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

 // Functional SDRAM pins, as in perf_tb.v (CAS2, burst 4).
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
 reg marked=0;
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
         if(physical_word==MARKER_WORD)marked=1;
       end
       default:$fatal(1,"unsupported SDRAM pin command");
     endcase
   end
 end

 reg [4095:0] image_file,signature_file;integer words,i,sig_begin,sig_end,out;
 reg [63:0] limit,cycles=0;
 always @(posedge clk)cycles<=cycles+1;
 initial begin
   if(!$value$plusargs("IMAGE=%s",image_file) || !$value$plusargs("WORDS=%d",words))$fatal(1,"missing image/word count");
   if(!$value$plusargs("SIG_BEGIN=%d",sig_begin) || !$value$plusargs("SIG_END=%d",sig_end) ||
      !$value$plusargs("SIGNATURE=%s",signature_file))$fatal(1,"missing signature range/file");
   if(!$value$plusargs("LIMIT=%d",limit))limit=64'd50000000;
   for(i=0;i<2097152;i=i+1)ram[i]=0;
   $readmemh(image_file,ram,0,words-1);
   repeat(5)@(negedge clk);reset=0;
   wait(marked || cycles>=limit);
   if(!marked)begin $display("ARCH TIMEOUT cycles=%0d",cycles);$finish;end
   $display("ARCH END %08x cycles=%0d",ram[MARKER_WORD],cycles);
   out=$fopen(signature_file,"w");
   for(i=sig_begin;i<sig_end;i=i+1)$fdisplay(out,"%08x",ram[i]);
   $fclose(out);
   $finish;
 end
endmodule
