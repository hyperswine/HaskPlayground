`timescale 1ns/1ps
// Independent transaction model: unrelated phase/frequency, random backend
// stalls, out-of-order line beats, all masks, and reset in each pipeline phase.
module clock_bridge_tb #(
 parameter real CPU_HALF=4.62962963,MEM_HALF=9.25925926,MEM_PHASE=1.7);
 reg cpu_clk=0;always #(CPU_HALF)cpu_clk=~cpu_clk;
 reg mem_clk=0;initial begin #(MEM_PHASE);forever #(MEM_HALF)mem_clk=~mem_clk;end
 reg reset=1,req=0,write=0;
 reg [20:0] address=0;reg [31:0] wdata=0;reg [3:0] mask=0;
 wire ready,done,word_valid;wire [31:0] rdata;wire [1:0] word_index;
 wire mr,mw,mready;wire [20:0] ma;wire [31:0] md;wire [3:0] mm;
 reg mdone=0,mvalid=0;reg [31:0] mdata=0;reg [1:0] mindex=0;
 sdram_clock_bridge dut(
  .cpu_clk(cpu_clk),.cpu_reset(reset),.cpu_req(req),.cpu_write(write),.cpu_address(address),
  .cpu_wdata(wdata),.cpu_mask(mask),.cpu_ready(ready),.cpu_done(done),.cpu_rdata(rdata),
  .cpu_word_valid(word_valid),.cpu_word_index(word_index),
  .mem_clk(mem_clk),.mem_reset(reset),.mem_req(mr),.mem_write(mw),.mem_address(ma),
  .mem_wdata(md),.mem_mask(mm),.mem_ready(mready),.mem_done(mdone),.mem_rdata(mdata),
  .mem_word_valid(mvalid),.mem_word_index(mindex));
 reg [31:0] memory[0:255],expected[0:255];
 integer backend_state=0,delay_left=0,beat=0,refresh_count=0,refreshes=0;
 integer accepted=0,completed=0,expected_operations=0,comparisons=0;
 reg [20:0] saved_address;reg [31:0] saved_data;reg [3:0] saved_mask;
 reg saved_write;reg [31:0] random_state=32'hc0f1783d;
 assign mready=!reset && backend_state==0 && refresh_count<18;
 function [31:0] merged(input [31:0] old_word,data,input [3:0] lanes);
  integer k;begin merged=old_word;for(k=0;k<4;k=k+1)if(lanes[k])merged[k*8+:8]=data[k*8+:8];end
 endfunction
 // Refresh blocks new acceptance, as the real controller does. Response beat
 // order 2,0,3,1 deliberately differs from replay order to detect wrong buses.
 function [1:0] beat_index(input integer b);
  begin case(b)0:beat_index=2;1:beat_index=0;2:beat_index=3;default:beat_index=1;endcase end
 endfunction
 always @(posedge mem_clk)begin
  mdone<=0;mvalid<=0;
  if(reset)begin backend_state<=0;refresh_count<=0;delay_left<=0;beat<=0;end
  else begin
   random_state<=random_state^(random_state<<13)^(random_state>>9);
   if(refresh_count==22)begin refresh_count<=0;refreshes<=refreshes+1;end
   else refresh_count<=refresh_count+1;
   case(backend_state)
    0:if(mr && mready)begin
     if(ma>255 || (!mw && ma[1:0]!=0))$fatal(1,"bad backend request address");
     accepted<=accepted+1;saved_address<=ma;saved_data<=md;saved_mask<=mm;saved_write<=mw;
     delay_left<=1+random_state[3:0];backend_state<=1;beat<=0;
    end
    1:if(delay_left!=0)delay_left<=delay_left-1;
      else if(saved_write)begin memory[saved_address]<=merged(memory[saved_address],saved_data,saved_mask);backend_state<=3;end
      else backend_state<=2;
    2:if(random_state[2:0]!=0)begin
     mvalid<=1;mindex<=beat_index(beat);mdata<=memory[saved_address+beat_index(beat)];
     if(beat==3)backend_state<=3;else beat<=beat+1;
    end
    3:begin mdone<=1;completed<=completed+1;backend_state<=0;end
   endcase
  end
 end
 task access(input wr,input [20:0] addr,input [31:0] data,input [3:0] lanes);
  integer beats,cycles;reg posted;begin
   @(negedge cpu_clk);req=1;write=wr;address=addr;wdata=data;mask=lanes;
   @(posedge cpu_clk);while(!ready)@(posedge cpu_clk);
   #0.1;expected_operations=expected_operations+1;
   if(wr)expected[addr]=merged(expected[addr],data,lanes);
   posted=done;
   @(negedge cpu_clk);req=0;write=!wr;address=21'h123456;wdata=32'hfaced00d;mask=0;
   beats=0;cycles=0;
   begin: response
    // A posted write may complete in the clock right after its acceptance.
    if(posted)begin if(!wr)$fatal(1,"read completed on acceptance");disable response;end
    while(1)begin
     @(posedge cpu_clk);#0.1;cycles=cycles+1;
     if(word_valid)begin
      if(wr || word_index!==beats[1:0] || beats>3)$fatal(1,"unexpected/repeated/reordered CPU beat");
      if(rdata!==expected[addr+beats])$fatal(1,"wrong CPU data %h at %h/%0d expected %h",rdata,addr,beats,expected[addr+beats]);
      beats=beats+1;comparisons=comparisons+1;
     end
     // Completion may arrive with the final replayed beat, never before it.
     if(done)begin if(!wr && beats!=4)$fatal(1,"done before the final replay beat");disable response;end
     if(cycles>300)$fatal(1,"transaction timeout");
    end
   end
   if(beats!=(wr ? 0:4))$fatal(1,"wrong number of CPU beats");
  end
 endtask
 task reset_read(input integer phase);
  begin
   @(negedge cpu_clk);req=1;write=0;address=0;mask=15;
   @(posedge cpu_clk);while(!ready)@(posedge cpu_clk);
   @(negedge cpu_clk);req=0;
   case(phase)
    0:wait(dut.mem_state==1);
    1:wait(dut.mem_state==2);
    2:wait(dut.cpu_state==2);
    3:wait(word_valid);
   endcase
   #0.137;reset=1;repeat(7)@(negedge cpu_clk);reset=0;
   repeat(30)begin @(posedge cpu_clk);#0.1;if(done || word_valid)$fatal(1,"stale response after reset");end
   // The interrupted read may have reached the model; compare counts in the
   // next epoch rather than treating an intentionally cancelled read as lost.
   accepted=0;completed=0;expected_operations=0;
   access(0,0,0,15);
  end
 endtask
 integer n,k;reg [31:0] seed=32'h193a6e02;
 reg saw_full=0;always @(posedge cpu_clk)if(!reset && dut.full)saw_full<=1;
 initial begin
  for(n=0;n<256;n=n+1)begin memory[n]=32'h10203040^(n*32'h19030607);expected[n]=memory[n];end
  repeat(6)@(negedge cpu_clk);reset=0;
  access(0,0,0,15);
  for(n=0;n<16;n=n+1)begin access(1,16+(n&3),32'hdeadbabe^n,n[3:0]);access(0,16,0,15);end
  for(n=0;n<600;n=n+1)begin
   seed=seed^(seed<<13);seed=seed^(seed>>17);seed=seed^(seed<<5);
   access(1,seed[7:0],seed,seed[27:24]);access(0,{seed[7:2],2'b0},0,15);
  end
  // Posted writes back to back fill the FIFO; the following read must see
  // every one of them, in order, including repeated writes to one word.
  for(n=0;n<40;n=n+1)begin
   for(k=0;k<6;k=k+1)begin
    seed=seed^(seed<<13);seed=seed^(seed>>17);seed=seed^(seed<<5);
    access(1,{3'b110,n[1:0],k[2]&seed[0],k[1:0]},seed,seed[27:24]|(k==0 ? 4'h1:4'h0));
   end
   access(0,{3'b110,n[1:0],3'b000},0,15);access(0,{3'b110,n[1:0],3'b100},0,15);
  end
  if(!saw_full)$fatal(1,"the request FIFO never filled");
  repeat(30)@(posedge cpu_clk);
  if(accepted!=expected_operations || completed!=expected_operations)$fatal(1,"lost/duplicate transaction %0d/%0d expected %0d",accepted,completed,expected_operations);
  if(refreshes<5)$fatal(1,"backpressure/refresh was not exercised");
  for(k=0;k<4;k=k+1)reset_read(k);
  repeat(30)@(posedge cpu_clk);
  if(accepted!=expected_operations || completed!=expected_operations)$fatal(1,"wrong post-reset transaction count");
  $display("PASS: CDC CPUhalf=%0.4f memhalf=%0.4f phase=%0.3f: all masks, 1553 operations including 240 back-to-back posted writes, %0d comparisons, %0d stalls/refreshes, four interrupted-read resets",CPU_HALF,MEM_HALF,MEM_PHASE,comparisons,refreshes);
  $finish;
 end
 initial begin #20000000;$fatal(1,"overall timeout");end
endmodule
