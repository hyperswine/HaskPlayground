`timescale 1ns/1ps
// Functional SDRAM pin/command model, not an electrical/timing model.
// It samples commands on the 180-degree SDRAM clock, returns CAS=2 data,
// and implements sequential four-word reads plus single-location masked writes.
module burst_memory_tb #(parameter FREQ=54000000,parameter real TAC_NS=0.0);
 localparam integer WORDS=2097152;
 localparam integer REFRESH_INTERVAL=FREQ/1000000*10;
 localparam real HALF_PERIOD=500000000.0/FREQ;
 reg clk=0;always #(HALF_PERIOD) clk=~clk;
 wire phase_clk=~clk;
 reg reset=1,req=0,write=0;
 reg [20:0] address=0;reg [31:0] wdata=0;reg [3:0] mask=15;
 wire ready,done,word_valid;wire [31:0] rdata;wire [1:0] word_index;
 wire sclk,cke,cs_n,cas_n,ras_n,we_n;
 wire [3:0] dqm;wire [10:0] sa;wire [1:0] ba;tri [31:0] dq;
 sdram_burst_memory #(.FREQ(FREQ)) dut(
  .clk(clk),.clk_sdram(phase_clk),.reset(reset),.req(req),.write(write),
  .address(address),.wdata(wdata),.mask(mask),.ready(ready),.done(done),.rdata(rdata),
  .word_valid(word_valid),.word_index(word_index),
  .O_sdram_clk(sclk),.O_sdram_cke(cke),.O_sdram_cs_n(cs_n),
  .O_sdram_cas_n(cas_n),.O_sdram_ras_n(ras_n),.O_sdram_wen_n(we_n),
  .O_sdram_dqm(dqm),.O_sdram_addr(sa),.O_sdram_ba(ba),.IO_sdram_dq(dq));

 reg [31:0] memory[0:WORDS-1];
 reg [31:0] expected[0:WORDS-1];
 reg [10:0] open_row[0:3];reg [3:0] bank_open=0;
 reg [10:0] mode=0;reg configured=0;
 reg model_drive=0;reg [31:0] model_data=0;
 assign #(TAC_NS) dq=model_drive ? model_data:32'bz;
 integer read_delay=0,read_beat=0;reg read_active=0;
 reg [20:0] read_base;reg [1:0] read_bank;reg read_precharge;
 integer commands_read=0,commands_write=0,commands_active=0;
 integer commands_precharge=0,commands_refresh=0,runtime_refresh=0;
 integer operations_read=0,operations_write=0,comparisons=0;
 integer init_i,bank_i,lane,physical_word;
 function [31:0] pattern(input [20:0] word_address);
  pattern=32'h1f3d5b79 ^ (word_address*32'h10204081) ^ (word_address<<11);
 endfunction
 function [31:0] merged(input [31:0] old_word,data,input [3:0] lanes);
  integer k;
  begin
   merged=old_word;
   for(k=0;k<4;k=k+1)if(lanes[k])merged[k*8+:8]=data[k*8+:8];
  end
 endfunction
 initial begin
  for(init_i=0;init_i<WORDS;init_i=init_i+1)begin memory[init_i]=pattern(init_i);expected[init_i]=pattern(init_i);end
  for(init_i=0;init_i<4;init_i=init_i+1)open_row[init_i]=0;
 end

 // Micron 256MSDRAM_G p13, CAS Latency: after READ at edge N, DQ starts
 // driving after N+CAS-1 and is valid by N+CAS. At CL2 the first beat
 // therefore starts after N+1 (plus tAC), followed by N+2/N+3/N+4.
 // https://people.eecs.berkeley.edu/~kubitron/courses/cs152-S04/handouts/256MSDRAM_G.pdf
 always @(posedge sclk)begin
  if(reset)begin
   read_active=0;read_delay=0;read_beat=0;model_drive=0;bank_open=0;configured=0;mode=0;
  end else begin
   if(sclk!==phase_clk || cke!==1 || cs_n!==0)$fatal(1,"SDRAM clock/select/CKE mismatch");
   if(read_active)begin
    if(read_delay>1)begin read_delay=read_delay-1;model_drive=0;end
    else begin
     if(read_delay==1)read_delay=0;
     model_drive=1;model_data=memory[read_base+read_beat];
     if(read_beat==3)begin read_active=0;if(read_precharge)bank_open[read_bank]=0;end
     else read_beat=read_beat+1;
    end
   end else model_drive=0;
   case({ras_n,cas_n,we_n})
    3'b111:begin end // NOP
    3'b010:begin // PRECHARGE
     commands_precharge=commands_precharge+1;
     if(sa[10])bank_open=0;else bank_open[ba]=0;
    end
    3'b001:begin // AUTO REFRESH (initialisation or autonomous refresh)
     if(bank_open!=0 || read_active || model_drive)$fatal(1,"refresh while a bank/burst is active");
     commands_refresh=commands_refresh+1;if(configured)runtime_refresh=runtime_refresh+1;
    end
    3'b000:begin // LOAD MODE REGISTER
     if(sa!==11'h222)$fatal(1,"mode register %h, expected 0x222: CAS2/read burst4/single writes",sa);
     if(ba!==0 || bank_open!=0)$fatal(1,"mode set with a bank open or wrong bank");
     mode=sa;configured=1;
    end
    3'b011:begin // ACTIVE
     if(!configured || bank_open[ba])$fatal(1,"activation before mode set or before auto-precharge");
     if(read_active || model_drive)$fatal(1,"activation overlaps a read burst");
     open_row[ba]=sa;bank_open[ba]=1;commands_active=commands_active+1;
    end
    3'b101:begin // READ with auto-precharge
     if(!configured || !bank_open[ba] || read_active || model_drive)$fatal(1,"READ without activation or overlapping a burst");
     if(sa[10]!==1 || sa[9:8]!==0 || sa[1:0]!==0 || dqm!==0)$fatal(1,"READ not aligned/unmasked/auto-precharged: A=%h DQM=%h",sa,dqm);
     read_base={ba,open_row[ba],sa[7:0]};read_bank=ba;read_precharge=sa[10];
     read_delay=mode[6:4]-1;read_beat=0;read_active=1;commands_read=commands_read+1;
    end
    3'b100:begin // WRITE: A9=1 in mode register means one location only
     if(!configured || !mode[9] || !bank_open[ba] || read_active || model_drive)$fatal(1,"WRITE missing activation/single-write mode or overlapping burst");
     if(sa[10]!==1 || sa[9:8]!==0)$fatal(1,"WRITE column/auto-precharge mismatch");
     physical_word={ba,open_row[ba],sa[7:0]};
     for(lane=0;lane<4;lane=lane+1)if(!dqm[lane])begin
      if((^dq[lane*8+:8])===1'bx)$fatal(1,"unknown/contentious active write byte");
      memory[physical_word][lane*8+:8]=dq[lane*8+:8];
     end
     bank_open[ba]=0;commands_write=commands_write+1;
    end
    default:$fatal(1,"unsupported SDRAM command %b",{ras_n,cas_n,we_n});
   endcase
  end
 end

 task access(input wr,input [20:0] addr,input [31:0] data,input [3:0] lanes);
  integer beats,wait_cycles;
  begin
   if(!wr && addr[1:0]!=0)$fatal(1,"test must request aligned read lines");
   @(negedge clk);req=1;write=wr;address=addr;wdata=data;mask=lanes;
   @(posedge clk);
   while(!ready)@(posedge clk);
   #1;
   if(wr)begin expected[addr]=merged(expected[addr],data,lanes);operations_write=operations_write+1;end
   else operations_read=operations_read+1;
   @(negedge clk);req=0;
   // The controller must retain accepted inputs through the later pin command.
   address=21'h155555;wdata=32'hbad0cafe;mask=0;write=!wr;
   beats=0;wait_cycles=0;
   begin: response
    while(1)begin
     @(posedge clk);#1;wait_cycles=wait_cycles+1;
     if(word_valid)begin
      if(wr || beats>=4 || word_index!==beats[1:0])$fatal(1,"wrong beat count/index: wr=%0d beat=%0d index=%0d",wr,beats,word_index);
      if(rdata!==expected[addr+beats])$fatal(1,"word %h beat %0d got %h expected %h",addr,beats,rdata,expected[addr+beats]);
      beats=beats+1;comparisons=comparisons+1;
     end
     if(done)disable response;
     if(wait_cycles>40)$fatal(1,"accepted operation timed out");
    end
   end
   if(beats!=(wr ? 0:4))$fatal(1,"completion before all read beats, got %0d",beats);
  end
 endtask
 task read_line(input [20:0] addr);begin access(0,addr,0,15);end endtask
 task poison_write(input [20:0] addr,input [3:0] lanes);
  begin access(1,addr,~expected[addr],lanes);end
 endtask

 reg previous_done=0;
 always @(posedge clk)begin
  #2;
  if(!reset && done && previous_done)$fatal(1,"duplicate completion pulse");
  previous_done=done;
 end
 integer n,before_refresh;reg [31:0] seed=32'h61c04af3;reg [20:0] line_address;
 initial begin
  repeat(4)@(negedge clk);reset=0;
  wait(ready);
  if(!configured || commands_precharge!=1 || commands_refresh<2)$fatal(1,"initialisation command sequence incomplete");
  read_line(0);read_line(21'h0000fc);read_line(21'h000100);
  for(bank_i=0;bank_i<4;bank_i=bank_i+1)begin
   read_line((bank_i<<19)+21'h7fffc); // last aligned line of every bank
   read_line(bank_i<<19);
  end
  // All sixteen masks, with every inactive input byte deliberately poisoned.
  for(n=0;n<16;n=n+1)begin
   poison_write(21'h001340+(n&3),n[3:0]);
   read_line(21'h001340);
   read_line(21'h001344); // single-location writes must not alter neighbours
  end
  poison_write(21'h1fffff,4'b0101);read_line(21'h1ffffc);
  poison_write(21'h07ffff,4'b1010);read_line(21'h07fffc);read_line(21'h080000);
  // Back-to-back bank/row changes and read-after-write ordering.
  for(n=0;n<80;n=n+1)begin
   seed=seed^(seed<<13);seed=seed^(seed>>17);seed=seed^(seed<<5);
   line_address=seed[20:0]&21'h1ffffc;
   poison_write(line_address+seed[22:21],seed[27:24]);
   read_line(line_address);
   read_line(line_address^21'h080000);
  end
  before_refresh=runtime_refresh;
  repeat(REFRESH_INTERVAL*3+30)@(posedge clk);
  if(runtime_refresh<before_refresh+3)$fatal(1,"autonomous idle refresh stopped");
  // Assert a new request while refresh has made ready low: the request waits
  // and is accepted exactly once when refresh finishes.
  wait(!ready);read_line(21'h1ffffc);
  repeat(20)@(posedge clk);
  if(commands_read!=operations_read || commands_write!=operations_write || commands_active!=operations_read+operations_write)
   $fatal(1,"duplicate/lost pin commands: reads %0d/%0d writes %0d/%0d active %0d",commands_read,operations_read,commands_write,operations_write,commands_active);
  $display("PASS: %0d MHz SDRAM pins CAS2/burst4/mode222: %0d reads, %0d masked writes, %0d beat comparisons, %0d autonomous refreshes",FREQ/1000000,operations_read,operations_write,comparisons,runtime_refresh);
  $finish;
 end
 initial begin repeat(200000)@(posedge clk);$fatal(1,"overall timeout");end
endmodule
