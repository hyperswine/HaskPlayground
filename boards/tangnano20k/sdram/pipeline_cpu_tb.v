`timescale 1ns/1ps
module pipeline_cpu_tb #(
 parameter ROM_FILE="output/tangnano20k/pipeline-rtl/rom.hex",
 parameter EXPECT_TRAPS=4,
 parameter LOADER_TEST=0,
 parameter UART_BIT=868,
 parameter PIN_MODEL=0,
 parameter LOADER_DUMP=0,
 parameter PIN_FREQ_MHZ=48,
 parameter PIN_PHASE_DEGREES=180,
 parameter real PIN_TAC_NS=0.0
);
 localparam STATUS=21'h180000,BOOT_COUNT=21'h1ffc00;
 localparam COUNTER_LINE=21'h140000,LONG_LINE=21'h0c0000;
 localparam real CLOCK_NS=PIN_MODEL ? 1000.0/PIN_FREQ_MHZ:10.0;
 reg clk=0;always #(CLOCK_NS/2.0) clk=~clk;
 reg reset=1,uart_rx=1;
 wire uart_tx,hold_cpu,cpu_write;
 wire [21:0] cpu_address;wire [31:0] cpu_wdata,cpu_rdata;
 wire [3:0] cpu_mask;
 sdram_pipeline_risc cpu(.clk(clk),.reset(reset),.enable(1'b1),
   .uart_rx(uart_rx),.uart_tx(uart_tx),.memory_data(cpu_rdata),.memory_hold(hold_cpu),
   .memory_address(cpu_address),.memory_write(cpu_write),.memory_wdata(cpu_wdata),.memory_mask(cpu_mask));
 wire req,write,ready;wire [20:0] address;wire [31:0] wdata;wire [3:0] mask;
 wire done,valid;wire [31:0] rdata;wire [1:0] word_index;
 reg fake_done=0,fake_valid=0;reg [31:0] fake_rdata=0;reg [1:0] fake_word_index=0;
 sdram_pipeline_cache #(.ROM_FILE(ROM_FILE)) cache(clk,reset,
   cpu_address,cpu_write,cpu_wdata,cpu_mask,hold_cpu,cpu_rdata,
   req,write,address,wdata,mask,ready,done,rdata,valid,word_index);

 reg [31:0] ram[0:2097151];
 reg [31:0] expected_image[0:16383];
 integer i,words,send_index;reg [4095:0] image_file;
 integer tick=0,delay=0,phase=0,reads=0,writes=0,held_clocks=0;
 integer long_held=0,ready_stalls=0,markers=0,quiet=0;
 reg saved_write=0,long_seen=0,break_sent=0,corrupt=0;
 reg [20:0] saved_address=0;reg [31:0] saved_data=0;reg [3:0] saved_mask=0;
 integer lane;
 always @(posedge clk)begin
   tick<=tick+1;
   if(!reset && hold_cpu)held_clocks<=held_clocks+1;
   if(!reset && hold_cpu && long_seen && !break_sent)long_held<=long_held+1;
   if(req && !ready)ready_stalls<=ready_stalls+1;
 end
 generate if(!PIN_MODEL)begin: delayed_backend
 assign ready=!reset && delay==0 && phase==0 && tick%7!=0;
 assign done=fake_done;assign valid=fake_valid;assign rdata=fake_rdata;assign word_index=fake_word_index;
 always @(posedge clk)begin
   fake_done<=0;fake_valid<=0;
   if(req && ready)begin
     saved_write<=write;saved_address<=address;saved_data<=wdata;saved_mask<=mask;
     delay<=12+tick%13;
     if(write)writes<=writes+1;
     else begin
       reads<=reads+1;
       if(address[1:0]!=0)$fatal(1,"unaligned line refill %h",address);
       if(address==COUNTER_LINE)delay<=1000;
       if(address==LONG_LINE && ram[STATUS]==32'h600d0001 && !long_seen)begin
         delay<=12000;long_seen<=1;
       end
     end
   end else if(delay>0)begin
     delay<=delay-1;
     if(delay==1)begin
       if(saved_write)begin
         for(lane=0;lane<4;lane=lane+1)
           if(saved_mask[lane])ram[saved_address][lane*8+:8]<=saved_data[lane*8+:8];
         fake_done<=1;
       end else phase<=1;
     end
   end else if(phase!=0 && tick%3!=0)begin
     // Uneven gaps between each beat exercise valid/index/data independently.
     if(phase<=4)begin
       fake_rdata<=ram[saved_address+phase-1] ^ ((corrupt && saved_address==COUNTER_LINE && phase==1) ? 1:0);
       fake_word_index<=phase-1;fake_valid<=1;phase<=phase+1;
     end else begin fake_done<=1;phase<=0;end
   end
 end
 end else begin: physical_backend
   wire sclk,cke,cs_n,cas_n,ras_n,we_n;
   wire [3:0] dqm;wire [10:0] sa;wire [1:0] ba;tri [31:0] dq;
   reg phase_clk=0;
   // Transport the core clock edges, including phases exceeding 180 degrees.
   // A delayed continuous assignment would reject pulses at those delays.
   always @(clk)phase_clk<=#(CLOCK_NS*PIN_PHASE_DEGREES/360.0)clk;
   sdram_burst_memory #(.FREQ(PIN_FREQ_MHZ*1000000)) mem(clk,phase_clk,reset,req,write,address,wdata,mask,
     ready,done,rdata,valid,word_index,sclk,cke,cs_n,cas_n,ras_n,we_n,dqm,sa,ba,dq);
   reg [10:0] open_row[0:3];reg [3:0] bank_open=0;
   reg [10:0] mode=0;reg configured=0;
   reg drive=0;reg [31:0] pin_data=0;
   // A functional output-access delay. This excludes FPGA I/O routing,
   // command setup/hold, jitter and PVT, so it is not an I/O timing signoff.
   assign #(PIN_TAC_NS) dq=drive ? pin_data:32'bz;
   integer read_delay=0,read_beat=0,pin_lane,physical_word;
   reg active_read=0;reg [20:0] read_base;reg [1:0] read_bank;
   reg [20:0] capture_base;
   always @(posedge clk)begin
     if(req && ready)begin
       if(write)writes<=writes+1;
       else begin reads<=reads+1;capture_base<=address;end
     end
     if(!reset && valid && rdata!==ram[capture_base+word_index])
       $fatal(1,"SDRAM capture word %h beat%0d=%h expected=%h",capture_base,word_index,rdata,ram[capture_base+word_index]);
   end
   // Micron 256MSDRAM_G p13, CAS Latency: after READ at edge N, DQ starts
   // driving after N+CAS-1 and is valid by N+CAS. The optional tAC delay
   // models output access after that earlier edge, not an extra full clock.
   // https://people.eecs.berkeley.edu/~kubitron/courses/cs152-S04/handouts/256MSDRAM_G.pdf
   // Consecutive beats and real completion replace the gapped backend.
   always @(posedge sclk)begin
     if(reset)begin active_read=0;read_delay=0;read_beat=0;drive=0;bank_open=0;configured=0;mode=0;end
     else begin
       if(cke!==1 || cs_n!==0)$fatal(1,"SDRAM select/CKE");
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
         3'b001:if(bank_open!=0 || active_read || drive)$fatal(1,"SDRAM refresh overlaps burst");
         3'b000:begin
           if(sa!==11'h222 || ba!==0 || bank_open!=0)$fatal(1,"SDRAM mode set %h bank%0d",sa,ba);
           mode=sa;configured=1;
         end
         3'b011:begin
           if(!configured || bank_open[ba] || active_read || drive)$fatal(1,"SDRAM activation overlap");
           open_row[ba]=sa;bank_open[ba]=1;
         end
         3'b101:begin
           if(!configured || !bank_open[ba] || active_read || drive)$fatal(1,"SDRAM read without activation");
           if(sa[10]!==1 || sa[9:8]!==0 || sa[1:0]!==0 || dqm!==0)$fatal(1,"SDRAM read alignment/mask");
           read_base={ba,open_row[ba],sa[7:0]};read_bank=ba;read_delay=mode[6:4]-1;read_beat=0;active_read=1;
         end
         3'b100:begin
           if(!configured || !mode[9] || !bank_open[ba] || active_read || drive)$fatal(1,"SDRAM write without activation");
           if(sa[10]!==1 || sa[9:8]!==0)$fatal(1,"SDRAM write column/precharge");
           physical_word={ba,open_row[ba],sa[7:0]};
           for(pin_lane=0;pin_lane<4;pin_lane=pin_lane+1)if(!dqm[pin_lane])begin
             if((^dq[pin_lane*8+:8])===1'bx)$fatal(1,"SDRAM unknown write data");
             ram[physical_word][pin_lane*8+:8]=dq[pin_lane*8+:8];
           end
           bank_open[ba]=0;
         end
         default:$fatal(1,"unsupported SDRAM pin command");
       endcase
     end
   end
 end endgenerate

 task send_byte(input [7:0] byte_value);
 integer bit_number;
 begin
   @(negedge clk);uart_rx=0;
   repeat(UART_BIT)@(negedge clk);
   for(bit_number=0;bit_number<8;bit_number=bit_number+1)begin
     uart_rx=byte_value[bit_number];repeat(UART_BIT)@(negedge clk);
   end
   uart_rx=1;repeat(UART_BIT)@(negedge clk);
 end endtask
 initial begin
   if(LOADER_TEST)begin
     wait(!reset);repeat(2000)@(negedge clk);
     send_byte(8'h50);send_byte(words[7:0]);send_byte(words[15:8]);
     for(send_index=0;send_index<words*4;send_index=send_index+1)
       send_byte(expected_image[send_index/4] >> ((send_index%4)*8));
     send_byte(LOADER_DUMP ? 8'h44:8'h52);
   end else begin
     wait(long_seen);repeat(3)@(negedge clk);
     send_byte(8'h03);break_sent=1;
   end
 end
 reg [7:0] transmitted;integer bit_number,j;
 reg [31:0] last_four=0;
 reg [31:0] dump_value=0;integer dump_digits=0,dump_lines=0,nibble;
 initial forever begin
   @(negedge uart_tx);
   repeat(UART_BIT+UART_BIT/2)@(posedge clk);
   for(bit_number=0;bit_number<8;bit_number=bit_number+1)begin
     transmitted[bit_number]=uart_tx;repeat(UART_BIT)@(posedge clk);
   end
   if(uart_tx!==1)$fatal(1,"UART framing");
   if(LOADER_TEST)begin
     $write("%c",transmitted);$fflush;
     if(LOADER_DUMP)begin
       if(transmitted==10)begin
         if(dump_digits!=8 || dump_value!==ram[dump_lines])$fatal(1,"debug ROM dump word%0d=%h backing=%h digits=%0d",dump_lines,dump_value,ram[dump_lines],dump_digits);
         dump_value=0;dump_digits=0;dump_lines=dump_lines+1;
         if(dump_lines==8)begin
           for(j=0;j<words;j=j+1)if(ram[j]!==expected_image[j])$fatal(1,"ROM uploaded word %0d mismatch",j);
           if(ram[words]!==0)$fatal(1,"ROM did not append zero sentinel");
           $display("PASS: debug ROM memory dump matches eight backing words");$finish;
         end
       end else begin
         if(transmitted>=48 && transmitted<=57)nibble=transmitted-48;
         else if(transmitted>=97 && transmitted<=102)nibble=transmitted-87;
         else $fatal(1,"debug ROM nonhex output %h",transmitted);
         dump_value={dump_value[27:0],nibble[3:0]};dump_digits=dump_digits+1;
       end
     end else begin
     if(transmitted==8'h4c)markers=markers+1;
     last_four={last_four[23:0],transmitted};
     if(last_four=="DONE")begin
       if(markers!=1)$fatal(1,"ROM DONE without guest marker; RAM %h %h %h %h expected %h %h %h %h",
         ram[0],ram[1],ram[2],ram[3],expected_image[0],expected_image[1],expected_image[2],expected_image[3]);
       for(j=0;j<words;j=j+1)if(ram[j]!==expected_image[j])$fatal(1,"ROM uploaded word %0d mismatch",j);
       if(ram[words]!==0)$fatal(1,"ROM did not append zero sentinel");
       $display("\nPASS: actual ROM UART loader, program execution and trap DONE");$finish;
     end
     end
   end else begin
     if((markers==0 && transmitted!=8'h50) || (markers==1 && transmitted!=8'h52) || markers>1)
       $fatal(1,"UART marker %0d unexpected %h",markers,transmitted);
     markers=markers+1;
   end
 end

 always @(negedge clk)begin
   if(!LOADER_TEST && !reset && ram[STATUS]===32'hbad00001)
     $fatal(1,"guest failure stage=%0d cycles=%0d retired=%0d",ram[STATUS+1],ram[STATUS+2],ram[STATUS+3]);
   if(!LOADER_TEST && !reset && ram[STATUS]===32'h600d0002 && markers==2 && cache.write_count==0 && cache.state==0)begin
     quiet=quiet+1;
     if(quiet==8)begin
       if(ram[BOOT_COUNT]!==2)$fatal(1,"Ctrl-C did not restart through ROM");
       if(ram[21'h080000]!==32'hbbccaa44)$fatal(1,"masked write/physical +2MiB");
       if(ram[21'h100000]!==32'hdeadbeef)$fatal(1,"physical +4MiB alias");
       if(ram[21'h1fffff]!==32'habcdef01)$fatal(1,"last SDRAM word");
       if(ram[21'h080400]!==32'h02b00513 || ram[21'h080401]!==32'h00008067)$fatal(1,"instruction patch backing data");
       if(ram[STATUS+4]!==32'hbbccaa44)$fatal(1,"forwarded partial result");
       if(ram[STATUS+5]!==5691265)$fatal(1,"branch/multiply loop");
       if(ram[STATUS+6]!==EXPECT_TRAPS)$fatal(1,"precise trap count");
       if(ram[STATUS+2]<1000 || ram[STATUS+3]==0 || ram[STATUS+3]>=64)$fatal(1,"hold/counter contract");
       if(ram[STATUS+7]<1000)$fatal(1,"mtime did not advance across memory hold");
       if(!break_sent || long_held<8000 || ready_stalls==0)$fatal(1,"hold/backpressure/UART leg was not exercised");
       $display("PASS: pipeline/cache guest, high RAM, masked stores, fence.i, IM/branches/traps, counters and Ctrl-C; %0d fills %0d writes %0d held clocks",reads,writes,held_clocks);
       $finish;
     end
   end else quiet=0;
 end
 initial begin
   corrupt=$test$plusargs("CORRUPT");
   if(!$value$plusargs("IMAGE=%s",image_file) || !$value$plusargs("WORDS=%d",words))$fatal(1,"missing image/word count");
   for(i=0;i<2097152;i=i+1)ram[i]=LOADER_TEST ? 32'h1f3d5b79^(i*32'h10204081)^(i<<11):0;
   if(LOADER_TEST)$readmemh(image_file,expected_image,0,words-1);
   else $readmemh(image_file,ram,0,words-1);
   ram[COUNTER_LINE]=32'h13579bdf;
   repeat(5)@(negedge clk);reset=0;
 end
 initial begin
   repeat(LOADER_TEST ? 700000:200000)@(posedge clk);
   $fatal(1,"timeout status=%h stage=%0d boots=%0d cpu=%h hold=%b cache=%0d fills=%0d writes=%0d markers=%0d long=%b",
     ram[STATUS],ram[STATUS+1],ram[BOOT_COUNT],cpu_address,hold_cpu,cache.state,reads,writes,markers,long_seen);
 end
endmodule
