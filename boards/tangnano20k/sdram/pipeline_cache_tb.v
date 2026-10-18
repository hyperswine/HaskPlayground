`timescale 1ns/1ps
module pipeline_cache_tb;
 reg clk=0;always #5 clk=~clk;
 reg reset=1;reg [21:0] address=22'h200000;reg wr=0;
 reg [31:0] data=0;reg [3:0] mask=15;
 wire hold_cpu;wire [31:0] rdata;
 wire req,mwrite,ready;wire [20:0] ma;wire [31:0] md;wire [3:0] mm;
 reg done=0,valid=0;reg [31:0] mrdata;reg [1:0] beat;
 // Match SdramPipeline: its registered request is the first logical stage.
 reg [21:0] port_address=22'h200000;reg port_write=0;
 reg [31:0] port_data=0;reg [3:0] port_mask=15;
 always @(posedge clk)begin
   if(reset)begin port_address<=22'h200000;port_write<=0;port_data<=0;port_mask<=15;end
   else if(!hold_cpu)begin port_address<=address;port_write<=wr;port_data<=data;port_mask<=mask;end
 end
 sdram_pipeline_cache cache(clk,reset,port_address,port_write,port_data,port_mask,hold_cpu,rdata,
   req,mwrite,ma,md,mm,ready,done,mrdata,valid,beat);
 reg [31:0] ram[0:2097151],reference[0:2097151],rom[0:511];
 integer delay=0,tick=0,phase=0,reads=0,writes=0,stalls=0;
 reg saved_write;reg [20:0] saved_address;reg [31:0] saved_data;reg [3:0] saved_mask;
 assign ready=delay==0 && phase==0 && tick%7!=0;
 integer lane;
 always @(posedge clk)begin
   tick<=tick+1;done<=0;valid<=0;
   if(req && ready)begin
     saved_write<=mwrite;saved_address<=ma;saved_data<=md;saved_mask<=mm;delay<=8;
     if(mwrite)writes<=writes+1;else begin reads<=reads+1;if(ma[1:0]!=0)$fatal(1,"unaligned burst");end
   end else if(delay>0)begin
     delay<=delay-1;
     if(delay==1)begin
       if(saved_write)begin
         for(lane=0;lane<4;lane=lane+1)if(saved_mask[lane])ram[saved_address][lane*8+:8]<=saved_data[lane*8+:8];
         done<=1;
       end else phase<=1;
     end
   end else if(phase!=0)begin
     if(phase<=4)begin
       mrdata<=ram[saved_address+phase-1];beat<=phase-1;valid<=1;phase<=phase+1;
     end else begin done<=1;phase<=0;end
   end
 end
 reg [21:0] commands[0:1023];reg command_write[0:1023];
 reg [31:0] values[0:1023];reg [3:0] masks[0:1023];
 reg [31:0] expected[0:2047];reg check_read[0:2047];
 integer n=0,issued=0,logical=0,checked=0,i,j,expected_writes=0;
 task add(input [21:0] a,input w,input [31:0] d,input [3:0] m);
 begin commands[n]=a;command_write[n]=w;values[n]=d;masks[n]=m;n=n+1;if(w)expected_writes=expected_writes+1;end endtask
 always @(negedge clk)begin
   if(issued<n)begin address=commands[issued];wr=command_write[issued];data=values[issued];mask=masks[issued];end
   else begin address=22'h200000;wr=0;data=0;mask=15;end
 end
 always @(posedge clk)begin
   if(!reset && !hold_cpu)begin
     if(logical>=3 && check_read[logical-3])begin
       if(rdata!==expected[logical-3])$fatal(1,"reply %0d expected %h got %h",logical-3,expected[logical-3],rdata);
       checked=checked+1;
     end
     expected[logical]=address[21] ? rom[address[8:0]]:reference[address[20:0]];
     check_read[logical]=!wr && issued<n;
     if(wr)for(j=0;j<4;j=j+1)if(mask[j])reference[address[20:0]][j*8+:8]=data[j*8+:8];
     logical=logical+1;if(issued<n)issued=issued+1;
     if(issued==n && logical>n+6 && cache.write_count==0 && cache.state==0)begin
       if(writes!=expected_writes)$fatal(1,"writes expected %0d got %0d",expected_writes,writes);
       for(j=0;j<4096;j=j+1)if(ram[j]!==reference[j])$fatal(1,"backing write mismatch %0d",j);
       $display("PASS: %0d reads, %0d ordered masked writes, %0d line fills, %0d held clocks",checked,writes,reads,stalls);$finish;
     end
   end else if(!reset)stalls=stalls+1;
 end
 initial begin
   $readmemh("boards/tangnano20k/bootrom/bootrom.hex",rom);
   for(i=0;i<2097152;i=i+1)begin ram[i]=32'habc00000^i;reference[i]=ram[i];end
   for(i=0;i<8;i=i+1)add(22'h200000+i,0,0,15);
   for(i=0;i<32;i=i+1)add(i,0,0,15);
   for(i=0;i<32;i=i+1)add(i,0,0,15);
   // Direct-mapped conflicts and a read queued behind each store.
   for(i=0;i<8;i=i+1)begin
     add(1024+i,0,0,15);add(i,1,32'hdeadbeef^i,15);add(i,0,0,15);
   end
   for(i=0;i<80;i=i+1)begin add(64+(i%8),1,i*32'h01010101,1<<(i%4));add(64+(i%8),0,0,15);end
   for(i=0;i<32;i=i+1)add(128+i,1,i,15);
   for(i=0;i<32;i=i+1)add(128+i,0,0,15);
   add(22'h1fffff,1,32'h87654321,15);add(22'h1fffff,0,0,15);
   repeat(4)@(negedge clk);reset=0;
 end
 initial begin repeat(100000)@(posedge clk);$fatal(1,"timeout logical=%0d state=%0d",logical,cache.state);end
endmodule
