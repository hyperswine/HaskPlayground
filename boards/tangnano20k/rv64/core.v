// Serial RV64IM + Zicsr/Zifencei experiment. The bus is physical 32-bit words.
// Every memory request is held to acceptance and its response is drained.
module rv64_core(input clk,reset,input rx_valid,input [7:0] rx_data,
 output reg rx_pop,input tx_ready,output reg tx_send,output reg [7:0] tx_data,
 output memory_req,output memory_write,output [20:0] memory_address,
 output [31:0] memory_wdata,input memory_ready,memory_done,input [31:0] memory_rdata);
 localparam HOST=0,COUNT0=1,COUNT1=2,UPLOAD=3,SEND=4,WAIT=5,
 FETCH=6,DECODE=7,OPERANDS=8,EXECUTE=9,RETIRE=10,
 LOAD=11,LOADHI=12,STORE=13,STOREHI=14,MERGE=15,TX=16,
 MDINIT=17,MDLOOP=18,MDCOMMIT=19,MDFIX=20,SHIFT=21,FINISH=22,CLEAR=23,COUNT2=24,COUNT3=25,VERIFY=26,VERIFYACC=27,VERIFYTX=28;
 reg [4:0] state,resume;
 reg running,abort_pending;
 reg [63:0] regs[0:31];
 reg [63:0] pc,next_pc,a,b,operand,result,effective,store_data;
 reg [31:0] instruction,low_word,bus_data;
 reg [20:0] bus_address;
 reg bus_write;
 reg [4:0] rd,rs1,rs2;
 reg [6:0] opcode,funct7;
 reg [2:0] funct3;
 reg [31:0] upload_count;
 reg wide_upload;
 reg [31:0] verify_crc;
 reg [3:0] verify_digit;
 reg [20:0] upload_index;
 reg [1:0] upload_byte,finish_index;
 reg [31:0] upload_word,program_end;
 reg [5:0] shift_count;
 reg [63:0] cycle_count,retired,mtvec,mepc,mcause,mtval,mstatus,mscratch;
 reg [63:0] md_hi,md_lo,md_operand;
 reg [64:0] md_total;
 reg [5:0] md_index;
 reg md_negative_lo,md_negative_hi,md_is_div,md_word;
 wire emergency=running && rx_valid && !rx_pop && rx_data==3;
 assign memory_req=state==SEND && !emergency;
 assign memory_write=bus_write;assign memory_address=bus_address;assign memory_wdata=bus_data;
 function [63:0] sx32(input [31:0] x);sx32={{32{x[31]}},x};endfunction
 function [63:0] imm_i(input [31:0] x);imm_i={{52{x[31]}},x[31:20]};endfunction
 function [63:0] imm_s(input [31:0] x);imm_s={{52{x[31]}},x[31:25],x[11:7]};endfunction
 function [63:0] imm_b(input [31:0] x);imm_b={{51{x[31]}},x[31],x[7],x[30:25],x[11:8],1'b0};endfunction
 function [63:0] imm_j(input [31:0] x);imm_j={{43{x[31]}},x[31],x[19:12],x[20],x[30:21],1'b0};endfunction
 function ram_address(input [63:0] x);
 ram_address=(x[63:23]==41'h100 && x[22:0]<=23'h7fffff) || x[63:16]==0;
 endfunction
 // 0x80000000 / 2^23 = 0x100. Upper bits must match, never silently truncate.
 task request(input wr,input [63:0] addr,input [31:0] data,input [4:0] after_state);
 begin bus_write<=wr;bus_address<=addr[22:2];bus_data<=data;resume<=after_state;state<=SEND;end
 endtask
 task trap(input [63:0] cause,input [63:0] value);
 begin
   mepc<=pc;mcause<=cause;mtval<=value;
   mstatus[7]<=mstatus[3];mstatus[3]<=0;mstatus[12:11]<=3;
   if(mtvec[63:2]!=0) begin pc<={mtvec[63:2],2'b00};state<=FETCH;end
   else begin running<=0;finish_index<=0;state<=FINISH;end
 end endtask
 wire [63:0] address=a+(opcode==7'h23 ? imm_s(instruction):imm_i(instruction));
 wire [63:0] alu_b=opcode==7'h13 || opcode==7'h1b ? imm_i(instruction):b;
 wire word_op=opcode==7'h1b || opcode==7'h3b;
 wire md_signed_a=funct3==1 || funct3==2 || funct3==4 || funct3==6;
 wire md_signed_b=funct3==1 || funct3==4 || funct3==6;
 wire [63:0] md_a=word_op ? (funct3==5 || funct3==7 ? {32'b0,a[31:0]}:sx32(a[31:0])):a;
 wire [63:0] md_b=word_op ? (funct3==5 || funct3==7 ? {32'b0,b[31:0]}:sx32(b[31:0])):b;
 wire [64:0] remainder={md_hi,md_lo[63]};
 wire [63:0] shifted_load={32'b0,memory_rdata} >> (effective[1:0]*8);
 reg [63:0] csr_old,csr_new;
 reg csr_valid,csr_readonly;
 always @* begin
   csr_valid=1;csr_readonly=0;
   case(instruction[31:20])
     12'h300:csr_old=mstatus;
     12'h301:begin csr_old=64'h8000000000001100;csr_readonly=1;end
     12'h304,12'h344:begin csr_old=0;csr_readonly=1;end
     12'h305:csr_old=mtvec;
     12'h340:csr_old=mscratch;
     12'h341:csr_old=mepc;
     12'h342:csr_old=mcause;
     12'h343:csr_old=mtval;
     12'hb00,12'hc00:begin csr_old=cycle_count;csr_readonly=instruction[31:20]==12'hc00;end
     12'hb02,12'hc02:begin csr_old=retired;csr_readonly=instruction[31:20]==12'hc02;end
     12'hf11,12'hf12,12'hf13,12'hf14:begin csr_old=0;csr_readonly=1;end
     default:begin csr_old=0;csr_valid=0;end
   endcase
   case(funct3[1:0])
     1:csr_new=funct3[2] ? {59'b0,rs1}:a;
     2:csr_new=csr_old | (funct3[2] ? {59'b0,rs1}:a);
     default:csr_new=csr_old & ~(funct3[2] ? {59'b0,rs1}:a);
   endcase
 end
 integer i;
 always @(posedge clk) begin
   rx_pop<=0;tx_send<=0;
   if(reset) begin
     state<=HOST;running<=0;abort_pending<=0;pc<=0;program_end<=0;upload_count<=0;
     cycle_count<=0;retired<=0;mtvec<=0;mepc<=0;mcause<=0;mtval<=0;mstatus<=0;mscratch<=0;
     for(i=0;i<32;i=i+1) regs[i]<=0;
   end else begin
     cycle_count<=cycle_count+1'b1;
     if(emergency) begin
       rx_pop<=1;running<=0;mtvec<=0;mstatus<=0;retired<=0;
       for(i=0;i<32;i=i+1) regs[i]<=0;
       // A coincident completion is already drained; do not wait for a
       // second pulse that will never arrive.
       if(state==WAIT && !memory_done) abort_pending<=1;
       else begin state<=HOST;abort_pending<=0;end
     end else case(state)
       HOST:if(rx_valid && !rx_pop) begin
         rx_pop<=1;
         case(rx_data)
           "P","Q":begin state<=COUNT0;wide_upload<=rx_data=="Q";end
           "T":begin finish_index<=0;state<=FINISH;end
           "V":begin upload_index<=0;verify_crc<=0;verify_digit<=0;state<=upload_count==0 ? VERIFYTX:VERIFY;end
           "H","R":begin
             running<=1;pc<=rx_data=="H" ? 64'h80000000:0;state<=FETCH;
             mtvec<=0;mstatus<=0;retired<=0;abort_pending<=0;
             for(i=0;i<32;i=i+1) regs[i]<=0;
           end
           "M":begin upload_index<=0;state<=CLEAR;end
           "X":begin pc<=0;mtvec<=0;for(i=0;i<32;i=i+1) regs[i]<=0;end
         endcase
       end
       COUNT0:if(rx_valid && !rx_pop) begin rx_pop<=1;upload_count[7:0]<=rx_data;state<=COUNT1;end
       COUNT1:if(rx_valid && !rx_pop) begin
         rx_pop<=1;upload_count[15:8]<=rx_data;upload_index<=0;upload_byte<=0;
         program_end<={14'b0,rx_data,upload_count[7:0],2'b00};
         upload_count[31:16]<=0;
         state<=wide_upload ? COUNT2:(({rx_data,upload_count[7:0]}==0 || {rx_data,upload_count[7:0]}>16384) ? HOST:UPLOAD);
         if(!wide_upload && {rx_data,upload_count[7:0]}>16384) upload_count<=0;
       end
       COUNT2:if(rx_valid && !rx_pop) begin rx_pop<=1;upload_count[23:16]<=rx_data;state<=COUNT3;end
       COUNT3:if(rx_valid && !rx_pop) begin
         rx_pop<=1;upload_count[31:24]<=rx_data;
         state<=({rx_data,upload_count[23:0]}==0 || {rx_data,upload_count[23:0]}>524288) ? HOST:UPLOAD;
         if({rx_data,upload_count[23:0]}>524288) upload_count<=0;
       end
       UPLOAD:if(rx_valid && !rx_pop) begin
         rx_pop<=1;upload_word[upload_byte*8+:8]<=rx_data;
         if(upload_byte==3) begin
           request(1,{41'b0,upload_index,2'b0},{rx_data,upload_word[23:0]},UPLOAD);
           upload_index<=upload_index+1'b1;upload_byte<=0;
           if(upload_index+1==upload_count) resume<=HOST;
         end else upload_byte<=upload_byte+1'b1;
       end
       CLEAR:begin
         request(1,{41'b0,upload_index,2'b0},0,CLEAR);upload_index<=upload_index+1'b1;
         if(upload_index==16383) begin resume<=HOST;program_end<=0;end
       end
       SEND:if(memory_ready) state<=WAIT;
       WAIT:if(memory_done) begin
         state<=abort_pending ? HOST:resume;abort_pending<=0;
         if(resume==DECODE) instruction<=memory_rdata;
       end
       VERIFY:request(0,{41'b0,upload_index,2'b0},0,VERIFYACC);
       VERIFYACC:begin
         verify_crc<=verify_crc ^ memory_rdata;upload_index<=upload_index+1'b1;
         if(upload_index+1==upload_count) begin verify_digit<=0;state<=VERIFYTX;end
         else state<=VERIFY;
       end
       VERIFYTX:if(tx_ready && !tx_send) begin
         tx_send<=1;
         if(verify_digit==8) begin tx_data<=10;state<=HOST;end
         else begin
           tx_data<=((verify_crc >> ((7-verify_digit)*4)) & 15) <10 ?
             48+((verify_crc >> ((7-verify_digit)*4)) & 15):87+((verify_crc >> ((7-verify_digit)*4)) & 15);
           verify_digit<=verify_digit+1'b1;
         end
       end
       FETCH:if(pc[1:0]!=0) trap(0,pc);
         else if(!ram_address(pc)) trap(1,pc);
         else request(0,pc,0,DECODE);
       DECODE:begin
         opcode<=instruction[6:0];rd<=instruction[11:7];funct3<=instruction[14:12];
         rs1<=instruction[19:15];rs2<=instruction[24:20];funct7<=instruction[31:25];state<=OPERANDS;
       end
       OPERANDS:begin
         a<=rs1==0 ? 0:regs[rs1];b<=rs2==0 ? 0:regs[rs2];next_pc<=pc+4;state<=EXECUTE;
       end
       EXECUTE:begin
         state<=RETIRE;result<=0;
         case(opcode)
           7'h37:result<=sx32({instruction[31:12],12'b0});
           7'h17:result<=pc+sx32({instruction[31:12],12'b0});
           7'h6f:begin result<=pc+4;next_pc<=pc+imm_j(instruction);end
           7'h67:if(funct3==0) begin result<=pc+4;next_pc<=(a+imm_i(instruction)) & ~64'd1;end else trap(2,{32'b0,instruction});
           7'h63:begin
             rd<=0;
             case(funct3)
               0:if(a==b) next_pc<=pc+imm_b(instruction);
               1:if(a!=b) next_pc<=pc+imm_b(instruction);
               4:if($signed(a)<$signed(b)) next_pc<=pc+imm_b(instruction);
               5:if($signed(a)>=$signed(b)) next_pc<=pc+imm_b(instruction);
               6:if(a<b) next_pc<=pc+imm_b(instruction);
               7:if(a>=b) next_pc<=pc+imm_b(instruction);
               default:trap(2,{32'b0,instruction});
             endcase
           end
           7'h13,7'h33,7'h1b,7'h3b:begin
             if((opcode==7'h33 || opcode==7'h3b) && funct7==1) begin
               if(word_op && funct3!=0 && !funct3[2]) trap(2,{32'b0,instruction});else state<=MDINIT;
             end else case(funct3)
               0:if((opcode==7'h33 || opcode==7'h3b) && funct7==7'h20)
                 result<=word_op ? sx32(a[31:0]-alu_b[31:0]):a-alu_b;
                 else if((opcode==7'h33 || opcode==7'h3b) && funct7!=0) trap(2,{32'b0,instruction});
                 else result<=word_op ? sx32(a[31:0]+alu_b[31:0]):a+alu_b;
               1,5:begin
                 if((word_op && (funct7!=0 && !(funct3==5 && funct7==7'h20))) ||
                    (!word_op && ((opcode==7'h13 && instruction[31:26]!=0 && !(funct3==5 && instruction[31:26]==6'h10)) ||
                    (opcode==7'h33 && funct7!=0 && !(funct3==5 && funct7==7'h20)))) ) trap(2,{32'b0,instruction});
                 else begin result<=word_op ? sx32(a[31:0]):a;
                   shift_count<=word_op ? {1'b0,alu_b[4:0]}:alu_b[5:0];state<=SHIFT;end
               end
               2:if(word_op || (opcode==7'h33 && funct7!=0)) trap(2,{32'b0,instruction});else result<=($signed(a)<$signed(alu_b));
               3:if(word_op || (opcode==7'h33 && funct7!=0)) trap(2,{32'b0,instruction});else result<=(a<alu_b);
               4:if(word_op || (opcode==7'h33 && funct7!=0)) trap(2,{32'b0,instruction});else result<=a^alu_b;
               6:if(word_op || (opcode==7'h33 && funct7!=0)) trap(2,{32'b0,instruction});else result<=a|alu_b;
               7:if(word_op || (opcode==7'h33 && funct7!=0)) trap(2,{32'b0,instruction});else result<=a&alu_b;
             endcase
           end
           7'h03,7'h23:begin
             effective<=address;store_data<=b;
             if((opcode==3 && funct3==7) || (opcode==7'h23 && funct3>3)) trap(2,{32'b0,instruction});
             else if((funct3[1:0]==1 && address[0]) || (funct3[1:0]==2 && address[1:0]!=0) || (funct3[1:0]==3 && address[2:0]!=0)) trap(opcode==3 ? 4:6,address);
             else if(ram_address(address)) begin
               if(opcode==3) request(0,address,0,LOAD);
               else if(funct3<2) request(0,address,0,MERGE);
               else request(1,address,b[31:0],funct3==3 ? STOREHI:RETIRE);
               if(opcode==7'h23) rd<=0;
             end else if(address==64'h10000000 && opcode==7'h23 && funct3<=2) begin rd<=0;state<=TX;end
             else if(address==64'h10000004 && opcode==3 && (funct3==2 || funct3==6)) result<={62'b0,rx_valid,tx_ready};
             else if(address==64'h10000008 && opcode==3 && (funct3==0 || funct3==4 || funct3==2 || funct3==6)) begin
               result<=rx_valid ? (funct3==0 ? {{56{rx_data[7]}},rx_data}:{56'b0,rx_data}):0;rx_pop<=rx_valid;
             end else if(address==64'h00100000 && opcode==7'h23 && (funct3==1 || funct3==2)) begin
               rd<=0;if(b[15:0]==16'h5555 || b[15:0]==16'h3333) begin running<=0;state<=FINISH;finish_index<=0;end
             end else trap(opcode==3 ? 5:7,address);
           end
           7'h0f:begin rd<=0;if(funct3!=0 && funct3!=1) trap(2,{32'b0,instruction});end
           7'h73:begin
             if(funct3==0) begin
               if(instruction==32'h30200073) begin rd<=0;next_pc<=mepc;mstatus[3]<=mstatus[7];mstatus[7]<=1;mstatus[12:11]<=0;end
               else if(instruction==32'h00000073) trap(11,0);
               else if(instruction==32'h00100073) trap(3,pc);
               else trap(2,{32'b0,instruction});
             end else if(funct3==4 || !csr_valid || (csr_readonly && (funct3[1:0]==1 || rs1!=0))) trap(2,{32'b0,instruction});
             else begin
               result<=csr_old;
               if(funct3[1:0]==1 || rs1!=0) case(instruction[31:20])
                 12'h300:mstatus<=csr_new & 64'h1888;
                 12'h305:mtvec<=csr_new & ~64'd3;
                 12'h340:mscratch<=csr_new;
                 12'h341:mepc<=csr_new & ~64'd3;
                 12'h342:mcause<=csr_new;
                 12'h343:mtval<=csr_new;
                 12'hb00:cycle_count<=csr_new;
                 12'hb02:retired<=csr_new;
               endcase
             end
           end
           default:trap(2,{32'b0,instruction});
         endcase
       end
       SHIFT:if(shift_count==0) begin if(word_op) result<=sx32(result[31:0]);state<=RETIRE;end
         else begin
           shift_count<=shift_count-1'b1;
           if(funct3==1) result<=result<<1;
           else if(instruction[30]) result<=$signed(result)>>>1;
           else result<=word_op ? {33'b0,result[31:1]}:{1'b0,result[63:1]};
         end
       LOAD:begin
         case(funct3)
           0:result<={{56{shifted_load[7]}},shifted_load[7:0]};
           1:result<={{48{shifted_load[15]}},shifted_load[15:0]};
           2:result<=sx32(memory_rdata);
           3:begin low_word<=memory_rdata;request(0,effective+4,0,LOADHI);end
           4:result<={56'b0,shifted_load[7:0]};
           5:result<={48'b0,shifted_load[15:0]};
           6:result<={32'b0,memory_rdata};
         endcase
         if(funct3!=3) state<=RETIRE;
       end
       LOADHI:begin result<={memory_rdata,low_word};state<=RETIRE;end
       MERGE:request(1,effective,
         (memory_rdata & ~((funct3==0 ? 32'hff:32'hffff) << (effective[1:0]*8))) |
         ((store_data[31:0] & (funct3==0 ? 32'hff:32'hffff)) << (effective[1:0]*8)),RETIRE);
       STOREHI:request(1,effective+4,store_data[63:32],RETIRE);
       TX:if(tx_ready) begin tx_send<=1;tx_data<=store_data[7:0];state<=RETIRE;end
       MDINIT:begin
         md_is_div<=funct3[2];md_word<=word_op;md_index<=0;
         if(funct3[2] && md_b==0) begin result<=funct3[1] ? md_a:~64'd0;state<=MDFIX;end
         else begin
           md_hi<=0;
           md_lo<=funct3[2] ? (md_signed_a && md_a[63] ? -md_a:md_a):(md_signed_b && md_b[63] ? -md_b:md_b);
           md_operand<=funct3[2] ? (md_signed_b && md_b[63] ? -md_b:md_b):(md_signed_a && md_a[63] ? -md_a:md_a);
           md_negative_lo<=(md_signed_a && md_a[63]) ^ (md_signed_b && md_b[63]);
           md_negative_hi<=funct3[2] ? (md_signed_a && md_a[63]):((md_signed_a && md_a[63]) ^ (md_signed_b && md_b[63]));
           state<=MDLOOP;
         end
       end
       MDLOOP:begin md_total<=md_is_div ? remainder-{1'b0,md_operand}:({1'b0,md_hi}+(md_lo[0] ? {1'b0,md_operand}:65'b0));state<=MDCOMMIT;end
       MDCOMMIT:begin
         if(md_is_div) begin md_hi<=md_total[64] ? remainder[63:0]:md_total[63:0];md_lo<={md_lo[62:0],!md_total[64]};end
         else begin md_hi<=md_total[64:1];md_lo<={md_total[0],md_lo[63:1]};end
         md_index<=md_index+1'b1;
         if(md_index==63) begin state<=STORE;end else state<=MDLOOP;
       end
       STORE:begin
         if(funct3==0 || funct3==4 || funct3==5) result<=md_negative_lo ? -md_lo:md_lo;
         else result<=md_negative_hi ? (md_is_div ? -md_hi:(~md_hi+(md_lo==0))):md_hi;
         state<=MDFIX;
       end
       MDFIX:begin if(md_word) result<=sx32(result[31:0]);state<=RETIRE;end
       RETIRE:begin
         if(next_pc[1:0]!=0) trap(0,next_pc);
         else begin
           if(rd!=0) regs[rd]<=result;retired<=retired+1'b1;
           pc<=next_pc;state<=FETCH;
         end
       end
       FINISH:if(tx_ready && !tx_send) begin
         tx_send<=1;
         case(finish_index) 0:tx_data<="D";1:tx_data<="O";2:tx_data<="N";3:tx_data<="E";endcase
         if(finish_index==3) state<=HOST;else finish_index<=finish_index+1'b1;
       end
       default:state<=HOST;
     endcase
     if(running && rx_valid && !rx_pop && rx_data==4 && tx_ready) begin
       rx_pop<=1;tx_send<=1;tx_data<=65+state;
     end
   end
 end
endmodule
