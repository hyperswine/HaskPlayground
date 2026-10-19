// Characterize Gowin BSRAM read-during-write on the board.
//
// A 2^DEPTH_BITS x 32 array, written and read on one clock in the cache's
// style (registered read with an enable), maps to narrow SDPB slices (8Kx2
// for 8K words, 16Kx1 for 16K). It is filled with f(a). Each trial writes
// ~f(W) to word W while reading word R = W ^ delta on the same edge, then
// reads W and R back on clean edges and restores both. Class k < 14 uses
// delta = 1<<k (random past the address width); class 14 a random nonzero
// delta; class 15 delta = 0 (the
// written word itself); class 16 reads R with no write (control); class 17
// also reads R on the edge before the write (the address is held across it);
// class 18 writes W with the read port disabled.
// After 2^TRIAL_BITS trials per class it prints one line per class over the
// UART (115200 8N1):
//   Ckk BAD FWD LOST HURT
// BAD: same-edge reads that differ from f(R) (class 15: that are not the new
// word); FWD: of those, reads equal to the write data (class 15: reads of the
// new word); LOST: clean readbacks of W that differ from ~f(W) (class 16:
// from f(W)); HURT: clean readbacks of R that differ from f(R) (class 15:
// from ~f(W)). Eight hex digits each. Then EVENTS lines "Ekk CLASS WWWWRRRR
// VALUE EXPECTED" for the first corrupted readbacks of R. Any byte received
// starts a run.
module top(input clk_27m,btn_s1,uart_rx,output reg uart_tx=1);
 parameter DEPTH_BITS=13,TRIAL_BITS=20;
 // GAP=1: an idle edge after every write, so the write port's address and
 // data never change on an edge that writes.
 parameter GAP=0;
 parameter BAUD_DIV=468; // clocks per UART bit, less one (54 MHz)
 localparam CLASSES=19;
 wire clk,clk_sdram,lock;
 sdram_pll pll(.clk_27m(clk_27m),.clk_core(clk),.clk_sdram(clk_sdram),.pll_lock(lock));
 reg [1:0] reset_sync=3;wire reset_request=btn_s1|!lock;
 always @(posedge clk or posedge reset_request)
   if(reset_request)reset_sync<=3;else reset_sync<={reset_sync[0],1'b0};
 wire reset=reset_sync[1];
 reg [31:0] mem[0:(1<<DEPTH_BITS)-1];
 reg we,re;reg [DEPTH_BITS-1:0] wa,ra;reg [31:0] wd,q;
 always @(posedge clk)begin
   if(we)mem[wa]<=wd;
   if(re)q<=mem[ra];
 end
 function [31:0] f(input [DEPTH_BITS-1:0] a);
   f=({{(32-DEPTH_BITS){1'b0}},a}*32'h9e3779b1)^32'h5a3c96e1;
 endfunction
 reg [31:0] lfsr=32'hace1ace1;
 wire [31:0] lfsr_next={lfsr[30:0],lfsr[31]^lfsr[21]^lfsr[1]^lfsr[0]};
 localparam IDLE=0,FILL=1,TRY=2,COLLIDE=3,CHECK=4,READ_W=5,CHECK_W=6,READ_R=7,CHECK_R=8,FIX_W=9,FIX_R=10,REPORT=11,PRE=12,IDLE_GAP=13;
 reg [3:0] state=IDLE;reg [4:0] cls;reg [TRIAL_BITS-1:0] trial;
 reg [DEPTH_BITS-1:0] fill;reg [DEPTH_BITS-1:0] w,r;
 reg [31:0] bad[0:CLASSES-1],fwd[0:CLASSES-1],lost[0:CLASSES-1],hurt[0:CLASSES-1];
 // The first EVENTS corrupted readbacks of R: class, W, R, value, expected.
 localparam EVENTS=32;
 reg [4:0] ev_cls[0:EVENTS-1];reg [15:0] ev_w[0:EVENTS-1],ev_r[0:EVENTS-1];
 reg [31:0] ev_q[0:EVENTS-1],ev_x[0:EVENTS-1];reg [5:0] events;
 integer i;
 wire [31:0] new_w=~f(w),old_r=f(r);
 wire written=cls!=16;
 wire reading=cls!=18;
 // UART: one line per class, 41 bytes each.
 reg [3:0] txbit;reg [9:0] txshift;reg [9:0] baud;reg [5:0] line;reg [5:0] col;reg sending;
 function [7:0] hex(input [3:0] v);hex=v<10 ? "0"+v:"A"+v-10;endfunction
 reg [7:0] ch;reg [31:0] field;
 always @* begin
   if(line<CLASSES)case((col-4)/9)0:field=bad[line];1:field=fwd[line];2:field=lost[line];default:field=hurt[line];endcase
   else case((col-4)/9)
     0:field={11'd0,ev_cls[line-CLASSES]};
     1:field={ev_w[line-CLASSES],ev_r[line-CLASSES]};
     2:field=ev_q[line-CLASSES];
     default:field=ev_x[line-CLASSES];
   endcase
   case(col)
     0:ch=line<CLASSES ? "C":"E";1:ch=hex(line<CLASSES ? line/10:(line-CLASSES)/10);
     2:ch=hex(line<CLASSES ? line%10:(line-CLASSES)%10);3,12,21,30:ch=" ";
     39:ch=8'd13;40:ch=8'd10;
     default:ch=hex(field[(7-(col-4)%9)*4+:4]);
   endcase
 end
 reg rx_meta=1,rx=1;
 always @(posedge clk)begin rx_meta<=uart_rx;rx<=rx_meta;end
 always @(posedge clk)begin
   we<=0;re<=0;
   if(reset)begin state<=IDLE;uart_tx<=1;sending<=0;end
   else case(state)
     IDLE:if(!rx)begin
       state<=FILL;fill<=0;
       for(i=0;i<CLASSES;i=i+1)begin bad[i]<=0;fwd[i]<=0;lost[i]<=0;hurt[i]<=0;end
       events<=0;
     end
     FILL:begin
       // GAP: the write inputs change only on edges that do not write.
       if(!GAP || !we)begin
         we<=1;wa<=fill;wd<=f(fill);fill<=fill+1'b1;
         if(&fill)begin state<=TRY;cls<=0;trial<=0;end
       end
     end
     TRY:begin
       lfsr<=lfsr_next;
       w<=lfsr[DEPTH_BITS-1:0];
       case(cls)
         15,16:r<=lfsr[DEPTH_BITS-1:0];
         // Class 14, and single-bit classes past the address width: random.
         default:r<=lfsr[DEPTH_BITS-1:0]^(cls<DEPTH_BITS ? {{(DEPTH_BITS-1){1'b0}},1'b1}<<cls:
                    lfsr[31:32-DEPTH_BITS]==0 ? 1:lfsr[31:32-DEPTH_BITS]);
       endcase
       state<=PRE;
     end
     // Class 17 loads the read address on the edge before the write too.
     PRE:begin re<=cls==17;ra<=r;state<=COLLIDE;end
     COLLIDE:begin we<=written;wa<=w;wd<=new_w;re<=reading;ra<=r;state<=CHECK;end // both on the next edge
     CHECK:state<=READ_W;
     READ_W:begin
       if(cls==15)begin if(q!=new_w)bad[cls]<=bad[cls]+1;else fwd[cls]<=fwd[cls]+1;end
       else if(reading && q!=old_r)begin bad[cls]<=bad[cls]+1;if(q==new_w)fwd[cls]<=fwd[cls]+1;end
       re<=1;ra<=w;state<=CHECK_W;
     end
     CHECK_W:state<=READ_R;
     READ_R:begin
       if(q!=(written ? new_w:f(w)))lost[cls]<=lost[cls]+1;
       re<=1;ra<=r;state<=CHECK_R;
     end
     CHECK_R:state<=FIX_W;
     FIX_W:begin
       if(q!=(cls==15 ? new_w:old_r))begin
         hurt[cls]<=hurt[cls]+1;
         if(events<EVENTS)begin
           ev_cls[events]<=cls;ev_w[events]<=w;ev_r[events]<=r;ev_q[events]<=q;ev_x[events]<=cls==15 ? new_w:old_r;
           events<=events+1'b1;
         end
       end
       we<=1;wa<=w;wd<=f(w);state<=GAP ? IDLE_GAP:FIX_R;
     end
     IDLE_GAP:state<=FIX_R;
     FIX_R:begin
       we<=1;wa<=r;wd<=old_r;
       trial<=trial+1'b1;state<=TRY;
       if(&trial)begin
         if(cls==CLASSES-1)begin state<=REPORT;line<=0;col<=0;sending<=0;baud<=0;end
         else cls<=cls+1'b1;
       end
     end
     REPORT:begin
       if(!sending)begin txshift<={1'b1,ch,1'b0};txbit<=0;baud<=0;sending<=1;end
       else if(baud==BAUD_DIV)begin
         baud<=0;uart_tx<=txshift[0];txshift<={1'b1,txshift[9:1]};txbit<=txbit+1'b1;
         if(txbit==10)begin
           sending<=0;uart_tx<=1;
           if(col==40)begin col<=0;
             if(line==CLASSES+EVENTS-1)state<=IDLE;else line<=line+1'b1;
           end else col<=col+1'b1;
         end
       end else baud<=baud+1'b1;
     end
   endcase
 end
endmodule
