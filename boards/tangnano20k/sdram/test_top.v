module top(input clk_27m,btn_s1,uart_rx,output reg uart_tx,
 output O_sdram_clk,O_sdram_cke,O_sdram_cs_n,O_sdram_cas_n,O_sdram_ras_n,O_sdram_wen_n,
 output [3:0] O_sdram_dqm,output [10:0] O_sdram_addr,output [1:0] O_sdram_ba,inout [31:0] IO_sdram_dq);
 wire clk,clk_sdram,lock;
 sdram_pll pll(.clk_27m(clk_27m),.clk_core(clk),.clk_sdram(clk_sdram),.pll_lock(lock));
 reg [1:0] reset_sync=3; wire reset_request=btn_s1|!lock;
 always @(posedge clk or posedge reset_request)
 if(reset_request) reset_sync<=3;else reset_sync<={reset_sync[0],1'b0};
 wire reset=reset_sync[1];
 reg req,write; reg [20:0] address;reg [31:0] wdata;reg [3:0] mask;
 wire ready,done;wire [31:0] rdata;
 sdram_memory mem(.clk(clk),.clk_sdram(clk_sdram),.reset(reset),.req(req),.write(write),
 .address(address),.wdata(wdata),.mask(mask),.ready(ready),.done(done),.rdata(rdata),
 .O_sdram_clk(O_sdram_clk),.O_sdram_cke(O_sdram_cke),.O_sdram_cs_n(O_sdram_cs_n),
 .O_sdram_cas_n(O_sdram_cas_n),.O_sdram_ras_n(O_sdram_ras_n),.O_sdram_wen_n(O_sdram_wen_n),
 .O_sdram_dqm(O_sdram_dqm),.O_sdram_addr(O_sdram_addr),.O_sdram_ba(O_sdram_ba),.IO_sdram_dq(IO_sdram_dq));
 // Each report: SDR1, phase, errors, comparisons, first address/expected/actual.
 reg [7:0] phase;reg [3:0] state;reg reading;reg [2:0] lane;
 reg [31:0] errors,checks,first_address,first_expected,first_actual;
 reg [24:0] hold_count;
 reg [199:0] report;reg [5:0] report_byte;reg [3:0] txbit;reg [9:0] txshift;reg [9:0] baud_count;
 reg [23:0] launch_wait;
 function [31:0] pattern(input [20:0] a,input [7:0] p);
 reg [31:0] h;begin
 h=({11'b0,a}*32'h9e3779b1)^32'ha5c35a69;
 case(p) 0:pattern=0;1:pattern=32'hffffffff;2:pattern=h;3:pattern=~h;
 default:pattern=h;endcase end endfunction
 wire [31:0] expected=pattern(address,phase);
 always @(posedge clk) begin
 req<=0;
 if(reset) begin state<=0;launch_wait<=0;uart_tx<=1;phase<=0;address<=0;reading<=0;lane<=0;
 errors<=0;checks<=0;first_address<=0;first_expected<=0;first_actual<=0;end
 else case(state)
 0:begin if(launch_wait<24'd2700000) launch_wait<=launch_wait+1'b1;
   else if(!uart_rx) begin phase<=8'hff;state<=3;end end
 1:begin
   req<=1;write<=!reading;
   // Phase 4 writes zero then all four lanes independently across every word.
   // Poison unselected lanes so ignoring byte enables cannot pass.
   wdata<=phase==4 && lane==0 ? 0 : phase==4 ?
     expected ^ ~(32'hff << ((lane-1)*8)) : expected;
   mask<=phase==4 && lane!=0 ? (4'b1<<(lane-1)):4'b1111;
   state<=9;
 end
 // Hold the registered request until accepted, even when refresh intervenes.
 9:begin req<=1;if(ready) begin req<=0;state<=2;end end
 2:if(done) begin
   if(reading) begin
    checks<=checks+1'b1;
    if(rdata!==expected) begin
      errors<=errors+1'b1;
      if(errors==0) begin first_address<={9'b0,address,2'b0};first_expected<=expected;first_actual<=rdata;end
    end
   end
   if(phase==4 && !reading && lane<4) begin lane<=lane+1'b1;state<=1;end
   else begin
    lane<=0;
    if(phase==5 ? address==0 : address==21'h1fffff) begin
     if(!reading) begin reading<=1;address<=0;state<=1;end
     else state<=3;
    end else begin address<=phase==5 ? address-1'b1:address+1'b1;state<=1;end
   end
 end
 3:begin report<={first_actual,first_expected,first_address,checks,errors,phase,32'h31524453};report_byte<=0;state<=4;end
 4:begin txshift<={1'b1,report[7:0],1'b0};txbit<=0;baud_count<=0;state<=5;end
 5:begin uart_tx<=txshift[0];
   if(baud_count==867) begin baud_count<=0;txshift<={1'b1,txshift[9:1]};
    if(txbit==9) begin
     uart_tx<=1;report<=report>>8;
     if(report_byte==24) state<=6;
     else begin report_byte<=report_byte+1'b1;state<=4;end
    end else txbit<=txbit+1'b1;
   end else baud_count<=baud_count+1'b1;
 end
 6:if(phase==5 || errors!=0) state<=8;
   else begin phase<=phase+1'b1;address<=0;reading<=0;
    if(phase==4) begin hold_count<=0;state<=7;end else state<=1;
   end
 7:begin hold_count<=hold_count+1'b1;
   if(hold_count==25'd13500000) begin reading<=1;address<=21'h1fffff;state<=1;end
 end
 default:state<=8;
 endcase
 end
endmodule
