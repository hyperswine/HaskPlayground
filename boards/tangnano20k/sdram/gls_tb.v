`timescale 1ns/1ps
// Gate-level run of a whole Gowin netlist of the SDRAM pipeline (module top,
// .vo or .vg) with gowin_models.v, the functional SDRAM pin model of
// perf_tb.v, a preloaded image and the netlist's own boot ROM, which is sent
// 'T' (it answers DONE) and then 'H' (run 0x8000_0000) over the UART.
module gls_tb #(parameter real CPU_MHZ=54.0);
 localparam real BIT_NS=868.0*1000.0/CPU_MHZ;
 reg clk_27m=0;always #18.5185 clk_27m=~clk_27m;
 reg btn=1,uart_rx=1;
 wire uart_tx,sd_clk,sd_cmd,sd_dat1,sd_dat2,sd_dat3;
 wire sclk,cke,cs_n,cas_n,ras_n,we_n;wire [3:0] dqm;wire [10:0] sa;wire [1:0] ba;tri [31:0] dq;
 top dut(.clk_27m(clk_27m),.btn_s1(btn),.uart_rx(uart_rx),.uart_tx(uart_tx),
  .sd_dat0(1'b1),.sd_clk(sd_clk),.sd_cmd(sd_cmd),.sd_dat1(sd_dat1),.sd_dat2(sd_dat2),.sd_dat3(sd_dat3),
  .O_sdram_clk(sclk),.O_sdram_cke(cke),.O_sdram_cs_n(cs_n),.O_sdram_cas_n(cas_n),.O_sdram_ras_n(ras_n),
  .O_sdram_wen_n(we_n),.O_sdram_dqm(dqm),.O_sdram_addr(sa),.O_sdram_ba(ba),.IO_sdram_dq(dq));
 reg [31:0] ram[0:2097151];
 reg [10:0] open_row[0:3];reg [10:0] mode=0;
 reg drive=0;reg [31:0] pin_data=0;
 assign dq=drive ? pin_data:32'bz;
 integer read_delay=0,read_beat=0,lane,word;reg active_read=0;reg [20:0] read_base;
 reg [63:0] sclks=0;
 always @(posedge sclk)begin
   sclks<=sclks+1;
   if(active_read)begin
     if(read_delay>1)begin read_delay=read_delay-1;drive=0;end
     else begin
       if(read_delay==1)read_delay=0;
       drive=1;pin_data=ram[read_base+read_beat];
       if(read_beat==3)active_read=0;else read_beat=read_beat+1;
     end
   end else drive=0;
   if(cs_n===1'b0)case({ras_n,cas_n,we_n})
     3'b000:mode=sa;
     3'b011:open_row[ba]=sa;
     3'b101:begin read_base={ba,open_row[ba],sa[7:0]};read_delay=mode[6:4]-1;read_beat=0;active_read=1;end
     3'b100:begin word={ba,open_row[ba],sa[7:0]};
       for(lane=0;lane<4;lane=lane+1)if(!dqm[lane])ram[word][lane*8+:8]=dq[lane*8+:8];end
     default:begin end
   endcase
 end
 task send(input [7:0] b);integer n;begin
   uart_rx=0;#(BIT_NS);for(n=0;n<8;n=n+1)begin uart_rx=b[n];#(BIT_NS);end uart_rx=1;#(BIT_NS*2);
 end endtask
 reg [7:0] rx;integer k;
 reg [8*64-1:0] line_text=0;
 initial forever begin
   @(negedge uart_tx);#(BIT_NS*1.5);
   for(k=0;k<8;k=k+1)begin rx[k]=uart_tx;#(BIT_NS);end
   $write("%c",rx);$fflush;
   if(rx==10)begin
     if(line_text=="FPR EXIT 0" || line_text=="FPR EXIT 1")begin
       $display("[gls] exit after %0d SDRAM clocks",sclks);$fflush;$finish;
     end
     line_text=0;
   end else line_text={line_text[8*63-1:0],rx};
 end
 reg [4095:0] image;integer words,i;reg [63:0] limit;
 initial begin
   if(!$value$plusargs("IMAGE=%s",image)||!$value$plusargs("WORDS=%d",words))$fatal(1,"missing +IMAGE/+WORDS");
   if(!$value$plusargs("LIMIT=%d",limit))limit=64'd400000000;
   for(i=0;i<2097152;i=i+1)ram[i]=0;
   $readmemh(image,ram,0,words-1);
   #2000;btn=0;
   #1000000;
   send("T");
   #(BIT_NS*60);
   $display("\n[gls] sending H after %0d SDRAM clocks",sclks);$fflush;
   send("H");
 end
 initial begin #10;wait(sclks>=limit);$display("\n[gls] limit reached");$fflush;$finish;end
endmodule
