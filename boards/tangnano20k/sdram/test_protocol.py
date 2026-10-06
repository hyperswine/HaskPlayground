#!/usr/bin/env python3
"""Exercise tester backpressure and prove poisoned lanes detect ignored masks.

This uses a behavioral word-memory adapter, not an SDRAM electrical model.
The physical full-capacity test remains the SDRAM validation.
"""
from pathlib import Path
import subprocess, tempfile
here=Path(__file__).resolve().parent
stub=r'''
module sdram_pll(input clk_27m,output clk_core,clk_sdram,pll_lock);
assign clk_core=clk_27m;assign clk_sdram=~clk_27m;assign pll_lock=1;endmodule
module sdram_memory(input clk,clk_sdram,reset,req,write,input[20:0]address,
 input[31:0]wdata,input[3:0]mask,output ready,output reg done,output reg[31:0]rdata,
 output O_sdram_clk,O_sdram_cke,O_sdram_cs_n,O_sdram_cas_n,O_sdram_ras_n,O_sdram_wen_n,
 output[3:0]O_sdram_dqm,output[10:0]O_sdram_addr,output[1:0]O_sdram_ba,inout[31:0]IO_sdram_dq);
 reg[31:0]ram[0:31];integer tick,busy,j;
 // Interrupt readiness between preparation and acceptance to model refresh.
 assign ready=!reset && busy==0 && tick%7!=0;
 always @(posedge clk) begin
  done<=0;
  if(reset) begin tick<=0;busy<=0;end
  else begin
   tick<=tick+1;
   if(busy>0) begin busy<=busy-1;if(busy==1)done<=1;end
   if(req&&ready)begin busy<=3;rdata<=ram[address];
    if(write)for(j=0;j<4;j=j+1)
`ifdef IGNORE_MASK
      ram[address][8*j+:8]<=wdata[8*j+:8];
`else
      if(mask[j])ram[address][8*j+:8]<=wdata[8*j+:8];
`endif
   end
  end
 end
endmodule
module tb;
reg clk=0,btn=1,rx=1;always #5 clk=~clk;
wire tx;wire[31:0]dq;integer ticks=0;
top dut(.clk_27m(clk),.btn_s1(btn),.uart_rx(rx),.uart_tx(tx),.IO_sdram_dq(dq));
initial begin #100 btn=0;#1000 rx=0;#100 rx=1;end
always @(posedge clk)begin
 ticks<=ticks+1;if(ticks>2000000)$fatal(1,"timeout: request may have been lost");
 if(dut.state==3 && dut.phase!=255)begin
`ifdef IGNORE_MASK
  if(dut.phase<4 && dut.errors!=0)$fatal(1,"unexpected early failure");
  if(dut.phase==4)begin
   if(dut.errors!=32)$fatal(1,"ignored mask escaped detection");
   $display("PASS: all 32 ignored-mask writes detected");$finish;
  end
`else
  if(dut.errors!=0)$fatal(1,"correct memory failed");
  if(dut.phase==5)begin
   if(dut.checks!=192)$fatal(1,"incomplete coverage");
   $display("PASS: six phases under refresh-style backpressure");$finish;
  end
`endif
 end
end
endmodule
'''
with tempfile.TemporaryDirectory(prefix='sdram-protocol-') as tmp:
    path=Path(tmp)
    top=here.joinpath('test_top.v').read_text().replace("24'd2700000","24'd20").replace("21'h1fffff","21'd31").replace("25'd13500000","25'd50")
    (path/'top.v').write_text(top);(path/'tb.v').write_text(stub)
    for bad in (False,True):
        output=path/('bad' if bad else 'good')
        subprocess.run(['iverilog','-g2012','-s','tb',*(['-DIGNORE_MASK'] if bad else []),'-o',str(output),str(path/'top.v'),str(path/'tb.v')],check=True)
        subprocess.run(['vvp',str(output)],check=True,timeout=30)
