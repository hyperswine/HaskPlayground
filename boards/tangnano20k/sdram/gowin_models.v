`timescale 1ns/1ps
// Behavioural models (for Verilator) of the Gowin GW2A primitives in the
// SDRAM pipeline's synthesis (.vg) and place-and-route (.vo) netlists. They
// follow simlib/gw2a/prim_sim.v (GSR and timing omitted; block RAMs keep
// words, not bit vectors), which Verilator cannot compile, with two
// deliberate differences:
//
// - rPLL is an ideal clock: CLKOUT = 27 MHz x (FBDIV_SEL+1)/(IDIV_SEL+1),
//   CLKOUTP that clock PSDA_SEL x 22.5 degrees late, LOCK after 5 us.
// - SDPB/SDPX9B read-during-write. prim_sim.v returns the old word. Gowin
//   synthesis relies on write-through instead (it maps an asynchronous-read
//   register file onto a BSRAM addressed by the next state), and a 512x32
//   BSRAM on the board always returned the new word (WRITE-UPDATE-2026-10-19.md).
//   That is the default here. +define+GOWIN_COLLIDE_ROW passes a write
//   through to reads of any word in its 32-bit row (in narrow modes, a crude
//   stand-in for the narrow-slice corruption seen on the board: it reproduces
//   the board's write-update panic but not every failure);
//   +define+GOWIN_COLLIDE_OLD restores prim_sim.v.
module VCC(output V);assign V=1'b1;endmodule
module GND(output G);assign G=1'b0;endmodule
module GSR(input GSRI);endmodule
module IBUF(output O,input I);assign O=I;endmodule
module OBUF(output O,input I);assign O=I;endmodule
module IOBUF(output O,inout IO,input I,OEN);assign IO=OEN ? 1'bz:I;assign O=IO;endmodule
module INV(output O,input I);assign O=~I;endmodule
module LUT1(output F,input I0);parameter INIT=2'h0;assign F=INIT[I0];endmodule
module LUT2(output F,input I0,I1);parameter INIT=4'h0;assign F=INIT[{I1,I0}];endmodule
module LUT3(output F,input I0,I1,I2);parameter INIT=8'h0;assign F=INIT[{I2,I1,I0}];endmodule
module LUT4(output F,input I0,I1,I2,I3);parameter INIT=16'h0;assign F=INIT[{I3,I2,I1,I0}];endmodule
module MUX2_LUT5(output O,input I0,I1,S0);assign O=S0 ? I1:I0;endmodule
module MUX2_LUT6(output O,input I0,I1,S0);assign O=S0 ? I1:I0;endmodule
module MUX2_LUT7(output O,input I0,I1,S0);assign O=S0 ? I1:I0;endmodule
module MUX2_LUT8(output O,input I0,I1,S0);assign O=S0 ? I1:I0;endmodule
module DFF(output reg Q,input D,CLK);parameter INIT=1'b0;initial Q=INIT;always @(posedge CLK)Q<=D;endmodule
module DFFE(output reg Q,input D,CLK,CE);parameter INIT=1'b0;initial Q=INIT;always @(posedge CLK)if(CE)Q<=D;endmodule
module DFFR(output reg Q,input D,CLK,RESET);parameter INIT=1'b0;initial Q=INIT;always @(posedge CLK)if(RESET)Q<=0;else Q<=D;endmodule
module DFFRE(output reg Q,input D,CLK,CE,RESET);parameter INIT=1'b0;initial Q=INIT;always @(posedge CLK)if(RESET)Q<=0;else if(CE)Q<=D;endmodule
module DFFS(output reg Q,input D,CLK,SET);parameter INIT=1'b1;initial Q=INIT;always @(posedge CLK)if(SET)Q<=1;else Q<=D;endmodule
module DFFSE(output reg Q,input D,CLK,CE,SET);parameter INIT=1'b1;initial Q=INIT;always @(posedge CLK)if(SET)Q<=1;else if(CE)Q<=D;endmodule
module DFFP(output reg Q,input D,CLK,PRESET);parameter INIT=1'b1;initial Q=INIT;always @(posedge CLK or posedge PRESET)if(PRESET)Q<=1;else Q<=D;endmodule
module DFFC(output reg Q,input D,CLK,CLEAR);parameter INIT=1'b0;initial Q=INIT;always @(posedge CLK or posedge CLEAR)if(CLEAR)Q<=0;else Q<=D;endmodule
module ALU(output SUM,COUT,input I0,I1,I3,CIN);
 parameter ALU_MODE=0;reg S,C;
 assign SUM=S^CIN;assign COUT=S ? CIN:C;
 always @* case(ALU_MODE)
  0:begin S=I0^I1;C=I0;end
  1:begin S=I0^~I1;C=I0;end
  2:begin S=I3 ? I0^I1:I0^~I1;C=I0;end
  3:begin S=I0^~I1;C=1'b1;end
  4:begin S=I0^~I1;C=I0;end
  5:begin S=~I0^I1;C=I1;end
  6:begin S=I0;C=1'b0;end
  7:begin S=~I0;C=1'b1;end
  8:begin S=I3 ? I0:~I0;C=I0;end
  default:begin S=I0&I1;C=I0&I1;end
 endcase
endmodule
module RAM16SDP4(output [3:0] DO,input [3:0] DI,WAD,RAD,input WRE,CLK);
 parameter INIT_0=16'h0,INIT_1=16'h0,INIT_2=16'h0,INIT_3=16'h0;
 reg [15:0] m0=INIT_0,m1=INIT_1,m2=INIT_2,m3=INIT_3;
 assign DO={m3[RAD],m2[RAD],m1[RAD],m0[RAD]};
 always @(posedge CLK)if(WRE)begin m0[WAD]<=DI[0];m1[WAD]<=DI[1];m2[WAD]<=DI[2];m3[WAD]<=DI[3];end
endmodule
// Registers follow prim_sim.v: AREG/BREG sample on CE, OUT_REG/PIPE_REG off
// in this netlist (checked at elaboration).
module MULT18X18(output [35:0] DOUT,output [17:0] SOA,SOB,input [17:0] A,B,SIA,SIB,
 input ASIGN,BSIGN,ASEL,BSEL,CLK,CE,RESET);
 parameter AREG=1'b0,BREG=1'b0,OUT_REG=1'b0,PIPE_REG=1'b0,ASIGN_REG=1'b0,BSIGN_REG=1'b0,SOA_REG=1'b0;
 parameter MULT_RESET_MODE="SYNC";
 initial if(OUT_REG||PIPE_REG||ASIGN_REG||BSIGN_REG||SOA_REG)$fatal(1,"MULT18X18 option not modelled");
 reg [17:0] ar=0,br=0;
 always @(posedge CLK)if(RESET)begin ar<=0;br<=0;end else if(CE)begin ar<=ASEL ? SIA:A;br<=BSEL ? SIB:B;end
 wire [17:0] a=AREG ? ar:(ASEL ? SIA:A),b=BREG ? br:(BSEL ? SIB:B);
 wire [35:0] ax={{18{ASIGN & a[17]}},a},bx={{18{BSIGN & b[17]}},b};
 assign DOUT=ax*bx;assign SOA=a;assign SOB=b;
endmodule
module ALU54D(output [53:0] DOUT,output [54:0] CASO,input [53:0] A,B,input [54:0] CASI,
 input ACCLOAD,ASIGN,BSIGN,CLK,CE,RESET);
 parameter AREG=1'b0,BREG=1'b0,ASIGN_REG=1'b0,BSIGN_REG=1'b0,ACCLOAD_REG=1'b0,OUT_REG=1'b0;
 parameter B_ADD_SUB=1'b0,C_ADD_SUB=1'b0,ALUD_MODE=0,ALU_RESET_MODE="SYNC";
 initial if(OUT_REG||ASIGN_REG||BSIGN_REG||ALUD_MODE!=0)$fatal(1,"ALU54D option not modelled");
 reg [53:0] ar=0,br=0;
 always @(posedge CLK)if(RESET)begin ar<=0;br<=0;end else if(CE)begin ar<=A;br<=B;end
 wire [53:0] a=AREG ? ar:A,b=BREG ? br:B;
 wire [54:0] ax={ASIGN & a[53],a},bx={BSIGN & b[53],b};
 // ACCLOAD feeds back the (unregistered) output; this netlist ties it low.
 wire [54:0] sum=(B_ADD_SUB ? -bx:bx)+(C_ADD_SUB ? -ax:ax);
 assign DOUT=sum[53:0];assign CASO=sum;
 initial #1 if(ACCLOAD!==1'b0)$fatal(1,"ALU54D ACCLOAD not modelled");
endmodule
// Semi-dual-port 16 Kbit block RAM, bypass or pipelined output. Same-address
// read and write on one edge returns the old word (prim_sim.v does too).
module SDPB(output [31:0] DO,input [31:0] DI,input [2:0] BLKSELA,BLKSELB,input [13:0] ADA,ADB,
 input CLKA,CLKB,CEA,CEB,OCE,RESETA,RESETB);
 parameter READ_MODE=1'b0,BIT_WIDTH_0=32,BIT_WIDTH_1=32,BLK_SEL_0=3'b000,BLK_SEL_1=3'b000,RESET_MODE="SYNC";
 parameter INIT_RAM_00=256'h0;
 parameter INIT_RAM_01=256'h0;
 parameter INIT_RAM_02=256'h0;
 parameter INIT_RAM_03=256'h0;
 parameter INIT_RAM_04=256'h0;
 parameter INIT_RAM_05=256'h0;
 parameter INIT_RAM_06=256'h0;
 parameter INIT_RAM_07=256'h0;
 parameter INIT_RAM_08=256'h0;
 parameter INIT_RAM_09=256'h0;
 parameter INIT_RAM_0A=256'h0;
 parameter INIT_RAM_0B=256'h0;
 parameter INIT_RAM_0C=256'h0;
 parameter INIT_RAM_0D=256'h0;
 parameter INIT_RAM_0E=256'h0;
 parameter INIT_RAM_0F=256'h0;
 parameter INIT_RAM_10=256'h0;
 parameter INIT_RAM_11=256'h0;
 parameter INIT_RAM_12=256'h0;
 parameter INIT_RAM_13=256'h0;
 parameter INIT_RAM_14=256'h0;
 parameter INIT_RAM_15=256'h0;
 parameter INIT_RAM_16=256'h0;
 parameter INIT_RAM_17=256'h0;
 parameter INIT_RAM_18=256'h0;
 parameter INIT_RAM_19=256'h0;
 parameter INIT_RAM_1A=256'h0;
 parameter INIT_RAM_1B=256'h0;
 parameter INIT_RAM_1C=256'h0;
 parameter INIT_RAM_1D=256'h0;
 parameter INIT_RAM_1E=256'h0;
 parameter INIT_RAM_1F=256'h0;
 parameter INIT_RAM_20=256'h0;
 parameter INIT_RAM_21=256'h0;
 parameter INIT_RAM_22=256'h0;
 parameter INIT_RAM_23=256'h0;
 parameter INIT_RAM_24=256'h0;
 parameter INIT_RAM_25=256'h0;
 parameter INIT_RAM_26=256'h0;
 parameter INIT_RAM_27=256'h0;
 parameter INIT_RAM_28=256'h0;
 parameter INIT_RAM_29=256'h0;
 parameter INIT_RAM_2A=256'h0;
 parameter INIT_RAM_2B=256'h0;
 parameter INIT_RAM_2C=256'h0;
 parameter INIT_RAM_2D=256'h0;
 parameter INIT_RAM_2E=256'h0;
 parameter INIT_RAM_2F=256'h0;
 parameter INIT_RAM_30=256'h0;
 parameter INIT_RAM_31=256'h0;
 parameter INIT_RAM_32=256'h0;
 parameter INIT_RAM_33=256'h0;
 parameter INIT_RAM_34=256'h0;
 parameter INIT_RAM_35=256'h0;
 parameter INIT_RAM_36=256'h0;
 parameter INIT_RAM_37=256'h0;
 parameter INIT_RAM_38=256'h0;
 parameter INIT_RAM_39=256'h0;
 parameter INIT_RAM_3A=256'h0;
 parameter INIT_RAM_3B=256'h0;
 parameter INIT_RAM_3C=256'h0;
 parameter INIT_RAM_3D=256'h0;
 parameter INIT_RAM_3E=256'h0;
 parameter INIT_RAM_3F=256'h0;
 localparam [16383:0] BITS={INIT_RAM_3F,INIT_RAM_3E,INIT_RAM_3D,INIT_RAM_3C,INIT_RAM_3B,INIT_RAM_3A,INIT_RAM_39,INIT_RAM_38,INIT_RAM_37,INIT_RAM_36,INIT_RAM_35,INIT_RAM_34,INIT_RAM_33,INIT_RAM_32,INIT_RAM_31,INIT_RAM_30,INIT_RAM_2F,INIT_RAM_2E,INIT_RAM_2D,INIT_RAM_2C,INIT_RAM_2B,INIT_RAM_2A,INIT_RAM_29,INIT_RAM_28,INIT_RAM_27,INIT_RAM_26,INIT_RAM_25,INIT_RAM_24,INIT_RAM_23,INIT_RAM_22,INIT_RAM_21,INIT_RAM_20,INIT_RAM_1F,INIT_RAM_1E,INIT_RAM_1D,INIT_RAM_1C,INIT_RAM_1B,INIT_RAM_1A,INIT_RAM_19,INIT_RAM_18,INIT_RAM_17,INIT_RAM_16,INIT_RAM_15,INIT_RAM_14,INIT_RAM_13,INIT_RAM_12,INIT_RAM_11,INIT_RAM_10,INIT_RAM_0F,INIT_RAM_0E,INIT_RAM_0D,INIT_RAM_0C,INIT_RAM_0B,INIT_RAM_0A,INIT_RAM_09,INIT_RAM_08,INIT_RAM_07,INIT_RAM_06,INIT_RAM_05,INIT_RAM_04,INIT_RAM_03,INIT_RAM_02,INIT_RAM_01,INIT_RAM_00};
 localparam W=BIT_WIDTH_0,L=$clog2(W);
 initial if(BIT_WIDTH_0!=BIT_WIDTH_1||RESET_MODE!="SYNC")$fatal(1,"SDPB option not modelled");
 reg [W-1:0] mem[0:16384/W-1];integer i;
 initial for(i=0;i<16384/W;i=i+1)mem[i]=BITS[i*W+:W];
 wire [13-L:0] wa=ADA[13:L],ra=ADB[13:L];
 reg [31:0] bp=0,pl=0;
 always @(posedge CLKA)if(CEA && BLKSELA==BLK_SEL_0)begin
  if(W<16)mem[wa]<=DI[W-1:0];
  else for(i=0;i<W/8;i=i+1)if(ADA[i])mem[wa][i*8+:8]<=DI[i*8+:8];
 end
 // A read on the edge of a write sees it: see the header.
 reg [W-1:0] fresh;
 always @* begin
  fresh=mem[ra];
`ifndef GOWIN_COLLIDE_OLD
`ifdef GOWIN_COLLIDE_ROW
  if(CEA && BLKSELA==BLK_SEL_0 && ADA[13:5]==ADB[13:5])begin
   if(W<16)fresh=DI[W-1:0];
   else for(i=0;i<W/8;i=i+1)if(ADA[i])fresh[i*8+:8]=DI[i*8+:8];
  end
`else
  if(CEA && BLKSELA==BLK_SEL_0 && wa==ra)begin
   if(W<16)fresh=DI[W-1:0];
   else for(i=0;i<W/8;i=i+1)if(ADA[i])fresh[i*8+:8]=DI[i*8+:8];
  end
`endif
`endif
 end
 always @(posedge CLKB)if(RESETB)begin bp<=0;pl<=0;end else begin
  if(OCE)pl<=bp;
  if(CEB && BLKSELB==BLK_SEL_1)bp[W-1:0]<=fresh;
 end
 assign DO=READ_MODE ? pl:bp;
endmodule
module SDPX9B(output [35:0] DO,input [35:0] DI,input [2:0] BLKSELA,BLKSELB,input [13:0] ADA,ADB,
 input CLKA,CLKB,CEA,CEB,OCE,RESETA,RESETB);
 parameter READ_MODE=1'b0,BIT_WIDTH_0=36,BIT_WIDTH_1=36,BLK_SEL_0=3'b000,BLK_SEL_1=3'b000,RESET_MODE="SYNC";
 parameter INIT_RAM_00=256'h0;
 parameter INIT_RAM_01=256'h0;
 parameter INIT_RAM_02=256'h0;
 parameter INIT_RAM_03=256'h0;
 parameter INIT_RAM_04=256'h0;
 parameter INIT_RAM_05=256'h0;
 parameter INIT_RAM_06=256'h0;
 parameter INIT_RAM_07=256'h0;
 parameter INIT_RAM_08=256'h0;
 parameter INIT_RAM_09=256'h0;
 parameter INIT_RAM_0A=256'h0;
 parameter INIT_RAM_0B=256'h0;
 parameter INIT_RAM_0C=256'h0;
 parameter INIT_RAM_0D=256'h0;
 parameter INIT_RAM_0E=256'h0;
 parameter INIT_RAM_0F=256'h0;
 parameter INIT_RAM_10=256'h0;
 parameter INIT_RAM_11=256'h0;
 parameter INIT_RAM_12=256'h0;
 parameter INIT_RAM_13=256'h0;
 parameter INIT_RAM_14=256'h0;
 parameter INIT_RAM_15=256'h0;
 parameter INIT_RAM_16=256'h0;
 parameter INIT_RAM_17=256'h0;
 parameter INIT_RAM_18=256'h0;
 parameter INIT_RAM_19=256'h0;
 parameter INIT_RAM_1A=256'h0;
 parameter INIT_RAM_1B=256'h0;
 parameter INIT_RAM_1C=256'h0;
 parameter INIT_RAM_1D=256'h0;
 parameter INIT_RAM_1E=256'h0;
 parameter INIT_RAM_1F=256'h0;
 parameter INIT_RAM_20=256'h0;
 parameter INIT_RAM_21=256'h0;
 parameter INIT_RAM_22=256'h0;
 parameter INIT_RAM_23=256'h0;
 parameter INIT_RAM_24=256'h0;
 parameter INIT_RAM_25=256'h0;
 parameter INIT_RAM_26=256'h0;
 parameter INIT_RAM_27=256'h0;
 parameter INIT_RAM_28=256'h0;
 parameter INIT_RAM_29=256'h0;
 parameter INIT_RAM_2A=256'h0;
 parameter INIT_RAM_2B=256'h0;
 parameter INIT_RAM_2C=256'h0;
 parameter INIT_RAM_2D=256'h0;
 parameter INIT_RAM_2E=256'h0;
 parameter INIT_RAM_2F=256'h0;
 parameter INIT_RAM_30=256'h0;
 parameter INIT_RAM_31=256'h0;
 parameter INIT_RAM_32=256'h0;
 parameter INIT_RAM_33=256'h0;
 parameter INIT_RAM_34=256'h0;
 parameter INIT_RAM_35=256'h0;
 parameter INIT_RAM_36=256'h0;
 parameter INIT_RAM_37=256'h0;
 parameter INIT_RAM_38=256'h0;
 parameter INIT_RAM_39=256'h0;
 parameter INIT_RAM_3A=256'h0;
 parameter INIT_RAM_3B=256'h0;
 parameter INIT_RAM_3C=256'h0;
 parameter INIT_RAM_3D=256'h0;
 parameter INIT_RAM_3E=256'h0;
 parameter INIT_RAM_3F=256'h0;
 parameter INITP_00=256'h0;
 parameter INITP_01=256'h0;
 parameter INITP_02=256'h0;
 parameter INITP_03=256'h0;
 parameter INITP_04=256'h0;
 parameter INITP_05=256'h0;
 parameter INITP_06=256'h0;
 parameter INITP_07=256'h0;
 localparam W=BIT_WIDTH_0,L=W==9 ? 3:W==18 ? 4:5;
 initial if(BIT_WIDTH_0!=BIT_WIDTH_1||RESET_MODE!="SYNC"||INIT_RAM_00!=0||INITP_00!=0)$fatal(1,"SDPX9B option not modelled");
 reg [W-1:0] mem[0:18432/W-1];integer i;
 initial for(i=0;i<18432/W;i=i+1)mem[i]=0;
 wire [13-L:0] wa=ADA[13:L],ra=ADB[13:L];
 reg [35:0] bp=0,pl=0;
 always @(posedge CLKA)if(CEA && BLKSELA==BLK_SEL_0)begin
  if(W<18)mem[wa]<=DI[W-1:0];
  else for(i=0;i<W/9;i=i+1)if(ADA[i])mem[wa][i*9+:9]<=DI[i*9+:9];
 end
 reg [W-1:0] fresh;
 always @* begin
  fresh=mem[ra];
`ifndef GOWIN_COLLIDE_OLD
`ifdef GOWIN_COLLIDE_ROW
  if(CEA && BLKSELA==BLK_SEL_0 && (ADA[13:5]==ADB[13:5]))begin
`else
  if(CEA && BLKSELA==BLK_SEL_0 && wa==ra)begin
`endif
   if(W<18)fresh=DI[W-1:0];
   else for(i=0;i<W/9;i=i+1)if(ADA[i])fresh[i*9+:9]=DI[i*9+:9];
  end
`endif
 end
 always @(posedge CLKB)if(RESETB)begin bp<=0;pl<=0;end else begin
  if(OCE)pl<=bp;
  if(CEB && BLKSELB==BLK_SEL_1)bp[W-1:0]<=fresh;
 end
 assign DO=READ_MODE ? pl:bp;
endmodule
module pROM(output [31:0] DO,input [13:0] AD,input CLK,CE,OCE,RESET);
 parameter READ_MODE=1'b0,BIT_WIDTH=32,RESET_MODE="SYNC";
 parameter INIT_RAM_00=256'h0;
 parameter INIT_RAM_01=256'h0;
 parameter INIT_RAM_02=256'h0;
 parameter INIT_RAM_03=256'h0;
 parameter INIT_RAM_04=256'h0;
 parameter INIT_RAM_05=256'h0;
 parameter INIT_RAM_06=256'h0;
 parameter INIT_RAM_07=256'h0;
 parameter INIT_RAM_08=256'h0;
 parameter INIT_RAM_09=256'h0;
 parameter INIT_RAM_0A=256'h0;
 parameter INIT_RAM_0B=256'h0;
 parameter INIT_RAM_0C=256'h0;
 parameter INIT_RAM_0D=256'h0;
 parameter INIT_RAM_0E=256'h0;
 parameter INIT_RAM_0F=256'h0;
 parameter INIT_RAM_10=256'h0;
 parameter INIT_RAM_11=256'h0;
 parameter INIT_RAM_12=256'h0;
 parameter INIT_RAM_13=256'h0;
 parameter INIT_RAM_14=256'h0;
 parameter INIT_RAM_15=256'h0;
 parameter INIT_RAM_16=256'h0;
 parameter INIT_RAM_17=256'h0;
 parameter INIT_RAM_18=256'h0;
 parameter INIT_RAM_19=256'h0;
 parameter INIT_RAM_1A=256'h0;
 parameter INIT_RAM_1B=256'h0;
 parameter INIT_RAM_1C=256'h0;
 parameter INIT_RAM_1D=256'h0;
 parameter INIT_RAM_1E=256'h0;
 parameter INIT_RAM_1F=256'h0;
 parameter INIT_RAM_20=256'h0;
 parameter INIT_RAM_21=256'h0;
 parameter INIT_RAM_22=256'h0;
 parameter INIT_RAM_23=256'h0;
 parameter INIT_RAM_24=256'h0;
 parameter INIT_RAM_25=256'h0;
 parameter INIT_RAM_26=256'h0;
 parameter INIT_RAM_27=256'h0;
 parameter INIT_RAM_28=256'h0;
 parameter INIT_RAM_29=256'h0;
 parameter INIT_RAM_2A=256'h0;
 parameter INIT_RAM_2B=256'h0;
 parameter INIT_RAM_2C=256'h0;
 parameter INIT_RAM_2D=256'h0;
 parameter INIT_RAM_2E=256'h0;
 parameter INIT_RAM_2F=256'h0;
 parameter INIT_RAM_30=256'h0;
 parameter INIT_RAM_31=256'h0;
 parameter INIT_RAM_32=256'h0;
 parameter INIT_RAM_33=256'h0;
 parameter INIT_RAM_34=256'h0;
 parameter INIT_RAM_35=256'h0;
 parameter INIT_RAM_36=256'h0;
 parameter INIT_RAM_37=256'h0;
 parameter INIT_RAM_38=256'h0;
 parameter INIT_RAM_39=256'h0;
 parameter INIT_RAM_3A=256'h0;
 parameter INIT_RAM_3B=256'h0;
 parameter INIT_RAM_3C=256'h0;
 parameter INIT_RAM_3D=256'h0;
 parameter INIT_RAM_3E=256'h0;
 parameter INIT_RAM_3F=256'h0;
 localparam [16383:0] BITS={INIT_RAM_3F,INIT_RAM_3E,INIT_RAM_3D,INIT_RAM_3C,INIT_RAM_3B,INIT_RAM_3A,INIT_RAM_39,INIT_RAM_38,INIT_RAM_37,INIT_RAM_36,INIT_RAM_35,INIT_RAM_34,INIT_RAM_33,INIT_RAM_32,INIT_RAM_31,INIT_RAM_30,INIT_RAM_2F,INIT_RAM_2E,INIT_RAM_2D,INIT_RAM_2C,INIT_RAM_2B,INIT_RAM_2A,INIT_RAM_29,INIT_RAM_28,INIT_RAM_27,INIT_RAM_26,INIT_RAM_25,INIT_RAM_24,INIT_RAM_23,INIT_RAM_22,INIT_RAM_21,INIT_RAM_20,INIT_RAM_1F,INIT_RAM_1E,INIT_RAM_1D,INIT_RAM_1C,INIT_RAM_1B,INIT_RAM_1A,INIT_RAM_19,INIT_RAM_18,INIT_RAM_17,INIT_RAM_16,INIT_RAM_15,INIT_RAM_14,INIT_RAM_13,INIT_RAM_12,INIT_RAM_11,INIT_RAM_10,INIT_RAM_0F,INIT_RAM_0E,INIT_RAM_0D,INIT_RAM_0C,INIT_RAM_0B,INIT_RAM_0A,INIT_RAM_09,INIT_RAM_08,INIT_RAM_07,INIT_RAM_06,INIT_RAM_05,INIT_RAM_04,INIT_RAM_03,INIT_RAM_02,INIT_RAM_01,INIT_RAM_00};
 localparam W=BIT_WIDTH,L=$clog2(W);
 reg [W-1:0] mem[0:16384/W-1];integer i;
 initial for(i=0;i<16384/W;i=i+1)mem[i]=BITS[i*W+:W];
 reg [31:0] bp=0,pl=0;
 always @(posedge CLK)if(RESET)begin bp<=0;pl<=0;end else begin
  if(OCE)pl<=bp;
  if(CE)bp[W-1:0]<=mem[AD[13:L]];
 end
 assign DO=READ_MODE ? pl:bp;
endmodule
// Ideal PLL (see the header). The netlists use CLKOUT, CLKOUTP and LOCK.
module rPLL(output reg CLKOUT=0,output reg CLKOUTP=0,output CLKOUTD,CLKOUTD3,output reg LOCK=0,
 input CLKIN,CLKFB,RESET,RESET_P,input [5:0] FBDSEL,IDSEL,ODSEL,input [3:0] PSDA,DUTYDA,FDLY);
 parameter FCLKIN="27",DYN_IDIV_SEL="false",IDIV_SEL=0,DYN_FBDIV_SEL="false",FBDIV_SEL=0,DYN_ODIV_SEL="false",ODIV_SEL=8;
 parameter PSDA_SEL="0000",DYN_DA_EN="false",DUTYDA_SEL="1000",CLKOUT_FT_DIR=1'b1,CLKOUTP_FT_DIR=1'b1;
 parameter CLKOUT_DLY_STEP=0,CLKOUTP_DLY_STEP=0,CLKFB_SEL="internal",CLKOUT_BYPASS="false",CLKOUTP_BYPASS="false";
 parameter CLKOUTD_BYPASS="false",DYN_SDIV_SEL=2,CLKOUTD_SRC="CLKOUT",CLKOUTD3_SRC="CLKOUT",DEVICE="GW2A-18";
 localparam [31:0] P=PSDA_SEL;
 localparam integer STEPS=(P[31:24]=="1")*8+(P[23:16]=="1")*4+(P[15:8]=="1")*2+(P[7:0]=="1");
 localparam real PERIOD=1000.0*(IDIV_SEL+1)/(27.0*(FBDIV_SEL+1));
 initial if(FCLKIN!="27"||DYN_IDIV_SEL!="false"||DYN_FBDIV_SEL!="false"||DYN_DA_EN!="false")$fatal(1,"rPLL option not modelled");
 initial begin #100;forever #(PERIOD/2.0) CLKOUT=~CLKOUT;end
 always @(CLKOUT)CLKOUTP<=#(PERIOD*STEPS/16.0)CLKOUT;
 initial begin #5000;LOCK=1;end
 assign CLKOUTD=1'b0;assign CLKOUTD3=1'b0;
endmodule
