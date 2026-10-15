// Integer RV32C/RV64C 2.0. Illegal and unsupported floating encodings are
// rejected; architectural HINTs become ADDI x0,x0,0. Keep original bits for mtval.
module rv_c_decompress(input rv64,input [15:0] c,output reg valid,output reg [31:0] word);
 wire [4:0] rd=c[11:7],rs2=c[6:2],rp1={2'b01,c[9:7]},rp2={2'b01,c[4:2]};
 wire [11:0] ci={{6{c[12]}},c[12],c[6:2]};
 wire [11:0] shamt={6'b0,c[12],c[6:2]};
 wire shift_valid=rv64 || !c[12];
 wire [11:0] addi4={2'b0,c[10:7],c[12:11],c[5],c[6],2'b0};
 wire [11:0] offw={5'b0,c[5],c[12:10],c[6],2'b0};
 wire [11:0] offd={4'b0,c[6:5],c[12:10],3'b0};
 wire [11:0] addi16={{2{c[12]}},c[12],c[4:3],c[5],c[2],c[6],4'b0};
 wire [20:0] jump={{9{c[12]}},c[12],c[8],c[10:9],c[6],c[7],c[2],c[11],c[5:3],1'b0};
 wire [12:0] branch={{4{c[12]}},c[12],c[6:5],c[2],c[11:10],c[4:3],1'b0};
 wire [11:0] lwsp={4'b0,c[3:2],c[12],c[6:4],2'b0};
 wire [11:0] ldsp={3'b0,c[4:2],c[12],c[6:5],3'b0};
 wire [11:0] swsp={4'b0,c[8:7],c[12:9],2'b0};
 wire [11:0] sdsp={3'b0,c[9:7],c[12:10],3'b0};
 function [31:0] ei(input [6:0] op,input [2:0] f,input [4:0] d,s,input [11:0] imm);
 ei={imm,s,f,d,op};endfunction
 function [31:0] er(input [6:0] op,input [2:0] f,input [6:0] f7,input [4:0] d,s,t);
 er={f7,t,s,f,d,op};endfunction
 function [31:0] es(input [2:0] f,input [4:0] s,t,input [11:0] imm);
 es={imm[11:5],t,s,f,imm[4:0],7'h23};endfunction
 function [31:0] eb(input [2:0] f,input [4:0] s,input [12:0] imm);
 eb={imm[12],imm[10:5],5'b0,s,f,imm[4:1],imm[11],7'h63};endfunction
 function [31:0] ej(input [4:0] d,input [20:0] imm);
 ej={imm[20],imm[10:1],imm[11],imm[19:12],d,7'h6f};endfunction
 always @* begin
  valid=1;word=0;
  case({c[1:0],c[15:13]})
   {2'b00,3'd0}:if(addi4!=0) word=ei(7'h13,0,rp2,2,addi4);else valid=0;
   {2'b00,3'd2}:word=ei(3,2,rp2,rp1,offw);
   {2'b00,3'd3}:if(rv64) word=ei(3,3,rp2,rp1,offd);else valid=0;
   {2'b00,3'd6}:word=es(2,rp1,rp2,offw);
   {2'b00,3'd7}:if(rv64) word=es(3,rp1,rp2,offd);else valid=0;
   {2'b01,3'd0}:word=(rd==0 || ci==0) ? 32'h13:ei(7'h13,0,rd,rd,ci);
   {2'b01,3'd1}:if(!rv64) word=ej(1,jump);else if(rd!=0) word=ei(7'h1b,0,rd,rd,ci);else valid=0;
   {2'b01,3'd2}:word=rd==0 ? 32'h13:ei(7'h13,0,rd,0,ci);
   {2'b01,3'd3}:if(rd==2) begin if(addi16!=0) word=ei(7'h13,0,2,2,addi16);else valid=0;end
     else if(ci!=0) word=rd==0 ? 32'h13:{{14{c[12]}},c[12],c[6:2],rd,7'h37};else valid=0;
   {2'b01,3'd4}:case(c[11:10])
    0:if(shift_valid) word=shamt==0 ? 32'h13:ei(7'h13,5,rp1,rp1,shamt);else valid=0;
    1:if(shift_valid) word=shamt==0 ? 32'h13:ei(7'h13,5,rp1,rp1,12'h400|shamt);else valid=0;
    2:word=ei(7'h13,7,rp1,rp1,ci);
    3:case({c[12],c[6:5]})
     0:word=er(7'h33,0,7'h20,rp1,rp1,rp2);
     1:word=er(7'h33,4,0,rp1,rp1,rp2);
     2:word=er(7'h33,6,0,rp1,rp1,rp2);
     3:word=er(7'h33,7,0,rp1,rp1,rp2);
     4:if(rv64) word=er(7'h3b,0,7'h20,rp1,rp1,rp2);else valid=0;
     5:if(rv64) word=er(7'h3b,0,0,rp1,rp1,rp2);else valid=0;
     default:valid=0;
    endcase
   endcase
   {2'b01,3'd5}:word=ej(0,jump);
   {2'b01,3'd6}:word=eb(0,rp1,branch);
   {2'b01,3'd7}:word=eb(1,rp1,branch);
   {2'b10,3'd0}:if(shift_valid) word=(rd==0 || shamt==0) ? 32'h13:ei(7'h13,1,rd,rd,shamt);else valid=0;
   {2'b10,3'd2}:if(rd!=0) word=ei(3,2,rd,2,lwsp);else valid=0;
   {2'b10,3'd3}:if(rv64 && rd!=0) word=ei(3,3,rd,2,ldsp);else valid=0;
   {2'b10,3'd4}:if(!c[12]) begin
    if(rs2==0) begin if(rd!=0) word=ei(7'h67,0,0,rd,0);else valid=0;end
    else word=rd==0 ? 32'h13:er(7'h33,0,0,rd,0,rs2);
   end else begin
    if(rs2==0) word=rd==0 ? 32'h00100073:ei(7'h67,0,1,rd,0);
    else word=rd==0 ? 32'h13:er(7'h33,0,0,rd,rd,rs2);
   end
   {2'b10,3'd6}:word=es(2,2,rs2,swsp);
   {2'b10,3'd7}:if(rv64) word=es(3,2,rs2,sdsp);else valid=0;
   default:valid=0;
  endcase
 end
endmodule
