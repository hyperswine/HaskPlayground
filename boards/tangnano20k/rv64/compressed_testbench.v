module compressed_tb;
 reg rv64=0;reg [15:0] c=0;wire valid;wire [31:0] word;integer x,n;
 rv_c_decompress dut(rv64,c,valid,word);
 initial begin
  for(x=0;x<2;x=x+1) begin rv64=x;
   for(n=0;n<65536;n=n+1) begin c=n;#1;$display("%0d %04x %0d %08x",x ? 64:32,c,valid,word);end
  end
 end
endmodule
