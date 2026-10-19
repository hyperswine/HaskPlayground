  // Inserted into the generated RAM wrapper by run_arch_tests.py.
  reg [4095:0] arch_image,arch_signature;
  integer arch_words,arch_i,arch_begin,arch_end,arch_out;
  initial begin
    for(arch_i=0;arch_i<16384;arch_i=arch_i+1)mem[arch_i]=0;
    if(!$value$plusargs("IMAGE=%s",arch_image) || !$value$plusargs("WORDS=%d",arch_words))$fatal(1,"missing image/word count");
    if(!$value$plusargs("SIG_BEGIN=%d",arch_begin) || !$value$plusargs("SIG_END=%d",arch_end) ||
       !$value$plusargs("SIGNATURE=%s",arch_signature))$fatal(1,"missing signature range/file");
    $readmemh(arch_image,mem,0,arch_words-1);
  end
  task arch_finish(input [31:0] marker);
  begin
    $display("ARCH END %08x cycles=%0d",marker,bram_tb.cycles);
    arch_out=$fopen(arch_signature,"w");
    for(arch_i=arch_begin;arch_i<arch_end;arch_i=arch_i+1)$fdisplay(arch_out,"%08x",mem[arch_i]);
    $fclose(arch_out);
    $finish;
  end endtask
  always @(posedge clkA)if(enA && weA && addrA==14'h3fff)arch_finish(datA);
  always @(posedge clkB)if(enB && weB && addrB==14'h3fff)arch_finish(datB);
