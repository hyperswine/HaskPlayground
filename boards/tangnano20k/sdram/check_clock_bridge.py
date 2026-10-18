#!/usr/bin/env python3
"""Check mailbox CDC independently and with the real SDRAM command/pin model.

These are digital protocol tests, including deliberately unrelated frequencies.
They do not model metastability or sign off SDRAM electrical I/O timing.
"""
from pathlib import Path
import subprocess

HERE = Path(__file__).resolve().parent
OUT = HERE.parents[2] / 'output/tangnano20k/sdram-clock-bridge-tests'
OUT.mkdir(parents=True, exist_ok=True)


def run(name, top, sources, parameters=()):
    executable = OUT / name
    subprocess.run(['iverilog', '-g2012', '-s', top, '-o', str(executable),
                    *sum((['-P', f'{top}.{key}={value}'] for key, value in parameters), []),
                    *map(str, sources)], check=True)
    result = subprocess.run(['vvp', str(executable)], capture_output=True, text=True, timeout=60)
    (OUT / f'{name}.log').write_text(result.stdout + result.stderr)
    assert result.returncode == 0, result.stdout + result.stderr
    assert 'PASS:' in result.stdout, result.stdout
    print(result.stdout, end='', flush=True)


for name, cpu, memory, phase in [
    ('108-54-p17', 4.62962963, 9.25925926, 1.7),
    ('108-54-p63', 4.62962963, 9.25925926, 6.3),
    ('unrelated', 5.137, 9.417, 2.091),
    ('memory-faster', 8.713, 3.619, 0.713),
]:
    run(name, 'clock_bridge_tb', [HERE/'clock_bridge.v', HERE/'clock_bridge_tb.v'],
        [('CPU_HALF', cpu), ('MEM_HALF', memory), ('MEM_PHASE', phase)])

# Reuse the independently developed command/pin oracle and its bank/row/mask/
# refresh acceptance suite. The generated wrapper inserts only the CDC and a
# distinct CPU clock; the oracle and its expected memory remain unchanged.
source = (HERE/'burst_memory_tb.v').read_text()


def replace_once(old, new):
    global source
    assert source.count(old) == 1, old
    source = source.replace(old, new)


replace_once('localparam real HALF_PERIOD=500000000.0/FREQ;',
             'localparam real HALF_PERIOD=500000000.0/108000000;\n'
             ' localparam real MEM_HALF_PERIOD=500000000.0/FREQ;')
replace_once('wire phase_clk=~clk;',
             'reg mem_clk=0;initial begin #1.7;forever #(MEM_HALF_PERIOD)mem_clk=~mem_clk;end\n'
             ' wire phase_clk;assign #(1000000000.0/FREQ*135.0/360.0)phase_clk=mem_clk;')
replace_once('sdram_burst_memory #(.FREQ(FREQ)) dut(', '''wire mem_req,mem_write,mem_ready,mem_done,mem_valid;
 wire [20:0]mem_address;wire [31:0]mem_wdata,mem_rdata;
 wire [3:0]mem_mask;wire [1:0]mem_index;
 sdram_clock_bridge bridge(
  .cpu_clk(clk),.cpu_reset(reset),.cpu_req(req),.cpu_write(write),.cpu_address(address),
  .cpu_wdata(wdata),.cpu_mask(mask),.cpu_ready(ready),.cpu_done(done),.cpu_rdata(rdata),
  .cpu_word_valid(word_valid),.cpu_word_index(word_index),
  .mem_clk(mem_clk),.mem_reset(reset),.mem_req(mem_req),.mem_write(mem_write),
  .mem_address(mem_address),.mem_wdata(mem_wdata),.mem_mask(mem_mask),
  .mem_ready(mem_ready),.mem_done(mem_done),.mem_rdata(mem_rdata),
  .mem_word_valid(mem_valid),.mem_word_index(mem_index));
 sdram_burst_memory #(.FREQ(FREQ)) dut(''')
replace_once('''  .clk(clk),.clk_sdram(phase_clk),.reset(reset),.req(req),.write(write),
  .address(address),.wdata(wdata),.mask(mask),.ready(ready),.done(done),.rdata(rdata),
  .word_valid(word_valid),.word_index(word_index),''', '''  .clk(mem_clk),.clk_sdram(phase_clk),.reset(reset),.req(mem_req),.write(mem_write),
  .address(mem_address),.wdata(mem_wdata),.mask(mem_mask),.ready(mem_ready),.done(mem_done),.rdata(mem_rdata),
  .word_valid(mem_valid),.word_index(mem_index),''')
replace_once('if(wait_cycles>40)', 'if(wait_cycles>160)')
replace_once('wait(ready);', 'wait(mem_ready);')
replace_once('repeat(REFRESH_INTERVAL*3+30)', 'repeat(REFRESH_INTERVAL*7+60)')
replace_once('wait(!ready);read_line', 'wait(!mem_ready);read_line')
source = source.replace('PASS: %0d MHz SDRAM pins', 'PASS: 108 MHz CPU CDC / %0d MHz SDRAM pins')
generated = OUT/'burst_clock_bridge_tb.v'
generated.write_text(source)
for tac in [0.0, 6.0]:
    run(f'pins-108-54-p135-tac{tac:g}', 'burst_memory_tb',
        [HERE/'clock_bridge.v', HERE/'sdram.v', HERE/'burst_memory.v', generated],
        [('FREQ', 54000000), ('TAC_NS', tac)])
