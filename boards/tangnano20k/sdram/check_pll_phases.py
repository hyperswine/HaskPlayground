#!/usr/bin/env python3
"""Check phase, frequency and lock against Gowin's installed rPLL model.

This simulation needs no Gowin license. It checks the PLL configuration,
not physical jitter or SDRAM electrical margin.
"""
import argparse
import hashlib
import json
from pathlib import Path
import subprocess

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]
DEFAULT_LIBRARY = Path('/Applications/GowinIDE.app/Contents/Resources/Gowin_EDA/IDE/simlib/gw2a/prim_sim.v')


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('--simlib', type=Path, default=DEFAULT_LIBRARY)
    p.add_argument('--out', type=Path, default=ROOT/'output/tangnano20k/sdram-pll-phases')
    args = p.parse_args()
    if not args.simlib.is_file():
        p.error('provide the vendor gw2a/prim_sim.v using --simlib')
    args.out.mkdir(parents=True, exist_ok=True)
    original = (HERE/'pll.v').read_text()
    assert original.count('PSDA_SEL("1010")') == 1
    result = {'complete': False, 'passed': False, 'electrical_signoff': False,
              'vendor_model_sha256': hashlib.sha256(args.simlib.read_bytes()).hexdigest(),
              'source_sha256': {f.name: hashlib.sha256(f.read_bytes()).hexdigest()
                                for f in (HERE/'pll.v', HERE/'dual_pll.v', Path(__file__))}, 'cases': []}
    (args.out/'results.json').write_text(json.dumps(result, indent=2)+'\n')
    for phase in (0, 112.5, 135, 157.5, 225, 337.5):
        bits = format(int(phase/22.5), '04b')
        rtl = args.out/f'pll-p{phase:g}.v'
        rtl.write_text(original.replace('PSDA_SEL("1010")', f'PSDA_SEL("{bits}")'))
        for split in (False, True):
            name = f'{"split72" if split else "single54"}-p{phase:g}'
            cpu = 72 if split else 54
            instantiate = ('sdram_dual_pll #(.CPU_MHZ(72)) p(clk,core,memory,sdram,lock);'
                           if split else 'sdram_pll p(clk,core,sdram,lock);assign memory=core;')
            source = f'''`timescale 1ns/1ps
module pll_phase_tb;
 reg clk=0;always #18.5185 clk=~clk;
 wire core,memory,sdram,lock;
 {instantiate}
 real last_memory=0,previous_memory=0,previous_core=0,separation;
 integer samples=0;
 function real absval(input real x);absval=x<0 ? -x:x;endfunction
 always @(posedge memory)begin
  if(lock && previous_memory>0 && absval($realtime-previous_memory-1000.0/54)>0.03)
   $fatal(1,"memory period %0.6f",$realtime-previous_memory);
  previous_memory=$realtime;last_memory=$realtime;
 end
 always @(posedge core)begin
  if(lock && previous_core>0 && absval($realtime-previous_core-1000.0/{cpu})>0.03)
   $fatal(1,"CPU period %0.6f",$realtime-previous_core);
  previous_core=$realtime;
 end
 always @(posedge sdram)if(lock)begin
  #0.001;separation=$realtime-0.001-last_memory;
  if(absval(separation-(1000.0/54*{phase}/360))>0.03)
   $fatal(1,"phase separation %0.6f",separation);
  samples=samples+1;
  if(samples==8)begin
   $display("PASS: {name}, eight locked samples, CPU {cpu} MHz, memory 54 MHz, separation %0.6f ns",separation);
   $finish;
  end
 end
 initial begin #1000000;$fatal(1,"PLL did not lock");end
endmodule
'''
            fixture = args.out/(name+'-tb.v');fixture.write_text(source)
            executable = args.out/name
            compiled = subprocess.run(['iverilog', '-g2012', '-s', 'pll_phase_tb', '-o', str(executable),
                str(rtl), str(HERE/'dual_pll.v'), str(fixture), str(args.simlib)],
                capture_output=True, text=True, timeout=60)
            if compiled.returncode:
                raise RuntimeError(compiled.stdout+compiled.stderr)
            run = subprocess.run(['vvp', str(executable)], capture_output=True, text=True, timeout=30)
            output = run.stdout+run.stderr
            (args.out/(name+'.log')).write_text(output)
            passed = run.returncode == 0 and 'PASS:' in output
            result['cases'].append({'name': name, 'phase_degrees': phase, 'phase_bits': bits,
                                   'passed': passed, 'output': output,
                                   'fixture_sha256': hashlib.sha256(source.encode()).hexdigest()})
            (args.out/'results.json').write_text(json.dumps(result, indent=2)+'\n')
            if not passed:
                raise AssertionError(output)
            print(next(line for line in output.splitlines() if line.startswith('PASS:')), flush=True)
    result.update(complete=True, passed=True)
    (args.out/'results.json').write_text(json.dumps(result, indent=2)+'\n')
    print('PASS: 12 vendor PLL model cases; physical jitter unqualified')


if __name__ == '__main__':
    main()
