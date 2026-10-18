#!/usr/bin/env python3
"""Exercise the three-stage cache port and undefined tag RAM collisions.

The fixture compares reads and every ordered masked write with an independent
RAM model. A simulation-only source copy returns X for same-address tag RAM
read/write collisions, as physical BSRAM is allowed to do. The production
collision guard must keep all replies correct; removing it must fail.
"""
import argparse
import hashlib
import json
from pathlib import Path
import subprocess

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]


def digest(text):
    return hashlib.sha256(text.encode()).hexdigest()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--out', type=Path, default=ROOT/'output/tangnano20k/pipeline-cache-regression')
    args = parser.parse_args()
    out = args.out.resolve();out.mkdir(parents=True, exist_ok=True)
    source = (HERE/'pipeline_cache.v').read_text()
    fixture = (HERE/'pipeline_cache_tb.v').read_text()
    results = []

    def run(name, rtl, tb, expected=0):
        rtl_path = out/(name+'.v');rtl_path.write_text(rtl)
        tb_path = out/(name+'-tb.v');tb_path.write_text(tb)
        executable = out/(name+'-sim')
        command = ['iverilog', '-g2012', '-s', 'pipeline_cache_tb', '-o', str(executable), str(rtl_path), str(tb_path)]
        compiled = subprocess.run(command, cwd=ROOT, capture_output=True, text=True, timeout=60)
        if compiled.returncode:
            raise AssertionError(compiled.stdout+compiled.stderr)
        executed = subprocess.run(['vvp', str(executable)], cwd=ROOT, capture_output=True, text=True, timeout=120)
        text = executed.stdout+executed.stderr
        (out/(name+'.log')).write_text(text)
        if executed.returncode != expected:
            raise AssertionError(f'{name}: exit {executed.returncode}\n{text}')
        if expected == 0:
            assert 'PASS: 201 reads, 121 ordered masked writes' in text, text
        else:
            assert 'undefined tag collision used as a hit' in text, text
        results.append({'name': name, 'passed': True, 'expected_exit': expected,
                        'rtl_sha256': digest(rtl), 'testbench_sha256': digest(tb), 'output': text})
        (out/'results.json').write_text(json.dumps({'complete': False, 'cases': results}, indent=2)+'\n')
        print(f'{name}: PASS', flush=True)

    run('normal', source, fixture)
    read = 'if(advance)entry_tag<=tags[cpu_address[LINE_BITS+1:2]];'
    poisoned_read = "if(advance)entry_tag<=(tag_write && tag_write_index==cpu_address[LINE_BITS+1:2]) ? {(20-LINE_BITS){1'bx}}:tags[cpu_address[LINE_BITS+1:2]];"
    assert source.count(read) == 1, 'update the collision injection for the current tag read port'
    poisoned = source.replace(read, poisoned_read)
    declaration = 'integer delay=0,'
    coverage = '''integer collisions=0;
 always @(posedge clk)if(!reset && cache.tag_conflict && !cache.a1[21])begin
   if(cache.advance)collisions=collisions+1;
   if(!cache.filled && cache.hit1!==1'b0)$fatal(1,"undefined tag collision used as a hit");
 end
 integer delay=0,'''
    finish = '$display("PASS: %0d reads,'
    checked_finish = '''if(collisions==0)$fatal(1,"tag collision poisoning was not exercised");
       $display("PASS: %0d logical tag collisions ignored",collisions);
       $display("PASS: %0d reads,'''
    assert fixture.count(declaration) == 1 and fixture.count(finish) == 1
    checked = fixture.replace(declaration, coverage).replace(finish, checked_finish)
    run('tag-collision-poison', poisoned, checked)
    guard = '!tag_conflict && '
    assert poisoned.count(guard) == 1
    run('missing-collision-guard', poisoned.replace(guard, ''), checked, expected=1)
    (out/'results.json').write_text(json.dumps({'complete': True,
        'source_sha256': digest(source), 'fixture_sha256': digest(fixture), 'cases': results}, indent=2)+'\n')


if __name__ == '__main__':
    main()
