#!/usr/bin/env python3
"""Run riscv-arch-test (RISCOF, Spike as reference) against SimpleRisc images.

For each selected image this generates the Verilog with Clash (reusing it
only when source and output hashes match), makes a simulation copy, builds a Verilator model with a stub
boot ROM that jumps straight to the preloaded image (no UART loader), and runs
RISCOF with the simplerisc DUT plugin and the spike reference plugin. The
ISA string, and with it the set of tests RISCOF selects, follows the image:
RV32IMCZicsr_Zifencei with RVC, RV32IMZicsr_Zifencei without.

Results go to output/arch-tests/results.md (a pass/fail table per suite and
test, one column per image) and results.json. Simulation only.
Use --regenerate after changing the installed compiler or dependency build;
the generation manifest tracks project inputs, not tool binaries.

  boards/tangnano20k/arch-tests/run_arch_tests.py [--images sdram-rvc bram ...]
"""
from pathlib import Path
import argparse
import configparser
import hashlib
import json
import os
import re
import shutil
import subprocess
import sys
import time

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]
sys.path.insert(0, str(HERE.parent/'sdram'))
from run_perf import sv_safe  # noqa: E402

ARCH_TEST_TAG = '3.9.1'
ARCH_TEST_URL = 'https://github.com/riscv-non-isa/riscv-arch-test.git'
BRAM_OVERSIZE = {f'I/{op}-01' for op in ('beq', 'bge', 'bgeu', 'blt', 'bltu', 'bne', 'jal')}

# name: (Clash module, top entity, RVC, RAM bytes, testbench)
IMAGES = {
    'sdram-rvc': ('SdramPipeline', 'sdram_pipeline_risc', 1, 8 << 20, 'sdram_tb'),
    'sdram': ('SdramPipeline', 'sdram_pipeline_risc', 0, 8 << 20, 'sdram_tb'),
    'bram-rvc': ('SimpleRisc', 'simple_risc', 1, 64 << 10, 'bram_tb'),
    'bram': ('SimpleRisc', 'simple_risc', 0, 64 << 10, 'bram_tb'),
}


def run(cmd, log=None, **kwargs):
    cmd = [str(x) for x in cmd]
    if log is None:
        return subprocess.run(cmd, check=True, **kwargs)
    with open(log, 'w') as f:
        result = subprocess.run(cmd, stdout=f, stderr=subprocess.STDOUT, **kwargs)
    if result.returncode:
        sys.stdout.write(Path(log).read_text()[-4000:])
        raise SystemExit(f'failed ({result.returncode}): {" ".join(cmd[:3])} ... log {log}')
    return result


def arch_test_dir(a):
    if not a.arch_test.exists():
        print(f'fetching riscv-arch-test {ARCH_TEST_TAG} into {a.arch_test}')
        run(['git', 'clone', '-q', '--depth', '1', '--branch', ARCH_TEST_TAG, ARCH_TEST_URL, a.arch_test])
    def git(*args):
        return subprocess.check_output(['git', '-C', str(a.arch_test), *args], text=True).strip()
    if git('rev-parse', 'HEAD') != git('rev-parse', f'{ARCH_TEST_TAG}^{{commit}}') or git('status', '--porcelain', '--untracked-files=no'):
        raise SystemExit(f'architecture tests must be an unmodified {ARCH_TEST_TAG} checkout')
    return a.arch_test


def hashes(paths, relative):
    return {str(p.relative_to(relative)): hashlib.sha256(p.read_bytes()).hexdigest() for p in sorted(paths)}


def generation_inputs(module, rvc):
    paths = list((ROOT/'src').rglob('*.hs')) + list((HERE.parent/'bootrom').glob('*.hex'))
    paths += [ROOT/p for p in ('cabal.project', 'cabal.project.freeze', 'haskplayground.cabal')]
    return {'module': module, 'rvc': rvc, 'sources': hashes(paths, ROOT)}


def generate(module, rvc, out, regenerate):
    # Clash emits both annotated top entities when compiling SdramPipeline.
    # Reuse its BRAM entity only with the same verified input/output manifest.
    if module == 'SimpleRisc' and not regenerate:
        sibling = out/'gen'/f'SdramPipeline-{rvc}'
        try:
            previous = json.loads((sibling.parent/f'SdramPipeline-{rvc}.json').read_text())
        except (OSError, ValueError):
            previous = None
        expected = {'inputs': generation_inputs('SdramPipeline', rvc),
                    'outputs': hashes(sibling.rglob('*.v'), sibling)}
        if previous == expected and list((sibling/'SimpleRisc.topEntity').glob('*.v')):
            return sibling/'SimpleRisc.topEntity'
    generated = out/'gen'/f'{module}-{rvc}'
    entity = generated/f'{module}.topEntity'
    generated.parent.mkdir(parents=True, exist_ok=True)
    manifest = generated.parent/f'{module}-{rvc}.json'
    inputs = generation_inputs(module, rvc)
    outputs = hashes(generated.rglob('*.v'), generated)
    try:
        previous = json.loads(manifest.read_text())
    except (OSError, ValueError):
        previous = None
    if regenerate or not list(entity.glob('*.v')) or previous != {'inputs': inputs, 'outputs': outputs}:
        # A failed generation must never leave a reusable success manifest.
        manifest.unlink(missing_ok=True)
        options = ['-isrc', '-hide-package', 'haskplayground'] + (['-DRISCV_COMPRESSED'] if rvc else [])
        print(f'clash: {module} rvc={rvc}')
        run(['cabal', 'run', '-v0', 'clash', '--', *options, f'src/{module}.hs', '--verilog', '-fclash-clear',
             '-outputdir', generated], log=out/'gen'/f'{module}-{rvc}.log', cwd=ROOT)
        if generation_inputs(module, rvc) != inputs:
            raise SystemExit('sources changed during Clash generation; rerun against a stable checkout')
        outputs = hashes(generated.rglob('*.v'), generated)
        if not list(entity.glob('*.v')):
            raise SystemExit(f'Clash produced no RTL in {entity}')
        manifest.write_text(json.dumps({'inputs': inputs, 'outputs': outputs}, indent=2)+'\n')
    return entity


def stub_rom(directory, ram_bytes):
    elf = directory/'stub_rom.elf'
    run(['riscv64-unknown-elf-gcc', '-march=rv32i', '-mabi=ilp32', '-nostdlib', '-nostartfiles',
         '-Ttext=0', f'-DRAM_END={0x8000_0000 + ram_bytes:#x}', HERE/'stub_rom.S', '-o', elf])
    run(['riscv64-unknown-elf-objcopy', '-O', 'binary', elf, directory/'stub_rom.bin'])
    code = (directory/'stub_rom.bin').read_bytes()
    words = [int.from_bytes(code[i:i+4], 'little') for i in range(0, len(code), 4)]
    rom = directory/'stub_rom.hex'
    rom.write_text('\n'.join(f'{w:08x}' for w in words + [0x13]*(512-len(words)))+'\n')
    return rom


def simulation_copy(entity, sim, testbench, rom):
    """sv_safe copies of the generated Verilog; the BRAM image also gets the
    stub ROM contents and the RAM preload/signature hook."""
    copied = []
    for source in sorted(entity.glob('*.v')):
        text = sv_safe(source.read_text())
        if testbench == 'bram_tb':
            text, roms = re.subn(r'(ROM\[512-1-(i_\d+)\] = rom_init\[\2\*32\+:32\];\n    end\n)',
                                 lambda m: m[1]+f'    $readmemh("{rom}", ROM);\n', text, count=1)
            copied += ['rom']*roms
            ram = 'reg [32-1:0] mem [16384-1:0];\n'
            if ram in text:
                text = text.replace(ram, ram+(HERE/'bram_ram_hook.vh').read_text(), 1)
                copied.append('ram')
        target = sim/source.name
        target.write_text(text)
        copied.append(target)
    if testbench == 'bram_tb':
        assert copied.count('rom') == 1 and copied.count('ram') == 1, 'generated Verilog changed shape'
    return [c for c in copied if isinstance(c, Path)]


def build_model(name, a, out):
    module, entity_name, rvc, ram_bytes, testbench = IMAGES[name]
    entity = generate(module, rvc, out, a.regenerate)
    sim = out/name/'sim'
    if sim.exists():
        shutil.rmtree(sim)
    sim.mkdir(parents=True)
    rom = stub_rom(sim, ram_bytes)
    sources = simulation_copy(entity, sim, testbench, rom)
    extra = []
    params = []
    if testbench == 'sdram_tb':
        s = HERE.parent/'sdram'
        extra = [s/'pipeline_cache.v', s/'clock_bridge.v', s/'sdram.v', s/'burst_memory.v']
        params = [f'-GROM_FILE="{rom}"']
    print(f'verilator: {name}')
    run(['verilator', '--binary', '--timing', '-O3', '-Wno-fatal', '-Wno-lint', '-Wno-style',
         '--top-module', testbench, '--Mdir', sim/'obj', '-j', '0', *params,
         *sources, *extra, HERE/f'{testbench}.v'], log=sim/'build.log', cwd=ROOT)
    return sim/'obj'/f'V{testbench}'


def isa_yaml(path, rvc):
    isa = 'RV32IMCZicsr_Zifencei' if rvc else 'RV32IMZicsr_Zifencei'
    misa = 0x4000_1104 if rvc else 0x4000_1100
    extensions = misa & 0x3ff_ffff
    path.write_text(f'''hart_ids: [0]
hart0:
  ISA: {isa}
  physical_addr_sz: 32
  User_Spec_Version: '2.3'
  Privilege_Spec_Version: '1.11'
  hw_data_misaligned_support: false
  supported_xlen: [32]
  misa:
   reset-val: {misa:#x}
   rv32:
     accessible: true
     mxl:
       implemented: true
       type:
           warl:
              dependency_fields: []
              legal:
                - mxl[1:0] in [0x1]
              wr_illegal:
                - Unchanged
     extensions:
       implemented: true
       type:
           warl:
              dependency_fields: []
              legal:
                - extensions[25:0] bitmask [{extensions:#09x}, 0x0000000]
              wr_illegal:
                - Unchanged
''')
    return path


def run_riscof(name, model, a, arch, out):
    rvc, ram_bytes = IMAGES[name][2], IMAGES[name][3]
    directory = out/name
    work = directory/'riscof_work'
    if work.exists():
        shutil.rmtree(work)
    config = configparser.ConfigParser()
    config.optionxform = str
    config['RISCOF'] = {'ReferencePlugin': 'spike', 'ReferencePluginPath': str(HERE/'spike'),
                        'DUTPlugin': 'simplerisc', 'DUTPluginPath': str(HERE/'simplerisc')}
    config['simplerisc'] = {'pluginpath': str(HERE/'simplerisc'),
                            'ispec': str(isa_yaml(directory/'simplerisc_isa.yaml', rvc)),
                            'pspec': str(HERE/'simplerisc'/'simplerisc_platform.yaml'),
                            'target_run': '1', 'jobs': str(a.jobs), 'sim': str(model),
                            'ram_bytes': hex(ram_bytes), 'limit': str(a.limit)}
    config['spike'] = {'pluginpath': str(HERE/'spike'), 'jobs': str(a.jobs)}
    with (directory/'config.ini').open('w') as f:
        config.write(f)
    suite = arch/'riscv-test-suite'
    print(f'riscof: {name}')
    started = time.time()
    subprocess.run([str(a.riscof), 'run', '--config', directory/'config.ini', '--suite', suite/'rv32i_m',
                    '--env', suite/'env', '--work-dir', work, '--no-browser'],
                   stdout=(directory/'riscof.log').open('w'), stderr=subprocess.STDOUT, cwd=directory)
    elapsed = time.time()-started
    results = {}
    for test_dir in sorted(work.glob('*/src/*.S')):
        suite_name, test = test_dir.parts[-3], test_dir.name[:-2]
        dut = test_dir/'dut'/'DUT-simplerisc.signature'
        ref = test_dir/'ref'/'Reference-spike.signature'
        log = test_dir/'dut'/'sim.log'
        lines = log.read_text().splitlines() if log.exists() else []
        note = next((x for x in lines if x.startswith('ARCH')), lines[-1] if lines else 'no simulator log')
        if not ref.exists():
            status = 'no reference'
        elif note.startswith('ARCH TOO BIG'):
            status = 'Too big'
        elif not dut.exists():
            status = 'Fail'
        else:
            d, r = dut.read_text().split(), ref.read_text().split()
            # A pass also needs the halt through the finisher (stub ROM marker).
            status = 'Pass' if r and d == r and note.startswith('ARCH END 600d0003') else 'Fail'
            if d != r:
                diffs = [i for i in range(max(len(d), len(r))) if (d[i:i+1] or [None]) != (r[i:i+1] or [None])]
                note += f'; {len(diffs)} of {len(r)} signature words differ, first at word {diffs[0]}'
        results[f'{suite_name}/{test}'] = {'status': status, 'note': note}
    return results, elapsed


def validation_errors(all_results):
    """Fail closed, including interrupted discovery and unexpected RAM skips.

    Counts and oversize exceptions belong to the pinned 3.9.1 suite. They
    must be reviewed explicitly when upgrading it, not learned from a run.
    RISCOF itself exits nonzero for the known BRAM missing signatures, so
    independently check every selected case instead of ignoring that exit.
    """
    errors = []
    for image, results in all_results.items():
        expected = 92 if IMAGES[image][2] else 63
        if len(results) != expected:
            errors.append(f'{image}: expected {expected} results, got {len(results)}')
        for test, result in results.items():
            allowed_skip = image.startswith('bram') and test in BRAM_OVERSIZE and result['status'] == 'Too big'
            if result['status'] != 'Pass' and not allowed_skip:
                errors.append(f'{image} {test}: {result["status"]}: {result["note"]}')
    if not all_results:
        errors.append('no images tested')
    return errors


def write_table(all_results, timings, out):
    images = list(all_results)
    tests = sorted({t for r in all_results.values() for t in r}, key=lambda t: (t.split('/')[0], t))
    lines = ['| suite | test | '+' | '.join(images)+' |', '|---|---|'+'---|'*len(images)]
    for t in tests:
        suite, test = t.split('/')
        cells = [all_results[i].get(t, {}).get('status', '—') for i in images]
        lines.append(f'| {suite} | {test} | '+' | '.join(cells)+' |')
    summary = ['| image | suite | tests | Pass | Fail | Too big |', '|---|---|---|---|---|---|']
    for i in images:
        for suite in sorted({t.split('/')[0] for t in all_results[i]}):
            statuses = [v['status'] for t, v in all_results[i].items() if t.split('/')[0] == suite]
            counts = [statuses.count(s) for s in ('Pass', 'Too big')]
            summary.append(f'| {i} | {suite} | {len(statuses)} | {counts[0]} | {len(statuses)-sum(counts)} | {counts[1]} |')
    failures = [f'- {i} {t}: {v["note"]}' for i in images for t, v in sorted(all_results[i].items())
                if v['status'] != 'Pass']
    text = ('# riscv-arch-test results\n\n' + '\n'.join(summary) + '\n\n' + '\n'.join(lines) + '\n\n'
            + ('Not passed:\n\n' + '\n'.join(failures) + '\n\n' if failures else 'Every test passed.\n\n')
            + 'Run time (RISCOF, both models): '
            + ', '.join(f'{i} {timings[i]:.0f} s' for i in images) + '\n')
    (out/'results.md').write_text(text)
    (out/'results.json').write_text(json.dumps(all_results, indent=1, sort_keys=True))
    return text


def main():
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument('--images', nargs='+', choices=list(IMAGES), default=list(IMAGES))
    p.add_argument('--arch-test', type=Path, default=Path.home()/'.cache'/f'riscv-arch-test-{ARCH_TEST_TAG}')
    p.add_argument('--riscof', type=Path, default=Path(os.environ.get('RISCOF', Path.home()/'.cache/riscof-venv/bin/riscof')))
    p.add_argument('--out', type=Path, default=ROOT/'output'/'arch-tests')
    p.add_argument('--jobs', type=int, default=os.cpu_count())
    p.add_argument('--limit', type=int, default=50_000_000, help='cycle limit per test')
    p.add_argument('--regenerate', action='store_true', help='rerun Clash even if generated Verilog exists')
    a = p.parse_args()
    if not a.riscof.exists():
        raise SystemExit(f'RISCOF not found at {a.riscof}: python3 -m venv ~/.cache/riscof-venv && '
                         '~/.cache/riscof-venv/bin/pip install riscof (or set RISCOF)')
    out = a.out.resolve()
    out.mkdir(parents=True, exist_ok=True)
    arch = arch_test_dir(a)
    all_results, timings = {}, {}
    for name in a.images:
        (out/name).mkdir(exist_ok=True)
        model = build_model(name, a, out)
        all_results[name], timings[name] = run_riscof(name, model, a, arch, out)
        passed = sum(v['status'] == 'Pass' for v in all_results[name].values())
        print(f'{name}: {passed}/{len(all_results[name])} passed')
    print(write_table(all_results, timings, out))
    print(f'results: {out/"results.md"}')
    errors = validation_errors(all_results)
    if errors:
        raise SystemExit('Architecture regression failed:\n'+'\n'.join(errors))


if __name__ == '__main__':
    main()
