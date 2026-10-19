#!/usr/bin/env python3
"""Execute the generated Clash pipeline with its real cache and a delayed RAM.

The test bypasses UART upload with a two-instruction ROM and a preloaded image.
UART Ctrl-C still travels through the actual receiver during a long cache miss.
Requires a generated SdramPipeline top entity; --compressed selects an RV32IMC
guest and must be paired with a RISCV_COMPRESSED-generated top entity.
"""
from pathlib import Path
import argparse
import re
import subprocess
from run_perf import sv_safe

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]
LOADER_PROGRAM = r'''
    .section .text.entry,"ax",@progbits
    .globl _start
_start:
    lui t0,0x10000
    li t1,76
    sb t1,0(t0)
    ebreak
'''
PROGRAM = r'''
    .section .text.entry,"ax",@progbits
    .globl _start
    .option norelax
_start:
    li s8,0x80600000
    li s9,0x807ff000
    li s7,1
    lw t0,0(s9)
    addi t0,t0,1
    sw t0,0(s9)
    li t1,2
    beq t0,t1,recovered
    li t1,1
    bne t0,t1,failed

    # SD SPI: actual CPU MMIO handshake, synchronized external loopback,
    # mode-0 byte exchange, and capability register. DMA stays unmapped.
    li t3,0x10001000
    lw t0,12(t3)
    li t1,0x53445031
    bne t0,t1,failed
    li t0,0x301
    sw t0,8(t3)
    lw t1,8(t3)
    bne t0,t1,failed
    li t0,0x96
    sw t0,0(t3)
spi_wait:
    lw t0,4(t3)
    bnez t0,spi_wait
    lw t0,0(t3)
    li t1,0x96
    bne t0,t1,failed
    li t0,0x300
    sw t0,8(t3)
    lw t1,8(t3)
    bne t0,t1,failed

    # A known forced miss must count physical held clocks, not retire them.
    li s7,2
    li s0,0x80500000
    li a2,0x0200bff8
    lw a3,0(a2)
    csrr s2,mcycle
    csrr s3,minstret
    lw a0,0(s0)
    csrr t0,mcycle
    csrr t1,minstret
    sub t0,t0,s2
    sub t1,t1,s3
    sw t0,8(s8)
    sw t1,12(s8)
    lw t2,0(a2)
    sub t2,t2,a3
    sw t2,28(s8)
    li t3,1000
    bltu t2,t3,failed
    li t2,1000
    bltu t0,t2,failed
    li t2,64
    bgeu t1,t2,failed
    beqz t1,failed
    li t2,0x13579bdf
    bne a0,t2,failed

    # Ordered masked writes and immediate loads through the unified cache.
    li s7,3
    li s0,0x80200000
    li t0,0x11223344
    sw t0,0(s0)
    li t0,0xaa
    sb t0,1(s0)
    li t0,0xbbcc
    sh t0,2(s0)
    lw t1,0(s0)
    li t2,0xbbccaa44
    bne t1,t2,failed
    sw t1,16(s8)
    lbu t1,1(s0)
    li t2,0xaa
    bne t1,t2,failed
    lb t1,1(s0)
    li t2,-86
    bne t1,t2,failed
    lhu t1,2(s0)
    li t2,0xbbcc
    bne t1,t2,failed
    lh t1,2(s0)
    li t2,-17460
    bne t1,t2,failed

    # Identical cache indices at different high physical addresses must differ.
    li s7,4
    li s1,0x80400000
    li t0,0xdeadbeef
    sw t0,0(s1)
    lw t1,0(s0)
    li t2,0xbbccaa44
    bne t1,t2,failed
    lw t1,0(s1)
    bne t1,t0,failed
    li s1,0x807ffffc
    li t0,0xabcdef01
    sw t0,0(s1)
    lw t1,0(s1)
    bne t1,t0,failed

    # Execute freshly written and then patched instructions (including FENCE.I).
    li s7,5
    li s1,0x80201000
    li t0,0x02a00513
    sw t0,0(s1)
    li t0,0x00008067
    sw t0,4(s1)
    fence.i
    jalr ra,0(s1)
    li t0,42
    bne a0,t0,failed
    li t0,0x02b00513
    sw t0,0(s1)
    fence.i
    jalr ra,0(s1)
    li t0,43
    bne a0,t0,failed

    li s7,6
    li a0,-123456789
    li a1,12345
    mul t0,a0,a1
    li t1,MUL_LOW
    bne t0,t1,failed
    mulh t0,a0,a1
    li t1,MUL_HIGH
    bne t0,t1,failed
    mulhsu t0,a0,a1
    bne t0,t1,failed
    mulhu t0,a0,a1
    li t1,MUL_UNSIGNED_HIGH
    bne t0,t1,failed
    div t0,a0,a1
    li t1,-10000
    bne t0,t1,failed
    rem t0,a0,a1
    li t1,-6789
    bne t0,t1,failed
    div t0,a0,zero
    li t1,-1
    bne t0,t1,failed
    rem t0,a0,zero
    bne t0,a0,failed
    li a0,0x80000000
    li a1,-1
    div t0,a0,a1
    bne t0,a0,failed
    rem t0,a0,a1
    bnez t0,failed

    li s7,7
    li t0,257
    li t2,0
square_loop:
    mul t1,t0,t0
    add t2,t2,t1
    addi t0,t0,-1
    bnez t0,square_loop
    li t1,5691265
    bne t2,t1,failed
    sw t2,20(s8)

    # Timer interrupts become due during eight forced cache misses. Mix
    # compressed branches and a straddling 32-bit instruction with load-use
    # dependencies. The handler must resume exactly once, preserving data;
    # stores on the branch's wrong path must never reach memory.
    li s7,10
    la t0,timer_handler
    csrw mtvec,t0
    li s11,0
    li a4,8
    li a5,0
    li s1,0x80510000
    li a6,0x10000
    li t0,0x5a5a1234
    sw t0,36(s8)
    li t0,128
    csrw mie,t0
    csrsi mstatus,8
timer_loop:
    li t3,0x0200bff8
    lw t0,0(t3)
    lw t1,4(t3)
    li t2,3000
    add t2,t0,t2
    sltu t0,t2,t0
    add t1,t1,t0
    li t3,0x02004000
    li t0,-1
    sw t0,0(t3)
    sw t1,4(t3)
    sw t2,0(t3)
    lw a0,0(s1)
    addi a0,a0,1
#ifdef RVC_GUEST
    .balign 4
    c.nop
    .option push
    .option norvc
    addi a0,a0,1
    .option pop
#else
    addi a0,a0,1
#endif
    li t0,2
    bne a0,t0,failed
    addi a5,a5,1
timer_wait:
    bltu s11,a5,timer_wait
    bne s11,a5,failed
    beq a0,t0,timer_taken
    sw zero,36(s8)
timer_taken:
    add s1,s1,a6
    addi a4,a4,-1
    bnez a4,timer_loop
    csrci mstatus,8
    csrw mie,zero
    sw s11,32(s8)
    lw t0,36(s8)
    li t1,0x5a5a1234
    bne t0,t1,failed

    # A private handler checks precise cause and trap value, then resumes.
    li s7,8
    la t0,trap_handler
    csrw mtvec,t0
    li s5,0
    li s10,4
    li s4,11
    li s6,0
    ecall
    li s4,2
    li s6,-1
    .word 0xffffffff
    li s4,4
    li s6,0x80200001
    .option push
    .option norvc
    lw a0,1(s0)
    .option pop
#ifndef RVC_GUEST
    li s4,0
    la t0,aligned_target
    addi t0,t0,2
    mv s6,t0
    jalr zero,0(t0)
    li t0,4
#else
    # A reserved compressed encoding must preserve raw mtval and resume at pc+2.
    li s4,2
    li s6,0
    li s10,2
    .half 0x0000
    li s10,4
    li t0,4
#endif
    bne s5,t0,failed
    sw s5,24(s8)
    csrw mtvec,zero

    li t0,0x10000000
    li t1,80
uart_pass:
    lw t2,4(t0)
    andi t2,t2,1
    beqz t2,uart_pass
    sb t1,0(t0)
    li t0,0x600d0001
    sw t0,0(s8)
    # The bench delays this line long enough to receive an entire Ctrl-C byte.
    li s0,0x80300000
    li s1,0x80301000
wait_for_break:
    lw t0,0(s0)
    lw t1,0(s1)
    j wait_for_break

recovered:
    li s7,9
    lw t0,0(s8)
    li t1,0x600d0001
    bne t0,t1,failed
    lw t0,16(s8)
    li t1,0xbbccaa44
    bne t0,t1,failed
    li t0,0x10000000
    li t1,82
uart_recovery:
    lw t2,4(t0)
    andi t2,t2,1
    beqz t2,uart_recovery
    sb t1,0(t0)
    li t0,0x600d0002
    sw t0,0(s8)
park:
    j park

    .balign 4
aligned_target:
    j failed
    .balign 4
trap_handler:
    csrr t0,mcause
    bne t0,s4,failed
    csrr t0,mtval
    bne t0,s6,failed
    addi s5,s5,1
    csrr t0,mepc
    add t0,t0,s10
    csrw mepc,t0
    mret
    .balign 4
timer_handler:
    csrr s2,mcause
    li s3,0x80000007
    bne s2,s3,failed
    csrr s2,mepc
#ifdef RVC_GUEST
    andi s2,s2,1
#else
    andi s2,s2,3
#endif
    bnez s2,failed
    li s3,0x02004000
    li s2,-1
    sw s2,0(s3)
    sw s2,4(s3)
    addi s11,s11,1
    mret
failed:
    sw s7,4(s8)
    li t0,0xbad00001
    sw t0,0(s8)
    j park
'''
LINK = '''
OUTPUT_ARCH(riscv)
ENTRY(_start)
MEMORY { RAM (rwx) : ORIGIN = 0x80000000, LENGTH = 8M }
SECTIONS {
  .text : { KEEP(*(.text.entry)) *(.text .text.*) } > RAM
  .rodata : { *(.rodata .rodata.*) } > RAM
  .data : { *(.data .data.* .sdata .sdata.*) } > RAM
  .bss : { *(.bss .bss.* .sbss .sbss.* COMMON) } > RAM
}
'''


def run(args, expected=0, timeout=180):
    p = subprocess.run([str(a) for a in args], cwd=ROOT, capture_output=True,
                       text=True, timeout=timeout)
    if (p.returncode == 0 if expected is None else p.returncode != expected):
        raise AssertionError(f'{args}: exit {p.returncode}\n{p.stdout}\n{p.stderr}')
    return p.stdout + p.stderr


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--generated', type=Path,
                        help='generated Clash directory; defaults to SdramPipeline-0 or -1 matching --compressed')
    parser.add_argument('--compressed', action='store_true')
    parser.add_argument('--simulator', choices=['iverilog', 'verilator'], default='iverilog')
    parser.add_argument('--cache-line-bits', type=int, choices=range(7, 13), default=12,
                        help='12 selects the production 64 KiB cache; 7 stresses small-cache conflicts')
    parser.add_argument('--cdc-bridge', action='store_true',help='cross into an offset half-speed backing-memory clock')
    parser.add_argument('--boot-loader', action='store_true', help='exercise the actual ROM and UART P/R upload')
    parser.add_argument('--uart-bit-cycles',type=int,default=868,
                        help='simulation-only UART timing override; hardware timing is 868')
    parser.add_argument('--pin-model',action='store_true',help='actual burst controller plus functional SDRAM pins; requires --boot-loader')
    parser.add_argument('--pin-freq-mhz',type=int,default=48,help='pin-model core/SDRAM frequency')
    parser.add_argument('--pin-phase-degrees',type=int,default=180,help='pin-model SDRAM clock edge delay relative to core')
    parser.add_argument('--pin-tac-ns',type=float,default=0.0,help='pin-model read data output-access delay')
    parser.add_argument('--burst-rtl',type=Path,default=HERE/'burst_memory.v',help='burst adapter RTL override for controlled diagnostics')
    parser.add_argument('--sdram-rtl',type=Path,default=HERE/'sdram.v',help='SDRAM controller RTL override for controlled diagnostics')
    parser.add_argument('--rom-file',type=Path,help='simulation ROM override')
    parser.add_argument('--loader-command',choices=['R','D','H'],default='R',help='H checks the Base T/Q/V/H protocol; D checks an eight-word debug-ROM dump')
    args = parser.parse_args()
    if args.uart_bit_cycles<32 or args.uart_bit_cycles>1024 or args.uart_bit_cycles%2:
        parser.error('--uart-bit-cycles must be even and between 32 and 1024')
    if args.pin_model and not args.boot_loader:
        parser.error('--pin-model currently requires --boot-loader')
    if args.pin_freq_mhz<1 or args.pin_freq_mhz>100:
        parser.error('--pin-freq-mhz must be between 1 and 100')
    if args.pin_phase_degrees<1 or args.pin_phase_degrees>359:
        parser.error('--pin-phase-degrees must be between 1 and 359')
    if not 0<=args.pin_tac_ns<1000/args.pin_freq_mhz:
        parser.error('--pin-tac-ns must be finite, nonnegative and less than one clock')
    if not args.pin_model and (args.pin_freq_mhz!=48 or args.pin_phase_degrees!=180 or args.pin_tac_ns!=0):
        parser.error('pin timing overrides require --pin-model')
    if args.loader_command!='R' and not args.boot_loader:
        parser.error('--loader-command requires --boot-loader')
    generated = (args.generated or ROOT/f'output/tangnano20k/sdram-generated/SdramPipeline-{int(args.compressed)}/SdramPipeline.topEntity').resolve()
    sources = [generated] if generated.is_file() else sorted(generated.glob('*.v'))
    if not sources:
        raise SystemExit(f'No generated Clash Verilog in {generated}; build SdramPipeline first.')
    out = ROOT/('output/tangnano20k/pipeline-rtl'+('-c' if args.compressed else '')+('-loader' if args.boot_loader else '')+
               ('-cdc' if args.cdc_bridge else '')+
               ('-pins' if args.pin_model else '')+(f'-uart{args.uart_bit_cycles}' if args.uart_bit_cycles!=868 else ''))
    if args.loader_command=='D':out=out.with_name(out.name+'-dump')
    if args.loader_command=='H':out=out.with_name(out.name+'-extended')
    if args.pin_model and (args.pin_freq_mhz!=48 or args.pin_phase_degrees!=180 or args.pin_tac_ns!=0):
        out=out.with_name(out.name+f'-f{args.pin_freq_mhz}-p{args.pin_phase_degrees}-t{args.pin_tac_ns:g}')
    if args.burst_rtl.resolve()!=(HERE/'burst_memory.v').resolve():
        out=out.with_name(out.name+'-adapter-'+args.burst_rtl.stem)
    if args.sdram_rtl.resolve()!=(HERE/'sdram.v').resolve():
        out=out.with_name(out.name+'-controller-'+args.sdram_rtl.stem)
    out=out.with_name(out.name+f'-l{args.cache_line_bits}-{args.simulator}')
    out.mkdir(parents=True, exist_ok=True)
    if args.uart_bit_cycles!=868:
        # Change only UART reload constants in simulation copies, never the
        # generated synthesis source. This accelerates loader debugging.
        copied=[];full_count=half_count=0
        for source in sources:
            text=source.read_text()
            text,n=re.subn(r"10'd867\b",f"10'd{args.uart_bit_cycles-1}",text);full_count+=n
            text,n=re.subn(r"10'd433\b",f"10'd{args.uart_bit_cycles//2-1}",text);half_count+=n
            target=out/source.name;target.write_text(text);copied.append(target)
        assert full_count>=6 and half_count>=1,(full_count,half_count)
        sources=copied
    program = (LOADER_PROGRAM if args.boot_loader else PROGRAM).replace('MUL_UNSIGNED_HIGH',str((((-123456789)&0xffffffff)*12345)>>32))
    program = program.replace('MUL_HIGH',str((-123456789*12345)>>32))
    program = program.replace('MUL_LOW',hex((-123456789*12345)&0xffffffff))
    (out/'program.S').write_text(program)
    (out/'link.ld').write_text(LINK)
    (out/'rom.hex').write_text(args.rom_file.read_text() if args.rom_file else
                             (HERE.parent/'bootrom/bootrom-sdram.hex').read_text() if args.boot_loader else
                             '\n'.join(['800002b7','00028067']+['00000013']*510)+'\n')
    run(['riscv64-unknown-elf-gcc', '-march='+('rv32imc_zicsr_zifencei' if args.compressed else 'rv32im_zicsr_zifencei'),
         '-mabi=ilp32', '-mcmodel=medany', '-nostdlib', '-nostartfiles',
         *(['-DRVC_GUEST'] if args.compressed else []), '-Wl,--no-relax',
         '-T',out/'link.ld',out/'program.S','-o',out/'program.elf'])
    run(['riscv64-unknown-elf-objcopy','-O','binary',out/'program.elf',out/'program.bin'])
    image=(out/'program.bin').read_bytes(); image+=b'\0'*((-len(image))%4)
    (out/'image.hex').write_text('\n'.join(f'{int.from_bytes(image[i:i+4],"little"):08x}'
                                         for i in range(0,len(image),4))+'\n')
    # Clash's Verilog backend can emit identifiers such as "new" and "byte";
    # these are legal Verilog identifiers but reserved in SystemVerilog.
    command=['iverilog','-g2005','-s','pipeline_cpu_tb','-P',f'pipeline_cpu_tb.ROM_FILE="{out}/rom.hex"',
         '-P','pipeline_cpu_tb.EXPECT_TRAPS=4',
         '-P',f'pipeline_cpu_tb.LINE_BITS={args.cache_line_bits}',
         '-P',f'pipeline_cpu_tb.LOADER_TEST={int(args.boot_loader)}',
         '-P',f'pipeline_cpu_tb.UART_BIT={args.uart_bit_cycles}',
         '-P',f'pipeline_cpu_tb.PIN_MODEL={int(args.pin_model)}',
         '-P',f'pipeline_cpu_tb.CDC_BRIDGE={int(args.cdc_bridge)}',
         '-P',f'pipeline_cpu_tb.PIN_FREQ_MHZ={args.pin_freq_mhz}',
         '-P',f'pipeline_cpu_tb.PIN_PHASE_DEGREES={args.pin_phase_degrees}',
         '-P',f'pipeline_cpu_tb.PIN_TAC_NS={args.pin_tac_ns}',
         '-P',f'pipeline_cpu_tb.LOADER_DUMP={int(args.loader_command=="D")}',
         '-P',f'pipeline_cpu_tb.LOADER_EXTENDED={int(args.loader_command=="H")}',
         '-o',out/'sim',*sources,HERE/'pipeline_cache.v',HERE/'clock_bridge.v',args.sdram_rtl.resolve(),args.burst_rtl.resolve(),HERE/'pipeline_cpu_tb.v']
    if args.simulator=='verilator':
        params=[str(command[i+1]).replace('pipeline_cpu_tb.', '-G', 1) for i,x in enumerate(command) if x=='-P']
        copied=[]
        for source in sources:
            target=out/('sv-'+source.name)
            target.write_text(sv_safe(source.read_text()));copied.append(target)
        run(['verilator','--binary','--timing','-O3','-Wno-fatal','-Wno-lint','-Wno-style',
             '--top-module','pipeline_cpu_tb','--Mdir',out/'obj','-j','4',*params,*copied,
             HERE/'pipeline_cache.v',HERE/'clock_bridge.v',args.sdram_rtl.resolve(),args.burst_rtl.resolve(),HERE/'pipeline_cpu_tb.v'],timeout=300)
        executable=[out/'obj/Vpipeline_cpu_tb']
    else:
        run(command)
        executable=['vvp',out/'sim']
    common=[*executable,f'+IMAGE={out}/image.hex',f'+WORDS={len(image)//4}']
    output=run(common,timeout=900 if args.boot_loader else 240)
    expected=('PASS: debug ROM memory dump' if args.loader_command=='D' else
              'PASS: extended ROM T/Q/V/H UART loader' if args.loader_command=='H' else
              'PASS: actual ROM UART loader' if args.boot_loader else 'PASS: pipeline/cache guest')
    assert expected in output,output
    (out/'pass.log').write_text(output)
    print(output,end='',flush=True)
    if args.boot_loader:
        return
    failed=run([*common,'+CORRUPT'],expected=None,timeout=240)
    assert 'guest failure stage=2' in failed,failed
    (out/'corrupt.log').write_text(failed)
    print('PASS: corrupt backend beat is rejected by the executing guest',flush=True)


if __name__=='__main__':
    main()
