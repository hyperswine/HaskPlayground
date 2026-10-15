#!/usr/bin/env python3
"""Exhaust all 16-bit encodings against GNU disassembly/assembly and both decoders.

GNU prints some RV128 shift aliases and reserved ADDI16SP-zero encodings even
in RV32/64 mode. Apply the C2.0 rules for these (and the absent F/D extensions)
explicitly; all remaining canonical instruction bits come from GNU assembler.
"""
import argparse,re,subprocess
from pathlib import Path

ROOT=Path(__file__).resolve().parents[3]
OUT=ROOT/'output/tangnano20k/rvc-tests'

def run(args,**kwargs):
    return subprocess.run(args,cwd=ROOT,check=True,**kwargs)

def canonical(c,mn,args,address,xlen):
    if mn in ('.insn','c.unimp') or mn.startswith('c.f'):return None
    args=args.split('#',1)[0].strip()
    parts=args.split(',') if args else []
    op=mn.removeprefix('c.')
    if op=='addi16sp' and int(parts[-1],0)==0:return None
    if xlen==32 and op in ('slli','srli','srai') and c&0x1000:return None
    if op in ('slli64','srli64','srai64'):return 'addi x0,x0,0' # zero-shamt HINT in RV32/64
    rd=parts[0] if parts else ''
    if (op=='addi' and (rd=='x0' or int(parts[-1],0)==0)) or (op in ('li','lui','mv','add','slli') and rd=='x0'):
        return 'addi x0,x0,0'
    if op in ('lw','ld','sw','sd','lwsp','ldsp','swsp','sdsp'):
        return op.removesuffix('sp')+' '+args
    if op=='addi4spn':return 'addi '+args
    if op=='addi16sp':return f'addi x2,x2,{parts[-1]}'
    if op in ('addi','addiw','andi','slli','srli','srai'):
        return f'{op} {rd},{rd},{parts[-1]}'
    if op=='li':return f'addi {rd},x0,{parts[-1]}'
    if op=='lui':return 'lui '+args
    if op in ('sub','xor','or','and','subw','addw','add'):
        return f'{op} {rd},{rd},{parts[-1]}'
    if op=='mv':return f'add {rd},x0,{parts[-1]}'
    if op=='jr':return f'jalr x0,0({rd})'
    if op=='jalr':return f'jalr x1,0({rd})'
    if op=='ebreak':return 'ebreak'
    if op in ('j','jal','beqz','bnez'):
        target=int(parts[-1].split()[0],16)
        offset=(target-address)&((1<<xlen)-1)
        if offset&(1<<(xlen-1)):offset-=1<<xlen
        relative=f'.{offset:+}'
        if op in ('j','jal'):return f'jal x{1 if op=="jal" else 0},{relative}'
        return f'{"beq" if op=="beqz" else "bne"} {rd},x0,{relative}'
    raise AssertionError((c,mn,args))

def oracle(xlen):
    r=run(['riscv64-unknown-elf-objdump','-D','-z','-b','binary','-m',f'riscv:rv{xlen}','-M','no-aliases,numeric',str(OUT/'all.bin')],capture_output=True,text=True)
    expected={};codes=[];source=['.option norvc','.option norelax','.text','.globl _start','_start:']
    for line in r.stdout.splitlines():
        m=re.match(r'\s*([0-9a-f]+):\s+([0-9a-f]{4})\s+(\S+)(?:\s+(.*))?$',line)
        if not m:continue
        address=int(m[1],16)
        if address%4:continue
        c=int(m[2],16);insn=canonical(c,m[3],m[4] or '',address,xlen)
        if insn is None:expected[c]=None
        else:codes.append(c);source.append(insn)
    assert len(expected)+len(codes)==49152
    asm=OUT/f'canonical-{xlen}.S';elf=asm.with_suffix('.elf');binary=asm.with_suffix('.bin')
    asm.write_text('\n'.join(source)+'\n')
    run(['riscv64-unknown-elf-gcc',f'-march=rv{xlen}imc',f'-mabi={"ilp32" if xlen==32 else "lp64"}','-nostdlib','-nostartfiles','-Wl,-Ttext=0','-Wl,--no-warn-rwx-segments',str(asm),'-o',str(elf)],capture_output=True,text=True)
    run(['riscv64-unknown-elf-objcopy','-O','binary',str(elf),str(binary)])
    bits=binary.read_bytes();assert len(bits)==4*len(codes),(xlen,len(bits),len(codes))
    for n,c in enumerate(codes):expected[c]=int.from_bytes(bits[n*4:n*4+4],'little')
    for c in range(3,65536,4):expected[c]=None
    return expected

def main():
    p=argparse.ArgumentParser(description=__doc__);p.add_argument('--skip-haskell',action='store_true');a=p.parse_args()
    OUT.mkdir(parents=True,exist_ok=True)
    (OUT/'all.bin').write_bytes(b''.join(c.to_bytes(2,'little')+b'\x01\0' for c in range(65536) if c&3!=3))
    run(['iverilog','-g2012','-s','compressed_tb','-o',str(OUT/'decoder'),'boards/tangnano20k/rv64/compressed.v','boards/tangnano20k/rv64/compressed_testbench.v'])
    rtl={}
    for line in run(['vvp',str(OUT/'decoder')],capture_output=True,text=True).stdout.splitlines():
        x,c,v,w=line.split();rtl[int(x),int(c,16)]=int(w,16) if v=='1' else None
    haskell={}
    if not a.skip_haskell:
        r=run(['cabal','exec','--','runghc','-isrc','test/RvcDecodeDump.hs'],capture_output=True,text=True)
        for line in r.stdout.splitlines():
            m=re.fullmatch(r'(32|64) ([0-9a-f]+) (-|[0-9a-f]+)',line)
            if m:haskell[int(m[1]),int(m[2],16)]=None if m[3]=='-' else int(m[3],16)
        assert len(haskell)==131072
    for xlen in [32,64]:
        expected=oracle(xlen)
        for c,w in expected.items():
            assert rtl[xlen,c]==w,(f'RV{xlen}C RTL',hex(c),rtl[xlen,c],w)
            if haskell:assert haskell[xlen,c]==w,(f'RV{xlen}C Clash',hex(c),haskell[xlen,c],w)
        print(f'PASS: RV{xlen}C all 65,536 encodings; {sum(w is not None for w in expected.values())} valid integer/HINT encodings; GNU canonical bits match RTL'+(' and Clash' if haskell else ''))
if __name__=='__main__':main()
