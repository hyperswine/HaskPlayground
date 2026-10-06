#!/usr/bin/env python3
"""Compile fixed independent RV64I/M reference vectors; run on board or RTL."""
import argparse, random, subprocess
from pathlib import Path
p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--freq-mhz',type=float,default=27)
p.add_argument('--port',default='/dev/cu.usbserial-20250303171')
p.add_argument('--simulate',action='store_true')
p.add_argument('--traps',action='store_true')
a=p.parse_args();here=Path(__file__).resolve().parent;root=here.parents[2]
out=root/'output/tangnano20k/rv64-tests';out.mkdir(parents=True,exist_ok=True)
mask=(1<<64)-1
ops=['add','sub','sll','srl','sra','slt','sltu','xor','or','and',
 'mul','mulh','mulhsu','mulhu','div','divu','rem','remu',
 'addw','subw','sllw','srlw','sraw','mulw','divw','divuw','remw','remuw']
def signed(x,n):return (x&((1<<n)-1))-(1<<n) if x&(1<<(n-1)) else x&((1<<n)-1)
def reference(op,x,y):
    n=32 if op.endswith('w') else 64
    xx=x&((1<<n)-1);yy=y&((1<<n)-1);sx=signed(xx,n);sy=signed(yy,n)
    base=op[:-1] if n==32 else op
    if base=='add':v=xx+yy
    elif base=='sub':v=xx-yy
    elif base=='sll':v=xx<<(yy&(n-1))
    elif base=='srl':v=xx>>(yy&(n-1))
    elif base=='sra':v=sx>>(yy&(n-1))
    elif base=='slt':v=int(sx<sy)
    elif base=='sltu':v=int(xx<yy)
    elif base=='xor':v=xx^yy
    elif base=='or':v=xx|yy
    elif base=='and':v=xx&yy
    elif base=='mul':v=xx*yy
    elif base=='mulh':v=(sx*sy)>>64
    elif base=='mulhsu':v=(sx*yy)>>64
    elif base=='mulhu':v=(xx*yy)>>64
    else:
        unsigned=base.endswith('u');ax=xx if unsigned else sx;by=yy if unsigned else sy
        q=((1<<n)-1) if by==0 else abs(ax)//abs(by)*(1 if (ax<0)==(by<0) else -1)
        v=q if base.startswith('div') else (ax if by==0 else ax-q*by)
    return (signed(v,32) if n==32 else v)&mask
rng=random.Random(64064)
values=[0,1,mask,1<<63,(1<<63)-1,1<<32,0xffffffff,0x80000000,0x7fffffff,31,32,63,64,127]
pairs=[(x,y) for x in values for y in [0,1,mask]]+[(rng.getrandbits(64),rng.getrandbits(64)) for _ in range(16)]
rows=[f'{{{op},0x{x:016x}ULL,0x{y:016x}ULL,0x{reference(name,x,y):016x}ULL}}' for x,y in pairs for op,name in enumerate(ops)]
switch='\n'.join(f'case {i}: __asm__ volatile("{op} %0,%1,%2":"=r"(v):"r"(x),"r"(y));break;' for i,op in enumerate(ops))
source='''#include <stdint.h>
#include "uart.h"
struct test {uint32_t op;uint64_t x,y,want;};
static const struct test tests[]={ROWS};
static uint64_t execute(uint32_t op,uint64_t x,uint64_t y) {uint64_t v=0;switch(op){SWITCH}return v;}
static volatile uint64_t wide;
int main(void){
 unsigned checks=0;
 for(unsigned i=0;i<sizeof(tests)/sizeof(tests[0]);i++){
  const struct test *t=&tests[i];if(execute(t->op,t->x,t->y)!=t->want){uart_puts("RV64 FAIL ");uart_put_uint(i);uart_putc('\\n');return 1;}checks++;
 }
 wide=0xfedcba9876543210ULL;if(wide!=0xfedcba9876543210ULL)return 2;checks++;
 uint64_t lw,lwu;uint32_t low=0x87654321;
 __asm__ volatile("lw %0,0(%2);lwu %1,0(%2)":"=&r"(lw),"=&r"(lwu):"r"(&low):"memory");
 if(lw!=0xffffffff87654321ULL || lwu!=0x87654321ULL)return 3;checks++;
 uint64_t iw,sw;
 __asm__ volatile("addiw %0,%2,1;sraiw %1,%2,31":"=&r"(iw),"=&r"(sw):"r"(0x123456787fffffffULL));
 if(iw!=0xffffffff80000000ULL || sw!=0)return 4;checks++;
 __asm__ volatile("slli %0,%1,63":"=r"(iw):"r"(1ULL));if(iw!=0x8000000000000000ULL)return 5;checks++;
 uart_puts("RV64 HOLDS ");uart_put_uint(checks);uart_putc('\\n');return 0;
}
'''.replace('ROWS',',\n'.join(rows)).replace('SWITCH',switch)
(out/'stress.c').write_text(source)
subprocess.run(['riscv64-unknown-elf-gcc','-march=rv64im_zicsr_zifencei','-mabi=lp64','-mcmodel=medany','-Os',
 '-ffreestanding','-fno-builtin','-nostdlib','-nostartfiles','-Wl,--no-warn-rwx-segments',
 '-T',str(here/'link.ld'),'-I',str(here.parent/'c'),*([str(here/'traps.S')] if a.traps else [str(here/'crt0.S'),str(here.parent/'c/uart.c'),str(out/'stress.c')]),'-o',str(out/'stress.elf')],check=True)
subprocess.run(['riscv64-unknown-elf-objcopy','-O','binary',str(out/'stress.elf'),str(out/'stress.bin')],check=True)
expected='RV64 TRAPS HOLD\nDONE' if a.traps else f'RV64 HOLDS {len(rows)+4}\nDONE'
if a.simulate:
    image=(out/'stress.bin').read_bytes();image+=b'\0'*((-len(image))%4)
    (out/'image.hex').write_text('\n'.join(f'{int.from_bytes(image[i:i+4],"little"):08x}' for i in range(0,len(image),4)))
    subprocess.run(['iverilog','-g2012','-s','rv64_tb','-o',str(out/'sim'),str(here/'core.v'),str(here.parent/'sdram/cache.v'),str(here/'testbench.v')],check=True)
    r=subprocess.run(['vvp',str(out/'sim'),f'+IMAGE={out}/image.hex'],capture_output=True,text=True,timeout=120)
else:
    r=subprocess.run(['python3',str(here.parent/'run_program.py'),str(out/'stress.bin'),'--port',a.port,'--freq-mhz',str(a.freq_mhz),'--ram-base','0x80000000','--timeout','120'],capture_output=True,text=True,timeout=135)
assert r.returncode==0,(r.stdout,r.stderr)
assert expected in r.stdout,(expected,r.stdout,r.stderr)
print('PASS: RV64 precise traps, full-width CSRs, mret and fault recovery' if a.traps else f'PASS: {len(rows)+4} independent RV64I/M/W/load/store reference checks')
