# Simulation stand-in for the boot ROM (address 0): no UART loader.
# Power-on (BOOTCAUSE 1) jumps to the preloaded image at 0x8000_0000. Any
# other entry is the end of the run: the finisher (BOOTCAUSE 3) stores
# 0x600d0003 to the last RAM word, anything else (a trap with mtvec 0, cause
# 0) stores 0xbad0000N. The testbench watches that word.
        .text
        .globl _start
_start:
        lui   t0, 0x100            # 0x0010_0000: finisher; +4 BOOTCAUSE
        lw    t1, 4(t0)
        li    t2, 1
        bne   t1, t2, finished
        lui   t0, 0x80000
        jr    t0
finished:
        li    t2, 3
        lui   t3, 0xbad00
        bne   t1, t2, 1f
        lui   t3, 0x600d0
1:      or    t3, t3, t1
        lui   t0, %hi(RAM_END)
        sw    t3, -4(t0)
2:      j     2b
