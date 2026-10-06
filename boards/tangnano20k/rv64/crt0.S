.section .text.entry,"ax",@progbits
.globl _start
_start:
.option push
.option norelax
la gp,__global_pointer$
.option pop
la sp,_stack_top
la t0,_bss_start
la t1,_bss_end
1: bgeu t0,t1,2f
sd zero,0(t0)
addi t0,t0,8
j 1b
2: call main
li t0,0x100000
li t1,0x5555
beqz a0,3f
li t1,0x3333
3: sw t1,0(t0)
4: j 4b
