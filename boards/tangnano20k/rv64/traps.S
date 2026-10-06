.section .text.entry,"ax",@progbits
.globl _start
_start:
.option push
.option norelax
la gp,__global_pointer$
.option pop
la sp,_stack_top
la t0,handler
csrw mtvec,t0
li s0,0
li s1,0
li s2,0
// A misaligned JALR must leave rd unchanged and trap against its source PC.
li t1,99
la t0,after_jal
addi t0,t0,2
jalr t1,t0,0
after_jal:
li t2,99
bne t1,t2,fail
li t0,1
ld zero,0(t0)
sd zero,0(t0)
li t0,0x80800000
ld zero,0(t0)
sd zero,0(t0)
.word 0xffffffff
ecall
ebreak
li t0,8
bne s0,t0,fail
li t0,38
bne s1,t0,fail
li t0,0
bne s2,t0,fail
// Full-width scratch CSR and counter reads.
li t0,0xfedcba9876543210
csrw mscratch,t0
csrr t1,mscratch
bne t0,t1,fail
li t0,0x8000000000001100
csrr t1,misa
bne t0,t1,fail
la a0,ok
call puts
li a0,0
j exit
fail:
la a0,bad
call puts
li a0,1
exit:
li t0,0x100000
li t1,0x5555
beqz a0,1f
li t1,0x3333
1:sw t1,0(t0)
2:j 2b
handler:
csrrw t0,mscratch,t0
csrr t0,mstatus
andi t0,t0,8
or s2,s2,t0
csrr t0,mcause
addi s0,s0,1
add s1,s1,t0
csrr t0,mepc
addi t0,t0,4
csrw mepc,t0
csrrw t0,mscratch,t0
mret
puts:
li t0,0x10000000
3:lbu t1,0(a0)
beqz t1,5f
4:lw t2,4(t0)
andi t2,t2,1
beqz t2,4b
sw t1,0(t0)
addi a0,a0,1
j 3b
5:ret
.section .rodata
ok:.asciz "RV64 TRAPS HOLD\n"
bad:.asciz "RV64 TRAPS FAIL\n"
