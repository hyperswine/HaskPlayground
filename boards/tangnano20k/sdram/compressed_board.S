// RV32IMC physical-board fixture: integer compressed forms, mixed fetch,
// pc+2/pc+4 links, precise raw mtval, IALIGN16, trap recovery and FENCE.I.
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
li s7,1
// Explicit compressed register/immediate and stack load/store references.
c.addi16sp sp,-32
c.addi4spn s0,sp,16
addi t0,sp,16
bne s0,t0,fail
c.li s0,-17
c.swsp s0,0(sp)
c.lwsp s1,0(sp)
bne s0,s1,fail
c.mv s2,s0
c.add s2,s1
li t0,-34
bne s2,t0,fail
c.li s0,7
c.slli s0,3
c.srli s0,2
c.srai s0,1
li t0,7
bne s0,t0,fail
c.andi s0,-2
c.li s1,3
c.sub s0,s1
c.xor s0,s1
c.or s0,s1
c.and s0,s1
li t0,3
bne s0,t0,fail
c.lui s0,1
c.addi s0,-1
addi a0,sp,16
c.sw s0,0(a0)
c.lw a1,0(a0)
li t0,4095
bne a1,t0,fail
c.li s0,0
c.beqz s0,1f
j fail
1:c.li s0,1
c.bnez s0,2f
j fail
2:
li s7,2
// RV32-only C.JAL must write pc+2, including from an upper halfword.
.balign 4
c.nop
c.jal subroutine
jal_linked:
la t2,jal_linked
bne t1,t2,fail
// A full-width JALR at an upper halfword must write pc+4.
la t0,subroutine
.balign 4
c.nop
.option push
.option norvc
jalr ra,0(t0)
.option pop
full_linked:
la t2,full_linked
bne t1,t2,fail
li s0,0
li s1,0
li t0,0x40001104
csrr t1,misa
bne t0,t1,fail
li t0,0x80000002
csrw mepc,t0
csrr t1,mepc
bne t0,t1,fail
// A compressed JALR writes pc+2 even at the upper half of a fetch word.
la t0,subroutine
.balign 4
c.nop
c.jalr t0
linked:
la t2,linked
bne t1,t2,fail
j faults
.balign 4
c.nop
subroutine:
mv t1,ra
c.jr ra
faults:
li s7,3
li s6,0
la s5,bad_zero
bad_zero:.2byte 0x0000
li s6,0x4002
la s5,bad_lwsp
bad_lwsp:.2byte 0x4002
la s5,compressed_break
mv s6,s5
compressed_break:c.ebreak
// A full-width ECALL begins at an upper halfword and must skip four bytes.
la s5,full_ecall
li s6,0
.balign 4
c.nop
.option push
.option norvc
full_ecall:ecall
.option pop
li s7,4
// Install a different compressed instruction, then fence/refetch it.
la t0,modified
li t1,0x9002
sh t1,0(t0)
fence.i
mv s5,t0
mv s6,t0
c.j modified
.balign 4
c.nop
modified:.2byte 0x0000
li s7,5
// The second half of a full-width instruction must fault at the end of RAM.
li t0,0x807ffffe
li t1,0x0013
sh t1,0(t0)
fence.i
la s4,after_cross
mv s5,t0
li s6,0x80800000
jalr zero,t0,0
after_cross:
li t0,6
bne s0,t0,fail
li t0,22 // 2+2+3+11+3+1
bne s1,t0,fail
la a0,ok
li a1,0
j exit
fail:
la a0,bad
li a1,1
exit:
call puts
li t0,0x100000
li t1,0x5555
beqz a1,1f
li t1,0x3333
1:sw t1,0(t0)
2:j 2b
handler:
csrrw t0,mscratch,t0
csrr t0,mepc
bne t0,s5,fail
csrr t0,mtval
bne t0,s6,fail
csrr t0,mcause
addi s0,s0,1
add s1,s1,t0
li t0,1
csrr t1,mcause
beq t0,t1,cross_handler
csrr t0,mepc
lhu t1,0(t0)
andi t1,t1,3
li t2,3
beq t1,t2,full_handler
addi t0,t0,2
j resume
full_handler:
addi t0,t0,4
j resume
cross_handler:
mv t0,s4
resume:
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
ok:.asciz "RV32C BOARD HOLDS\n"
bad:.asciz "RV32C BOARD FAIL\n"
