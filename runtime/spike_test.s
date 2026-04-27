# FP-RISC generated RISC-V assembly (RV64GC)
# Every value is a heap pointer in a0..a7.
# Primitives unbox, compute, rebox.

    .option nopic
    .text


    .globl calc
    .type  calc, @function
calc:
    # prologue: frame=16 bytes
    addi  sp, sp, -16
    sd    ra, 8(sp)
    sd    s0, 0(sp)
    addi  s0, sp, 16
    la    a0, calc_Dlam0  # global calc$lam0
    # epilogue
    ld    ra, 8(sp)
    ld    s0, 0(sp)
    addi  sp, sp, 16
    ret
    .size calc, .-calc

    .globl safediv
    .type  safediv, @function
safediv:
    # prologue: frame=16 bytes
    addi  sp, sp, -16
    sd    ra, 8(sp)
    sd    s0, 0(sp)
    addi  s0, sp, 16
    la    a0, safediv_Dlam2  # global safediv$lam2
    # epilogue
    ld    ra, 8(sp)
    ld    s0, 0(sp)
    addi  sp, sp, 16
    ret
    .size safediv, .-safediv

    .globl __eval_0
    .type  __eval_0, @function
__eval_0:
    # prologue: frame=32 bytes
    addi  sp, sp, -32
    sd    ra, 24(sp)
    sd    s0, 16(sp)
    addi  s0, sp, 32
    li    a0, 5
    call  rt_box_int
    call  calc_Dlam0
    # result in a0
    sd    a0, -8(s0)  # $anf0
    ld    a0, -8(s0)  # $anf0
    # epilogue
    ld    ra, 24(sp)
    ld    s0, 16(sp)
    addi  sp, sp, 32
    ret
    .size __eval_0, .-__eval_0

    .globl __eval_1
    .type  __eval_1, @function
__eval_1:
    # prologue: frame=32 bytes
    addi  sp, sp, -32
    sd    ra, 24(sp)
    sd    s0, 16(sp)
    addi  s0, sp, 32
    # box int 4 into a1
    addi  sp, sp, -16
    sd    a0, 0(sp)          # save a0
    li    a0, 4
    call  rt_box_int
    mv    a1, a0
    ld    a0, 0(sp)          # restore a0
    addi  sp, sp, 16
    li    a0, 3
    call  rt_box_int
    call  rt_prim_add_int
    # result in a0
    sd    a0, -8(s0)  # $anf2
    ld    a0, -8(s0)  # $anf2
    # epilogue
    ld    ra, 24(sp)
    ld    s0, 16(sp)
    addi  sp, sp, 32
    ret
    .size __eval_1, .-__eval_1

    .globl __eval_2
    .type  __eval_2, @function
__eval_2:
    # prologue: frame=32 bytes
    addi  sp, sp, -32
    sd    ra, 24(sp)
    sd    s0, 16(sp)
    addi  s0, sp, 32
    li    a0, 10
    call  rt_box_int
    call  safediv_Dlam2
    # result in a0
    sd    a0, -8(s0)  # $anf3
    ld    t0, -8(s0)  # $anf3
    li    a0, 3
    call  rt_box_int
    # indirect call
    lw    t1, 0(t0)          # load object tag
    li    t2, 5              # TAG_CLOSURE
    beq   t1, t2, _Licall0_clos
    jalr  ra, t0, 0          # plain function pointer
    j     _Licall0_done
_Licall0_clos:
    ld    t1, 16(t0)         # fn_ptr (_centry) from closure header
    jalr  ra, t1, 0          # t0=closure, a0=new_arg
_Licall0_done:
    # result in a0
    sd    a0, -16(s0)  # $anf4
    ld    a0, -16(s0)  # $anf4
    # epilogue
    ld    ra, 24(sp)
    ld    s0, 16(sp)
    addi  sp, sp, 32
    ret
    .size __eval_2, .-__eval_2

    .globl __eval_3
    .type  __eval_3, @function
__eval_3:
    # prologue: frame=32 bytes
    addi  sp, sp, -32
    sd    ra, 24(sp)
    sd    s0, 16(sp)
    addi  s0, sp, 32
    li    a0, 7
    call  rt_box_int
    call  safediv_Dlam2
    # result in a0
    sd    a0, -8(s0)  # $anf5
    ld    t0, -8(s0)  # $anf5
    li    a0, 0
    call  rt_box_int
    # indirect call
    lw    t1, 0(t0)          # load object tag
    li    t2, 5              # TAG_CLOSURE
    beq   t1, t2, _Licall1_clos
    jalr  ra, t0, 0          # plain function pointer
    j     _Licall1_done
_Licall1_clos:
    ld    t1, 16(t0)         # fn_ptr (_centry) from closure header
    jalr  ra, t1, 0          # t0=closure, a0=new_arg
_Licall1_done:
    # result in a0
    sd    a0, -16(s0)  # $anf6
    ld    a0, -16(s0)  # $anf6
    # epilogue
    ld    ra, 24(sp)
    ld    s0, 16(sp)
    addi  sp, sp, 32
    ret
    .size __eval_3, .-__eval_3

    .globl calc_Dlam0
    .type  calc_Dlam0, @function
calc_Dlam0:
    # prologue: frame=64 bytes
    addi  sp, sp, -64
    sd    ra, 56(sp)
    sd    s0, 48(sp)
    addi  s0, sp, 64
    sd    a0, -8(s0)  # $a00
    # box int 1 into a1
    addi  sp, sp, -16
    sd    a0, 0(sp)          # save a0
    li    a0, 1
    call  rt_box_int
    mv    a1, a0
    ld    a0, 0(sp)          # restore a0
    addi  sp, sp, 16
    ld    a0, -8(s0)  # $a00
    call  rt_prim_add_int
    # result in a0
    sd    a0, -16(s0)  # $anf8
    ld    a0, -16(s0)  # $anf8
    sd    a0, -24(s0)  # a
    # box int 2 into a1
    addi  sp, sp, -16
    sd    a0, 0(sp)          # save a0
    li    a0, 2
    call  rt_box_int
    mv    a1, a0
    ld    a0, 0(sp)          # restore a0
    addi  sp, sp, 16
    ld    a0, -24(s0)  # a
    call  rt_prim_mul_int
    # result in a0
    sd    a0, -32(s0)  # $anf10
    ld    a0, -32(s0)  # $anf10
    sd    a0, -40(s0)  # b
    # box int 1 into a1
    addi  sp, sp, -16
    sd    a0, 0(sp)          # save a0
    li    a0, 1
    call  rt_box_int
    mv    a1, a0
    ld    a0, 0(sp)          # restore a0
    addi  sp, sp, 16
    ld    a0, -40(s0)  # b
    call  rt_prim_sub_int
    # result in a0
    sd    a0, -48(s0)  # $anf12
    ld    a0, -48(s0)  # $anf12
    # epilogue
    ld    ra, 56(sp)
    ld    s0, 48(sp)
    addi  sp, sp, 64
    ret
    .size calc_Dlam0, .-calc_Dlam0

    .globl safediv_Dlam1
    .type  safediv_Dlam1, @function
safediv_Dlam1:
    # prologue: frame=64 bytes
    addi  sp, sp, -64
    sd    ra, 56(sp)
    sd    s0, 48(sp)
    addi  s0, sp, 64
    sd    a0, -8(s0)  # $a01
    sd    a1, -16(s0)  # $a12
    # box int 0 into a1
    addi  sp, sp, -16
    sd    a0, 0(sp)          # save a0
    li    a0, 0
    call  rt_box_int
    mv    a1, a0
    ld    a0, 0(sp)          # restore a0
    addi  sp, sp, 16
    ld    a0, -16(s0)  # $a12
    call  rt_prim_eq
    # result in a0
    sd    a0, -24(s0)  # $anf14
    ld    a0, -24(s0)  # $anf14
    sd    a0, -32(s0)  # $if3
    ld    t0, -32(s0)  # $if3
    lw    t1, 0(t0)   # load tag
    li    t2, 1
    bne   t1, t2, _Larm3
    li    a0, 0
    call  rt_box_int
    # -> a0
    j     _Lcend2
_Larm3:
    lw    t1, 0(t0)   # load tag
    li    t2, 0
    bne   t1, t2, _Larm4
    ld    a1, -16(s0)  # $a12
    ld    a0, -8(s0)  # $a01
    call  rt_prim_div_int
    # result in a0
    sd    a0, -40(s0)  # $anf16
    ld    a0, -40(s0)  # $anf16
    j     _Lcend2
_Larm4:
_Lcend2:
    # epilogue
    ld    ra, 56(sp)
    ld    s0, 48(sp)
    addi  sp, sp, 64
    ret
    .size safediv_Dlam1, .-safediv_Dlam1

    .globl safediv_Dlam1_centry
    .type  safediv_Dlam1_centry, @function
safediv_Dlam1_centry:
    mv    a1, a0
    ld    a0, 24(t0)
    j     safediv_Dlam1
    .size safediv_Dlam1_centry, .-safediv_Dlam1_centry

    .globl safediv_Dlam2
    .type  safediv_Dlam2, @function
safediv_Dlam2:
    # prologue: frame=32 bytes
    addi  sp, sp, -32
    sd    ra, 24(sp)
    sd    s0, 16(sp)
    addi  s0, sp, 32
    sd    a0, -8(s0)  # $a01
    ld    a2, -8(s0)  # $a01
    la    a0, safediv_Dlam1_centry
    li    a1, 1
    call  rt_make_closure
    # result in a0
    sd    a0, -16(s0)  # $anf17
    ld    a0, -16(s0)  # $anf17
    # epilogue
    ld    ra, 24(sp)
    ld    s0, 16(sp)
    addi  sp, sp, 32
    ret
    .size safediv_Dlam2, .-safediv_Dlam2

# ---- Runtime extern declarations ----
    .extern rt_alloc
    .extern rt_alloc_con
    .extern rt_alloc_tuple
    .extern rt_rc_inc
    .extern rt_rc_dec
    .extern rt_box_int
    .extern rt_box_float
    .extern rt_box_string_static
    .extern rt_prim_add_int
    .extern rt_prim_sub_int
    .extern rt_prim_mul_int
    .extern rt_prim_div_int
    .extern rt_prim_add_float
    .extern rt_prim_sub_float
    .extern rt_prim_mul_float
    .extern rt_prim_div_float
    .extern rt_prim_eq
    .extern rt_prim_neq
    .extern rt_prim_lt
    .extern rt_prim_gt
    .extern rt_prim_le
    .extern rt_prim_ge
    .extern rt_prim_and
    .extern rt_prim_or
    .extern rt_prim_str_concat
    .extern rt_prim_negate_int
    # List tier 1 (Cons cells)
    .extern rt_list_cons
    .extern rt_list_nil
    .extern rt_list_map
    .extern rt_list_filter
    .extern rt_list_fold
    .extern rt_list_foldl
    .extern rt_list_foldr
    .extern rt_list_head
    .extern rt_list_tail
    .extern rt_list_length
    .extern rt_list_append
    .extern rt_list_reverse
    .extern rt_list_zip
    .extern rt_list_zip_with
    .extern rt_list_any
    .extern rt_list_all
    .extern rt_list_concat
    # List tier 2 (SoA)
    .extern rt_list_soa_create
    .extern rt_list_soa_get
    .extern rt_list_soa_set
    .extern rt_list_soa_map
    .extern rt_list_soa_map_all
    .extern rt_list_soa_filter
    .extern rt_list_soa_fold
    .extern rt_list_soa_to_cons
    .extern rt_list_cons_to_soa
    # List tier 3 (streams / fusion)
    .extern rt_stream_from_list
    .extern rt_stream_map
    .extern rt_stream_filter
    .extern rt_stream_fold
    .extern rt_stream_zip_with
    .extern rt_stream_take
    .extern rt_stream_force
    # SIMD (RVV numeric SoA)
    .extern rt_simd_map_add_int
    .extern rt_simd_map_mul_int
    .extern rt_simd_map_add_float
    .extern rt_simd_map_mul_float
    .extern rt_simd_fold_sum_int
    .extern rt_simd_fold_sum_float
    .extern rt_simd_zip_add_int
    .extern rt_simd_zip_mul_float
    .extern rt_simd_dot_product
    # Closures
    .extern rt_make_closure
    .extern rt_apply_closure
    .extern rt_apply_closure_n
