// RVMODEL_* macros for SimpleRisc (riscv-arch-test 3.x, RISCOF).
// Boot: the image is linked and loaded at 0x8000_0000, where the boot ROM
// (or the simulation stub ROM) jumps. Halt: the QEMU-virt finisher at
// 0x0010_0000 resets the CPU into the ROM with BOOTCAUSE 3. The signature is
// read back from RAM between begin_signature and end_signature.
#ifndef _COMPLIANCE_MODEL_H
#define _COMPLIANCE_MODEL_H

#define RVMODEL_DATA_SECTION

#define RVMODEL_HALT                                                    \
  li t0, 0x00100000;                                                    \
  li t1, 0x5555;                                                        \
  sw t1, 0(t0);                                                         \
1: j 1b;

#define RVMODEL_BOOT

#define RVMODEL_DATA_BEGIN                                              \
  .align 4; .global begin_signature; begin_signature:

#define RVMODEL_DATA_END                                                \
  .align 4; .global end_signature; end_signature:

#define RVMODEL_IO_INIT
#define RVMODEL_IO_WRITE_STR(_R, _STR)
#define RVMODEL_IO_CHECK()
#define RVMODEL_IO_ASSERT_GPR_EQ(_S, _R, _I)
#define RVMODEL_IO_ASSERT_SFPR_EQ(_F, _R, _I)
#define RVMODEL_IO_ASSERT_DFPR_EQ(_D, _R, _I)

// CLINT (QEMU virt layout): msip at +0x0, mtimecmp at +0x4000.
#define RVMODEL_SET_MSW_INT                                             \
  li t1, 1;                                                             \
  li t2, 0x02000000;                                                    \
  sw t1, 0(t2);

#define RVMODEL_CLEAR_MSW_INT                                           \
  li t2, 0x02000000;                                                    \
  sw x0, 0(t2);

#define RVMODEL_CLR_MSW_INT RVMODEL_CLEAR_MSW_INT

#define RVMODEL_CLEAR_MTIMER_INT                                        \
  li t2, 0x02004000;                                                    \
  li t1, -1;                                                            \
  sw t1, 4(t2);                                                         \
  sw t1, 0(t2);

#define RVMODEL_CLR_MTIMER_INT RVMODEL_CLEAR_MTIMER_INT

#define RVMODEL_CLEAR_MEXT_INT
#define RVMODEL_CLR_MEXT_INT

#endif // _COMPLIANCE_MODEL_H
