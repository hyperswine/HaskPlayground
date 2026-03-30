; branches.asm
; Exercises JZ (jump-if-zero) and JNZ (jump-if-nonzero) with the branches
; actually taken, skipping over code that must not execute.
;
; Expected output: 1 2

LOAD r0, 0     ; [pc 0] r0 = 0  (zero  → JZ will be taken)
JZ   r0, 4     ; [pc 1] TAKEN: jump to pc 4, skipping pcs 2-3
LOAD r1, 99    ; [pc 2] SKIPPED — must not reach here
HALT           ; [pc 3] SKIPPED

LOAD r1, 1     ; [pc 4] landing pad: we jumped here
OUT  r1        ; [pc 5] output 1

LOAD r0, 5     ; [pc 6] r0 = 5  (nonzero → JNZ will be taken)
JNZ  r0, 10    ; [pc 7] TAKEN: jump to pc 10, skipping pcs 8-9
LOAD r1, 99    ; [pc 8] SKIPPED — must not reach here
HALT           ; [pc 9] SKIPPED

LOAD r1, 2     ; [pc 10] landing pad: we jumped here
OUT  r1        ; [pc 11] output 2
HALT           ; [pc 12]
