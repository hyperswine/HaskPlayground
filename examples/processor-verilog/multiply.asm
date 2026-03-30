; multiply.asm
; Computes 6 x 7 = 42 by looping 7 times and adding 6 each iteration.
; Demonstrates accumulation via a counted loop.
;
; Expected output: 42

LOAD r0, 0     ; r0 = result (accumulator)
LOAD r1, 7     ; r1 = loop counter
ADD  r0, r0, 6 ; [pc 2] result += 6
SUB  r1, r1, 1 ; [pc 3] counter--
JNZ  r1, 2     ; [pc 4] loop back to pc 2 while counter != 0
OUT  r0        ;        output 42
HALT
