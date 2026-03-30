; countdown.asm
; Counts from 10 down to 1, outputting each value.
; Demonstrates a JNZ loop.
;
; Expected output: 10 9 8 7 6 5 4 3 2 1

LOAD r0, 10    ; r0 = counter
OUT  r0        ; [pc 1] output current count
SUB  r0, r0, 1 ; [pc 2] decrement
JNZ  r0, 1     ; [pc 3] loop back to pc 1 while nonzero
HALT
