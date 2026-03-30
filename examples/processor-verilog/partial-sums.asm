; partial-sums.asm
; Computes the running sum 1+2+3+4+5, outputting each partial total.
; Demonstrates chained ADD with intermediate OUT instructions.
;
; Expected output: 1 3 6 10 15  (triangular numbers T1..T5)

LOAD r0, 0     ; r0 = accumulator
ADD  r0, r0, 1 ; r0 = 1
OUT  r0        ; output 1
ADD  r0, r0, 2 ; r0 = 3
OUT  r0        ; output 3
ADD  r0, r0, 3 ; r0 = 6
OUT  r0        ; output 6
ADD  r0, r0, 4 ; r0 = 10
OUT  r0        ; output 10
ADD  r0, r0, 5 ; r0 = 15
OUT  r0        ; output 15
HALT
