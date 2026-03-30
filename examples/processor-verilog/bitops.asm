; bitops.asm
; Nibble masking and bit-combining using AND and OR.
; r0 = 0xAB (171): split into nibbles, recombine, invert low nibble.
;
; Expected output:
;   11   (0x0B  low nibble of 0xAB)
;  160   (0xA0  high nibble — upper bits masked, lower cleared)
;  171   (0xAB  original value)
;  251   (0xFB  low nibble OR'd with 0xF0)

LOAD r0, 171     ; r0 = 0xAB = 171
AND  r1, r0, 15  ; r1 = 0xAB & 0x0F = 0x0B = 11
AND  r2, r0, 240 ; r2 = 0xAB & 0xF0 = 0xA0 = 160
OR   r3, r1, 240 ; r3 = 0x0B | 0xF0 = 0xFB = 251
OUT  r1          ; 11
OUT  r2          ; 160
OUT  r0          ; 171
OUT  r3          ; 251
HALT
