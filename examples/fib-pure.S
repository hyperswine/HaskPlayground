# Fibonacci Calculator for RISC-V64
# Computes fibonacci numbers and prints them
# For use with spike + pk (RISC-V Proxy Kernel)
# Linux ABI syscalls

.section .rodata
prompt_msg:
.string "Enter n for fib(n): "
result_prefix:
.string "fib("
result_middle:
.string ") = "
newline:
.string "\n"

.section .bss
.align 8
input_buffer:
.space 32

.section .text
.global _start

_start:
    # Print prompt
    li a0, 1  # stdout
    la a1, prompt_msg  # message
    li a2, 20  # length
    li a7, 64  # sys_write
    ecall

    # Read input from stdin
    li a0, 0  # stdin
    la a1, input_buffer  # buffer
    li a2, 32  # max bytes
    li a7, 63  # sys_read
    ecall

    # Parse the input number (simple atoi)
    la a0, input_buffer
    jal ra, parse_number
    mv s0, a0  # Save n in s0

    # Compute fibonacci
    mv a0, s0
    jal ra, fibonacci
    mv s1, a0  # Save result in s1

    # Print "fib("
    li a0, 1
    la a1, result_prefix
    li a2, 4
    li a7, 64
    ecall

    # Print n
    mv a0, s0
    jal ra, print_number

    # Print ") = "
    li a0, 1
    la a1, result_middle
    li a2, 4
    li a7, 64
    ecall

    # Print result
    mv a0, s1
    jal ra, print_number

    # Print newline
    li a0, 1
    la a1, newline
    li a2, 1
    li a7, 64
    ecall

    # Exit
    li a0, 0
    li a7, 93  # sys_exit
    ecall


# fibonacci: Compute nth Fibonacci number
# Input: a0 = n
# Output: a0 = fib(n)
# Uses iterative algorithm to avoid stack overflow
fibonacci:
    # Handle base cases: fib(0) = 0, fib(1) = 1
    sltiu t0, a0, 2
    bne t0, zero, fib_return

    # Iterative fibonacci
    # t0 = fib(i-2), initially 0
    # t1 = fib(i-1), initially 1
    # t2 = counter (i)
    # a0 = n

    li t0, 0  # fib(0) = 0
    li t1, 1  # fib(1) = 1
    li t2, 2  # counter starts at 2

fib_loop:
    # Check if we're done
    bge t2, a0, fib_done

    # Compute next fibonacci number
    add t3, t0, t1  # fib(i) = fib(i-1) + fib(i-2)

    # Shift values
    mv t0, t1  # fib(i-2) = old fib(i-1)
    mv t1, t3  # fib(i-1) = new fib(i)

    # Increment counter
    addi t2, t2, 1

    j fib_loop

fib_done:
    mv a0, t1  # Return fib(n)

fib_return:
    ret


# parse_number: Convert ASCII string to integer
# Input: a0 = pointer to string
# Output: a0 = parsed number
# Clobbers: t0, t1, t2, t3
parse_number:
    mv t0, a0  # Current position
    li t1, 0  # Accumulated result

parse_loop:
    lbu t2, 0(t0)  # Load character

    # Check for newline or null terminator
    sltiu t3, t2, 1
    bne t3, zero, parse_done

    addi t3, t2, -10  # Check for '\n'
    sltiu t3, t3, 1
    bne t3, zero, parse_done

    # Check if digit (0-9)
    addi t3, t2, -48  # '0' = 48
    sltiu t4, t3, 10
    beq t4, zero, parse_done  # Not a digit, done

    # result = result * 10 + digit
    t1 = multiply t1 10
    add t1, t1, t3

    # Next character
    addi t0, t0, 1
    j parse_loop

parse_done:
    mv a0, t1
    ret


# print_number: Convert integer to ASCII and print
# Input: a0 = number to print
# Clobbers: t0-t6, a1, a2, a7
print_number:
    # Save registers
    addi sp, sp, -32
    sd ra, 0(sp)
    sd s0, 8(sp)
    sd s1, 16(sp)

    mv s0, a0  # Save number

    # Handle zero specially
    bne s0, zero, print_nonzero

    # Print '0'
    li t0, 48  # '0'
    sb t0, 24(sp)
    li a0, 1  # stdout
    addi sp, sp, 24
    mv a1, sp  # pointer to '0'
    addi sp, sp, -24
    li a2, 1  # length
    li a7, 64  # sys_write
    ecall
    j print_done

print_nonzero:
    # Build digits in reverse on stack
    li s1, 0  # Digit counter
    mv t0, s0  # Working copy

print_digit_loop:
    # Check if done
    beq t0, zero, print_reverse

    # Get digit: digit = num % 10
    li t4, 10  # Divisor
    remu t1, t0, t4

    # Convert to ASCII
    addi t1, t1, 48  # '0' = 48

    # Store on stack
    addi t2, sp, 24
    add t2, t2, s1
    sb t1, 0(t2)

    # Next digit
    divu t0, t0, t4
    addi s1, s1, 1

    j print_digit_loop

print_reverse:
    # Now reverse and print the string
    li t0, 0  # Source index (from end)
    addi t1, sp, 24
    add t1, t1, s1
    addi t1, t1, -1  # Point to last digit

    addi t2, sp, 24  # Destination (start of buffer)

reverse_loop:
    beq t0, s1, reverse_done

    # Copy byte
    lbu t3, 0(t1)
    sb t3, 0(t2)

    # Move pointers
    addi t1, t1, -1  # Source moves backward
    addi t2, t2, 1  # Dest moves forward
    addi t0, t0, 1  # Counter

    j reverse_loop

reverse_done:
    # Print the buffer
    li a0, 1  # stdout
    addi a1, sp, 24  # Buffer
    mv a2, s1  # Length
    li a7, 64  # sys_write
    ecall

print_done:
    # Restore registers
    ld ra, 0(sp)
    ld s0, 8(sp)
    ld s1, 16(sp)
    addi sp, sp, 32

    ret
