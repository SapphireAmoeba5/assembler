.entry _start

    ;; parameters
    ;; x0 : base (unsigned)
    ;; x1 : exponent (unsigned)
pow:
    push x2
    push x1

    mov x2, 1
_pow_loop:
    mul x2, x0
    sub x1, 1
    jnz _pow_loop

    mov x0, x2

    pop x1
    pop x2

    ret


_start:
    mov x0, 5
    mov x1, 5
    call pow
