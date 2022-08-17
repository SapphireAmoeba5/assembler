.entry _start

factorial:
    cmp x0, 1
    ja _factorial_continue
    mov x0, 1
    ret

_factorial_continue:
    push x0
    sub x0, 1
    call factorial
    pop x1
    mul x0, x1
    ret

_start:
    mov x0, 5
    call factorial
