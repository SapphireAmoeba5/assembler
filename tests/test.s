.entry _start


_start:
    mov x0, _start

    lidt _idt

    lea x1, _idt

    lea x0, divide_by_zero
    str x0, [(x1) * 1]

    add x1, 8

    lea x0, invalid_instruction
    str x0, [(x1) * 1]

    add x1, 1016
    lea x0, syscall_interrupt
    str x0, [(x1) * 1]

    int 0x80

    mov x0, 1+2+3+4
    mov x4, 0xdeadbeef
    hlt

divide_by_zero:
    mov x0, 69420
    reti

invalid_instruction:
    mov x0, 0xdeadbeef
    reti

syscall_interrupt:
    mov x0, 0xc0ffee
    cmp x0, 0xc0ffee
    reti

_idt:
