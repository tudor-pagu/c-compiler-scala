.section .note.GNU-stack,"",@progbits

.section .rodata
output: .asciz "Hello world\n"

.text
.globl main

main:
    pushq %rbp
    movq %rsp, %rbp

    leaq output(%rip), %rdi
    call printf

    movq %rbp, %rsp
    pop %rbp

    movl $0, %eax
    ret
