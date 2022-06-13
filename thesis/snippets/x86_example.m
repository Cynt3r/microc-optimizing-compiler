section .text
global _start
extern exit

_start:
    mov rax, 10     ; store number 10
loop:
    dec rax         ; decrement
    cmp rax, 0      ; compare with number 0
    jne loop        ; jump if not equal
done:
    call exit