[bits 64]
add rdx, 1
call [edx]
mov rax, rdx
jmp a
nop
nop
nop
nop
a: mov  rax, 0x123456789a
push rax
jmp b
nop
nop
nop
nop
b: jmp  0x123456789abc
