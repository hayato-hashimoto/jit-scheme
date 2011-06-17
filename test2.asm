[bits 64]
mov rax, 0x1234
push rax
push rcx
push rdx
push rbx
push rsp
mov  rax, [rsp-128]
mov  rax, [rsp-0x12]
add rsp, 8
ret
