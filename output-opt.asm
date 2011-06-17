[bits 64]
 mov rax, c

 add rax,[rsp+0x8]
 mov [rsp-0x8],rax
 mov rax,0x30d40

 mov [rsp-0x10],rax
 mov rax,0x1

 mov [rsp-0x18],rax
 add rsp,byte -0x18
 call b
 add rsp,byte +0x18
 ret

c: mov rax,0x0

 cmp rax,[rsp+0x10]
 jnz a
 mov rax,[rsp+0x8]
 ret
a: mov rax,[rsp+0x18]
 mov [rsp-0x8],rax
 mov rax,[rsp+0x18]
 mov [rsp-0x10],rax
 mov rax,0xffffffffffffffff

 add rax,[rsp+0x10]
 mov [rsp-0x18],rax
 mov rax,[rsp+0x8]
 add rax,[rsp+0x10]
 mov [rsp-0x20],rax
 mov rbx,[rsp-0x8]
 add rsp,byte -0x20
 mov rdi,[rsp]
 mov rsi,[rsp+0x8]
 call rbx
 add rsp,byte +0x20
 ret

 b: mov rax,[rsp+0x18]
 mov [rsp-0x8],rax
 mov rax,[rsp+0x18]
 mov [rsp-0x10],rax
 mov rax,[rsp+0x10]
 mov [rsp-0x18],rax
 mov rax,[rsp+0x8]
 mov [rsp-0x20],rax
 mov rbx,[rsp-0x8]
 add rsp,byte -0x20
 mov rdi,[rsp]
 mov rsi,[rsp+0x8]
 call rbx
 add rsp,byte +0x20
 ret
