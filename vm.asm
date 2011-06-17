[bits 64]
  mov eax, [eax]
  mov edx, [eax]
  mov ecx, [eax]
  mov edi, [eax]
  mov eax, [ebx]
  mov eax, [ecx]
  mov eax, [edi]
  mov rax, 0x123
  mov rdx, [rax]
  mov rcx, [rax]
  mov rdi, 0x12
  mov rax, [rbx]
  mov rax, [rcx]
  mov rax, [rdi]
loop:
  mov edx, [ebx]
  mul eax
  sub ecx, 0x1
  cmp ecx, 0
  jne loop
  ret
