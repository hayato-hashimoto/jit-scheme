 	mov    c, %rax
 	add    0x8(%rsp),%rax
 	mov    %rax,-0x8(%rsp)
 	movabs $0x30d40,%rax
 	mov    %rax,-0x10(%rsp)
 	movabs $0x1,%rax
 	mov    %rax,-0x18(%rsp)
 	add    $0xffffffffffffffe8,%rsp
 	callq  b
 	add    $0x18,%rsp
 	retq   

c:
  	movabs $0x0,%rax
  	cmp    0x10(%rsp),%rax
  	jne    a
  	mov    0x8(%rsp),%rax
  	retq   
a:
	mov    0x18(%rsp),%rax
	mov    %rax,-0x8(%rsp)
	mov    0x18(%rsp),%rax
	mov    %rax,-0x10(%rsp)
	movabs $0xffffffffffffffff,%rax
	add    0x10(%rsp),%rax
	mov    %rax,-0x18(%rsp)
	mov    0x8(%rsp),%rax
	add    0x10(%rsp),%rax
	mov    %rax,-0x20(%rsp)
	mov    -0x8(%rsp),%rbx
	add    $0xffffffffffffffe0,%rsp
	mov    (%rsp),%rdi
	mov    0x8(%rsp),%rsi
	callq  *%rbx
	add    $0x20,%rsp
	retq   

b:
     	mov    0x18(%rsp),%rax
     	mov    %rax,-0x8(%rsp)
     	mov    0x18(%rsp),%rax
     	mov    %rax,-0x10(%rsp)
     	mov    0x10(%rsp),%rax
     	mov    %rax,-0x18(%rsp)
     	mov    0x8(%rsp),%rax
     	mov    %rax,-0x20(%rsp)
     	mov    -0x8(%rsp),%rbx
     	add    $0xffffffffffffffe0,%rsp
     	mov    (%rsp),%rdi
     	mov    0x8(%rsp),%rsi
     	callq  *%rbx
     	add    $0x20,%rsp
     	retq   
