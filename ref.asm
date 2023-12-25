section .text
	global _start
	
                extern ExitProcess
_start:
	
                
	mov rax, 10
	push rax
	
	mov rax, 2
	push rax
	
	pop rax
	
	pop rbx
	
	xor rdx, rdx
	div rbx
	
	push rax
	
	push qword [rsp + 0]

	
	pop rdi
	
	mov rcx, rdi
	sub rsp, 32
	call ExitProcess

	mov rcx, 0
	sub rsp, 32
	call ExitProcess
