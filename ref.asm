section .text
	global _start
	
            extern ExitProcess
_start:
	
            
	mov rax, 0
	push rax
	
label0_entry:
	mov rax, 10
	push rax
	
	push qword [rsp + 16]

	
	pop rax
	
	pop rdx
	
	cmp rax,rdx
	 mov rax,1
	 mov rdi,0
	 cmove rax,rdi
	push rax
	
	pop rax
	
	test rax,rax
	jz label0_exit
	pop rax
	
	push qword [rsp + 0]

	
	mov rax, 1
	push rax
	
	pop rdx
	
	mov rax ,qword [rsp+0]
	add rax,rdx
	mov qword [rsp + 0],rax

	add rsp, 0

	jmp label0_entry
label0_exit:
	push qword [rsp + 8]

	
	pop rdi
	
	mov rcx, rdi
	sub rsp, 32
	call ExitProcess

	mov rcx, 0
	sub rsp, 32
	call ExitProcess
