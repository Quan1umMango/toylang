section .text
	global _start
	
            extern ExitProcess
_start:
	
            
	mov rax, 0 ; Initial Condition
	push rax

loop_entry:
  mov rax, 10 ; End Condition 
  push rax

  pop rdx
  mov rax, qword [rsp+0]
  cmp rax,rdx 
  mov rax, 1
  mov rdx, 0
  cmove rax,rdx 
  push rax 

  pop rax 
  test rax, 1
  jz loop_exit

  pop rax 
  add rax, 1
  push rax 
  jmp loop_entry
  


loop_exit:
  pop rdi
  mov rcx, rdi
  call ExitProcess
  
