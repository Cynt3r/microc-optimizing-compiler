bar:
	push rbp
	mov rbp, rsp
	sub rsp, 16
bb_1:
	mov eax, 42
	mov [rbp - 8], eax
	jmp guard_2
guard_2:
	mov eax, [rbp - 8]
	cmp eax, 0
	je finally_2
	jmp body_2
body_2:
	mov eax, [rbp + 16]
	cmp eax, 0
	je else_3
	jmp then_3
then_3:
	mov eax, 5
	mov [rbp - 16], eax
	jmp finally_3
else_3:
	mov eax, 10
	mov [rbp - 16], eax
	jmp finally_3
finally_3:
	mov eax, [rbp - 16]
	mov edi, 0
	mov r10d, eax
	push rdx
	mov edx, 0
	mov eax, edi
	idiv r10d
	pop rdx
	mov [rbp - 8], eax
	jmp guard_2
finally_2:
	mov eax, [rbp - 16]
	mov eax, eax
	mov rsp, rbp
	pop rbp
	ret