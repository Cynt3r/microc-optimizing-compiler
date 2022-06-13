baz:
	push rbp
	mov rbp, rsp
	sub rsp, 16
bb_1:
	mov eax, 60
	mov edi, [rbp + 16]
	mov r10d, edi
	imul r10d, eax
	cmp r10d, 0
	je else_2
	jmp then_2
then_2:
	mov eax, 42
	mov [rbp - 8], eax
	jmp finally_2
else_2:
	mov eax, 10
	mov [rbp - 8], eax
	jmp finally_2
finally_2:
	mov eax, [rbp - 8]
	cmp eax, 0
	je else_3
	jmp then_3
then_3:
	mov eax, 1
	mov edi, [rbp - 8]
	mov r10d, edi
	add r10d, eax
	mov [rbp - 8], r10d
	jmp finally_3
else_3:
	mov eax, [rbp - 8]
	mov edi, [rbp + 16]
	mov r10d, edi
	add r10d, eax
	mov [rbp + 16], r10d
	jmp finally_3
finally_3:
	mov eax, [rbp - 8]
	mov edi, [rbp + 16]
	mov r10d, 60
	mov r14d, r10d
	imul r14d, edi
	mov edi, r14d
	sub edi, eax
	mov eax, edi
	mov rsp, rbp
	pop rbp
	ret