baz:
	push rbp
	mov rbp, rsp
	sub rsp, 16
bb_1:
	mov eax, 60
	mov edi, [rbp + 16]
	mov r10d, edi
	imul r10d, eax
	mov [rbp - 16], r10d
	mov eax, r10d
	cmp eax, 0
	je else_2
then_2:
	mov eax, 42
	mov [rbp - 8], eax
	jmp finally_2
else_2:
	mov eax, 10
	mov [rbp - 8], eax
finally_2:
	mov eax, 1
	mov edi, [rbp - 8]
	mov r10d, edi
	add r10d, eax
	mov [rbp - 8], r10d
	mov eax, r10d
	mov edi, [rbp - 16]
	mov r10d, edi
	sub r10d, eax
	mov eax, r10d
	mov rsp, rbp
	pop rbp
	ret