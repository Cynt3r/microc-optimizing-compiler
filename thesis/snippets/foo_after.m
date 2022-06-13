foo:
	push rbp
	mov rbp, rsp
	sub rsp, 16
bb_1:
	mov eax, [rbp + 16]
	mov edi, 1440
	mov r10d, edi
	imul r10d, eax
	mov [rbp - 8], r10d
	mov eax, 10
	mov edi, [rbp - 8]
	mov r10d, edi
	add r10d, eax
	mov [rbp - 16], r10d
	mov eax, r10d
	mov rsp, rbp
	pop rbp
	ret