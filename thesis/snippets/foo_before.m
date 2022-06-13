foo:
	push rbp
	mov rbp, rsp
	sub rsp, 32
bb_1:
	mov eax, 24
	mov edi, 60
	mov r10d, edi
	imul r10d, eax
	mov [rbp - 8], r10d
	mov eax, [rbp + 16]
	mov edi, [rbp - 8]
	mov r10d, edi
	imul r10d, eax
	mov [rbp - 16], r10d
	mov eax, 1400
	mov edi, [rbp - 8]
	cmp edi, eax
	jle bb_1_leq_1
	mov r10d, 1
	jmp bb_1_afterGt_1
bb_1_leq_1:
	mov r10d, 0
bb_1_afterGt_1:
	cmp r10d, 0
	je else_2
	jmp then_2
then_2:
	mov eax, 10
	mov edi, [rbp - 16]
	mov r10d, edi
	add r10d, eax
	mov [rbp - 24], r10d
	jmp finally_2
else_2:
	mov eax, 10
	mov edi, [rbp - 16]
	mov r10d, edi
	sub r10d, eax
	mov [rbp - 24], r10d
	jmp finally_2
finally_2:
	mov eax, [rbp - 24]
	mov eax, eax
	mov rsp, rbp
	pop rbp
	ret