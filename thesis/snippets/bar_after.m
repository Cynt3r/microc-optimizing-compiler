bar:
	push rbp
	mov rbp, rsp
	sub rsp, 16
bb_1:
	mov eax, [rbp + 16]
	cmp eax, 0
	je else_2
then_2:
	mov eax, 5
	mov [rbp - 8], eax
	jmp finally_2
else_2:
	mov eax, 10
	mov [rbp - 8], eax
finally_2:
	mov eax, [rbp - 8]
	mov rsp, rbp
	pop rbp
	ret