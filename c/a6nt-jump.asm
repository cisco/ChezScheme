; We do not use Microsoft's implementation because its longjmp unwinds
; the stack to support C++ destructors, and the stack frames generated
; by Chez Scheme do not have the required information for this to work
; properly.
; See https://docs.microsoft.com/en-us/cpp/build/x64-calling-convention

.code

S_setjmp proc
	; store nonvolatile registers & control words
	mov [rcx], rbx
	mov [rcx+08h], rbp
	mov [rcx+10h], rdi
	mov [rcx+18h], rsi
	mov [rcx+20h], rsp
	mov [rcx+28h], r12
	mov [rcx+30h], r13
	mov [rcx+38h], r14
	mov [rcx+40h], r15
	stmxcsr [rcx+48h]
	fnstcw [rcx+4ch]
	movdqu [rcx+50h], xmm6
	movdqu [rcx+60h], xmm7
	movdqu [rcx+70h], xmm8
	movdqu [rcx+80h], xmm9
	movdqu [rcx+90h], xmm10
	movdqu [rcx+0a0h], xmm11
	movdqu [rcx+0b0h], xmm12
	movdqu [rcx+0c0h], xmm13
	movdqu [rcx+0d0h], xmm14
	movdqu [rcx+0e0h], xmm15
	; store return address
	mov rax, [rsp]
	mov [rcx+0f0h], rax
	xor eax, eax
	ret
S_setjmp endp

S_longjmp proc
	; restore nonvolatile registers & control words
	mov rbx, [rcx]
	mov rbp, [rcx+08h]
	mov rdi, [rcx+10h]
	mov rsi, [rcx+18h]
	mov rsp, [rcx+20h]
	mov r12, [rcx+28h]
	mov r13, [rcx+30h]
	mov r14, [rcx+38h]
	mov r15, [rcx+40h]
	ldmxcsr [rcx+48h]
	fldcw [rcx+4ch]
	movdqu xmm6, [rcx+50h]
	movdqu xmm7, [rcx+60h]
	movdqu xmm8, [rcx+70h]
	movdqu xmm9, [rcx+80h]
	movdqu xmm10, [rcx+90h]
	movdqu xmm11, [rcx+0a0h]
	movdqu xmm12, [rcx+0b0h]
	movdqu xmm13, [rcx+0c0h]
	movdqu xmm14, [rcx+0d0h]
	movdqu xmm15, [rcx+0e0h]
	; restore return address
	mov rax, [rcx+0f0h]
	mov [rsp], rax
	mov rax, rdx
	ret
S_longjmp endp
	
end
