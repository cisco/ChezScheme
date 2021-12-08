PUBLIC	S_setjmp
PUBLIC	S_longjmp

_TEXT	SEGMENT

S_setjmp PROC
	mov [RCX], RBX
	mov [RCX+08h], RBP
	mov [RCX+10h], RDI
	mov [RCX+18h], RSI
	mov [RCX+20h], RSP
	mov [RCX+28h], R12
	mov [RCX+30h], R13
	mov [RCX+38h], R14
	mov [RCX+40h], R15
	stmxcsr [RCX+48h]
	movdqu [RCX+50h], XMM6
	movdqu [RCX+60h], XMM7
	movdqu [RCX+70h], XMM8
	movdqu [RCX+80h], XMM9
	movdqu [RCX+90h], XMM10
	movdqu [RCX+0A0h], XMM11
	movdqu [RCX+0B0h], XMM12
	movdqu [RCX+0C0h], XMM13
	movdqu [RCX+0D0h], XMM14
	movdqu [RCX+0E0h], XMM15
	mov RAX, [RSP]
	mov [RCX+0F0h], RAX
	mov RAX, 0
	ret
S_setjmp ENDP

S_longjmp PROC
	mov RBX, [RCX]
	mov RBP, [RCX+08h]
	mov RDI, [RCX+10h]
	mov RSI, [RCX+18h]
	mov RSP, [RCX+20h]
	mov R12, [RCX+28h]
	mov R13, [RCX+30h]
	mov R14, [RCX+38h]
	mov R15, [RCX+40h]
	ldmxcsr [RCX+48h]
	movdqu XMM6, [RCX+50h]
	movdqu XMM7, [RCX+60h]
	movdqu XMM8, [RCX+70h]
	movdqu XMM9, [RCX+80h]
	movdqu XMM10, [RCX+90h]
	movdqu XMM11, [RCX+0A0h]
	movdqu XMM12, [RCX+0B0h]
	movdqu XMM13, [RCX+0C0h]
	movdqu XMM14, [RCX+0D0h]
	movdqu XMM15, [RCX+0E0h]
	mov RAX, [RCX+0F0h]
	mov [RSP], RAX
	mov RAX, RDX
	ret
S_longjmp ENDP

_TEXT ENDS

END
