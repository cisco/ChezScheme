; See "a6nt-jump.asm" for an explanation of why this implementation exists

        AREA .text, CODE, READONLY
        EXPORT S_setjmp
        EXPORT S_longjmp
        
S_setjmp
	str  x18, [x0, #0]
	str  x19, [x0, #8]
	str  x20, [x0, #16]
	str  x21, [x0, #24]
	str  x22, [x0, #32]
	str  x23, [x0, #40]
	str  x24, [x0, #48]
	str  x25, [x0, #56]
	str  x26, [x0, #64]
	str  x27, [x0, #72]
	str  x28, [x0, #80]
	str  x29, [x0, #88]
	str  x30, [x0, #96]
	mov  x2, sp
	str  x2, [x0, #104]
	str  d8, [x0, #112]
	str  d9, [x0, #120]
	str  d10, [x0, #128]
	str  d11, [x0, #136]
	str  d12, [x0, #144]
	str  d13, [x0, #152]
	str  d14, [x0, #160]
	str  d15, [x0, #168]
	mov  x0, 0
	ret

S_longjmp
	ldr  x18, [x0, #0]
	ldr  x19, [x0, #8]
	ldr  x20, [x0, #16]
	ldr  x21, [x0, #24]
	ldr  x22, [x0, #32]
	ldr  x23, [x0, #40]
	ldr  x24, [x0, #48]
	ldr  x25, [x0, #56]
	ldr  x26, [x0, #64]
	ldr  x27, [x0, #72]
	ldr  x28, [x0, #80]
	ldr  x29, [x0, #88]
	ldr  x30, [x0, #96]
	ldr  x2, [x0, #104]
	mov  sp, x2
	ldr  d8, [x0, #112]
	ldr  d9, [x0, #120]
	ldr  d10, [x0, #128]
	ldr  d11, [x0, #136]
	ldr  d12, [x0, #144]
	ldr  d13, [x0, #152]
	ldr  d14, [x0, #160]
	ldr  d15, [x0, #168]
	mov  x0, x1
	ret

	END
