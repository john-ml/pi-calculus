	.file	"testregister.c"
	.text
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC0:
	.string	"%p %p\n"
	.section	.text.startup,"ax",@progbits
	.p2align 4,,15
	.globl	main
	.type	main, @function
main:
.LFB24:
	.cfi_startproc
	leaq	main(%rip), %rbx
	leaq	.LC0(%rip), %rsi
	subq	$8, %rsp
	.cfi_def_cfa_offset 16
	movl	$1, %edi
	xorl	%eax, %eax
	movq	%rbx, %rcx
	movq	%rbx, %rdx
	call	__printf_chk@PLT
	xorl	%eax, %eax
	addq	$8, %rsp
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE24:
	.size	main, .-main
	.text
	.p2align 4,,15
	.globl	f
	.type	f, @function
f:
.LFB23:
	.cfi_startproc
	addl	%esi, %edi
	movl	8(%rsp), %eax
	addl	%edi, %edx
	addl	%edx, %ecx
	addl	%r8d, %ecx
	addl	%r9d, %ecx
	addl	%ecx, %eax
	addl	16(%rsp), %eax
	ret
	.cfi_endproc
.LFE23:
	.size	f, .-f
	.ident	"GCC: (Ubuntu 7.4.0-1ubuntu1~18.04.1) 7.4.0"
	.section	.note.GNU-stack,"",@progbits
