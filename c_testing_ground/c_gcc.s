	.file	"c.c"
	.text
	.globl	main
	.type	main, @function
main:
	pushq	%rbp
	movq	%rsp, %rbp
	movss	.LC0(%rip), %xmm0
	movss	%xmm0, -12(%rbp)
	movss	.LC1(%rip), %xmm0
	movss	%xmm0, -8(%rbp)
	movss	-12(%rbp), %xmm0
	divss	-8(%rbp), %xmm0
	movss	%xmm0, -4(%rbp)
	movl	$0, %eax
	popq	%rbp
	ret
	.size	main, .-main
	.section	.rodata
	.align 4
.LC0:
	.long	1077936128
	.align 4
.LC1:
	.long	1073741824
	.section	.note.GNU-stack,"",@progbits
