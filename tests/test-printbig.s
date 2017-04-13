	.file	"./tests/test-printbig.ll"
	.text
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp1:
	.cfi_def_cfa_offset 16
	movl	$10, %edi
	callq	printbig
	movl	%eax, %ecx
	movl	%ecx, 4(%rsp)
	movl	$.Lfmt, %edi
	xorl	%eax, %eax
	movl	%ecx, %esi
	callq	printf
	xorl	%eax, %eax
	popq	%rdx
	ret
.Ltmp2:
	.size	main, .Ltmp2-main
	.cfi_endproc

	.type	.Lfmt,@object           # @fmt
	.section	.rodata.str1.1,"aMS",@progbits,1
.Lfmt:
	.asciz	"%d\n"
	.size	.Lfmt, 4

	.type	.Lmmt,@object           # @mmt
.Lmmt:
	.asciz	"%s\n"
	.size	.Lmmt, 4


	.section	".note.GNU-stack","",@progbits
