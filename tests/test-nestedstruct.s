	.file	"tests/test-nestedstruct.ll"
	.text
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# BB#0:                                 # %entry
	subq	$40, %rsp
.Ltmp1:
	.cfi_def_cfa_offset 48
	movl	$10, 36(%rsp)
	movq	$.Lname, 24(%rsp)
	movl	$100, 20(%rsp)
	movl	$200, 16(%rsp)
	movq	$.Lname1, 8(%rsp)
	movl	36(%rsp), %esi
	movl	$.Lfmt, %edi
	xorl	%eax, %eax
	callq	printf
	movq	24(%rsp), %rsi
	movl	$.Lmmt, %edi
	xorl	%eax, %eax
	callq	printf
	movl	20(%rsp), %esi
	movl	$.Lfmt, %edi
	xorl	%eax, %eax
	callq	printf
	movl	16(%rsp), %esi
	movl	$.Lfmt, %edi
	xorl	%eax, %eax
	callq	printf
	movq	8(%rsp), %rsi
	movl	$.Lmmt, %edi
	xorl	%eax, %eax
	callq	printf
	xorl	%eax, %eax
	addq	$40, %rsp
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

	.type	.Lname,@object          # @name
	.section	.rodata.str1.16,"aMS",@progbits,1
	.align	16
.Lname:
	.asciz	"nested structs work"
	.size	.Lname, 20

	.type	.Lname1,@object         # @name1
	.align	16
.Lname1:
	.asciz	"nested horse works"
	.size	.Lname1, 19


	.section	".note.GNU-stack","",@progbits
