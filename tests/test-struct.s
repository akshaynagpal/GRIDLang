	.file	"tests/test-struct.ll"
	.text
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# BB#0:                                 # %entry
	movl	$10, -4(%rsp)
	movl	$20, -8(%rsp)
	movq	$.Lname, -16(%rsp)
	xorl	%eax, %eax
	ret
.Ltmp0:
	.size	main, .Ltmp0-main
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
.Lname:
	.asciz	"struct works"
	.size	.Lname, 13


	.section	".note.GNU-stack","",@progbits
