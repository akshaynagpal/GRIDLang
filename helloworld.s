	.file	"helloworld.ll"
	.text
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp6:
	.cfi_def_cfa_offset 32
	subq	$416, %rsp              # imm = 0x1A0
.Ltmp7:
	.cfi_def_cfa_offset 448
.Ltmp8:
	.cfi_offset %rbx, -32
.Ltmp9:
	.cfi_offset %r14, -24
.Ltmp10:
	.cfi_offset %r15, -16
	movl	$1, 412(%rsp)
	movl	$2, 388(%rsp)
	movl	$3, 364(%rsp)
	movl	$4, 340(%rsp)
	movl	$5, 316(%rsp)
	movl	$6, 292(%rsp)
	movl	$9, 260(%rsp)
	movl	$11, 236(%rsp)
	leaq	28(%rsp), %r15
	leaq	392(%rsp), %rax
	movq	%rax, 28(%rsp)
	leaq	368(%rsp), %rax
	movq	%rax, 392(%rsp)
	leaq	344(%rsp), %rax
	movq	%rax, 368(%rsp)
	leaq	320(%rsp), %rax
	movq	%rax, 344(%rsp)
	leaq	296(%rsp), %rax
	movq	%rax, 320(%rsp)
	leaq	184(%rsp), %rbx
	movq	%rbx, 296(%rsp)
	leaq	8(%rsp), %r14
	leaq	240(%rsp), %rcx
	xorl	%esi, %esi
	xorl	%edx, %edx
	movq	%r14, %rdi
	movq	%rbx, %r8
	callq	passby_check
	leaq	216(%rsp), %rcx
	xorl	%esi, %esi
	xorl	%edx, %edx
	movq	%r14, %rdi
	movq	%rbx, %r8
	callq	passby_check
	movl	$.Lmmt, %edi
	movl	$.Lname1, %esi
	xorl	%eax, %eax
	callq	printf
	movq	%r15, 208(%rsp)
	jmp	.LBB0_1
	.align	16, 0x90
.LBB0_2:                                # %while_body
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	208(%rsp), %rax
	movq	(%rax), %rax
	movq	%rax, 208(%rsp)
	movl	20(%rax), %esi
	movl	$.Lfmt, %edi
	xorl	%eax, %eax
	callq	printf
.LBB0_1:                                # %while
                                        # =>This Inner Loop Header: Depth=1
	movq	208(%rsp), %rax
	cmpq	%rbx, (%rax)
	jne	.LBB0_2
# BB#3:                                 # %merge
	xorl	%eax, %eax
	addq	$416, %rsp              # imm = 0x1A0
	popq	%rbx
	popq	%r14
	popq	%r15
	ret
.Ltmp11:
	.size	main, .Ltmp11-main
	.cfi_endproc

	.globl	passby_check
	.align	16, 0x90
	.type	passby_check,@function
passby_check:                           # @passby_check
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp15:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp16:
	.cfi_def_cfa_offset 24
	subq	$24, %rsp
.Ltmp17:
	.cfi_def_cfa_offset 48
.Ltmp18:
	.cfi_offset %rbx, -24
.Ltmp19:
	.cfi_offset %r14, -16
	movq	%r8, %rbx
	movq	%rcx, %r14
	movl	%esi, 20(%rsp)
	movl	%edx, 16(%rsp)
	movslq	20(%rsp), %rax
	imulq	$88, %rax, %rax
	addq	%rdi, %rax
	movslq	16(%rsp), %rcx
	imulq	$44, %rcx, %rcx
	leaq	20(%rcx,%rax), %rax
	movq	%rax, 8(%rsp)
	jmp	.LBB1_1
	.align	16, 0x90
.LBB1_2:                                # %while_body
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	8(%rsp), %rax
	movq	(%rax), %rax
	movq	%rax, 8(%rsp)
	movl	20(%rax), %esi
	movl	$.Lfmt2, %edi
	xorl	%eax, %eax
	callq	printf
.LBB1_1:                                # %while
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rsp), %rax
	cmpq	%rbx, (%rax)
	jne	.LBB1_2
# BB#3:                                 # %merge
	movq	8(%rsp), %rax
	movq	%r14, (%rax)
	movq	8(%rsp), %rax
	movq	(%rax), %rax
	movq	%rax, 8(%rsp)
	movq	%rbx, (%rax)
	addq	$24, %rsp
	popq	%rbx
	popq	%r14
	ret
.Ltmp20:
	.size	passby_check, .Ltmp20-passby_check
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
	.asciz	"Printing p8 now......"
	.size	.Lname, 22

	.type	.Lname1,@object         # @name1
	.align	16
.Lname1:
	.asciz	"Printing p8 now......"
	.size	.Lname1, 22

	.type	.Lfmt2,@object          # @fmt2
	.section	.rodata.str1.1,"aMS",@progbits,1
.Lfmt2:
	.asciz	"%d\n"
	.size	.Lfmt2, 4

	.type	.Lmmt3,@object          # @mmt3
.Lmmt3:
	.asciz	"%s\n"
	.size	.Lmmt3, 4


	.section	".note.GNU-stack","",@progbits
