	.file	"helloworld.ll"
	.text
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp7:
	.cfi_def_cfa_offset 16
	pushq	%r15
.Ltmp8:
	.cfi_def_cfa_offset 24
	pushq	%r14
.Ltmp9:
	.cfi_def_cfa_offset 32
	pushq	%r13
.Ltmp10:
	.cfi_def_cfa_offset 40
	pushq	%r12
.Ltmp11:
	.cfi_def_cfa_offset 48
	pushq	%rbx
.Ltmp12:
	.cfi_def_cfa_offset 56
	subq	$408, %rsp              # imm = 0x198
.Ltmp13:
	.cfi_def_cfa_offset 464
.Ltmp14:
	.cfi_offset %rbx, -56
.Ltmp15:
	.cfi_offset %r12, -48
.Ltmp16:
	.cfi_offset %r13, -40
.Ltmp17:
	.cfi_offset %r14, -32
.Ltmp18:
	.cfi_offset %r15, -24
.Ltmp19:
	.cfi_offset %rbp, -16
	movl	$1, 404(%rsp)
	movl	$2, 380(%rsp)
	movl	$3, 356(%rsp)
	movl	$4, 332(%rsp)
	movl	$5, 308(%rsp)
	movl	$6, 284(%rsp)
	movl	$9, 252(%rsp)
	movl	$11, 228(%rsp)
	leaq	384(%rsp), %r13
	movq	%r13, 20(%rsp)
	leaq	360(%rsp), %rax
	movq	%rax, 384(%rsp)
	leaq	336(%rsp), %r15
	movq	%r15, 360(%rsp)
	leaq	312(%rsp), %r12
	movq	%r12, 336(%rsp)
	leaq	288(%rsp), %rax
	movq	%rax, 312(%rsp)
	leaq	176(%rsp), %rbx
	movq	%rbx, 288(%rsp)
	leaq	(%rsp), %rbp
	leaq	232(%rsp), %rcx
	xorl	%esi, %esi
	xorl	%edx, %edx
	movq	%rbp, %rdi
	movq	%rbx, %r8
	callq	addToGrid
	leaq	208(%rsp), %r14
	xorl	%esi, %esi
	xorl	%edx, %edx
	movq	%rbp, %rdi
	movq	%r14, %rcx
	movq	%rbx, %r8
	callq	addToGrid
	xorl	%esi, %esi
	xorl	%edx, %edx
	movq	%rbp, %rdi
	movq	%r12, %rcx
	movq	%rbx, %r8
	callq	deleteFromGrid
	xorl	%esi, %esi
	xorl	%edx, %edx
	movq	%rbp, %rdi
	movq	%r15, %rcx
	movq	%rbx, %r8
	callq	deleteFromGrid
	xorl	%esi, %esi
	xorl	%edx, %edx
	movq	%rbp, %rdi
	movq	%r15, %rcx
	movq	%rbx, %r8
	callq	deleteFromGrid
	xorl	%esi, %esi
	xorl	%edx, %edx
	movq	%rbp, %rdi
	movq	%r12, %rcx
	movq	%rbx, %r8
	callq	deleteFromGrid
	xorl	%esi, %esi
	xorl	%edx, %edx
	movq	%rbp, %rdi
	movq	%r14, %rcx
	movq	%rbx, %r8
	callq	deleteFromGrid
	xorl	%esi, %esi
	xorl	%edx, %edx
	movq	%rbp, %rdi
	movq	%r13, %rcx
	movq	%rbx, %r8
	callq	deleteFromGrid
	xorl	%esi, %esi
	xorl	%edx, %edx
	movq	%rbp, %rdi
	leaq	288(%rsp), %rcx
	movq	%rbx, %r8
	callq	deleteFromGrid
	movl	$.Lmmt, %edi
	movl	$.Lname1, %esi
	xorl	%eax, %eax
	callq	printf
	leaq	20(%rsp), %rax
	movq	%rax, 200(%rsp)
	jmp	.LBB0_1
	.align	16, 0x90
.LBB0_2:                                # %while_body
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	200(%rsp), %rax
	movq	(%rax), %rax
	movq	%rax, 200(%rsp)
	movl	20(%rax), %esi
	movl	$.Lfmt, %edi
	xorl	%eax, %eax
	callq	printf
.LBB0_1:                                # %while
                                        # =>This Inner Loop Header: Depth=1
	movq	200(%rsp), %rax
	cmpq	%rbx, (%rax)
	jne	.LBB0_2
# BB#3:                                 # %merge
	xorl	%eax, %eax
	addq	$408, %rsp              # imm = 0x198
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	ret
.Ltmp20:
	.size	main, .Ltmp20-main
	.cfi_endproc

	.globl	deleteFromGrid
	.align	16, 0x90
	.type	deleteFromGrid,@function
deleteFromGrid:                         # @deleteFromGrid
	.cfi_startproc
# BB#0:                                 # %entry
	subq	$24, %rsp
.Ltmp22:
	.cfi_def_cfa_offset 32
	movl	%esi, 20(%rsp)
	movl	%edx, 16(%rsp)
	movslq	20(%rsp), %rax
	imulq	$88, %rax, %rax
	addq	%rdi, %rax
	movslq	16(%rsp), %rdx
	imulq	$44, %rdx, %rdx
	leaq	20(%rdx,%rax), %rax
	jmp	.LBB1_1
	.align	16, 0x90
.LBB1_3:                                # %while_body
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	8(%rsp), %rax
	movq	(%rax), %rax
.LBB1_1:                                # %while
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, 8(%rsp)
	movq	8(%rsp), %rax
	movq	(%rax), %rax
	cmpq	%r8, %rax
	je	.LBB1_4
# BB#2:                                 # %while
                                        #   in Loop: Header=BB1_1 Depth=1
	cmpq	%rcx, %rax
	jne	.LBB1_3
.LBB1_4:                                # %merge
	movq	8(%rsp), %rax
	cmpq	%r8, (%rax)
	je	.LBB1_6
# BB#5:                                 # %merge40
	movq	(%rcx), %rax
	movq	8(%rsp), %rdx
	movq	%rax, (%rdx)
	movq	%r8, (%rcx)
	addq	$24, %rsp
	ret
.LBB1_6:                                # %then
	movl	$.Lmmt3, %edi
	movl	$.Lname5, %esi
	xorl	%eax, %eax
	callq	printf
	addq	$24, %rsp
	ret
.Ltmp23:
	.size	deleteFromGrid, .Ltmp23-deleteFromGrid
	.cfi_endproc

	.globl	addToGrid
	.align	16, 0x90
	.type	addToGrid,@function
addToGrid:                              # @addToGrid
	.cfi_startproc
# BB#0:                                 # %entry
	subq	$24, %rsp
.Ltmp25:
	.cfi_def_cfa_offset 32
	movl	%esi, 20(%rsp)
	movl	%edx, 16(%rsp)
	movslq	20(%rsp), %rax
	imulq	$88, %rax, %rax
	addq	%rdi, %rax
	movslq	16(%rsp), %rdx
	imulq	$44, %rdx, %rdx
	leaq	20(%rdx,%rax), %rax
	movq	%rax, 8(%rsp)
	.align	16, 0x90
.LBB2_1:                                # %while
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rsp), %rax
	cmpq	%r8, (%rax)
	je	.LBB2_4
# BB#2:                                 # %while_body
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	8(%rsp), %rax
	movq	(%rax), %rax
	movq	%rax, 8(%rsp)
	cmpq	%rcx, %rax
	jne	.LBB2_1
# BB#3:                                 # %then
	movl	$.Lmmt7, %edi
	movl	$.Lname9, %esi
	xorl	%eax, %eax
	callq	printf
	addq	$24, %rsp
	ret
.LBB2_4:                                # %merge30
	movq	8(%rsp), %rax
	movq	%rcx, (%rax)
	movq	8(%rsp), %rax
	movq	(%rax), %rax
	movq	%rax, 8(%rsp)
	movq	%r8, (%rax)
	addq	$24, %rsp
	ret
.Ltmp26:
	.size	addToGrid, .Ltmp26-addToGrid
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

	.type	.Lname4,@object         # @name4
	.section	.rodata.str1.16,"aMS",@progbits,1
	.align	16
.Lname4:
	.asciz	"Player already removed from coordinate"
	.size	.Lname4, 39

	.type	.Lname5,@object         # @name5
	.align	16
.Lname5:
	.asciz	"Player already removed from coordinate"
	.size	.Lname5, 39

	.type	.Lfmt6,@object          # @fmt6
	.section	.rodata.str1.1,"aMS",@progbits,1
.Lfmt6:
	.asciz	"%d\n"
	.size	.Lfmt6, 4

	.type	.Lmmt7,@object          # @mmt7
.Lmmt7:
	.asciz	"%s\n"
	.size	.Lmmt7, 4

	.type	.Lname8,@object         # @name8
	.section	.rodata.str1.16,"aMS",@progbits,1
	.align	16
.Lname8:
	.asciz	"Player already on grid coordinate"
	.size	.Lname8, 34

	.type	.Lname9,@object         # @name9
	.align	16
.Lname9:
	.asciz	"Player already on grid coordinate"
	.size	.Lname9, 34


	.section	".note.GNU-stack","",@progbits
