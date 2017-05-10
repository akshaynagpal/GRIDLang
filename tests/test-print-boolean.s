	.text
	.file	"tests/test-print-boolean.ll"
	.globl	listNoderule
	.align	16, 0x90
	.type	listNoderule,@function
listNoderule:                           # @listNoderule
	.cfi_startproc
# BB#0:                                 # %entry
	pushl	%esi
.Ltmp0:
	.cfi_def_cfa_offset 8
	subl	$20, %esp
.Ltmp1:
	.cfi_def_cfa_offset 28
.Ltmp2:
	.cfi_offset %esi, -8
	movl	40(%esp), %eax
	movl	36(%esp), %ecx
	movl	32(%esp), %edx
	movl	28(%esp), %esi
	movl	%esi, 16(%esp)
	movl	%edx, 12(%esp)
	movl	%ecx, 8(%esp)
	movl	%eax, 4(%esp)
	movl	$1, %eax
	addl	$20, %esp
	popl	%esi
	retl
.Ltmp3:
	.size	listNoderule, .Ltmp3-listNoderule
	.cfi_endproc

	.globl	Playerrule
	.align	16, 0x90
	.type	Playerrule,@function
Playerrule:                             # @Playerrule
	.cfi_startproc
# BB#0:                                 # %entry
	pushl	%esi
.Ltmp4:
	.cfi_def_cfa_offset 8
	subl	$20, %esp
.Ltmp5:
	.cfi_def_cfa_offset 28
.Ltmp6:
	.cfi_offset %esi, -8
	movl	40(%esp), %eax
	movl	36(%esp), %ecx
	movl	32(%esp), %edx
	movl	28(%esp), %esi
	movl	%esi, 16(%esp)
	movl	%edx, 12(%esp)
	movl	%ecx, 8(%esp)
	movl	%eax, 4(%esp)
	movl	$1, %eax
	addl	$20, %esp
	popl	%esi
	retl
.Ltmp7:
	.size	Playerrule, .Ltmp7-Playerrule
	.cfi_endproc

	.globl	triggerRule
	.align	16, 0x90
	.type	triggerRule,@function
triggerRule:                            # @triggerRule
	.cfi_startproc
# BB#0:                                 # %entry
	pushl	%edi
.Ltmp8:
	.cfi_def_cfa_offset 8
	pushl	%esi
.Ltmp9:
	.cfi_def_cfa_offset 12
	subl	$28, %esp
.Ltmp10:
	.cfi_def_cfa_offset 40
.Ltmp11:
	.cfi_offset %esi, -12
.Ltmp12:
	.cfi_offset %edi, -8
	movl	56(%esp), %eax
	movl	52(%esp), %ecx
	movl	48(%esp), %edx
	movl	44(%esp), %esi
	movl	40(%esp), %edi
	movl	%edi, 24(%esp)
	movl	%esi, 20(%esp)
	movl	%edx, 16(%esp)
	movl	%ecx, 12(%esp)
	movl	%eax, 8(%esp)
	xorl	%eax, %eax
	addl	$28, %esp
	popl	%esi
	popl	%edi
	retl
.Ltmp13:
	.size	triggerRule, .Ltmp13-triggerRule
	.cfi_endproc

	.globl	gameloop
	.align	16, 0x90
	.type	gameloop,@function
gameloop:                               # @gameloop
	.cfi_startproc
# BB#0:                                 # %entry
	subl	$12, %esp
.Ltmp14:
	.cfi_def_cfa_offset 16
	movl	$0, 8(%esp)
	movl	$0, currentPlayerIndex
	jmp	.LBB3_1
	.align	16, 0x90
.LBB3_2:                                # %while_body
                                        #   in Loop: Header=BB3_1 Depth=1
	calll	checkGameEnd
	movl	%eax, 8(%esp)
	movl	currentPlayerIndex, %eax
	incl	%eax
	xorl	%edx, %edx
	divl	playerOrderSize
	movl	%edx, currentPlayerIndex
.LBB3_1:                                # %while
                                        # =>This Inner Loop Header: Depth=1
	cmpl	$0, 8(%esp)
	je	.LBB3_2
# BB#3:                                 # %merge
	xorl	%eax, %eax
	addl	$12, %esp
	retl
.Ltmp15:
	.size	gameloop, .Ltmp15-gameloop
	.cfi_endproc

	.globl	checkGameEnd
	.align	16, 0x90
	.type	checkGameEnd,@function
checkGameEnd:                           # @checkGameEnd
	.cfi_startproc
# BB#0:                                 # %entry
	pushl	%eax
.Ltmp16:
	.cfi_def_cfa_offset 8
	movl	$1, %eax
	popl	%edx
	retl
.Ltmp17:
	.size	checkGameEnd, .Ltmp17-checkGameEnd
	.cfi_endproc

	.globl	main
	.align	16, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# BB#0:                                 # %entry
	subl	$28, %esp
.Ltmp18:
	.cfi_def_cfa_offset 32
	movb	$1, 27(%esp)
	movzbl	27(%esp), %eax
	movl	%eax, 4(%esp)
	movl	$.Lfmt9, (%esp)
	calll	printf
	movl	$1, playerOrderSize
	calll	gameloop
	xorl	%eax, %eax
	addl	$28, %esp
	retl
.Ltmp19:
	.size	main, .Ltmp19-main
	.cfi_endproc

	.globl	colocation
	.align	16, 0x90
	.type	colocation,@function
colocation:                             # @colocation
	.cfi_startproc
# BB#0:                                 # %entry
	pushl	%esi
.Ltmp20:
	.cfi_def_cfa_offset 8
	subl	$20, %esp
.Ltmp21:
	.cfi_def_cfa_offset 28
.Ltmp22:
	.cfi_offset %esi, -8
	movl	40(%esp), %eax
	movl	36(%esp), %ecx
	movl	32(%esp), %edx
	movl	28(%esp), %esi
	movl	%esi, 16(%esp)
	movl	%edx, 12(%esp)
	movl	%ecx, 8(%esp)
	movl	%eax, 4(%esp)
	xorl	%eax, %eax
	addl	$20, %esp
	popl	%esi
	retl
.Ltmp23:
	.size	colocation, .Ltmp23-colocation
	.cfi_endproc

	.type	playerOrderSize,@object # @playerOrderSize
	.bss
	.globl	playerOrderSize
	.align	4
playerOrderSize:
	.long	0                       # 0x0
	.size	playerOrderSize, 4

	.type	currentPlayerIndex,@object # @currentPlayerIndex
	.globl	currentPlayerIndex
	.align	4
currentPlayerIndex:
	.long	0                       # 0x0
	.size	currentPlayerIndex, 4

	.type	.Lfmt,@object           # @fmt
	.section	.rodata.str1.1,"aMS",@progbits,1
.Lfmt:
	.asciz	"%d\n"
	.size	.Lfmt, 4

	.type	.Lmmt,@object           # @mmt
.Lmmt:
	.asciz	"%s\n"
	.size	.Lmmt, 4

	.type	.Lfmt1,@object          # @fmt1
.Lfmt1:
	.asciz	"%d\n"
	.size	.Lfmt1, 4

	.type	.Lmmt2,@object          # @mmt2
.Lmmt2:
	.asciz	"%s\n"
	.size	.Lmmt2, 4

	.type	.Lfmt3,@object          # @fmt3
.Lfmt3:
	.asciz	"%d\n"
	.size	.Lfmt3, 4

	.type	.Lmmt4,@object          # @mmt4
.Lmmt4:
	.asciz	"%s\n"
	.size	.Lmmt4, 4

	.type	.Lfmt5,@object          # @fmt5
.Lfmt5:
	.asciz	"%d\n"
	.size	.Lfmt5, 4

	.type	.Lmmt6,@object          # @mmt6
.Lmmt6:
	.asciz	"%s\n"
	.size	.Lmmt6, 4

	.type	.Lfmt7,@object          # @fmt7
.Lfmt7:
	.asciz	"%d\n"
	.size	.Lfmt7, 4

	.type	.Lmmt8,@object          # @mmt8
.Lmmt8:
	.asciz	"%s\n"
	.size	.Lmmt8, 4

	.type	.Lfmt9,@object          # @fmt9
.Lfmt9:
	.asciz	"%d\n"
	.size	.Lfmt9, 4

	.type	.Lmmt10,@object         # @mmt10
.Lmmt10:
	.asciz	"%s\n"
	.size	.Lmmt10, 4

	.type	.Lfmt11,@object         # @fmt11
.Lfmt11:
	.asciz	"%d\n"
	.size	.Lfmt11, 4

	.type	.Lmmt12,@object         # @mmt12
.Lmmt12:
	.asciz	"%s\n"
	.size	.Lmmt12, 4


	.section	".note.GNU-stack","",@progbits
