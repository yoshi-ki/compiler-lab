.data
.balign	8
l.19:	# 1500.000000
	.long	0x0
	.long	0x40977000
l.15:	# 1.000000
	.long	0x0
	.long	0x3ff00000
.text
fact.7:
	movl	$l.15, %eax
	movsd	0(%eax), %xmm1
	comisd	%xmm1, %xmm0
	jne	je_else.23
	movl	$l.15, %eax
	movsd	0(%eax), %xmm0
	ret
je_else.23:
	movl	$l.15, %eax
	movsd	0(%eax), %xmm1
	movsd	%xmm1, 0(%ebp)
	movsd	%xmm0, %xmm1
	subsd	0(%ebp), %xmm1
	movsd	%xmm0, 0(%ebp)
	movsd	%xmm1, %xmm0
	addl	$8, %ebp
	call	fact.7
	subl	$8, %ebp
	movsd	0(%ebp), %xmm1
	mulsd	%xmm1, %xmm0
	ret
.globl	min_caml_start
min_caml_start:
.globl	_min_caml_start
_min_caml_start: # for cygwin
	pushl	%eax
	pushl	%ebx
	pushl	%ecx
	pushl	%edx
	pushl	%esi
	pushl	%edi
	pushl	%ebp
	movl	32(%esp),%ebp
	movl	36(%esp),%eax
	movl	%eax,min_caml_hp
	movl	$l.19, %eax
	movsd	0(%eax), %xmm0
	call	fact.7
	call	min_caml_print_int
	popl	%ebp
	popl	%edi
	popl	%esi
	popl	%edx
	popl	%ecx
	popl	%ebx
	popl	%eax
	ret
