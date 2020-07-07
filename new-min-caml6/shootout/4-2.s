.data
.balign	8
.text
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
	movl	$1, %eax
	movl	$2, %ebx
	movl	$3, %ecx
	movl	min_caml_hp, %edx
	addl	$8, min_caml_hp
	movl	%ecx, 4(%edx)
	movl	%ebx, 0(%edx)
	movl	min_caml_hp, %edx
	addl	$16, min_caml_hp
	movl	%ecx, 8(%edx)
	movl	%ebx, 4(%edx)
	movl	%eax, 0(%edx)
	movl	4(%edx), %eax
	call	min_caml_snd
	cmpl	$3, %eax
	jne	je_else.28
	movl	$1, %eax
	call	min_caml_print_int
	jmp	je_cont.29
je_else.28:
	movl	$2, %eax
	call	min_caml_print_int
je_cont.29:
	popl	%ebp
	popl	%edi
	popl	%esi
	popl	%edx
	popl	%ecx
	popl	%ebx
	popl	%eax
	ret
