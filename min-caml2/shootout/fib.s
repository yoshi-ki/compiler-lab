.data
.balign	8
.text
fib.17:
	cmpl	$2, %eax
	jl	jge_else.42
	movl	%eax, %ebx
	subl	$1, %ebx
	movl	%eax, 0(%ebp)
	movl	%ebx, %eax
	addl	$8, %ebp
	call	fib.17
	subl	$8, %ebp
	movl	0(%ebp), %ebx
	subl	$2, %ebx
	movl	%eax, 4(%ebp)
	movl	%ebx, %eax
	addl	$8, %ebp
	call	fib.17
	subl	$8, %ebp
	movl	4(%ebp), %ebx
	addl	%ebx, %eax 
	ret
jge_else.42:
	movl	$1, %eax
	ret
loop.19:
	cmpl	$0, %eax
	jne	je_else.43
	ret
je_else.43:
	movl	$30, %ebx
	movl	%eax, 0(%ebp)
	movl	%ebx, %eax
	addl	$8, %ebp
	call	fib.17
	subl	$8, %ebp
	addl	$8, %ebp
	call	min_caml_print_int
	subl	$8, %ebp
	addl	$8, %ebp
	call	min_caml_print_newline
	subl	$8, %ebp
	movl	0(%ebp), %eax
	subl	$1, %eax
	jmp	loop.19
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
	movl	$100, %eax
	call	loop.19
	popl	%ebp
	popl	%edi
	popl	%esi
	popl	%edx
	popl	%ecx
	popl	%ebx
	popl	%eax
	ret
