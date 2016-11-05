%include "int_arithmetics.mac"
%include "help_macro.mac"

extern  printf
extern  scanf

FLOAT80_EXP_OFFSET equ 0x3fff
FLOAT64_EXP_OFFSET equ 0x3ff
FLOAT80_INF_NAN_EXP equ 0x7fff
FLOAT64_INF_NAN_EXP equ 0x7ff
FLOAT64_INF equ 0x7f_f0_00_00_00_00_00_00
FLOAT80_INF_MANTISSA equ 0x80_00_00_00_00_00_00_00
FLOAT80_NAN_MANTISSA equ 0xc0_00_00_00_00_00_00_00
REMOVE_SIGN_BIT_WORD equ 0x7fff
GET_SIGN_BIT_WORD equ 0x8000

%macro is_zero 2
	; for float80, result in %2
	xor %2, %2
	mov ax, [%1 + 8]
	and ax, REMOVE_SIGN_BIT_WORD
	test ax, ax
	jnz %%exit
	mov rax, [%1]
	test rax, rax
	jnz %%exit
	mov %2, 1
	%%exit:
%endmacro
%macro is_inf 2
	; for float80, result in %2
	xor %2, %2
	mov ax, [%1 + 8]
	and ax, REMOVE_SIGN_BIT_WORD
	cmp ax, FLOAT80_INF_NAN_EXP
	jne %%exit
	mov rax, [%1]
	shl rax, 1
	test rax, rax
	jnz %%exit
	mov %2, 1
	%%exit:
%endmacro
%macro is_nan 2
	; for float80, result in %2
	xor %2, %2
	mov ax, [%1 + 8]
	and ax, REMOVE_SIGN_BIT_WORD
	cmp ax, FLOAT80_INF_NAN_EXP
	jne %%exit
	mov rax, [%1]
	shl rax, 1
	test rax, rax
	jz %%exit
	mov %2, 1
	%%exit:
%endmacro
%macro test_routine 2
	mov rcx, 10
	mov rdi, result_float80
	mov rsi, first_float80
  rep movsb

	push second_float80
	push result_float80
	call %1
	add rsp, 0x10

	push result_float80
	push result_float64
	call float80_to_float64
	add rsp, 0x10

	mov rdi, %2
	mov rax, 3
	movq xmm0, [first_float64]
	movq xmm1, [second_float64]
	movq xmm2, [result_float64]
	call printf
%endmacro


section .data
	first_float64 dq 0.0
	second_float64 dq 0.0
	result_float64 dq 0.0

	first_float80 dt 0.0
	second_float80 dt 0.0
	result_float80 dt 0.0

	intro_msg: db 10, "Hello! This is a test for my float arithmetics.", 10
	input_1: db "Input firstr float64: "
	input_2: db "Input second float64: "
	input_pattern db "%lf", 0
	print_add db "%.16g + %.16g = %.16g", 10, 0
	print_sub db "%.16g - %.16g = %.16g", 10, 0
	print_mul db "%.16g * %.16g = %.16g", 10, 0
	print_div db "%.16g / %.16g = %.16g", 10, 0

section .text
	global  main

main:
	sub rsp, 0x8        ; align stack

	print_string intro_msg, input_1 - intro_msg
	print_string input_1, input_2 - input_1
	mov rdi, input_pattern
	mov rsi, first_float64
	call scanf
	print_string input_2, input_pattern - input_2
	mov rdi, input_pattern
	mov rsi, second_float64
	call scanf

	push first_float64
	push first_float80
	call float64_to_float80
	add rsp, 0x10
	push second_float64
	push second_float80
	call float64_to_float80
	add rsp, 0x10

	test_routine add_floats, print_add
	test_routine sub_floats, print_sub
	test_routine mul_floats, print_mul
	test_routine div_floats, print_div

	add rsp, 0x8
	xor al, al
	ret


float80_to_float64:
	push rbp
	push r13
	mov rbp, rsp
	mov r13, 0x18
	add_64 rbp, r13
	push rbx
	mov rbx, [rbp + 8]    ; float80
	mov rbp, [rbp]        ; float64

	push rax
	push rcx

	; check special cases
	is_zero rbx, rcx
	test rcx, rcx
	jnz .zero
	is_inf rbx, rcx
	test rcx, rcx
	jnz .inf
	is_nan rbx, rcx
	test rcx, rcx
	jnz .nan
	jmp .normal_number

	.zero:
	mov rax, [rbx + 2]
	mov [rbp], rax
	jmp .done
	.inf:
	mov ax, [rbx + 8]
	and ax, GET_SIGN_BIT_WORD
	or ax, FLOAT64_INF_NAN_EXP << 4
	shl rax, 63 - 15
	mov [rbp], rax
	jmp .done
	.nan:
	mov ax, [rbx + 8]
	and ax, GET_SIGN_BIT_WORD
	or ax, 0x7ff8
	shl rax, 63 - 15
	mov [rbp], rax
	jmp .done

	.normal_number:
	mov ax, [rbx + 8]
	mov cx, ax
	and cx, GET_SIGN_BIT_WORD   ; save sign
	shl rcx, 63 - 15
	and ax, REMOVE_SIGN_BIT_WORD   ; exp in ax
	mov r13w, FLOAT80_EXP_OFFSET - FLOAT64_EXP_OFFSET
	sub_64 ax, r13w, r10w, r11w
	bt ax, 15
	jc .underflow
	cmp ax, FLOAT64_INF_NAN_EXP
	jae .overflow
	shl rax, 63 - 15 + 4     ; new exp
	or rcx, rax

	.reduce_mantissa:
	mov rax, [rbx]
	btr rax, 63
	shr rax, 11
	or rax, rcx
	mov [rbp], rax
	jmp .done

	.overflow:
		; set inf
		mov rax, FLOAT64_INF
		or rax, rcx
		mov [rbp], rax
		jmp .done
	.underflow:
		; set zero
		mov [rbp], rcx

	.done:
	pop rcx
	pop rax
	pop rbx
	pop r13
	pop rbp
	ret

float64_to_float80:
	push rbp
	push r13
	mov rbp, rsp
	mov r13, 0x18
	add_64 rbp, r13
	push rbx
	mov rbx, [rbp + 8]   ; float64
	mov rbp, [rbp]       ; float80

	push rax
	push rcx

	mov ax, [rbx + 6]  ; sign and exp
	mov cx, ax
	and cx, GET_SIGN_BIT_WORD     ; save sign in cx
	and ax, FLOAT64_INF_NAN_EXP << 4
	cmp ax, FLOAT64_INF_NAN_EXP << 4
	je .inf_or_nan
	test ax, ax
	jz .zero

	; normal number
	shr ax, 4
	mov r13w, FLOAT80_EXP_OFFSET - FLOAT64_EXP_OFFSET
	add_64 ax, r13w, r11w
	jmp .store_exp_and_sign

	.inf_or_nan:
	or ax, FLOAT80_INF_NAN_EXP

	.store_exp_and_sign:
	or ax, cx
	mov [rbp + 8], ax

	.extend_mantissa:
	mov rax, [rbx]
	shl rax, 11
	bts rax, 63
	mov [rbp], rax
	jmp .done

	.zero:
		mov rax, [rbx]
		shl rax, 12
		test rax, rax
		jnz .very_small_num
		mov [rbp + 8], cx
		mov qword [rbp], 0
		jmp .done
		.very_small_num:
			mov r13w, FLOAT80_EXP_OFFSET - FLOAT64_EXP_OFFSET
			add_64 cx, r13w, r11w
			mov [rbp + 8], cx
			shr rax, 1
			bt rax, 63
			mov [rbp], rax

	.done:
	pop rcx
	pop rax
	pop rbx
	pop r13
	pop rbp
	ret

mul_floats:
; Multiplies 2 float numbers in extended (80 bit) format
; Input via references in stack
; Damages r10 and r11
	push rbp
	push r13
	mov rbp, rsp
	mov r13, 0x18
	add_64 rbp, r13
	push rbx
	mov rbx, [rbp + 8]   ; second num
	mov rbp, [rbp]       ; first num

	push rax
	push rcx
	push rdx
	push r15
	push r14

	xor r15, r15
	xor r14, r14

	mov r15w, [rbp + 8]
	mov r14w, [rbx + 8]
	mov cx, r15w
	xor cx, r14w
	and cx, GET_SIGN_BIT_WORD       ; sign of result

	; check special cases
	is_nan rbp, rdx
	test rdx, rdx
	jnz .nan
	is_nan rbx, rdx
	test rdx, rdx
	jnz .nan

	is_inf rbp, dl
	or dh, dl
	shl dh, 1
	is_zero rbp, dl
	or dh, dl
	shl dh, 1
	is_inf rbx, dl
	or dh, dl
	shl dh, 1
	is_zero rbx, dl
	or dh, dl
	shr dx, 2
	shr dl, 6
	mov ax, dx      ; save flags   000000inf_flag1,zero_flag1|000000inf_flag2,zero_flag2
	xor dl, dh
	cmp dl, 0x3    ; 0*inf or inf*0
	je .nan
	or al, ah
	cmp al, 0x1    ; 0*number or number*0
	je .zero
	cmp al, 0x2    ; inf*number or number*inf
	je .inf
	jmp .normal_numbers

	.nan:
		or cx, FLOAT80_INF_NAN_EXP
		mov rax, FLOAT80_NAN_MANTISSA
		mov [rbp], rax
		mov [rbp + 8], cx
	jmp .done
	.inf:
		or cx, FLOAT80_INF_NAN_EXP
		mov rax, FLOAT80_INF_MANTISSA
		mov [rbp], rax
		mov [rbp + 8], cx
	jmp .done
	.zero:
		xor rax, rax
		mov [rbp], rax
		mov [rbp + 8], cx
	jmp .done

	.normal_numbers:
	and r15w, REMOVE_SIGN_BIT_WORD
	and r14w, REMOVE_SIGN_BIT_WORD
	mov r13w, FLOAT80_EXP_OFFSET
	sub_64 r15, r13
	add_64 r15, r14
	; new exp in r15w
	bt r15, 63
	jc .zero
	cmp r15, FLOAT80_INF_NAN_EXP
	jae .inf

	mov rax, [rbp]
	mov r11, [rbx]
	shr rax, 1
	shr r11, 1
	mul_64 r11

	; normalize mantissa
	shld rdx, rax, 2
	shl rax, 2
	bt rdx, 63
	jc .correct_exp
		shld rdx, rax, 1
		shl rax, 1
		jmp .correct_done
		.correct_exp:
			mov r13w, 1
			add_64 r15w, r13w, r11w
			cmp r15w, FLOAT80_INF_NAN_EXP
			je .inf
	.correct_done:

	.return_result:
	or r15w, cx              ; return sign to result
	mov [rbp], rdx
	mov [rbp + 8], r15w

	.done:
	pop r14
	pop r15
	pop rdx
	pop rcx
	pop rax
	pop rbx
	pop r13
	pop rbp
	ret

div_floats:
; Divides floats in extended (80 bit) format
; Input via references in stack
; Damages r10 and r11
	push rbp
	push r13
	mov rbp, rsp
	mov r13, 0x18
	add_64 rbp, r13
	push rbx
	mov rbx, [rbp + 8]   ; divider
	mov rbp, [rbp]       ; dividend

	push rax
	push rcx
	push rdx
	push r15
	push r14

	xor r15, r15
	xor r14, r14

	mov r15w, [rbp + 8]
	mov r14w, [rbx + 8]
	mov cx, r15w
	xor cx, r14w
	and cx, GET_SIGN_BIT_WORD       ; sign of result

	; check special cases
	is_nan rbp, rdx
	test rdx, rdx
	jnz .nan
	is_nan rbx, rdx
	test rdx, rdx
	jnz .nan
	is_zero rbx, rdx
	test rdx, rdx
	jnz .nan         ; division by zero
	is_inf rbx, dh
	is_inf rbp, dl
	test dl, dh
	jnz .nan         ; inf/inf
	test dh, dh
	jnz .zero		 ; division by inf
	is_zero rbp, rdx
	test rdx, rdx
	jnz .zero         ; 0/num
	is_inf rbp, rdx
	test rdx, rdx
	jnz .inf		 ; inf/num
	jmp .normal_numbers

	.nan:
		or cx, FLOAT80_INF_NAN_EXP
		mov rax, FLOAT80_NAN_MANTISSA
		mov [rbp], rax
		mov [rbp + 8], cx
	jmp .done
	.inf:
		or cx, FLOAT80_INF_NAN_EXP
		mov rax, FLOAT80_INF_MANTISSA
		mov [rbp], rax
		mov [rbp + 8], cx
	jmp .done
	.zero:
		xor rax, rax
		mov [rbp], rax
		mov [rbp + 8], cx
	jmp .done

	.normal_numbers:
	and r15w, REMOVE_SIGN_BIT_WORD
	and r14w, REMOVE_SIGN_BIT_WORD
	mov r13w, FLOAT80_EXP_OFFSET
	add_64 r15, r13
	sub_64 r15, r14
	; new exp in r15w
	bt r15, 63
	jc .zero
	cmp r15, FLOAT80_INF_NAN_EXP
	jae .inf

	mov rdx, [rbp]
	mov r11, [rbx]
	shr rdx, 2
	shr r11, 1
	xor rax, rax
	div_64 r11

	; normalize mantissa
	bt rax, 63
	jc .return_result
		shl rax, 1
		mov r13w, 1
		sub_64 r15w, r13w, r10w, r11w
		bt r15w, 15
		jc .zero

	.return_result:
	or r15w, cx              ; return sign to result
	mov [rbp], rax
	mov [rbp + 8], r15w

	.done:
	pop r14
	pop r15
	pop rdx
	pop rcx
	pop rax
	pop rbx
	pop r13
	pop rbp
	ret

add_floats:
; Addition for floats in extended (80 bit) format
; Input via references in stack
; Damages r10 and r11
	push rbp
	push r13
	mov rbp, rsp
	mov r13, 0x18
	add_64 rbp, r13
	push rbx
	mov rbx, [rbp + 8]	 ; second summand
	mov rbp, [rbp]       ; first summand

	push rax
	push rcx
	push rdx
	push r15
	push r14

	mov r15w, [rbp + 8]
	mov r14w, [rbx + 8]
	mov r13w, r15w
	xor r13w, r14w
	and r13w, GET_SIGN_BIT_WORD       ; difference of signs flag

	; special cases
	is_nan rbp, dx
	test dx, dx
	jnz .nan
	is_nan rbx, dx
	test dx, dx
	jnz .nan
	is_zero rbx, dx
	test dx, dx
	jnz .done              ; return_1st_summand
	is_zero rbp, dx
	test dx, dx
	jnz .return_2st_summand
	is_inf rbp, dh
	is_inf rbx, dl
	test dx, dx
	jz .normal_numbers
	test dh, dl
	jz .inf

	.sum_of_infs:
		test r13w, r13w
		jnz .nan
		jmp .done
	.inf:
		is_inf rbp, dx
		test dx, dx
		jnz .done
		jmp .return_2st_summand
	.nan:
		mov rax, 0x7f_ff_c0_00_00_00_00_00
		mov [rbp + 2], rax
		jmp .done
	.return_2st_summand:
		mov rax, [rbx]
		mov [rbp], rax
		mov ax, [rbx + 8]
		mov [rbp + 8], ax
		jmp .done

	.normal_numbers:
	mov rax, [rbp]
	mov rdx, [rbx]
	and r15w, REMOVE_SIGN_BIT_WORD
	and r14w, REMOVE_SIGN_BIT_WORD
	mov cx, r15w
	sub_64 cx, r14w, r10w, r11w
	bt cx, 15
	jnc .correct_2nd_mantissa
		; correct 1st
		negative_64 cx, r10w, r11w
		test ch, ch
		jnz .return_2st_summand
		shr rax, cl
		shr rax, 1       ; for sign
		test rax, rax
		jz .return_2st_summand
		shr rdx, 1       ; for sign
		mov r15w, r14w
		jmp .equal_exp
	.correct_2nd_mantissa:
		test ch, ch
		jnz .done
		shr rdx, cl
		shr rdx, 1     ; for sign
		test rdx, rdx
		jz .done
		shr rax, 1     ; for sign
	.equal_exp:

	mov cx, [rbp + 8]
	bt cx, 15
	jnc .first_summand_is_positive
		negative_64 rax
	.first_summand_is_positive:
	mov cx, [rbx + 8]
	bt cx, 15
	jnc .second_summand_is_positive
		negative_64 rdx
	.second_summand_is_positive:

	add_64 rax, rdx

	test r13w, r13w
	jz .equal_signs
		; different signs
		test rax, rax
		jz .zero
		bt rax, 63
		setc cl
		shl cx, 15
		shl rax, 1        ; remove sign bit from mantissa
		jnc .normalize_mantissa
			negative_64 rax
		.normalize_mantissa:
		bt rax, 63
		jc .set_sign
		test r15w, r15w
		jz .zero          ; underflow
		shl rax, 1
		mov r13w, 1
		sub_64 r15w, r13w, r10w, r11w
		jmp .normalize_mantissa

	.equal_signs:
		mov cx, [rbp + 8]
		and cx, GET_SIGN_BIT_WORD      ; sign of result
		bt cx, 15
		jnc .skip_neg
			negative_64 rax
		.skip_neg:
		bt rax, 63
		jc .inc_exp
			shl rax, 1
		jmp .set_sign
		.inc_exp:
			mov r13w, 1
			add_64 r15w, r13w, r11w
			cmp r15w, FLOAT80_INF_NAN_EXP
			je .return_inf
		jmp .set_sign

	.set_sign:
		or r15w, cx
	jmp .write_result

	.zero:
		mov [rbp], rax
		mov [rbp + 8], ax
	jmp .done
	.return_inf:
		mov [rbp + 8], r15w
		mov rax, FLOAT80_INF_MANTISSA
		mov [rbp], rax
	jmp .done

	.write_result:
		mov [rbp + 8], r15w
		mov [rbp], rax

	.done:
	pop r14
	pop r15
	pop rdx
	pop rcx
	pop rax
	pop rbx
	pop r13
	pop rbp
	ret

sub_floats:
; Substraction for floats in extended (80 bit) format
; Input via references in stack
; Damages r10 and r11
	push rbp
	push r13
	mov rbp, rsp
	mov r13, 0x18
	add_64 rbp, r13
	push rbx
	mov rbx, [rbp + 8]    ; subtrahend (second num)
	mov rbp, [rbp]

	mov r13w, [rbx + 8]
	btc r13w, 15          ; change sign of subtrahend
	mov [rbx + 8], r13w

	push rbx
	push rbp
	call add_floats
	pop rbp
	pop rbx

	btc r13w, 15          ; change sign of subtrahend back
	mov [rbx + 8], r13w

	pop rbx
	pop r13
	pop rbp
	ret
