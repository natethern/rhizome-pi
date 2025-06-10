; @(#)$Id: chkpt.asm,v 1.1 1998/07/31 10:38:21 qfwfq Exp $

; $Log: chkpt.asm,v $
; Revision 1.1  1998/07/31 10:38:21  qfwfq
; In Win32, make method of calling API somewhat more 'legal'
; Hoping that it makes program more robust.
;

	.386

_TEXT	segment dword public use32 'CODE'

_rk_cr_chkpt	proc near
	mov eax,4[esp]
	mov edx,0[esp]
	mov 0[eax],edx
	mov 4[eax],ebx
	mov 8[eax],esp
	mov 12[eax],ebp
	mov 16[eax],esi
	mov 20[eax],edi
	xor eax,eax
	ret
_rk_cr_chkpt	endp

_rk_cr_switch	proc near
	mov edx,4[esp]
	mov eax,8[esp]
	mov ecx,0[edx]
	mov ebx,4[edx]
	mov esp,8[edx]
	mov ebp,12[edx]
	mov esi,16[edx]
	mov edi,20[edx]
	mov [esp],ecx
	ret
_rk_cr_switch	endp

public	_rk_cr_chkpt, _rk_cr_switch

_TEXT	ends

	end
