page 60,130
; +--------------------------------------+
; |    Copyright(C) 1993 lucifer         |
; |      For OPTASM.EXE release 1.60     |
; |        & OLINK.EXE  release 1.00     |
; +--------------------------------------+

;======================================================================
_ALL	group	_TEXT,_DATA
_TEXT	segment	word public 'CODE'
	assume	cs:_ALL,ds:_ALL,es:_ALL,ss:_ALL
_TEXT	ends
_DATA	segment	word public 'DATA'
_DATA	ends

_MODULE_MAX	equ	64		; module code size (byte)
LF		equ	BP
VS		equ	BH
HS		equ	BL
HFP		equ	DH
VFP		equ	CH
HBP		equ	DL
VBP		equ	CL
VS_HS		equ	BX
HFP_HBP		equ	DX
VFP_VBP		equ	CX


;======================================================================
;		プログラムセグメント

_TEXT	segment

;----------------------------------------------------------------------
	org	00100h

_module_entry	proc	near

	; Display 'Copyright & kind of CRT'
		mov	dx,offset _ALL:Copyright
		mov	cx,Copyright_size
		cmp	_ALL:[_module_size],_MODULE_MAX + 1
		sbb	ax,ax
		sub	dx,ax
		add	cx,ax
		mov	bx,2
		mov	ah,040h
		int	021h

	; exit
		mov	ax,4c00h
		int	021h

_module_entry	endp


;----------------------------------------------------------------------
	org	0011eh

_module_size	dw	_user_conv_end2 - _user_convert


;----------------------------------------------------------------------
	org	00120h

_user_convert	proc	near
		db	_MODULE_MAX dup (90H)
_user_conv_end	label	near
	org	_user_convert

	include	module.inc	; module00.inc 等に書き換えるか
				; rename しておく事。

_user_conv_end2	label	near
	org	_user_conv_end
_user_convert	endp


;----------------------------------------------------------------------
_TEXT	ends


;======================================================================
;		データセグメント

_DATA	segment

Copyright	db	007h		; For big size module

		db	'Copyright(C) 1993 by '
		db	_PROGRAMER,00dh,00ah
		db	"30行BIOS's CRT module for '"
		db	_MODULE_NAME,"'",00dh,00ah

Copyright_size	equ	$ - Copyright

_DATA	ends

;======================================================================
	end	_module_entry
