;------------------------------------------------
; ctype.inc
;
;		Copyright (c) 1992 by c.mos
;------------------------------------------------

;----- Equations -----

_10		equ	00000010b		; 10 digit
_UP		equ	00000100b		; upper case
_LO		equ	00001000b		; lower case
_16		equ	00010000b		; hexa decimal
_FN		equ	10000000b		; file name char


;----- Is upper/lower/alpha/digit/kanji? -----
;--> AL :char
;<-- CY :yes

		public	isupper,isalpha,islower,isdigit,iskanji
isupper:	cmp	al,'A'
		jb	isnot
		cmp	al,'Z'+1
		ret

isalpha:	call	isupper
		jc	isal9
islower:	cmp	al,'a'
		jb	isnot
		cmp	al,'z'+1
isal9:		ret

isdigit:	cmp	al,'0'
		jb	isnot
		cmp	al,'9'+1
		ret

iskanji:	ifkanji yes
isnot:		clc
		ret

;----- Is file name? -----
;--> AL :char (20h<=AL<80h)
;<-- CY :file name

		public	isfilename
isfilename	proc
		pushm	<bx,cx>
		mov	cl,al
		and	cl,7
		mov	ch,80h
		shr	ch,cl
		mov	bl,al
		clr	bh
		shrm	bx,3
		add	bx,offset cgroup: fnbittbl - 4
		test	byte ptr [bx],ch
		popm	<cx,bx>
		jz	isfl9
yes:		stc
isfl9:		ret
isfilename	endp

fnbittbl	db	01011111b,11000100b,11111111b,11000000b
;			 !"#$%&'  ()*+,-./  01234567  89:;<=>?
		db	11111111b,11111111b,11111111b,11100011b
;			@ABCDEFG  HIJKLMNO  PQRSTUVW  XYZ[\]^_
		db	11111111b,11111111b,11111111b,11110110b
;			`abcdefg  hijklmno  pqrstuvw  xyz{|}~

;------------------------------------------------
;	End of ctype.inc
;------------------------------------------------
