;****************************
;	'inst.asm'
;****************************

	include	vz.inc

;--- Equations ---

KSYM_SHIFT	equ	'\'
KSYM_CTRL	equ	'^'
KSYM_ALT	equ	'@'
KSYM_L		equ	'['
KSYM_R		equ	']'
LOCALMAC	equ	80h

XKEYCNT		equ	15
PKEYCNT		equ	6
ELSECNT		equ	12		; ##156.91
HISTCNT		equ	5

	cseg

;--- External symbols ---

	extrn	grctbl		:byte
	extrn	mg_opterr	:byte
	extrn	mg_synerr	:byte
	extrn	pkeytbl		:byte
	extrn	readmac		:byte
	extrn	symtbl		:byte

	extrn	aliasbuf	:word
	extrn	ckeymsg		:word
	extrn	ckeytbl		:word
	extrn	csmax		:word
	extrn	custm_end	:word
	extrn	elsetbl		:word
	extrn	fkeytbl		:word
	extrn	hist_top	:word
	extrn	lbuf		:word
	extrn	lbuf_end	:word	; ##156.117
	extrn	macrobuf	:word
	extrn	macrosz		:word	; ##156.118
	extrn	optptr		:word
	extrn	syssw		:word
	extrn	tboxtbl		:word
	extrn	tempend		:word
	extrn	temptop		:word
	extrn	paltblp		:word

	extrn	cputc		:near
	extrn	cputcrlf	:near
	extrn	cputs		:near
	extrn	disperr		:near
	extrn	dispmsg		:near
	extrn	dspscr		:near
	extrn	getnum		:near
	extrn	insmacro	:near
	extrn	isalpha		:near
	extrn	isdigit		:near
	extrn	isupper		:near
	extrn	memmove		:near
	extrn	putnum		:near
	extrn	scandeciw	:near
	extrn	scannum		:near
	extrn	setoption	:near
	extrn	setoptnum	:near
	extrn	toupper		:near
	extrn	wrdicmp		:near
	extrn	xkeytbl		:near
	extrn	chkmem		:near

	endcs

	dseg

;--- Local work ---

errcnt		db	0
inedit		db	0
enddefp		dw	0
recount		dw	0
storep		dw	0
sysmnu		db	0
lbufovf		db	0		; ##156.117

	endds

	cseg

;--- Customize class table ---

classtbl	db	'C'
		dw	offset cgroup:df_cmdkey
		db	'F'
		dw	offset cgroup:df_fnckey
		db	'T'
		dw	offset cgroup:df_tbox
		db	'O'
		dw	offset cgroup:df_option
		db	'H'
		dw	offset cgroup:df_history
		db	'E'
		dw	offset cgroup:df_else

classtbl2	db	'S'
		dw	offset cgroup:df_sysmenu
		db	'M'
		dw	offset cgroup:df_macro
		db	'P'
		dw	offset cgroup:df_menu
		db	'A'
		dw	offset cgroup:df_alias
		db	0

	endcs

	iseg
	assume	ds:cgroup

;--- Init command key table ---

	public	initckey
initckey proc
	mov	di,ckeytbl
	mov	cx,CMCNT*2
	mov	al,INVALID
    rep	stosb
	mov	di,offset cgroup:pkeytbl
	mov	cx,PKEYCNT
    rep	stosb
	ret
initckey endp

;--- Customize main ---
;--> SI :referece file ptr
;<-- NZ :error

	public	customize
customize proc
	mov	byte ptr [si-1],LF
	mov	errcnt,0
	lodsb
	dec	si
	cmp	al,SPC
_if a
	inc	si
	call	skipline
	test	syssw,SW_DEFTTL
  _ifn z
	call	putttl
  _endif
_endif
	mov	ax,csmax
	dec	ax
	mov	enddefp,ax
	mov	bx,offset cgroup:classtbl
	mov	di,temptop
	call	readclass
	tstb	errcnt
	ret
customize endp

;--- Syntax error ---

	public	synerr
synerr	proc
	inc	errcnt
	clr	bx
	xchg	bx,optptr
	tst	bx
	jnz	opterr
	call	skipline
putttl:	push	si
	mov	byte ptr [si-2],'$'
_repeat
	dec	si
	cmp	byte ptr [si-1],LF
_until e
	tstb	errcnt
_ifn z
	mov	dx,offset cgroup:mg_synerr
	call	cputs
_endif
	mov	dx,si
	call	cputs
	pop	si
	jmps	opte8
opterr:
	mov	dx,offset cgroup:mg_opterr
	call	cputs
	dec	bx
_repeat
	mov	dl,[bx]
	inc	bx
	cmp	dl,SPC
  _break be
	call	cputc
_until
	mov	si,bx
opte8:	call	cputcrlf
	ret
synerr	endp

;--- Insert converted data ---
;--> BX :pointer offset

	public	insertcvt
insertcvt proc
	pushm	<cx,si,di>
	mov	ax,tempend
	sub	ax,temptop
	push	ax
	mov	di,[bx]
	cmp	bx,offset cgroup:ckeymsg
_if ae
	mov	di,[bx+2]
	cmp	di,[bx]
  _ifn e
	dec	di
  _endif
	add	ax,di
	sub	ax,[bx]
_endif
	push	di
	call	movearea
	mov	si,temptop
	pop	di
	pop	cx
	call	memmove
	popm	<di,si,cx>
	ret	
insertcvt endp

;--- Move customize area ---
;-->
; BX :pointer offset
; AX :area size

	public	movearea
movearea proc
	push	bx
	mov	di,[bx]
	mov	si,[bx+2]
	cmp	si,di
	pushf
	add	di,ax
	mov	cx,tempend
	sub	cx,si
	mov	ax,di			; ##154.63
	add	ax,cx
	call	chkmem
	call	memmove
	popf
_if e
	mov	byte ptr [si],0
_endif
	mov	ax,di
	sub	ax,si
_repeat
	inc	bx
	inc	bx
	add	[bx],ax
	cmp	bx,offset cgroup:custm_end - 2	; ##155.70
_while b
	pop	bx
	ret	
movearea endp

;--- Is next class ? ---

nextclass1 proc
	tst	bx
_ifn z
	push	si
	mov	tempend,di
	call	insertcvt
	mov	di,temptop
	pop	si
_endif
	ret
nextclass1 endp

;--- Command key ---

df_cmdkey proc
ckey1:	mov	bx,offset cgroup:ckeymsg
	call	nextclass
	call	scannum
	jc	ckey_x
	dec	si
	mov	bx,dx
	tst	bx
	jz	ckey2
	dec	bx
	shl	bx,1
	add	bx,ckeytbl
ckey2:
	call	skpsp
	je	ckey1
	cmp	al,':'
	je	ckeymsg1
	cmp	al,';'
	je	ckey1
	call	scankey
	jc	ckey_x
	tst	dx
	jz	pkey1
	call	storekey
	jc	ckey_x
	jmp	ckey2
pkey1:
	mov	ah,al
	mov	al,0FFh
	call	isprekey
	jnc	ckey_x
	mov	bx,offset cgroup:pkeytbl
	add	bl,al
	adc	bh,0
	mov	[bx],ah
	jmp	ckey2
ckey_x:	stc
	ret
ckeymsg1:
	mov	al,dl
ckmsg1:	stosb
	lodsb
	cmp	al,SPC
	jb	ckmsg2
	cmp	al,';'
	jne	ckmsg1
ckmsg2:	clr	al
	stosb
	jmp	ckey1
df_cmdkey endp

;--- Function key label ---

df_fnckey proc
fkey1:	mov	al,-1
	stosb
	mov	bx,offset cgroup:fkeytbl
	call	nextclass
	dec	di
	push	di
_repeat
	call	scanstr
_until c
	mov	al,CR
	stosb
	pop	ax
	jmp	fkey1
tbox_x:
fkey_x:	pop	di
	stc
	ret
df_fnckey endp

;--- Text box ---

df_tbox proc
	clr	al
	stosb
tbox1:	mov	bx,offset cgroup:tboxtbl
	call	nextclass
	push	di
	inc	di
	call	scanstr
	jc	tbox_x
	call	scannumc		; mn_wd
	jc	tbox_x
	pop	bx
	mov	[bx],al
	jmp	tbox1
df_tbox endp

;--- Option ---

df_option proc	
opt1:
	clr	bx
	call	nextclass
	cmp	al,'-'
_if e
	lodsb
_endif
opt2:	call	isalpha
	jnc	opt_x
	call	setoption
	jc	opt_x
	inc	recount
	call	skpsp
	je	opt1
	cmp	al,';'
	je	opt1
	jmp	opt2
df_option endp

;--- History ---

histsym		db	"SFCAW"

df_history proc
	clr	bx
hist1:	call	nextclass
	cmp	al,':'
_if e
	cmp	di,temptop
  _if a
	call	nextclass1
  _endif
	call	skpsp
	call	toupper
	push	di
	mov	di,offset cgroup:histsym
	mov	cx,HISTCNT
  repne scasb
	pop	di
	jne	hist_x
	mov	bx,HISTCNT-1
	sub	bx,cx
	shl	bx,1
	add	bx,offset cgroup:hist_top
hist2:	call	skipline
hist3:	lodsb
_endif
	call	skpsp1
	je	hist1
	cmp	al,'"'
_if e
	call	scanstr1
	jmp	hist3
_endif
	cmp	al,';'
	je	hist2
_repeat
	stosb
	lodsb
	cmp	al,SPC
_until be
	clr	al
	stosb
	jmp	hist3
opt_x:
hist_x:
else_x:	stc
	ret
df_history endp

;--- Else ---

df_else proc
else1:	clr	bx
	call	nextclass
	call	scannum
	jc	else_x
	cmp	dx,ELSECNT
	ja	else_x
	cmp	dx,10			; ##155.77
	je	df_grctbl
	cmp	dx,12			; ##156.91
	je	df_paltbl
	cmp	dx,11
_if e
	dec	dx
_endif
	dec	dx
	js	else_x
	shl	dx,1
	mov	bx,offset cgroup:elsetbl
	add	bx,dx
	call	skpsp1
	je	else1
	mov	di,temptop
	call	copytoeol
	push	si
	mov	tempend,di
	call	insertcvt
	pop	si
	jmp	else1
df_grctbl:
	mov	di,offset cgroup:grctbl
	jmps	dftbl1
df_paltbl:				; ##156.91
	mov	di,paltblp
	tst	di
	jz	else1
dftbl1:	call	scanstr
	jmp	else1
df_else endp

	endis

	cseg
	assume	ds:nothing

;--- Store key code  ---

storekey proc
	cmp	dx,CM_COMMON
	jb	stkey1
	cmp	dx,CM_FILER
	jae	stkey1
storekey1:
	call	isprekey
_if c
	call	scanpkey
	jc	stkey9
	cmp	al,INVALID
  _if e
	mov	al,dl
	jmps	stkey1
  _endif
	tst	al
	js	stkey2
_endif
stkey1:
	tst	al
	js	stkey3
	cmp	byte ptr es:[bx],INVALID
_if e
stkey2:	mov	es:[bx],al
_else
stkey3:	mov	es:[bx+1],al
_endif
	clc
stkey9:	ret
storekey endp

;--- Macro key ---

df_macro proc
	mov	readmac,0
mac1:
	clr	al
	stosb
	mov	bx,offset cgroup:macrobuf
	call	nextclass
	dec	di
	push	di
	call	scannum
	jc	mac_x1
	cmp	al,':'
_if e
	or	dl,LOCALMAC
_else
	tstb	readmac
  _if z
	mov	readmac,dl
  _endif
_endif
	mov	es:[di],dl
	inc	di
	inc	di
	inc	di
	mov	bx,di
	tst	dl
_ifn s
	mov	word ptr es:[bx],INVALID
  _repeat
	call	skpsp1
	call	scankey
    _break c
	mov	dx,ax
	call	storekey1
mac_x1:	jc	mac_x
	lodsb
  _until
	dec	si
	inc	di
	inc	di
	call	scanstr0
	jc	mac_x
_endif
mac4:
	lodsb
	cmp	al,LF
	je	mac5
	cmp	al,SPC
	jbe	mac4
	cmp	al,';'
	je	mac_rem
	stosb
	ja	mac4
	cmp	al,'"'
	je	mac_str
	cmp	al,SYMCHR
	jne	mac4	
mac_chr:lodsb
	stosb
	cmp	al,SYMCHR
	jne	mac_chr
	jmp	mac4
mac_str:call	scanstr1
	jmp	mac4
mac_rem:call	skipline
mac5:
	tstb	inedit
_ifn z
	cmp	di,lbuf_end
  _if ae
	mov	lbufovf,TRUE
	jmps	mac_x
  _endif
_endif
	cmp	si,enddefp
	jae	mac6
	mov	al,[si]
	cmp	al,'*'
	je	mac6
	call	isdigit
	jnc	mac4
mac6:	clr	al
	stosb
	mov	ax,di
	sub	ax,bx
	mov	es:[bx-2],ax
	pop	ax
	jmp	mac1
mac_x:
	pop	di
	stc
	ret
df_macro endp

;--- Pop up menu ---

df_sysmenu:
	mov	sysmnu,TRUE
df_menu proc
menu1:	clr	al
	stosb
	mov	bx,offset cgroup:macrobuf
	call	nextclass
	dec	di
	call	scannum
	jmpl	c,menu_x
	or	dl,MENUMASK
	mov	es:[di],dl
	inc	di
	inc	di
	inc	di
	push	di
	pushm	<si,di>
	call	scanstr
	pop	di
	jc	menu_x21
	call	scannumc		; mn_wd
	jc	menu_x21
	stosb
	call	scannumc		; mn_c
menu_x21:jmpl	c,menu_x2
	stosb
	mov	cx,ax
	push	di
	clr	ax
	stosw
	stosw
	call	scannumc		; mn_valwd
	jc	menu2
	pop	di
	push	di
	stosb
	call	scannumc		; mn_sel
	jc	menu2
	stosb
menu2:	
	pop	di
	add	di,4
	clr	bx
	tstb	sysmnu
_if z
	mov	bx,di			; BX :item ptr
	push	cx
	clr	ax
    rep	stosw				; DI :message ptr
	pop	cx
_endif
	mov	ax,si
	pop	si
	push	ax
	lodsb
	call	scanstr
	pop	si
;	dec	si
	jcxz	menu70
menu3:
	call	skpsp
	je	menu3
	call	scanstr
_if c
	push	ax
	clr	al
	stosb
	pop	ax
_else
	call	skpsp
	je	menu6
	dec	si
	cmp	al,','
	jne	menu6
	inc	si
	call	skpsp
_endif
	cmp	al,'A'
	jae	menu5
	mov	ah,al
	sub	al,SPC
	js	menu_x1
	push	bx
	mov	bx,offset cgroup:symtbl
	xlat	symtbl
	pop	bx
	xchg	ah,al
	tst	ah
	js	menu6
	and	ah,00001111b
	jz	menu_x1
	cmp	ah,4
	ja	menu_x1
	cmp	ah,3			; %
_ifn e
	push	ax
	call	scandeciw
	pop	ax
_else
	lodsb
menu5:	pushm	<cx,di>
	call	setoptnum
	mov	dl,cl
	popm	<di,cx>
	jc	menu_x1
	mov	ah,3
_endif
	mov	al,dl
	mov	es:[bx],ax
menu6:
	inc	bx
	inc	bx
	loop	menu3
menu70:
	pop	bx
	mov	ax,di
	sub	ax,bx
	mov	es:[bx-2],ax
	jmp	menu1
menu_x2:inc	sp
	inc	sp
menu_x1:pop	di
	dec	di
	dec	di
	dec	di
menu_x:	stc
	ret
df_menu endp

;--- Alias ---

df_alias proc
alias1:
	clr	al
	stosb
	mov	bx,offset cgroup:aliasbuf
	call	nextclass
	dec	di
	call	skpsp1
	je	alias1
	call	copytoeol
	jmps	alias1
df_alias endp

;--- Copy to end of line ---

copytoeol proc
cpeol1:	stosb
cpeol2:	lodsb
	cmp	al,CR
	je	cpeol2
	cmp	al,LF
	jne	cpeol1
	clr	al
	stosb
	ret
copytoeol endp

;--- Skip space,tab ---
;<-- ZR :next line

skpsp	proc
	lodsb
skpsp1:	cmp	al,SPC
	ja	skps9
	cmp	al,LF
	jne	skpsp
skps9:	ret
skpsp	endp

;--- Skip to next line ---

skipline proc
	pushm	<cx,di,es>
	movseg	es,ds
	mov	di,si
	dec	di
	mov	al,LF
	mov	cx,-1
  repne	scasb
	mov	si,di
	popm	<es,di,cx>
	ret
skipline endp

;--- Scan connma & numerics ---
;<--
; CY :no more data
; AX :result data

scannumc proc
	call	skpsp
	cmp	al,';'
_if e
	call	skipline
	stc
	ret
_endif
	cmp	al,','
_if e
	call	skpsp
	call	scannum
	mov	ax,dx
	dec	si
	clc
	ret
_endif
	dec	si
	stc
	ret
scannumc endp

;--- Scan string ---
;-->
; DI :store string ptr
;<--
; CY :error
; SI :placed on next char

scanstr0:
	lodsb
scanstr proc
	call	skpsp1
	je	scstr_x
	cmp	al,','
	je	scanstr0
	cmp	al,'"'
	je	scanstr1
scstr_x:stc	
	ret
scans1:
	stosb
scanstr1:
	lodsb
	cmp	al,LF
	je	scans8
scans2:	cmp	al,'"'
	je	scans8
	cmp	al,'$'
	jne	scans1
	lodsb
	cmp	al,'$'			; ##1.5
	je	scans1
	cmp	al,'('
	je	scancode
	cmp	al,'"'
	je	scans1
	cmp	al,SPC
	jbe	scans_cr
	mov	ah,al
	mov	al,'$'
	stosw
	jmp	scanstr1
scans8:	clr	al
	stosb
	ret
scancode:
	mov	al,'$'
	call	scannum
	jc	scanstr1
	xchg	al,dl
	stosb
	cmp	dl,','
	je	scancode
	jmp	scanstr1
scans_cr:
	lodsb
	cmp	al,SPC
	jbe	scans_cr
	jmp	scans2
scanstr endp

;--- Scan key symbol ---
;-->
; AL :1st char
;<--
; CY :error
; AL :key code
; SI :placed on next char

scankey	proc
	pushm	<bx,dx,di>
	clr	dl
	cmp	al,KSYM_CTRL
	je	scank1
	mov	dl,10100000b
	cmp	al,KSYM_SHIFT
	je	scank1
	mov	dl,00100000b
	cmp	al,KSYM_ALT
	je	scank1
	mov	dl,10000000b
	cmp	al,KSYM_L
	je	scank2
	jmps	scank_x
scank1:	
	lodsb
	cmp	al,KSYM_L
	jne	scank11
	cmp	byte ptr [si],SPC
	jbe	scank11
	cmp	byte ptr [si+1],SPC
	ja	scank2
scank11:tst	dl
	js	scank_x
	call	toupper
	sub	al,40h
	jb	scank_x
	cmp	al,1Fh
	ja	scank_x
	jmps	scank_o
scank2:	
	tst	dl
	js	scank3
	mov	dl,11000000b
	jz	scank3
	mov	dl,11100000b
scank3:
	lodsb
	call	toupper
	cmp	al,'F'
	je	scanfk
	dec	si
	push	es
	movseg	es,cs
	mov	di,offset cgroup:xkeytbl
	mov	cx,XKEYCNT
_repeat
	mov	bx,di
	add	bx,4
	call	wrdicmp
	cmp	di,bx
	je	scanxk5
	cmp	byte ptr es:[di],SPC
	je	scanxk5
	mov	di,bx
_loop
	pop	es
	jmps	scank_x
scanxk5:
	pop	es
	mov	si,ax
	mov	al,XKEYCNT+10h
	sub	al,cl
	jmps	scank_o
scanfk:
	push	dx
	lodsb
	call	scannum
	mov	ah,al
	mov	al,dl
	pop	dx
	jc	scank_x
	cmp	ah,KSYM_R
	jne	scank_x
scank_o:
	or	al,dl			; clc
	jmps	scank9
scank_x:
	stc
scank9:	popm	<di,dx,bx>
	ret
scankey	endp

;--- Check prefix key ---
;--> AL :key code
;<-- CY :prefix key (AL :key No.)

isprekey proc
	pushm	<cx,di,es>
	movseg	es,cs
	mov	di,offset cgroup:pkeytbl
	mov	cx,PKEYCNT
  repne scasb
	clc
_if e
	sub	cx,PKEYCNT-1
	neg	cx
	mov	al,cl
	stc
_endif
	popm	<es,di,cx>
	ret	
isprekey endp

;--- Scan follow key ---
;-->
; AL :prefix key No.
; DX :key code
;<--
; NC,AX :key code
; CY :error

scanpkey proc
	mov	ah,al
	add	ah,2
	rorm	ah,3
	lodsb
	cmp	al,'0'			; [ESC]0
	je	pkeyn
	call	toupper
	sub	al,40h
	jb	pkey_x
	cmp	al,1Fh
	ja	pkey_x
	add	al,ah
	clr	ah
	clc
	ret
pkey_x:	stc
	ret
pkeyn:	mov	al,INVALID
	ret
scanpkey endp

;--- Re-customize ---

	public	se_recust
se_recust proc
	mov	recount,0
	mov	inedit,TRUE	
	call	getnum
	mov	ax,[bp].tend
	dec	ax
	mov	enddefp,ax
	mov	si,[bp].ttop
	mov	bx,offset cgroup:classtbl2
	mov	di,lbuf
	inc	di
	inc	di
	movseg	es,ss
	call	readclass
_if c
	mov	[bp].tcp,si
	call	disperr
	call	putnum
	stc
_else
	push	macrosz			; ##156.118
	push	recount
	mov	bx,sp
	mov	dl,M_RECUST
	call	dispmsg
	add	sp,4
	clc
_endif
	pushf
	call	dspscr	
	popf
	ret
se_recust endp

;--- Check next class ---

nextclass proc
nclas1:	call	skipline
	tstb	inedit
_ifn z
	push	si
	call	recust
	pop	si
_endif
nclas2:	cmp	si,enddefp
	jae	nclas3
	call	skpsp
	je	nclas2
	cmp	al,';'
	je	nclas1
	cmp	al,'*'
	jne	nclas9
nclas3:
	dec	si
	tstb	inedit
_if z
	call	nextclass1
_else
	dec	di
_endif
	inc	sp
	inc	sp
	clc
nclas9:	ret
nextclass endp

;--- Copy to buffer ---

recust	proc
	mov	cx,di
	mov	si,lbuf
	inc	si
	inc	si
	inc	si
	push	si
	sub	cx,si
	jle	recust9
	tst	bx
	jz	recust9
	inc	recount
	dec	si
	inc	cx
	mov	di,storep
	pushm	<ds,es>
	movseg	ds,ss
	movseg	es,cs
	cmp	bx,offset cgroup:macrobuf
_if e
	mov	al,sysmnu
	call	insmacro
_else	
	call	rec_alias
_endif
	popm	<es,ds>
	jc	re_ovflow
	mov	storep,ax
recust9:pop	di
	ret

re_ovflow:
	popm	<ax,ax,si,ax,ax,ax,ax>	; !!!
	mov	dl,E_NOBUFFER
	stc
	ret
recust	endp

rec_alias proc
	tst	di
_if z
	mov	di,cs:[bx]
_endif
	mov	ax,di
	add	ax,cx
	cmp	ax,cs:[bx+2]
	ja	reals_x
	call	memmove
	dec	ax
	clc
	ret
reals_x:stc
	ret
rec_alias endp

;--- Read class symbol ---
;-->
; DX :text end
; BX :class table
; DI :store ptr
;<--
; CY :error

class0:
	call	skipline
readclass proc
class1:	cmp	si,enddefp
	jae	class8
	lodsb
	cmp	al,'*'
	jne	class0
	call	skpsp
	je	class8
	call	toupper
	call	isupper
	jnc	class0
	mov	storep,0
	mov	sysmnu,0
	push	bx
class2:
	cmp	al,cs:[bx]
	je	class3
	inc	bx
	inc	bx
	inc	bx
	tstb	cs:[bx]
	jnz	class2
	pop	bx
	tstb	inedit
	jnz	class0
	stc
	ret
class3:
	push	bx
	call	cs:[bx+1]
	pop	ax
	pop	bx
	jnc	class0
	tstb	inedit
_if z
	pushm	<bx,ax,di>
	call	synerr
	popm	<di,bx>
	jmp	class3
_endif
	mov	dl,E_RECUST
	clr	al
	xchg	lbufovf,al
	tst	al
_ifn z
	mov	dl,E_NOLINE
_endif
	stc
	ret
class8:	clc
	ret
readclass endp

	endcs
	end

;****************************
;	End of 'inst.asm'
; Copyright (C) 1989 by c.mos
;****************************
