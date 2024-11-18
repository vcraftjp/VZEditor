;****************************
;	'menu.asm'
;****************************

	include	vz.inc

;--- Equations ---

LOC_TTL		equ	0FF01h
OPE_GET		equ	0
OPE_NOT		equ	8
OPE_EQU		equ	31
ITEM_DRAW	equ	0
ITEM_KEY	equ	1
SEL_CR		equ	0
SEL_KEY		equ	1
CHECKMARK	equ	'*'

_item		struc
it_num		db	?
it_type		db	?
_item		ends

	cseg

;--- External symbols ---

	extrn	mg_offon	:byte
	extrn	windloc		:byte

	extrn	ckeymsg		:word
	extrn	ckeytbl		:word
	extrn	macrobuf	:word
	extrn	menubit		:word
	extrn	refloc		:word
	extrn	tboxtbl		:word
	extrn	wbuf		:word

	extrn	dispframe	:near
	extrn	dispkeysym	:near
	extrn	doswindow	:near
	extrn	editloc		:near
	extrn	fillset		:near
	extrn	fillspc		:near
	extrn	getc		:near
	extrn	getloc		:near
	extrn	gets		:near
	extrn	getwindow	:near
	extrn	issilent	:near
	extrn	loadwloc	:near
	extrn	locate		:near
	extrn	popwindow	:near
	extrn	printf		:near
	extrn	pushwindow	:near
	extrn	putc		:near
	extrn	puts		:near
	extrn	putspc		:near
	extrn	puts_s		:near
	extrn	puts_t		:near
	extrn	redraw		:near
	extrn	savewloc	:near
	extrn	scannum		:near
	extrn	schcmdsym	:near
	extrn	schmacro	:near
	extrn	schmenu		:near
	extrn	setatr		:near
	extrn	setdoswindow	:near
	extrn	setoptval	:near
	extrn	setwindow	:near
	extrn	skipchar	:near
	extrn	skipstr		:near
	extrn	strlen		:near
	extrn	strskip		:near
	extrn	toupper		:near

	endcs

	dseg

;--- Local work ---

mn_top		db	0		; top y
mn_h		db	0		; display height

	endds

	eseg
	assume	ds:cgroup

;--- Constants ---

pf_deci		db	"%d",0

;****************************
;	Pop up menu
;****************************
;-->
; BX :menu record ptr
; DL,DH :window top-left (0:use refloc)
; SI :menu title
;<--
; NC :
;   AL :select item No.
;   AH :select by... 0=CR, 1=Direct key
; CY :(AH=0FFh)
;   AL :-1=escape, 0=ack CR, else illegual key

	public	popupmenu
popupmenu proc
	push	ds
	movseg	ds,cs
	tstb	[bx].mn_c
	stc
	jz	popmn9
	call	savewloc
	mov	cx,word ptr [bx].mn_wd
	call	adjwin
	mov	[bx].mn_wd,cl
	clr	cl			; mn_top
	mov	word ptr mn_top,cx
	call	drawborder
	call	do_menu
	mov	menubit,0
	call	endwindow
	call	loadwloc
popmn9:	pop	ds
	ret
popupmenu endp

endwindow proc
	pushf
	push	ax
	call	popwindow
	pop	ax
	popf
	ret
endwindow endp

;--- System menu ---
;--> CL :system menu No.

	public	sysmenu
sysmenu	proc
	pushm	<dx,ds>
	movseg	ds,cs
	mov	dl,cl
	call	schmenu
	popm	<ds,dx>
	jc	sysmnu9
	mov	bx,si
	add	si,type _menu
	call	popupmenu
sysmnu9:ret
sysmenu	endp

;--- Do menu ---
;-->
; BX :menu record ptr
;<--
; CY :escape
; AL,AH :return value

do_menu	proc
domn1:
	call	drawmenu
domn2:	mov	al,CSR_OFF
	call	getc
	tst	al
	jz	domn_k
	mov	cl,[bx].mn_sel
	mov	dx,word ptr mn_top
	add	dh,dl
	cmp	al,CM_D
	je	domn_d
	cmp	al,CM_U
	je	domn_u
	cmp	al,CM_CR
	je	domn_cr
	tst	cl
	js	domn4
	cmp	al,SPC
	je	domn_esc
	ja	domn_cmd
domn4:	cmp	al,CM_ESC
	jne	domn2
domn_esc:
	mov	ax,INVALID
	stc
	ret
domn_x:	mov	ah,0
domn_k:	mov	al,ah
	mov	ah,-1
	stc
	ret
domn_cr:	
	tst	cl
	js	domn_x
	mov	al,[bx].mn_sel
	mov	ah,SEL_CR
	jmps	domn9
domn_d:	
	tst	cl
_if s
	cmp	dh,[bx].mn_c
  _if b
	inc	dl
  _endif
_else
	inc	cl
	cmp	cl,[bx].mn_c
  _if ae
	clr	cl
  _endif
_endif
	jmps	domn5

domn_u:	
	tst	cl
_if s
	tst	dl
  _ifn z
	dec	dl
  _endif
_else
	dec	cl
  _if s
	mov	cl,[bx].mn_c
	dec	cl
  _endif
_endif
domn5:	mov	[bx].mn_sel,cl
	mov	mn_top,dl
	jmp	domn1

domn_cmd:
	cmp	cl,-1
	je	domn2
	mov	di,[bx].mn_ext
	tst	di
	jz	domnc1
	mov	ah,ITEM_KEY
	call	extfunc
	jc	domnc0
	cmp	al,[bx].mn_c
	jb	domnc5
domnc0:	jmp	domn2
domnc1:
	clr	dl
	mov	ah,al
domnc2:	push	ax
	mov	di,bx
	add	di,type _menu
	call	setmsgp
	pop	ax
	call	chkkey
_ifn e
	inc	dx
	cmp	dl,[bx].mn_c
	jb	domnc2
	jmp	domn2
_endif
	mov	al,dl
domnc5:	mov	[bx].mn_sel,al
	mov	ah,SEL_KEY
domn9:	clc
	ret
do_menu	endp

extfunc proc
	pushm	<bx,cx,dx,ds>
	call	di
	popm	<ds,dx,cx,bx>
	ret
extfunc endp

;--- Check 1st key ---
;--> AH :1st key
;<-- ZR :found

chkkey	proc
	lodsb
	tst	al
	jle	chkky_x
	cmp	al,SPC
	jbe	chkkey
	cmp	al,5Fh
	ja	chkkey
	cmp	al,ah
	ret
chkky_x:clz
	ret
chkkey	endp

;--- Draw menu ---
; BX :menu record ptr

drawmenu proc
	call	issilent
	mov	cx,word ptr mn_top
	mov	al,[bx].mn_sel
	tst	al
_ifn s
	cmp	al,cl
  _if b
	mov	cl,al
	mov	mn_top,al
  _else
	add	ch,cl
	cmp	al,ch
    _if ae
	sub	al,ch
	inc	al
	add	cl,al
	mov	mn_top,cl
    _endif
  _endif
_endif
	clr	dh
drmn1:
	clr	dl
	call	locate
	call	fillset
	call	itematr
	mov	ax,1
	shl	ax,cl
	test	menubit,ax
	mov	al,SPC
_ifn z
	mov	al,CHECKMARK
_endif
	call	putc
	mov	di,[bx].mn_ext
	tst	di
_ifn z
	mov	al,cl
	mov	ah,ITEM_DRAW
	call	extfunc
_else
	mov	di,bx
	add	di,type _menu
	mov	dl,cl
	call	setmsgp
	call	drawitem
_endif
	mov	dl,[bx].mn_wd
	call	fillspc
	inc	dh
	inc	cl
	cmp	dh,mn_h
	jmpl	b,drmn1
	ret
drawmenu endp

dritm1:
	call	putc
drawitem proc
	mov	al,[si]
	tst	al
	jle	dritm8
	inc	si
	cmp	al,SPC
	jbe	dritm1
	cmp	al,5Fh
	ja	dritm1
	cmp	cl,[bx].mn_sel
_ifn e
	push	ax
	mov	al,ATR_W1ST
	call	setatr
	pop	ax
_endif
	call	putc
	call	itematr
dritm8:	call	puts_t
dritm9:	ret
drawitem endp

;--- Set item attr ---
; CL :item No.

itematr	proc
	mov	al,ATR_WTXT
	cmp	cl,[bx].mn_sel
_if e
	mov	al,ATR_WSEL
_endif
	call	setatr
	ret
itematr	endp

;--- Draw border ---
; SI :title

drawborder proc
	call	issilent
	mov	al,ATR_WFRM
	call	dispframe
	tst	si
_ifn z
	tstb	[si]
  _ifn z
	mov	al,ATR_WTTL
	call	setatr
	mov	dx,LOC_TTL
	call	locate
	call	putspc
	call	puts_t
	call	putspc
  _endif
_endif
	ret
drawborder endp

;--- Set message ptr ---
;-->
; DI :message pool ptr
; DL :message No.
;<--
; SI :message ptr

	public	setmsgp
setmsgp	proc
	pushm	<cx,dx,di,es>
	movseg	es,ds
	inc	dl
_repeat
	call	skipstr
	dec	dl
_until z
	mov	si,di
	popm	<es,di,dx,cx>
	ret
setmsgp	endp

;--- Adjust window position ---
;-->
; DL,DH :window position
; CL,CH :window size
; SI :title str
;<--
; DL,DH :adjusted positon

adjwin	proc
	push	bx
	pushm	<cx,dx>
	call	doswindow
	mov	bx,cx
	sub	bx,0304h		; ##156.86
	popm	<dx,cx>
	tst	dx
_if z
	mov	dx,refloc
	inc	dh
_endif
	tst	si
_ifn z
	call	strlen
	add	al,4
	cmp	al,cl
  _if a
	mov	cl,al
  _endif
_endif
	mov	al,windloc
	clr	ah
	call	adjwin1
	xchg	bh,bl
	xchg	ch,cl
	xchg	dh,dl
	shr	al,1
	mov	ah,1
	call	adjwin1
	xchg	bh,bl
	xchg	ch,cl
	xchg	dh,dl
	call	setwindow
	mov	refloc,dx
	call	pushwindow
	pop	bx
	ret

adjwin1:
	cmp	cl,bl
_if a
	mov	cl,bl
_endif
	cmp	cl,1
_if b
	mov	cl,1
_endif
	test	al,0100b		; center
_ifn z
	mov	dl,bl
	sub	dl,cl
	shr	dl,1
_else
	cmp	dl,ah
	jb	adjw1
	test	al,0001b		; left
  _ifn z
adjw1:	mov	dl,ah
  _else
	mov	ah,dl
	add	ah,cl
	sub	ah,bl
    _if a
	sub	dl,ah	
    _endif
  _endif
_endif
	inc	dl
	ret	
adjwin	endp

;--- Get string from window ---
;-->
; DL :title No. (if zero,BX=title str)
; SI :buffer ptr
; CX :string length
;<--
; NC :enter, CY :escape
; CX :get string length

	public	windgets,windgetsc,windgets1
windgets proc
	mov	al,GETS_INIT
	jmps	windgets1
windgetsc:
	mov	al,GETS_COPY
windgets1:
	pushm	<dx,ds>
	movseg	ds,cs
	call	savewloc
	pushm	<ax,si,cx>
	tst	dl
_ifn z
	mov	di,tboxtbl
	dec	dl
	call	setmsgp
	lodsb				;1st byte is window size
	mov	cl,al
_else
	mov	si,bx
	clr	cl
_endif
	mov	ch,1
	clr	dx
	call	adjwin
	call	drawborder
	popm	<cx,si,ax>
	call	gets
	call	endwindow
	call	loadwloc
	popm	<ds,dx>
	ret
windgets endp

	endes

IFNDEF NOFILER

	cseg

;****************************
;	Menu bar
;****************************
;--- Draw menu bar ---
;--> BX :menu bar record ptr

	public	drawmbar
drawmbar proc
	push	ds
	call	getwindow
	pushm	<cx,dx>
	call	setdoswindow
	movseg	ds,cs
	mov	dx,word ptr [bx].mb_px
	push	dx
	clr	dl
	call	locate
	call	fillset
	mov	al,ATR_WTXT
	call	setatr
	pop	dx
	call	fillspc
	mov	si,[bx].mb_ttl
	clr	cl
	jmps	drbar3
drbar2:	call	putc	
drbar3:	lodsb
	cmp	al,SPC
	jb	drbar8
	je	drbar2
	push	ax
	mov	al,ATR_W1ST
	call	setatr
	pop	ax
	call	putc
	mov	al,ATR_WTXT
	call	setatr
	call	puts_s
	dec	si
	inc	cl
	cmp	cl,[bx].mb_c
	jb	drbar3
drbar8:	mov	dl,[bx].mb_sx
	call	fillspc
	popm	<dx,cx>
	call	setwindow
	pop	ds
	ret
drawmbar endp

;--- Do menu bar ---
;-->
; BX :menu bar record ptr
; DL :input key
;<--
; CY :not found
; NC,AL :select item No.
; DL,DH :item location

	public	do_mbar
do_mbar	proc
	push	ds
	movseg	ds,cs
	clr	cl
	mov	si,[bx].mb_ttl
	mov	al,dl
	call	toupper
	mov	dl,al
_repeat
	lodsb
	cmp	al,SPC
  _cont e
	jb	dobar_x
	cmp	al,dl
	je	dobar2
	call	skipchar
	inc	cl
	cmp	cl,[bx].mb_c
_while b
dobar_x:stc
	jmps	dobar9
dobar2:	
	mov	ax,si
	dec	ax
	sub	ax,[bx].mb_ttl
	mov	dx,word ptr [bx].mb_px
	add	dl,al
	mov	al,cl
	clc
dobar9:	pop	ds
	ret
do_mbar	endp

	endcs

ENDIF

	eseg

;****************************
;	Command menu
;****************************
;-->
; AX :0=macro menu
; BX :menu record ptr
; SI :default title
;<--
; CY :escape
; AL :item No.
; AH :item type
; SI :item message ptr

	public	cmdmenu
cmdmenu	proc
	tst	ax
	jmpl	z,macromenu	
	mov	[bx].mn_ext,offset cgroup:cmditem
cmdmnu1:
	push	si
	call	msgpoolp
	tstb	[di]
_ifn z
	mov	si,di
_endif
	clr	dx
	call	popupmenu
	jc	cmdmnu9
	push	ax
	mov	dl,al
	call	msgpoolp
	call	setmsgp
	pop	ax
	mov	dh,ah
	clr	ah
	push	ax
	call	cmditemp
	pop	ax
	tstw	[di]
	jz	cmdmnu8
	mov	ax,[di]
	cmp	ah,MCHR_VAR
	jne	cmdmnu8
	pushm	<ax,bx,dx>
	call	click_var
	popm	<dx,bx,ax>
	tst	dh
	jnz	cmdmnu8
	pop	si
	jmp	cmdmnu1
cmdmnu8:clc
cmdmnu9:pop	di
	ret
cmdmenu	endp

msgpoolp proc
	mov	al,[bx].mn_c
cmditemp:
	clr	ah
	shl	ax,1
	mov	di,bx
	add	di,type _menu
	add	di,ax
	ret
msgpoolp endp

;--- Click Variables ---
; AL :var No.

click_var proc
	mov	cl,al
	clr	ch
	mov	al,OPE_GET
	push	cx
	call	setoptval
	pop	cx
	tst	al
	js	clk_num
	mov	al,OPE_NOT
	jmps	clk_sw
clk_num:
	push	cx
	call	windgetval
	pop	cx
	jc	clkvar9
	mov	al,OPE_EQU
clk_sw:
	clr	ch
	call	setoptval
	call	redraw
	call	editloc
	movseg	ds,cs
clkvar9:ret
click_var endp

	public	windgetval,windgetval1
windgetval proc
	clr	dl
	mov	bx,si
windgetval1:
	mov	si,wbuf
	mov	cx,STRSZ
	call	windgets
	mov	dx,-1
	jc	getvar9
	mov	dx,-2
	stc
	jcxz	getvar9
	lodsb
	call	scannum
	jnc	getvar9
getvar8:clr	dx
getvar9:ret
windgetval endp

;--- Get command message ptr ---
;-->
; CL :item No.
;<--
; SI :message ptr

getcmdmsgp proc
	pushm	<cx,di>
	call	msgpoolp
	mov	dl,cl
	call	setmsgp
	tstb	[si]
	jnz	cmdmp8
	mov	al,cl
	call	cmditemp
	mov	ax,[di]
	cmp	ah,MCHR_CALL
	je	cmdmp2
	cmp	ah,MCHR_CMD
	jne	cmdmp8
	mov	si,ckeymsg
	mov	cl,al
_repeat
	lodsb
	tst	al
	jz	cmdmp7
	cmp	al,cl
	je	cmdmp8
	call	strskip
_until
cmdmp2:
	mov	dl,al
	call	schmacro
	jc	cmdmp7
	inc	si
	inc	si
	jmps	cmdmp8
cmdmp7:	clr	si
cmdmp8:	popm	<di,cx>
	ret
getcmdmsgp endp

;--- Command item function ---
;-->
; AH :0=draw (AL :item No)
;     1=Check Direct key (AL :key)
;<-- (AH=1)
; CY :error
; AL :select item No.

cmditem	proc
	tst	ah
	jz	cmddraw
cmdkey:
	clr	cx
	mov	ah,al
cmdkey1:push	ax
	call	getcmdmsgp
	pop	ax
	tst	si
	jz	cmdkey2
	call	chkkey
_ifn e
cmdkey2:inc	cx
	cmp	cl,[bx].mn_c
	jne	cmdkey1
	stc
	ret
_endif
	mov	al,cl
	clc
	ret
cmddraw:	
	tstb	[bx].mn_valwd
	jz	cmddr2
	mov	cl,al
	call	cmditemp
	mov	ax,[di]
	cmp	ah,MCHR_CALL
	je	cmddr_mac
	cmp	ah,MCHR_CMD
	jne	cmddr2
cmddr_cmd:
	push	cx
	mov	ah,'#'
	call	dispkeynum
	call	dispkeycmd
	pop	cx
	jmps	cmddr5
cmddr_mac:
	mov	ah,'&'
	call	dispkeynum
	call	dispkeymac
	jmps	cmddr5
cmddr2:
	call	getcmdmsgp
	tst	si
	jz	cmddr5
	call	drawitem
cmddr5:
	mov	dl,[bx].mn_wd
	sub	dl,[bx].mn_valwd
	call	fillspc
	tstb	[bx].mn_valwd
	jz	cmddr7
	cmp	[di].it_type,MCHR_CMD
	jb	cmddr7
	call	dispval
cmddr7:	mov	dl,[bx].mn_wd
	call	fillspc
	ret
cmditem	endp

;--- Disp value ---
;-->
; SI :item msg end
; DI :item slot ptr

dispval	proc
	pushm	<bx,cx,dx,di>
	mov	ax,[di]
	cmp	ah,MCHR_CMD
	je	val_cmd
	cmp	ah,MCHR_VAR
	je	val_var
	cmp	ah,MCHR_CALL
	jne	dspval8
val_cmd:
	call	getcmdmsgp
	tst	si
	jz	dspval8
	mov	al,':'
	call	putc
	call	puts_t
	jmps	dspval8
val_var:
	mov	cl,al
	clr	ch
	mov	al,OPE_GET
	call	setoptval
	tst	al
	js	valvar1
	tstb	[si-1]
_if z
	mov	si,offset cgroup:mg_offon
_endif
	tst	dx
_ifn z
	call	skipchar
_endif
	call	puts_s
	jmps	dspval8
valvar1:
	call	putval
dspval8:popm	<di,dx,cx,bx>
	ret
dispval	endp

	public	putval
putval	proc
	pushm	<bx,si>
	mov	si,offset cgroup:pf_deci
	push	dx
	mov	bx,sp
	call	printf
	pop	ax
	popm	<si,bx>
	ret
putval	endp

;--- Disp key No. ---
; AL :key No.
; AH :'#' or '&'
; CL :item No.

dispkeynum proc
	cmp	cl,[bx].mn_sel
_ifn e
	push	ax
	mov	al,ATR_HELP
	call	setatr
	pop	ax
_endif
	push	ax
	mov	al,ah
	call	putc
	pop	ax
	push	ax
	aam
	add	ax,3030h
	push	ax
	mov	al,ah
	call	putc
	pop	ax
	call	putc
	call	putspc
	pop	ax
	ret
dispkeynum endp

;--- Disp command key ---
; AL :key No.

dispkeycmd proc
	push	di
	push	ax
	mov	si,ckeytbl
	clr	ah
	dec	ax
	shl	ax,1
	add	si,ax
	lodsw
	call	dispkeysym		; ##1.5
	pop	ax
	call	schcmdsym
_ifn c
	push	ax
	mov	dl,[bx].mn_wd
	sub	dl,[bx].mn_valwd
	sub	dl,2
	call	fillspc
	pop	ax
	call	putc
_endif
	pop	di
	ret
dispkeycmd endp

;--- Disp macro key ---
;--> AL :key No.
;<-- SI :macro ptr

dispkeymac proc
	mov	dl,al
	call	schmacro
_ifn c
dispkeymac1:
	lodsw
	call	dispkeysym
_endif
	ret
dispkeymac endp

;--- Macro menu ---
;-->
; BX :menu record ptr
; SI :default title
;<--
; CY :escape
; AL :item No.
; AH :MCHR_CALL

macromenu proc
edtmac0:mov	[bx].mn_ext,offset cgroup:drawmac
	push	si
	mov	cx,-1
	call	cntmacro
	pop	si
	mov	[bx].mn_c,cl
	cmp	cl,[bx].mn_sel
_if be
	dec	cl
	mov	[bx].mn_sel,cl
_endif
	mov	di,bx
	add	di,type _menu
	tstb	[di]
_ifn z
	mov	si,di
_endif
	clr	dx
	call	popupmenu
_if c
	tst	al
	jns	edtmac0
	not	al
	tst	al
	jz	edtmac_c
	jmps	edtmac8
_endif
	cbw
	mov	cx,ax
	call	cntmacro
	mov	al,[si]
edtmac8:mov	ah,MCHR_CALL
	clc
	ret
edtmac_c:
	clr	si
	stc
	ret
macromenu endp

drawmac proc
	tst	ah
	jz	drmac0
	stc
	ret
drmac0:
	mov	cx,ax
	push	cx
	call	cntmacro
	pop	cx
	lodsb
	mov	ah,'&'
	call	dispkeynum
	inc	si
	inc	si
	call	dispkeymac1
	mov	dl,[bx].mn_wd
	sub	dl,[bx].mn_valwd
	call	fillspc
	mov	al,':'
	call	putc
	tstb	[si]
_if z
	inc	si
_endif
	call	puts
	ret
drawmac endp

;--- Count macro ---
;--> CX :count

cntmacro proc
	mov	si,macrobuf
_repeat
	lodsb
	tst	al
  _break z
	js	cntmac3
	jcxz	cntmac2
	lodsw
	add	si,ax
	dec	cx
  _cont
cntmac2:dec	si
	ret
cntmac3:lodsw
	add	si,ax
_until
	not	cx
	ret
cntmacro endp

	endes
	end

;****************************
;	End of 'menu.asm'
; Copyright (C) 1989 by c.mos
;****************************
