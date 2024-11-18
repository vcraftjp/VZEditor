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

;--- External symbols ---

	wseg
	extrn	windloc		:byte

	extrn	ckeymsg		:word
	extrn	ckeytbl		:word
	extrn	macrobuf	:word
	extrn	macrosz		:word
	extrn	menubit		:word
	extrn	refloc		:word
	extrn	tboxtbl		:word
	extrn	wbuf		:word
	extrn	drawfunc	:word
	extrn	retval		:word
	extrn	head1st		:word
	extrn	readmac		:word
	endws

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
	extrn	schmdlmac	:near
	extrn	setatr		:near
	extrn	setdoswindow	:near
	extrn	setoptval	:near
	extrn	setwindow	:near
	extrn	skipchar	:near
	extrn	skipstr		:near
	extrn	strlen		:near
	extrn	strskip		:near
	extrn	toupper		:near
	extrn	set_schopt	:near
	extrn	dspscr		:near
	extrn	cvt_cmdsym	:near
	extrn	schsysmenu	:near
	extrn	cnt_module	:near
	extrn	isrun		:near

	dseg

;--- Local work ---

titlep		dw	0
mn_top		db	0		; top y
mn_h		db	0		; display height
mg_offon	db	'OFF ON',0
macmenu		db	0
mdlnum		dw	0

	endds

	eseg
	assume	ds:cgroup

;--- Constants ---

pf_deci		db	"%d",0
pf_size		db	"%5u",0

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
	movseg	ds,ss
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
	mov	ss:retval,ax		; ##16
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
	movseg	ds,ss
	mov	dl,cl
	call	schsysmenu
	popm	<ds,dx>
	jc	sysmnu9
	mov	bx,si
	add	si,type _menu
	cmp	cl,MNU_FEXEC		; ##16
_if e
	call	msgpoolp
	mov	si,di
_endif
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
	mov	cl,[bx].mn_sel
	tst	al
	jz	domn_k
	mov	dx,word ptr mn_top
	add	dh,dl
	cmp	al,CM_D
	je	domn_d
	cmp	al,CM_U
	je	domn_u
	cmp	al,CM_CR
	je	domn_cr
	cmp	al,CM_R
	je	domn_shift
	cmp	al,CM_L
	je	domn_shift
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
domn_shift:			; ##16
	mov	ah,cl		;
	or	ah,80h		;
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
	je	domn21
	mov	di,[bx].mn_ext
	tst	di
	jz	domnc1
	mov	ah,ITEM_KEY
	call	extfunc
	jc	domn21
	cmp	al,[bx].mn_c
	jb	domnc5
domn21:	jmp	domn2
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
	cmp	al,'$'		; ##16
	je	dritm7
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
dritm7:	call	itematr
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

redraw_border	proc
		pushm	<ds,es>
		movseg	ds,ss
		call	set_schopt
		mov	si,titlep
		call	drawborder
		popm	<es,ds>
		ret
redraw_border	endp

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
	mov	ax,cx
	add	ax,0306h		; DOS/V mergin
	mul	ah
	cmp	ax,WD*HIGHT
_if ae
	mov	ax,(WD-6)*(HIGHT-3)
	div	cl
	mov	ch,al
_endif
	pushm	<cx,dx>
	call	doswindow
	mov	bx,cx
	sub	bx,0304h		; ##156.86
	popm	<dx,cx>
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
	tst	dx
_if z
	mov	dx,refloc
	tst	dx
  _if z
	mov	al,1100b
  _endif
	inc	dh
_endif
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
	push	ax
	cmp	cl,bl
_if a
	mov	cl,bl
_endif
	cmp	cl,1
_if b
	mov	cl,1
_endif
	and	al,0101b
	cmp	al,0100b		; center
_if e
	mov	dl,bl
	sub	dl,cl
	shr	dl,1
_endif
	cmp	dl,ah
	jb	adjw1
	cmp	al,0001b		; left
_if e
adjw1:	mov	dl,ah
_endif
	cmp	al,0101b		; right
_if e
	mov	dl,bl
_endif
	mov	ah,dl
	add	ah,cl
	sub	ah,bl
_if a
	sub	dl,ah	
_endif
	inc	dl
	pop	ax
	ret	
adjwin	endp

;****************************
;   Get string from window
;****************************
;-->
; DL :title No.
;     (if DL<=0, BX=title str, -DL=window size
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
	movseg	ds,ss
	call	savewloc
	push	drawfunc
	mov	drawfunc,offset cgroup:redraw_border
	pushm	<ax,si,cx>
	tst	dl
_if g
	mov	di,tboxtbl
	dec	dl
	call	setmsgp
	lodsb				;1st byte is window size
	mov	cl,al
_else
	mov	si,bx
	neg	dl			; &g(ptr,wsize)
	mov	cl,dl
_endif
	mov	ch,1
	clr	dx
	call	adjwin
	mov	titlep,si
	call	drawborder
	popm	<cx,si,ax>
	call	gets
	call	endwindow
	pop	drawfunc
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
	movseg	ds,ss
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
drbar8:	mov	[bx].mb_c,cl
	mov	dl,[bx].mb_sx
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
	movseg	ds,ss
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

;----- Get menu bar key -----
;-->
; BX :menu bar record ptr
; CX :select item No.
;<--
; DL :1st key

		public	get_mbar
get_mbar	proc
		push	ds
		movseg	ds,ss
		mov	si,[bx].mb_ttl
_repeat
	_break	cxz
		call	skipchar
_loop
		lodsb
		mov	dl,al
		pop	ds
		ret
get_mbar	endp

		endcs

ENDIF

	eseg

;****************************
;	Command menu
;****************************
;-->
; AX :NZ=macro menu
; BX :menu record ptr
; SI :default title
;<--
; CY :escape
; AL :item No.
; AH :item type
; SI :item message ptr

	public	cmdmenu
cmdmenu	proc
	tst	al
	jmpln	z,macromenu
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
	call	getitemnum
IFDEF NEWEXPR
	tst	ah
	jns	cmdmnu8
ELSE
	cmp	ah,MCHR_VAR
	jne	cmdmnu8
ENDIF
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
IFDEF NEWEXPR
	mov	cx,ax
	and	ch,not MENU_VAR
ELSE
	mov	cl,al
	clr	ch
ENDIF
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
	clr	dl
	mov	al,GETS_INIT
	mov	bx,si
	call	windgetval
	pop	cx
	jc	clkvar9
	mov	al,OPE_EQU
clk_sw:
IFNDEF NEWEXPR
	clr	ch
ENDIF
	call	setoptval
	call	redraw
	cmp	ss:drawfunc,offset cgroup:dspscr
_if e
	call	editloc
_endif
	movseg	ds,ss
clkvar9:ret
click_var endp

	public	windgetval
windgetval proc
	mov	si,wbuf
	mov	cx,STRSZ
	call	windgets1
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
	call	getitemnum
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
	call	schmdlmac
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
	call	getitemnum
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
	call	getitemnum
	cmp	ah,MCHR_CMD
	je	val_cmd
IFDEF NEWEXPR
	tst	ah
	js	val_var
ELSE
	cmp	ah,MCHR_VAR
	je	val_var
ENDIF
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
IFDEF NEWEXPR
	mov	cx,ax
	and	ch,not MENU_VAR
ELSE
	mov	cl,al
	clr	ch
ENDIF
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
	call	schmdlmac
_ifn c
dispkeymac1:
	lodsw
	call	dispkeysym
_endif
	ret
dispkeymac endp

;---- Get item number -----

getitemnum	proc
		mov	ax,[di]
		cmp	ah,MCHR_CHR
	_if e
		call	cvt_cmdsym
	  _if c
		mov	al,0
	  _endif
		mov	ah,MCHR_CMD
	_endif
		ret
getitemnum	endp

;--- Macro menu ---
;-->
; AL :menu type (MENU_)
; BX :menu record ptr
; SI :default title
;<--
; CY :escape
; AL :item No.
; AH :MCHR_CALL

macromenu proc
	mov	macmenu,al
	cmp	al,MENU_MDLMAC
_if e
	mov	ax,retval
	mov	mdlnum,ax
	mov	cx,ax
edtmac0:
	mov	mdlnum,cx
	call	cnt_module
	tst	si
  _if z
	clr	cx
	jmp	edtmac0
  _endif
	call	skip_mdlttl
_endif
edtmac1:
	push	si
	mov	al,macmenu
	cmp	al,MENU_MODULE
_if e
	mov	ax,offset cgroup:draw_module
_else
	mov	ax,offset cgroup:drawmac
_endif
	mov	[bx].mn_ext,ax
	mov	cx,-1
	call	cntmacro
	pop	si
	jcxz	macmnu_x
	mov	[bx].mn_c,cl
	cmp	cl,[bx].mn_sel
_if be
	dec	cl
	mov	[bx].mn_sel,cl
_endif
	cmp	macmenu,MENU_MDLMAC
_ifn e
	mov	di,bx
	add	di,type _menu
	tstb	[di]
  _ifn z
	mov	si,di
  _endif
_endif
	clr	dx
	cmp	al,MENU_MODULE
	je	mdlmenu1
	push	si
	call	popupmenu
	pop	si
_if c
	tst	al
  _ifn s
	cmp	macmenu,MENU_MDLMAC
    _if e
	mov	cx,mdlnum
	cmp	al,CM_R
      _if e
	inc	cx
	jmp	edtmac0
      _endif
	dec	cx
	jmp	edtmac0
    _endif
	jmps	edtmac1
  _endif
	not	al
	tst	al
	jz	edtmac_c
_else
	cbw
	push	ax
	mov	cx,ax
	call	cntmacro
	pop	ax
	cmp	macmenu,MENU_MDLMAC
  _if e
	mov	readmac,si
	clc
	ret
  _endif
	mov	al,[si]
_endif
	mov	ah,MCHR_CALL
	clc
	ret
edtmac_c:
	clr	si
macmnu_x:
	stc
	ret
mdlmenu1:
	call	popupmenu
	ret
macromenu endp

;--- Draw Macro menu ---

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

;--- Draw Module menu ---

draw_module	proc
		cmp	ah,ITEM_DRAW
	_ifn e
		stc
		ret
	_endif
		mov	cx,ax
		push	cx
		mov	si,macrobuf
		call	cnt_module
		pop	cx
		mov	ax,[si].mh_nextmdl
		tst	ax
	_if z
		mov	ax,macrobuf+2
		sub	ax,macrosz
		dec	ax
	_endif
		sub	ax,si
		push	bx
		push	ax
		mov	al,' '
		mov	dl,[si].mh_flag
		test	dl,MDL_REMOVE
	_ifn z
		mov	al,'R'
	_endif
		test	dl,MDL_EXT
	_ifn z
		mov	al,'E'
	_endif
		call	putc
		call	isrun
		mov	al,SPC
	_if c
		mov	al,'!'
	_else
		test	dl,MDL_SLEEP
	  _ifn z
		mov	al,'.'
	  _endif
	_endif
		call	putc
		call	putspc
		call	skip_mdlttl
		call	puts
		mov	dl,[bx].mn_wd
		sub	dl,6
		call	fillspc
		mov	si,offset cgroup:pf_size
		mov	bx,sp
		call	printf
		pop	ax
		pop	bx
		ret
draw_module	endp

;----- Count macro/module -----
;--> CX :count

cntmacro	proc
		mov	si,macrobuf
		mov	al,macmenu
		cmp	al,MENU_MODULE
	_if e
		call	cnt_module
skip_mdlttl:
		add	si,[si].mh_namelen
		add	si,type _mdlhead + 1
		ret
	_endif
		cmp	al,MENU_MDLMAC
		je	cntmdlmac
_repeat
		lodsb
		tst	al
	_break z
		js	cntmac1
		jcxz	cntmac2
		lodsw
		add	si,ax
		dec	cx
	_cont
cntmac1:	lodsw
		add	si,ax
_until
		not	cx
		ret
cntmac2:	dec	si
		ret
cntmacro	endp

cntmdlmac	proc
		pushm	<ax,cx>
		mov	cx,mdlnum
		call	cnt_module
		popm	<cx,ax>
		inc	si
		lodsw
		add	si,ax
_repeat
		lodsb
		cmp	al,MDL_HEADER
	_break e
		tst	al
	_break z
		js	cntmmac1
		jcxz	cntmmac2
		lodsw
		add	si,ax
		dec	cx
	_cont
cntmmac1:	lodsw
		add	si,ax
_until
		not	cx
		ret
cntmmac2:	dec	si
		ret
cntmdlmac	endp

		endes
		end

;****************************
;	End of 'menu.asm'
; Copyright (C) 1989 by c.mos
;****************************
