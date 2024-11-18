;****************************
;	'wind.asm'
;****************************

	include	vz.inc

;--- Equations ---

H_MIN		equ	3
V_MIN		equ	10

wform0		equ	tw_px
wform9		equ	fsiz

;--- External symbols ---

	wseg
	extrn	hsplit		:byte
	extrn	vsplit		:byte
	extrn	wndc		:byte
	extrn	wsplit0		:byte

	extrn	conbufsz	:word
	extrn	textc		:word
	extrn	w_act		:word
	extrn	w_back		:word
	extrn	w_busy		:word
	extrn	w_free		:word
	extrn	w_act0		:word
	extrn	w_back0		:word
	extrn	windrec		:word
	endws

	extrn	chgline		:near
	extrn	closeall	:near
	extrn	displabel	:near
	extrn	dispmsg		:near
	extrn	dispstat	:near
	extrn	doswindow	:near
	extrn	dspscr		:near
	extrn	getkey		:near
	extrn	newline		:near
	extrn	popupmenu	:near
	extrn	printf		:near
	extrn	resetscrnh	:near
	extrn	setdoswindow	:near
	extrn	schsysmenu	:near
	extrn	textsize	:near
	extrn	memclear	:near

	dseg

;--- Local work ---

dspc		dw	0
w_save		dw	0
	endds

	eseg
	assume	ds:cgroup

	public	ld_wact
ld_wact proc
	mov	bp,ss:w_act
	tst	bp
	ret
ld_wact endp

	endes

	cseg

;--- Init text record ---
;--> di :buffer top

	public	winit
winit	proc
	movseg	es,ss
	call	windcount
	push	di
_repeat
	mov	bp,di
	push	cx
	mov	cx,type _text
	call	memclear
	pop	cx
	clr	ax
	cmp	cx,2
  _if a
	mov	ax,di
  _endif
	mov	[bp].w_next,ax
_loop
	mov	di,offset cgroup:w_free
;	movseg	es,ss
	pop	ax
	stosw				; w_free
	clr	ax
	stosw				; w_act
	stosw				; w_back
	stosw				; w_busy
	mov	ax,bp
	stosw				; w_ext
	ret
winit	endp

	public	windcount
windcount proc
	mov	cl,wndc
	clr	ch
	tst	cx
_if z
	inc	cx
_endif
	tstw	conbufsz
_ifn z
	inc	cx
_endif
	inc	cx			; ##16
	ret
windcount endp

;--- Window open ---

	public	wndopn
wndopn	proc
	mov	ax,[bp].w_next
	mov	w_free,ax
	mov	bx,bp
	clr	ax
	call	wlast
_ifn c
	mov	[bp].w_next,bx
_else
	mov	w_busy,bx
_endif
	mov	bp,bx
	mov	[bp].w_next,0
	mov	bx,w_act
	tst	bx
	jz	wopn1
	tstb	[bx].wsplit
_if z
	mov	w_back,bx
wopn1:	call	inifmt
_else
	call	cpyfmt
_endif
	mov	w_act,bp
	ret
wndopn	endp

;--- Window close ---

	public	wndcls
wndcls	proc
	call	wndrmv
	mov	ax,w_free
	mov	bp,w_act
	mov	[bp].w_next,ax
	mov	w_free,bp
wndcls1:
	mov	bp,w_busy
	clr	ax
_repeat
	tst	bp
  _break z
	tstb	[bp].wnum
	jz	wcls2
	cmp	bp,w_back
	je	wcls2
	mov	ax,bp	
wcls2:	mov	bp,[bp].w_next
_until
	mov	bx,w_back
	tst	bx
_if z
	clr	bp
	jmps	wcls8
_endif
	tst	ax
	jz	wcls4
	mov	bp,w_act
	tstb	[bp].wsplit
_if z
	mov	bp,ax
	call	inifmt
	mov	ax,bp
wcls4:	mov	w_back,ax
	mov	bp,bx
	call	inifmt
_else
	mov	bx,bp
	mov	bp,ax
	call	cpyfmt
_endif
wcls8:	mov	w_act,bp
	ret
wndcls	endp

;--- Window change ---

	public	wndchg
wndchg	proc
	call	wndrmv
	clr	ax
	call	wlast
	mov	ax,w_act
_ifn c
	mov	[bp].w_next,ax
_else
	mov	w_busy,ax
_endif
	mov	bp,ax
	mov	[bp].w_next,0
	ret
wndrmv:
	mov	ax,w_act
	mov	bp,ax
	mov	bx,[bp].w_next
	call	wlast
_ifn c
	mov	[bp].w_next,bx
	ret
_endif
	mov	w_busy,bx
	ret
wndchg	endp

;--- Set window number ---

	public	setwnum
setwnum proc
	push	bp
	mov	bp,w_busy
	clr	ax
	tstw	conbufsz
	jz	snum1
	dec	ax
snum1:
_repeat
	tst	bp
  _break z
	inc	ax
	mov	[bp].wnum,al
	mov	bp,[bp].w_next
_until
	mov	textc,ax
	pop	bp
	ret
setwnum endp

;--- Set w_record pointer ---
;--> AX :target pointer
;<-- CY :BP=AX

wlast	proc
	mov	bp,w_busy
	cmp	bp,ax
	je	wlast3
_repeat
	cmp	[bp].w_next,ax
  _break e
	mov	bp,[bp].w_next
_until
	clc
	ret
wlast3:	stc
	ret
wlast	endp

;--- Set window format ---

settextw proc
	mov	word ptr [bp].tw_px,dx
	mov	word ptr [bp].tw_sx,cx
	dec	ch
	mov	[bp].tw_cy,ch
	inc	ch
	mov	[bp].wsplit,al
check_wy:
	mov	al,[bp].tw_cy
	dec	al
	cmp	al,[bp].wy
_if b
	mov	[bp].wy,al
_endif
	cmp	al,[bp].wys		; ##154.67
_if b
	mov	[bp].wys,al
_endif
	ret
settextw endp

	public	inifmt
inifmt	proc
	call	doswindow
	inc	dh
	dec	ch
	mov	al,SPLIT_A
	call	settextw
	mov	[bp].fofs,0
	ret
inifmt	endp

cpyfmt	proc
	pushm	<ds,es>
	movseg	ds,ss
	movseg	es,ss
	mov	al,[bx].wsplit
	mov	[bp].wsplit,al
	lea	si,[bx].wform0
	lea	di,[bp].wform0
	mov	cx,wform9-wform0
    rep movsb
	popm	<es,ds>
	call	check_wy
	ret
cpyfmt	endp

	assume	ds:nothing

;--- Change window ---

	public	se_chgwind
se_chgwind proc
	mov	ax,w_back
	tst	ax
_ifn z
	mov	bp,ax
	xchg	ax,w_act
	mov	w_back,ax
	tstb	[bp].wsplit
  _if z
	call	dspscr
;  _else				; ##153.57
;	call	maptexts
  _endif
_endif
	clc
	ret
se_chgwind endp

	assume	ds:cgroup

;--- Split mode ---

	public	se_splitmode,mc_splitmode
se_splitmode proc
	mov	al,-1
mc_splitmode:
	movseg	ds,ss
	mov	bx,w_back
	tst	bx
_ifn z
	call	splitmode
	call	dspscr
_endif
	clc
	ret
splitmode:
	tst	al
	jns	splitwd1
	mov	al,[bp].wsplit
	tst	al
	jz	yokowd
	cmp	al,SPLIT_V
	jb	tatewd
fullwd:
	call	inifmt
	tst	bx
_ifn z
	xchg	bp,bx
	call	inifmt
_endif
	ret

splitwd:
	mov	al,[bp].wsplit
splitwd1:
	tst	al
	jz	fullwd
	cmp	al,SPLIT_V
	jae	tatewd
yokowd:
	call	doswindow
	inc	dh
	mov	ch,hsplit
	dec	ch
	mov	al,SPLIT_U
	call	settextw
	xchg	bp,bx
	call	doswindow
	mov	dh,hsplit
	inc	dh
	sub	ch,dh
	mov	al,SPLIT_D
	call	settextw
	ret
tatewd:
	call	doswindow
	add	dh,2
	sub	ch,2
	mov	cl,vsplit
	mov	al,SPLIT_L
	call	settextw
	xchg	bp,bx
	call	doswindow
	add	dh,2
	sub	ch,2
	mov	dl,vsplit
	inc	dl
	sub	cl,dl
	mov	al,SPLIT_R
	call	settextw
	ret
se_splitmode endp

;--- Split position ---

	public	se_splitpos
se_splitpos proc
	tstb	[bp].wsplit
	jz	wpos9
	mov	dl,M_MOVE
	call	dispmsg
wpos1:	mov	al,CSR_OFF
	mov	dl,SYS_GETC
	call	getkey
	jz	wpos1
	cmp	al,CM_CR
	jnz	wpos2
	call	newline
wpos9:	clc
	ret
wpos2:
	movseg	ds,ss
	push	ax
	call	doswindow
	pop	ax
	cmp	[bp].wsplit,SPLIT_V
	jae	wposv
wposh:	mov	ah,ch
	sub	ah,H_MIN
	mov	cl,hsplit
	cmp	al,CM_U
_if e
	cmp	cl,H_MIN
	je	wpos1
	dec	cx
_else
	cmp	al,CM_D
	jne	wpos1
	cmp	cl,ah
	je	wpos1
	inc	cx
_endif
	clr	ch
	mov	word ptr hsplit,cx	; hsplit2 = 0
	mov	bx,w_back
	push	bp
	cmp	[bp].wsplit,SPLIT_U
_ifn e
	xchg	bp,bx
_endif
	call	yokowd
	jmps	wpos3
wposv:	
	mov	ah,cl
	sub	ah,V_MIN
	mov	cl,vsplit
	cmp	al,CM_R
_if e
	cmp	cl,ah
	je	wpos1
	inc	cx
	inc	cx
_else
	cmp	al,CM_L
	jne	wpos1
	cmp	cl,V_MIN
	je	wpos1
	dec	cx
	dec	cx
_endif
	mov	vsplit,cl
	mov	bx,w_back
	push	bp
	cmp	[bp].wsplit,SPLIT_L
_ifn e
	xchg	bp,bx
_endif
	call	tatewd
wpos3:	call	dspscr
	pop	bp
	jmp	wpos1
se_splitpos endp

;--- Change 20/25 line ---

	public	se_xline,resetscr,resetwind
se_xline proc
	call	chgline
resetscr:
	pushm	<ds,es>
	call	resetwind
	call	dspscr
	popm	<es,ds>
	clc
	ret
resetwind:
	push	bp
	movseg	ds,ss
	call	resetscrnh
	mov	bx,w_back
	test	[bp].wsplit,1		; ##156.115
_ifn z
	xchg	bp,bx
_endif
	call	splitwd
	pop	bp
	ret
se_xline endp

;--- Change active text ---		; ##153.37

	public	se_chgtext,mc_chgtext,wndsel
se_chgtext proc
	movseg	ds,ss
	mov	dl,MNU_TEXT
	call	schsysmenu
	jc	chgt9
	mov	bx,si
	mov	ax,textc
	mov	[bx].mn_c,al
	mov	[bx].mn_ext,offset cgroup:dumptext
	mov	al,[bp].wnum
	tst	al
  _ifn z
	dec	al
  _endif
	mov	[bx].mn_sel,al
	clr	dx
	mov	si,bx
	add	si,type _menu
	call	popupmenu
_if c
	cmp	al,CM_CHGTEXT
	jne	chgt9
	mov	al,[bp].wnum
_endif
chgtext:
	call	seekbp
chgt1:	call	wndsel
	call	dspscr
chgt9:	ret

mc_chgtext:
	movseg	ds,ss
	tst	ax
	js	seekid
	jz	chgt_cons		; ##156.135
	dec	ax
	jmp	chgtext
seekid:
	neg	ax
	mov	bp,w_busy
_repeat
	tst	bp
  _break z	
	cmp	ax,[bp].textid
	je	chgt1
	mov	bp,[bp].w_next
_until
	stc
	ret

wndsel:
	mov	bx,w_act
	cmp	bp,w_back
	je	chgt7
	cmp	bp,bx
	je	chgt9
	mov	w_act,bp
	tst	bx
	jz	chgt9
	jmp	cpyfmt
chgt7:
	mov	w_act,bp
	mov	w_back,bx
	test	[bx].wsplit,1
_ifn z
	xchg	bp,bx
_endif
	jmp	splitwd
se_chgtext endp

;--- Open/close console window ---

	public	se_console
se_console proc
	movseg	ds,ss
	tstb	[bp].wnum
	jnz	chgt_cons
	mov	bx,[bp].w_next		; ##152.19
	tst	bx
	jmpl	z,closeall
	mov	ax,w_save
	tst	ax
_if z
	mov	ax,w_back
_endif
_repeat
	cmp	bx,ax
	je	wcons2			; ##153.28
	mov	bx,[bx].w_next
	tst	bx
_until z
	mov	bx,w_back
wcons2:
	mov	bp,bx
	cmp	bp,w_back
	jne	chgcon1
	call	wndcls1
	call	dspscr
	ret
se_console endp

	public	chgt_cons
chgt_cons proc
	mov	bp,w_busy
	tstb	[bp].wnum
	jnz	chgcon_x
	mov	ax,w_act
	cmp	ax,bp
	je	chgcon1
	mov	w_save,ax
	tstw	w_back
	jnz	chgcon1
	mov	w_back,ax
chgcon1:call	wndsel
	call	dspscr
	ret
chgcon_x:
	stc
	ret
chgt_cons endp

;--- Dump text name ---
;--> CX :cursor

pf_size		db	"%8ld",0

	public	sel_number
dumptext proc
	tst	ah
_ifn z
sel_number:
	cmp	al,'0'
	jbe	seltx_x
	cmp	al,'9'
	jbe	seltx1
	cmp	al,'A'
	jb	seltx_x
	sub	al,'A'-'1'-9
seltx1:	sub	al,'1'
	clc
	ret
seltx_x:stc
	ret	
_endif
	push	bp
	call	seekbp
	call	displabel
	call	textsize
	pushm	<dx,ax>
	mov	bx,sp
	mov	si,offset cgroup:pf_size
	call	printf
	popm	<ax,ax>
	pop	bp
	ret
dumptext endp

seekbp	proc
skbp1:	mov	bp,w_busy
	tstb	[bp].wnum
_if z
	mov	bp,[bp].w_next
_endif
_repeat
	tst	al
  _break z
	mov	bp,[bp].w_next
	dec	al
	tst	bp
	jz	skbp1
_until
	ret
seekbp	endp

;----- Reset active/back window -----

		public	wnd_reset
wnd_reset	proc
		pushm	<bx,bp,ds>
		movseg	ds,ss
		mov	bp,w_act0
		tst	bp
	_ifn z
		mov	bx,w_back0
		tst	bx
  	  _ifn z
		mov	w_act,bp
		mov	w_back,bx
		mov	al,wsplit0
		tst	al
	    _ifn z
		test	al,1
	      _ifn z
		xchg	bx,bp
	      _endif
		cmp	al,SPLIT_V
	      _if b
		call	yokowd
	      _else
		call	tatewd
	     _endif
	    _endif
		call	dspscr
	  _endif
	_endif
		popm	<ds,bp,bx>
		ret
wnd_reset	endp

	endcs
	end

;****************************
;	End of 'wind.asm'
; Copyright (C) 1989 by c.mos
;****************************
