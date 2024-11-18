	.xlist
;****************************
;	'vz.inc'
;****************************

;DEBUG		equ	TRUE
;NOSWAP		equ	TRUE
REXP		equ	TRUE		; VWX support by wing
NEWEXPR		equ	TRUE		; new pointer handler by Sakakibara


IFNDEF PC98
  IFNDEF J31
    IFNDEF IBMAX
      IFNDEF US
;i186		equ	TRUE
      ENDIF
;NO2BYTEHAN	equ	TRUE
    ENDIF
  ENDIF
ENDIF

;IFDEF SLIM				; N/A from 1.6
;  IFNDEF J31
;    IFNDEF i186
;i186		equ	TRUE
;    ENDIF
;  ENDIF
;ENDIF

;NEWBLOCK	equ	TRUE

	include	std.inc

	.xlist

;****************************
;	Equations
;****************************
;
;--- Assemble switches ---

IFDEF IBMV
DOSV		equ	TRUE
ENDIF
IFDEF IBMJ
DOSV		equ	TRUE
ENDIF

IFNDEF PC98
  IFNDEF J31
    IFNDEF JBM
      IFNDEF IBM
IBM		equ	TRUE
      ENDIF
    ENDIF
  ENDIF
ENDIF

IFNDEF PC98
  IFNDEF J31
    IFNDEF IBMJ
      IFNDEF JBM
MSG_US		equ	TRUE
      ENDIF
    ENDIF
  ENDIF
ENDIF

IFNDEF PC98
NO2BYTEHAN	equ	TRUE
ENDIF

;--- Hardware ID ---

ID_PC98		equ	00h
ID_PC98Hi	equ	01h
ID_J31		equ	10h
ID_IBM		equ	20h
ID_EGA		equ	21h
ID_AX		equ	02h
ID_DOSV		equ	04h
ID_PS55		equ	28h
IDN_PC98	equ	0F0h

;--- Command code ---

CM_MAC		equ	02
CM_ESC		equ	03
CM_CR		equ	04
CM_U		equ	05
CM_D		equ	06
CM_LEDIT	equ	07
CM_L		equ	07
CM_R		equ	08
CM_COMMON	equ	09
CM_WL		equ	09
CM_WR		equ	10
CM_TOUCH	equ	14
CM_TAB		equ	21
CM_CTRL		equ	22
CM_SCOPY	equ	24
CM_PUTLINE	equ	29
CM_SEDIT	equ	30
CM_FILER1	equ	43
CM_SFTCR	equ	53
CM_CANCEL	equ	54
CM_CHGTEXT	equ	63
CM_OPENFILE	equ	70
CM_READFILE	equ	71
CM_NEWFILE	equ	72
CM_APPEND	equ	76
CM_RENAME	equ	82
CM_SEDITMAX	equ	85
CM_FILER	equ	88
CM_DOS		equ	95
CM_CONS		equ	95
CM_ALIAS	equ	96
CM_SPREAD	equ	97
CM_TOFILER	equ	98
CMCNT		equ	100
CM_FILER3	equ	101
CM_FILER4	equ	122

;----- Event types -----

EV_ABORT	equ	1
EV_START	equ	2
EV_OPEN		equ	3
EV_CLOSE	equ	4
EV_EDIT		equ	5
EV_VIEW		equ	6
EV_TIMER	equ	7
EV_FILER	equ	8
EVENTCNT	equ	8

;--- Messages ---

E_PATH		equ	1
E_READ		equ	2
E_WRITE		equ	3
E_TEMP		equ	4
E_NOLINE	equ	5
E_OPEN		equ	6
E_NODISK	equ	7
E_NOTEXT	equ	8
E_NOSTACK	equ	9
E_NOMEM		equ	10
E_NOBUFFER	equ	11
E_EXEC		equ	12
E_RECUST	equ	13

M_QSAVE		equ	14
M_SAVING	equ	15
M_QSAVEM	equ	16
M_QCLRSTK	equ	17
M_QQUIT		equ	18
M_XFOUND	equ	19
M_QCHGALL	equ	20
M_QRPLONE	equ	21
M_RPLTOTAL	equ	22
M_MOVE		equ	23
M_COMPOK	equ	24
M_COMPNG	equ	25
M_XKAKO		equ	26
M_OPENED	equ	27
M_MARK		equ	28
M_RECUST	equ	29
M_EXIST		equ	30
M_PATHERR	equ	31
M_RDONLY	equ	32
M_NEW		equ	33
M_XSAVE		equ	34
M_XAPPEND	equ	35
M_NOTCHG	equ	36
M_ABANDON	equ	37
M_DELETE	equ	38

;--- Text windows ---

W_READ		equ	1
W_WRITE		equ	2
W_NEW		equ	3
W_APPEND	equ	4
W_PATH		equ	5
W_FIND		equ	6
W_REPLACE	equ	7
W_COPY		equ	8
W_LINE		equ	9
W_RENAME	equ	10
W_FINDTTL	equ	11
W_RENFILE	equ	12
W_MKDIR		equ	13
W_DOSBOX	equ	14
W_CMDBOX	equ	15
W_DSTBOX	equ	16

;--- System mode ---

SYS_SEDIT	equ	0
SYS_GETS	equ	1
SYS_DOS		equ	2
SYS_FILER	equ	3
SYS_GETC	equ	4

;--- Parse path result ---

PRS_WILD	equ	00000001b
PRS_EXT		equ	00000010b
PRS_NAME	equ	00000100b
PRS_DIR		equ	00001000b
PRS_DRV		equ	00010000b
PRS_ROOT	equ	00100000b
PRS_ENDDIR	equ	10000000b

;--- Char attribute ---

ATR_TXT		equ	0
ATR_CTR		equ	1
ATR_NUM		equ	2
ATR_BLK		equ	3
ATR_STT		equ	4
ATR_STT2	equ	5
ATR_STTR	equ	6
ATR_MSG		equ	7
ATR_WFRM	equ	8
ATR_WTTL	equ	9
ATR_WTXT	equ	10
ATR_W1ST	equ	11
ATR_WSEL	equ	12
ATR_HELP	equ	13
ATR_KEY		equ	14
ATR_GRD		equ	15
ATR_BCSR	equ	16
ATR_FKEY	equ	17
ATR_DOS		equ	18
ATR_CTR2	equ	19
ATR_PATH	equ	20

;--- Graphic char type ---

GRC		equ	0F0h
GRC_H		equ	GRC+0
GRC_V		equ	GRC+1
GRC_TL		equ	GRC+2
GRC_TR		equ	GRC+3
GRC_BL		equ	GRC+4
GRC_BR		equ	GRC+5
GRC_VL		equ	GRC+6
GRC_VR		equ	GRC+7
GRC_TAB		equ	GRC+8
GRC_CR		equ	GRC+9
GRC_EOF		equ	GRC+10

;----- Control char attribute -----

CA_CTRL		equ	00000001b
CA_CR		equ	00000010b
CA_TAB		equ	00000100b
CA_EOF		equ	00001000b
CA_RMGN		equ	00010000b
CA_ZENSPC	equ	00100000b
CA_XZEN		equ	01000000b

;--- Display switch ---

DSP_LINE	equ	00000001b
DSP_CR		equ	00000010b
DSP_TAB		equ	00000100b
DSP_EOF		equ	00001000b
DSP_RMGN	equ	00010000b		; ##16
DSP_ZENSPC	equ	00100000b		; ##16
DSP_NUM		equ	01000000b
DSP_SYS		equ	10000000b
DSP_BSLASH	equ	0000000100000000b
DSP_UNDER	equ	0000001000000000b
DSP_FULPATH	equ	0000010000000000b
DSP_PATHCASE	equ	0000100000000000b
DSP_IBMGRPH	equ	0001000000000000b
DSP_98JIS83	equ	0001000000000000b	; ##156.106
DSP_DOSVFLICK	equ	0001000000000000b	; ##156.131
DSP_STLHEAD	equ	0010000000000000b	; ##156.99
DSP_SMOOTH	equ	0100000000000000b	; ##16

;--- Edit switch ---

EDT_INDENT	equ	00000001b
EDT_SCROLL	equ	00000010b
EDT_EOF		equ	00000100b
EDT_BACKUP	equ	00001000b
EDT_UNTAB	equ	00010000b
EDT_PASTE	equ	00100000b
EDT_PGTTL	equ	01000000b
EDT_PGHALF	equ	10000000b
EDT_WRDSCH	equ	0000000100000000b
EDT_ICASE	equ	0000001000000000b
EDT_NOSTK	equ	0000010000000000b
EDT_LOGMOVE	equ	0000100000000000b
EDT_VIEW	equ	0001000000000000b
EDT_AUTOWD	equ	0010000000000000b
EDT_CUTSPC	equ	0100000000000000b
EDT_INDENTZEN	equ	1000000000000000b	; ##153.33

;--- System switch ---

SW_CON		equ	00000010b
SW_SKIPESC	equ	00000100b
SW_REDRAW	equ	00001000b
SW_CLRSTACK	equ	00010000b
SW_CLMOVW	equ	00100000b		; Eo ##16
SW_YES		equ	01000000b
SW_REVYN	equ	10000000b		; YN ##16
SW_ASKNEW	equ	0000000100000000b
SW_QUIT		equ	0000001000000000b
SW_FP		equ	0000010000000000b
SW_BEEP		equ	0000100000000000b
SW_RO		equ	0001000000000000b
SW_INIOPT	equ	0010000000000000b	; Sz ##16
;SW_VIEWRET	equ	0100000000000000b	; Sv ##16

;--- Extended switch ---

ESW_ESCKEY	equ	00000001b
ESW_EZKEY	equ	00000010b		; ##153.30
ESW_FPQUIT	equ	00000100b		; ##156.109
ESW_TRUSH	equ	00001000b		; ux
ESW_MAC		equ	00010000b		; mx
ESW_VWX		equ	00100000b		; +
ESW_IZENHAN	equ	01000000b		; + VWX
ESW_REXP	equ	10000000b		; +
ESW_FKEY	equ	0000000100000000b
ESW_DEFTTL	equ	0000001000000000b

;--- Command flag ---

CMF_TCH		equ	00000001b
CMF_VMOVE	equ	00000010b
CMF_VPOS	equ	00000100b
CMF_REMAP	equ	00001000b
CMF_VAL		equ	10000000b

;----- DOS Command exec switch -----

DOS_TBOX	equ	0001b
DOS_GO		equ	0010b
DOS_RETURN	equ	0100b
DOS_BOXTTL	equ	1000b

;--- Macro extra char ---

MCHR_MENU	equ	1
MCHR_CMD	equ	2
MCHR_VAR	equ	3
MCHR_CALL	equ	4
MCHR_REM	equ	5
MCHR_LABEL	equ	6
MCHR_JUMP	equ	7
MCHR_IF		equ	8
MCHR_RET	equ	9
MCHR_END	equ	10
MCHR_CHR	equ	11
MCHR_STR	equ	12

;--- Block mode ---

BLK_LINE	equ	1
BLK_CHAR	equ	2
BLK_RECT	equ	4

;----- Paging mode -----

PG_SCRN		equ	0
PG_TTLSCH	equ	1
PG_STRSCH	equ	2

;--- Cursor type ---

CSR_INS		equ	0
CSR_OVR		equ	1
CSR_SYS		equ	4
CSR_OFF		equ	-1

;--- Get string mode ---

GETS_INIT	equ	0
GETS_COPY	equ	1
GETS_DOS	equ	4
GETS_DOSC	equ	5
GETS_DOSBOX	equ	8

;--- System menu ---

MNU_HELP	equ	0
MNU_MACRO	equ	1
MNU_ALIAS	equ	2
MNU_TEXT	equ	3
MNU_RPLMODE	equ	4
MNU_FMASK	equ	5
MNU_FSORT	equ	6
MNU_FCOMP	equ	7
MNU_FEXEC	equ	8
MNU_FQUICK	equ	9
MNU_MODULE	equ	10
MNU_VZPATH	equ	11
MNU_FATTR	equ	12
MNU_FFILE	equ	13
MENU_MODULE	equ	97
MENU_MDLMAC	equ	98
MENU_MACRO	equ	99
MENUMASK	equ	11100000b

IFDEF NEWEXPR
MENU_VAR	equ	10000000b
ENDIF

;--- Split mode ---

SPLIT_A		equ	0
SPLIT_H		equ	2
SPLIT_U		equ	SPLIT_H
SPLIT_D		equ	SPLIT_H+1
SPLIT_V		equ	4
SPLIT_L		equ	SPLIT_V
SPLIT_R		equ	SPLIT_V+1

;--- FEP controle code ---

FEP_ON		equ	0
FEP_OFF		equ	1
FEP_ACT		equ	2
FEP_MASK	equ	3

;--- Text modify mode ---

TCH_MOD		equ	1
TCH_RO		equ	-1
TCH_VIEW	equ	80h

;--- Large file mode ---		; ##16

FL_HEAD		equ	00000001b
FL_TAIL		equ	00000010b
FL_HEADX	equ	00000100b
FL_TAILX	equ	00001000b
FL_READEND	equ	00010000b
FL_RENAME	equ	00100000b
FL_LOG		equ	01000000b
FL_BINARY	equ	10000000b

;----- Module macro -----

MDL_SLEEP	equ	00000001b
MDL_REMOVE	equ	00000010b
MDL_EXT		equ	00000100b
MDL_KEYMAC	equ	10000000b
MDL_HEADER	equ	0FFh

;----- Profile option -----

PRO_WRITE	equ	0000001b
PRO_CREATE	equ	0000010b
PRO_CLOSE	equ	0000100b
PRO_AUTO	equ	0001000b
PRO_LOGCLOSE	equ	0010000b
PRO_CMDHIST	equ	0100000b
PRO_NOHIST	equ	1000000b

;--- Miscellaneous ---

NULLCODE	equ	0FFh
WD		equ	80
HIGHT		equ	25
STRSZ		equ	128
TTLSTRSZ	equ	64
TMPSZ		equ	256
PATHSZ		equ	64
MASKSZ		equ	32
BACKSLASH	equ	0FCh
SYMCHR		equ	27h
MARKCNT		equ	4
ONEMS		equ	0FFFh
MIN_TBSIZE	equ	0400h
PACKSZ		equ	13
TEXTTOP		equ	2
TMPSLOTCNT	equ	512
BMPSIZE		equ	128
DOSLEN		equ	128
PAGEMAX		equ	BMPSIZE*8
EMSMASK		equ	0F0h
EMSMASKW	equ	0F000h
TMAXMGN		equ	0FFFDh

;****************************
;    Segment definitions
;****************************

_work	segment	word public 'WORK'
_work	ends

_data	segment	word public 'DATA'
_data	ends

_base	segment	word public 'BASE'
_base	ends

_code	segment	byte public 'CODE'
_code	ends

_exec	segment	byte public 'EXEC'
_exec	ends

_hard	segment byte public 'HARD'
_hard	ends

_init	segment word public 'INIT'
_init	ends

_tail	segment word public 'TAIL'
_tail	ends

cgroup	group	_work,_data,_base,_code,_exec,_hard,_init,_tail

wseg	macro
_work	segment
	endm

endws	macro
_work	ends
	endm

dseg	macro
_data	segment
	endm

endds	macro
_data	ends
	endm

bseg	macro
_base	segment
	endm

endbs	macro
_base	ends
	endm

cseg	macro
_code	segment
	endm

endcs	macro
_code	ends
	endm

eseg	macro
_exec	segment
	endm

endes	macro
_exec	ends
	endm

hseg	macro
_hard	segment
	endm

endhs	macro
_hard	ends
	endm

iseg	macro
_init	segment
	endm

endis	macro
_init	ends
	endm

;****************************
;    Macro definitions
;****************************

GDATA	macro	sym,def,val1
	public	sym
sym	def	val1
	endm

tstl	macro	var
	mov	ax,word ptr var
	or	ax,word ptr var+2
	endm

clrl	macro	var
	mov	word ptr var,0
	mov	word ptr var+2,0
	endm

ldl	macro	var
	mov	ax,word ptr var
	mov	dx,word ptr var+2
	endm

stl	macro	var
	mov	word ptr var,ax
	mov	word ptr var+2,dx
	endm
	
addl	macro	var
	add	ax,word ptr var
	adc	dx,word ptr var+2
	endm

subl	macro	var
	sub	ax,word ptr var
	sbb	dx,word ptr var+2
	endm
	
cmpl	macro	var
	local	cmpl1
	cmp	dx,word ptr var+2
	jne	cmpl1
	cmp	ax,word ptr var
cmpl1:	
	endm

cmphl	macro	reg_h,reg_l
	local	cmphl1
	cmp	dx,reg_h
	jne	cmphl1
	cmp	ax,reg_l
cmphl1:
	endm

addlw	macro	var,reg
	add	word ptr var,reg
	adc	word ptr var+2,0
	endm

sublw	macro	var,reg
	sub	word ptr var,reg
	sbb	word ptr var+2,0
	endm

IFDEF DEBUG
IFDEF PC98
beep	macro
	pushf
	pushm	<ax,cx>
	outi	37h,06h
	mov	cx,1000h
	loop	$
	outi	37h,07h
	popm	<cx,ax>
	popf
	endm

beepon	macro
	push	ax
	outi	37h,6
	pop	ax
	endm

beepoff	macro
	push	ax
	outi	37h,7
	pop	ax
	endm
ELSE
beep	macro
	push	ax
	mov	ax,0E07h
	int	10h
	pop	ax
	endm
ENDIF
ENDIF

ofs	macro	label
	dw	offset cgroup:label
	endm

;****************************
;    Structure definitions
;****************************

;--- Memory control block ---

_mcb		struc
mcb_id		db	?
mcb_psp		dw	?
mcb_size	dw	?
_mcb		ends

;--- Screen record ---

_point		struc
x		db	?
y		db	?
_point		ends

_rect		struc
px		db	?
py		db	?
sx		db	?
sy		db	?
_rect		ends

;--- Menu record ---

_menu		struc
mn_wd		db	?
mn_c		db	?
mn_valwd	db	?
mn_sel		db	?		; if -1, no cursor
mn_ext		dw	?		; extern function ptr
_menu		ends

;--- Menu bar record ---

_mbar		struc
mb_px		db	?
mb_py		db	?
mb_sx		db	?
mb_c		db	?
mb_ttl		dw	?
_mbar		ends

;--- Directoy record ---

_dir		struc
dr_attr		db	?
dr_time		dw	?
dr_date		dw	?
dr_size		dd	?
dr_pack		db	13 dup(?)
_dir		ends

;--- DTA record ---

_dta		struc
		db	15h dup(?)
dta_attr	db	?
dta_time	dw	?
dta_date	dw	?
dta_size	dd	?
dta_pack	db	PACKSZ dup(?)
_dta		ends

;--- Filer window record ---

_filer		struc
fl_back		dw	?
fl_pooltop	dw	?
fl_seg		dw	?
fl_poolend	dw	?
fl_poolp	dw	?
fl_selcnt	dw	?
fl_bcsr		dw	?
fl_files	dw	?
fl_home		dw	?
fl_wpx		db	?
fl_wpy		db	?
fl_wsx		db	?
fl_wsy		db	?
fl_tsy		dw	?
fl_ttlsx	db	?
fl_which	db	?
fl_free		dd	?
fl_clust	dw	?
fl_overflow	db	?
fl_curf		db	?
fl_path		db	PATHSZ dup(?)
fl_mask		db	MASKSZ dup(?)
fl_lastpath	db	PATHSZ dup(?)
_filer		ends

;--- FEP descriptor ---

_fep		struc
fp_int		db	?		; INT No.
fp_idoff	dw	?		; check offset
fp_absoff	db	?		; 0=vector rel, 1=absolute
fp_id		dw	?		; check word
_fep		ends

;----- Log file table -----

_logtbl		struc
lg_lnumb	dw	?
lg_nowp		dd	?
lg_eofp		dd	?
lg_lf		db	?
_logtbl		ends

;----- Module Header -----

_mdlhead	struc
mh_num		db	0FFh		; Dummy No.(=Menu #31)
mh_size		dw	?		; Header size
mh_flag		db	?		;
mh_nextmdl	dw	?		; Next module header ptr
mh_namelen	dw	?		; module name length
_mdlhead	ends

;--- Text record ---

_text		struc
w_next		dw	?		; next record link ptr
wnum		db	?		; window number
tchf		db	?		; touch flag (0=nop,1=modified,-1=RO)
wsplit		db	?		; window split mode
blkm		db	?		; block mode (1=line,2=char)
wy		db	?		; y loc. in screen
wnxt		db	?		; next line y loc. in screen
wys		db	?		; wy keeper

nodnumb		db	?		; disp number flag
lnumb		dw	?		; line number
dnumb		dw	?		; disp number
lnumb0		dw	?		; line number offset
dnumb0		dw	?		; disp number offset

ttops		dw	?		; text area top seg
tends		dw	?		; text area end seg or EMS entry
ttop		dw	?		; text top ptr
tend		dw	?		; text end ptr
tmax		dw	?		; text buffer end ptr
thom		dw	?		; screen home ptr
tbtm		dw	?		; screen bottom ptr
tnow		dw	?		; current line ptr
tnxt		dw	?		; next line ptr

btop		dw	?		; line buffer top ptr
bend		dw	?		; line end ptr
tcp		dw	?		; current ptr (common)
tfld		dw	?		; field start ptr (common)
bmax		dw	?		; line buffer end ptr
bhom		dw	?		; screen home ptr (in buffer)
bbtm		dw	?		; screen bottom ptr (in buffer)
lbseg		dw	?		; line buffer seg
inbuf		db	?		;
tw_px		db	?		;+ text window
tw_py		db	?		;+
tw_sx		db	?		;+
tw_sy		db	?		;+
tw_cy		db	?		;+
fsiz		db	?		; field size
fskp		db	?		; display skip x
fofs		db	?		; H-scroll offset x
lxs		db	?		; lx keeper
lx		db	?		; x loc. in field
ly		db	?		; y loc. in line
tabr		db	?		; Tab size
exttyp		db	?		; file ext type
ctype		db	?		; current char type
ckanj		db	?		; 1=kanji
ccode		dw	?		; current char code
namep		dw	?		; file name ptr

largf		db	?		; large text flag
temp		db	?		; temporary file flag
readp		dd	?		; text read long ptr
eofp		dd	?		; EOF lptr
headp		dd	?		; head lptr
headsz		dw	?		; head size
tailp		dd	?		; tail lptr
tailsz		dw	?		; tail size
textid		dw	?		; text ID No.
w1		dw	?		; work 1
w2		dw	?		; work 2
w3		dw	?		; work 3
bofs		dw	?		; block mark offset in line
blkx		db	?		; block mark x loc.
fsiz0		db	?		; save of fsiz

tbmax		dw	?		; size of text buffer  ##1.5
tbalt		dw	?		; size of temp. block  ##1.5

tblkp		dd	?		; block lptr
tnowp		dd	?		; current lptr
trgtp		dd	?		; target lptr
toldp		dd	?		; old current lptr
tretp		dd	?		; return lptr
tmark		dd	MARKCNT dup(?)	; mark lptr

path		db	PATHSZ dup (?)	; path name area
tsstr		dw	?		; title search string ptr
blktgt		dw	?		; block target flag
inpcnt		dw	?		; input counter
ektbl		dw	?		; event key table ptr
extword		dw	?		; file ext word
labelp		dw	?		; label name ptr
lnumb9		dw	?		; last line number
dspsw1		db	?		; alternate dspsw
atrtxt1		db	?
atrstt1		db	?
atrpath1	db	?
_text		ends

;****************************
;	End of 'vz.inc'
; Copyright (C) 1989 by c.mos
;****************************

	.list
	assume	cs:cgroup, ss:cgroup
