	.xlist
	page	66,132

;****************************
;	'std.inc'
;****************************

;NEWBLOCK	equ	TRUE

IFNDEF i8086
  IFDEF i186
		.186
  ENDIF
ENDIF

;--- Equations ---

BELL		equ	07h
BS		equ	08h
TAB		equ	09h
LF		equ	0Ah
CR		equ	0Dh
EOF		equ	1Ah
ESCP		equ	1Bh
SPC		equ	20h
CRLF		equ	0A0Dh
TRUE		equ	1
FALSE		equ	0
ON		equ	1
OFF		equ	0
NULL		equ	0
INVALID		equ	-1

;--- Interrupt number ---

INT_EMS		equ	67h
INT_EZKEY	equ	0E0h

;--- DOS functions ---

F_CONINE	equ	01h		; Read Keyboard and Echo
F_DSPCHR	equ	02h		; Display Character
F_CONIO		equ	06h		; Direct Console I/O
F_CONIN		equ	07h		; Direct Console Input
F_READKEY	equ	08h		; Read Keyboard
F_DSPSTR	equ	09h		; Display String
F_LINEIN	equ	0Ah		; Bufferd Keyboard Input
F_SELDRV	equ	0Eh		; Select Disk
F_CURDRV	equ	19h		; Current Disk
F_SETDTA	equ	1Ah		; Set Disk Transfer Addr
F_SETVCT	equ	25h		; Set Vector
F_GETDATE	equ	2Ah		; Get Date
F_GETDTA	equ	2Fh		; Get Disk Transfer Addr
F_VERSION	equ	30h		; Get DOS Version Number
F_KEEP		equ	31h		; Keep Process
F_GETDPB	equ	32h		; Get DPB Addr
F_CTRL_C	equ	33h		; CONTROL-C Check
F_GETVCT	equ	35h		; Get Interrupt Vector
F_GETDRV	equ	36h		; Get Disk Free Space
F_SWITCHAR	equ	37h		; Set/Get switch char
F_MKDIR		equ	39h		; Create Sub-Directory
F_RMDIR		equ	3Ah		; Remove a Directory Entry
F_CHDIR		equ	3Bh		; Change Current Directory
F_CREATE	equ	3Ch		; Create a File
F_OPEN		equ	3Dh		; Open a File
F_CLOSE		equ	3Eh		; Close a File Handle
F_READ		equ	3Fh		; Read From File/Device
F_WRITE		equ	40h		; Write to File/Device
F_DELETE	equ	41h		; Delete a Directory Entry
F_SEEK		equ	42h		; Move a File Pointer
F_ATTR		equ	43h		; Change Attributes
F_IOCTRL	equ	44h		; I/O Control for Devices
F_CURDIR	equ	47h		; Get Current Drive Dir
F_MALLOC	equ	48h		; Allocate Memory
F_FREE		equ	49h		; Free Allocated Memory
F_REALLOC	equ	4Ah		; Modify Allocated Memory
F_EXEC		equ	4Bh		; Load/Execute a Program
F_TERM		equ	4Ch		; Terminate a Process
F_GETCHILD	equ	4Dh		; Get Child's Return Code
F_FINDDIR	equ	4Eh		; Find Match File
F_NEXTDIR	equ	4Fh		; Find Next Dir Entry
F_SETPSP	equ	50h		; Set PSP Address
F_GETPSP	equ	51h		; Get PSP Address
F_PARMTBL	equ	52h		;*Get System Parameter Address
F_RENAME	equ	56h		; Move a Dir Entry
F_STAMP		equ	57h		; Get/Set File Time & Date

;--- DOS error code ---

ENOFILE 	equ	2
ENOPATH 	equ	3
EMFILE	 	equ	4
EACCES		equ	5
ECONTR		equ	7
ENOMEM		equ	8

;--- DOS file attribute ---

FA_RDONLY	equ	00000001b
FA_HIDDEN	equ	00000010b
FA_SYSTEM	equ	00000100b
FA_LABEL	equ	00001000b
FA_BINARY	equ	00001000b
FA_DIREC	equ	00010000b
FA_ARCH		equ	00100000b
FA_SEL		equ	10000000b

;--- DOS open mode ---

O_READ		equ	0
O_WRITE		equ	1
O_UPDATE	equ	2

;--- Macro definitions ---

IFDEF STDSEG
cgroup		group	_TEXT,_DATA

_TEXT		segment	byte public 'TEXT'
_TEXT		ends
_DATA		segment	word public 'DATA'
_DATA		ends

cseg		macro	name
_TEXT		segment
		assume	cs:cgroup, ds:cgroup
		endm

endcs		macro
_TEXT		ends
		endm

dseg		macro
_DATA		segment
		endm

endds		macro
_DATA		ends
		endm
ENDIF

tst	macro	reg
	or	reg,reg
	endm

tstb	macro	label
	cmp	byte ptr label,0
	endm

tstw	macro	label
	cmp	word ptr label,0
	endm

clr	macro	reg
	xor	reg,reg
	endm

stz	macro
	xor	al,al
	endm

clz	macro
	or	al,-1
	endm

outi	macro	port,imm
	mov	al,imm
	out	port,al
	endm

jmps	macro	label
	jmp	short label
	endm

jmpl	macro	cc,label
	local	next
	jn&cc	next
	jmp	label
next:
	endm

jmpln	macro	cc,label
	local	next
	j&cc	next
	jmp	label
next:
	endm

jmpw	macro
	local	next
	jmps	next
next:
	endm

ldseg	macro	seg1,data
	mov	ax,data
	mov	seg1,ax
	endm

movseg	macro	seg1,seg2
	push	seg2
	pop	seg1
	endm

msdos	macro	cmd,prm
ifb	<prm>
	mov	ah,cmd
else
	movhl	ax,cmd,prm
endif
	int	21h
	endm

emm	macro	cmd,prm
ifb	<prm>
	mov	ah,cmd
else
	movhl	ax,cmd,prm
endif
	int	INT_EMS
	endm

pushm	macro	regs
	irp	reg,<regs>
	push	reg
	endm
	endm

popm	macro	regs
	irp	reg,<regs>
	pop	reg
	endm
	endm

pushall	macro	regs
IFDEF i186
	pusha
ELSE
	pushm	<ax,bx,cx,dx,si,di,bp>
ENDIF
ifnb	<regs>
	pushm	<regs>
endif
	endm

popall	macro	regs
ifnb	<regs>
	popm	<regs>
endif
IFDEF i186
	popa
ELSE
	popm	<bp,di,si,dx,cx,bx,ax>
ENDIF
	endm

shiftm	macro	op,reg,imm
IFDEF i186
	op	reg,imm
ELSE
	rept	imm
	op	reg,1
	endm
ENDIF
	endm

shlm	macro	reg,imm
	shiftm	shl,reg,imm
	endm

shrm	macro	reg,imm
	shiftm	shr,reg,imm
	endm

sarm	macro	reg,imm
	shiftm	sar,reg,imm
	endm

rolm	macro	reg,imm
	shiftm	rol,reg,imm
	endm

rorm	macro	reg,imm
	shiftm	ror,reg,imm
	endm

rclm	macro	reg,imm
	shiftm	rcl,reg,imm
	endm

rcrm	macro	reg,imm
	shiftm	rcr,reg,imm
	endm

movhl	macro	wreg,imm_h,imm_l
	mov	wreg,((imm_h) shl 8)+((imm_l) and 0FFh)
	endm

bios	macro	cmd
	mov	ah,cmd
	int	18h
	endm

eios	macro	cmd
	mov	cl,cmd
	int	0DCh
	endm

bios_v	macro	cmd,prm
ifb	<prm>
	mov	ah,cmd
else
	movhl	ax,cmd,prm
endif
	int	10h
	endm

bios_k	macro	cmd,prm
ifb	<prm>
	mov	ah,cmd
else
	movhl	ax,cmd,prm
endif
	int	16h
	endm

peek	macro	sym
IFDEF DEBUG
	public	sym
ENDIF
	endm

skip1		macro	reg
		jmps	$+3
		endm

skip2		macro	reg
		jmps	$+4
		endm

;--- Far pointer record ---

_farptr	struc
@off	dw	?
@seg	dw	?
_farptr	ends

;----- wchar handle -----

IFDEF US
ifkanji		macro	kjyes
		endm
ELSE
ifkanji		macro	kjyes
		local	kjno
		cmp	al,81h
		jb	kjno
		cmp	al,9Fh
		jbe	kjyes
		cmp	al,0E0h
		jb	kjno
		cmp	al,0FCh
		jbe	kjyes
kjno:
		endm	
ENDIF

mstojis         macro
IF 0
		cmp	al, 80h
		adc	al, 61h
	_ifn c
		sub	al, 0A2h
	_endif
		rcl	ah, 1
		add	ax, 1F21h
		and	ah, 7Fh
ENDIF
; 8140h...FCFCh
                and     ah, 3fh
                shl     ah, 1
                sub     al, 9fh
        _ifn ae
                dec     ah
                cmp     al, 0e1h
                adc     al, 5fh
        _endif
                sbb     ax, 0dfdfh
                endm

;--- Block statement ---

IFDEF	NEWBLOCK
	include	block.inc
ELSE

	$base	=	0
	$field	=	1
	$add	=	1
	$nest	=	0

_if	macro	cc
	$begin
	$jcc	n,cc,%$base,%$field
	endm

_ifn	macro	cc
	$begin
	$jcc	<>,cc,%$base,%$field
	endm

_else	macro
	$end
	$jmp	%$base,%($field+1)
	$label	%$base,%$field
	$field	=	$field+1
	endm

_endif	macro
	$end
	$label	%$base,%$field
	$end2
	endm

_repeat	macro
	$begin
	$label	%$base,%$field
	endm

_until	macro	cc
	$end
	$jcc	n,cc,%$base,%$field
	$label	%$base,%($field+1)
	$end2
	endm

_while	macro	cc
	$end
	$jcc	<>,cc,%$base,%$field
	$label	%$base,%($field+1)
	$end2
	endm

_loop	macro
	$end
	$loop	%$base,%$field
	$label	%$base,%($field+1)
	$end2
	endm

_break	macro	cc
	$jcc	<>,cc,%$base,%($field+1)
	endm

_cont	macro	cc
	$jcc	<>,cc,%$base,%$field
	endm

$begin	macro
	$field	=	$field shl 1
	$nest	=	$nest + 1
	endm

$end	macro
if ($nest le 1)
	$add	=	1
	$base	=	$base mod 256
else
	$add	=	16 shl ($nest * 2)
	$base	=	$base mod ($add * 4)
endif
	endm

$end2	macro
	$base	=	$base + $add
	$field	=	$field shr 1
	$nest	=	$nest - 1
	endm

$jmp	macro	l1,l2
	jmp	short $L&l1&_&l2
	endm

$jcc	macro	n,cc,l1,l2
ifnb	<cc>
	j&n&cc	$L&l1&_&l2
else
	$jmp	l1,l2
endif
	endm

$loop	macro	l1,l2
	loop	$L&l1&_&l2
	endm

$label	macro	l1,l2
$L&l1&_&l2:
	endm

ENDIF

;****************************
;	End of 'std.inc'
; Copyright (C) 1989 by c.mos
;****************************
	.list	
