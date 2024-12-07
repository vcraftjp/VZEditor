;----- New Block statement -----

$base = 0
$shft = 1
$bfld = 0
$here = 0

_if		macro	cc
		$jcc	n,cc,%$base
		$begin
		endm

_ifn		macro	cc
		$jcc	<>,cc,%$base
		$begin
		endm

_else		macro
		$end
		$jmp	%$base
		$label	%$here
		$begin
		endm

_endif		macro
		$end
		$label	%$here
		endm

_repeat		macro
		$label	%$base,_
		$begin
		endm

_until		macro	cc
		$end
		$jcc	n,cc,%$here,_
		$label	%$here
		endm

_while		macro	cc
		$end
		$jcc	<>,cc,%$here,_
		$label	%$here
		endm

_loop		macro
		$end
		$loop	%$here,_
		$label	%$here
		endm

_break		macro	cc
		$cont
		$jcc	<>,cc,%$here
		endm

_cont		macro	cc
		$cont
		$jcc	<>,cc,%$here,_
		endm

$begin		macro
		$base = $base + 1
		$bfld = $bfld or $shft
		$shft = $shft shl 1
		endm

$end		macro
		$here = $base
		$test = $shft
		rept	16
		$test = $test shr 1
		$here = $here - 1
	if ($test le $bfld)
		$bfld = $bfld and (not $test)
	  ife ($bfld)
		$shft = 1
	  endif
		exitm
	endif
		endm
		endm

$cont		macro
		$here = $base
		$test = $shft
		rept	16
		$test = $test shr 1
		$here = $here - 1
	if ($test le $bfld)
		exitm
	endif
		endm
		endm

$jmp		macro	L1,L2
		jmp	short $L&L1&L2
		endm

$jcc		macro	n,cc,L1,L2
	ifnb <cc>
		j&n&cc	$L&L1&L2
	else
		$jmp	L1,L2
	endif
		endm

$loop		macro	L1,L2
		loop	$L&L1&L2
		endm

$label		macro	L1,L2
$L&L1&L2:
		endm

;----- End of block.inc -----
