echo off
	if not "%1" == "" goto UPDATE
	echo *
	echo   Usage: mksrc ドライブ名
	echo *
	goto END
:ERR
	echo *
	echo   %1\SRC\SRC.EXE が見つかりません
	echo *
	goto END
:UPDATE
	echo *
	echo   ドライブ %1 に VZ 1.6 オリジナルディスクを入れてください.
	echo *
	pause
	if not exist %src\src.exe goto ERR
	%1src\src
	copy vz.inc vzorg.inc
	copy key98.asm keyfm.asm
	copy scrn98.asm scrnfm.asm
	copy vzfm.inc vz.inc
	for %%f in (*.bdf) do bupdate -o %%f
	echo *
	if not errorlevel 1 echo   お疲れさま. 更新終了です.
:END
