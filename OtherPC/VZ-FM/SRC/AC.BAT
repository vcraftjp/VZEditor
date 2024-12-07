echo off
rem	-------	Assemble a source -------
rem		... called by cmos.def [f¥10],[SHIFT]+[f¥10]
rem
rem	%ASM%	:assembler name cap (opt,t,m) ex. set asm=opt
rem	%MASM%	:assemble option (/ml) ex. set masm=/ml

echo ===== %ASM%asm %1 =====
if not "%2"=="" goto asm2
if not "%HARD%"=="" goto asm1d
%ASM%asm %MASM% %1;
goto end

:asm1d
%ASM%asm %MASM% /d%HARD% %1;
goto end

:asm2
if not "%HARD%"=="" goto asm2d
%ASM%asm %MASM% /%2 %1;
goto end

:asm2d
%ASM%asm %MASM% /d%HARD% /%2 %1;
goto end

:end
if errorlevel 1 echo 
