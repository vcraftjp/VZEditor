echo off
rem	-------	Make vz???.com -------
rem	usage: mk [98,J31,IBM,IBMJ,AX,55,US]
rem	       mk vmap
rem
rem	%ASM%	:assembler name cap (opt,t,m)  ex: set asm=opt

if exist dummy goto init
type nul >dummy

:init
if not "%1"=="" goto init2
if not "%HARD%"=="" goto makevz
goto mk_98

:init2
type nul >dummy
for %%a in (98 j31 J31 ibm IBM ibmj IBMJ ax AX 55 us US) do if "%1"=="%%a" goto mk_%%a
command /c ac %1
lk %1

:mk_98
set HARD=
set masm=/dPC98
goto makevz

:mk_J31
set HARD=J31
set masm=/dJ31
goto makevz

:mk_IBM
set HARD=IBM
set masm=/dIBMV
goto makevz

:mk_IBMJ
set HARD=IBMJ
set masm=/dIBMJ
goto makevz

:mk_AX
set HARD=AX
set masm=/dIBMAX
goto makevz

:mk_55
set HARD=55
set masm=/dJBM
goto makevz

:mk_US
set HARD=US
set masm=/dUS
goto makevz

:makevz
echo ===== Make VZ%HARD% %2 =====
set %ASM%asm=%MASM%
if "%ASM%"=="opt" goto optmake
if "%ASM%"=="t" goto tmake
make vz.mak
goto link

:optmake
optasm @vz.omk
goto link

:tmake
make -fvz.mak
goto link

:link
if errorlevel 1 goto error
lk

:error
echo 
