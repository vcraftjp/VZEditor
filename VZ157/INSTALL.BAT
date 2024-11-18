echo off
rem ===========================================================
rem 	VZ Editor PC9801, J-3100, IBM PC/AX Version 1.57
rem 	Install program		by c.mos
rem ===========================================================

if not "%1"=="" goto START
echo Usage: " A:\>install c:<Return>     (install from A: to C:\)"
echo        " A:\>install c: \vz<Return> (install from A: to C:\VZ)"
echo                        ^(space)
goto END

:START
set VZDIR=
if "%2"=="" goto VZSEL
set VZDIR=%2
if not exist %1%VZDIR%\*.* md %1%VZDIR%
cd %1%VZDIR%

:VZSEL
cd \ibm
vzsel >nul
cd \
if errorlevel 5 goto AX
if errorlevel 3 goto IBM
if errorlevel 2 goto J31
if errorlevel 1 goto PC98

:ERROR
echo ★動作機種が違います(Illegal hardware).
goto END

:PC98
echo ★PC-9801版ＶＺをドライブ %1%VZDIR% にインストールします.
dir %1 /w
echo:
pause
copy readme.doc %1
copy vz.com %1
copy vz.def %1
copy ezkey.com %1
copy tool.def %1
copy block.def %1
copy keisen.def %1
copy zenhan.def %1
copy game.def %1
copy moremac.def %1
goto DONE

:J31
echo ★J-3100版ＶＺをドライブ %1%VZDIR% にインストールします.
dir %1 /w
echo:
pause
copy readme.doc %1
copy \j31\vzj31.com %1vz.com
copy \j31\vzj31.def %1vz.def
copy \j31\froll.com %1
copy tool.def %1
copy block.def %1
copy keisen_j.def %1keisen.def
copy zenhan.def %1
copy \j31\gamej31.def %1game.def
copy moremac.def %1
copy \j31\vzj31.doc %1
goto DONE

:IBM
echo Install VZ Editor IBM PC version to Drive %1%VZDIR%
dir %1 /w
echo:
pause
copy \ibm\vzibm.com %1vz.com
copy \ibm\vzibm.def %1vz.def
goto END

:AX
echo ★IBM PC/AX版ＶＺをドライブ %1%VZDIR% にインストールします.
dir %1 /w
echo:
pause
copy readme.doc %1
copy \ibm\vz.bat %1
copy \ibm\vzibm.com %1
copy \ibm\vzibm.def %1
copy \ibm\vzax.def %1
copy \ibm\vzsel.com %1
copy \ibm\vzibm.doc %1
goto DONE

:DONE
copy vmap.com %1
%1
dir
set VZDEF=
echo:
echo ★インストールを完了しました. vz[ﾘﾀｰﾝ]で起動します.

:END
