@echo off
echo off
cls
rem ===========================================================
rem 	VZ Editor  Version 1.6  Install program
rem ===========================================================

set VZDEF=
if not "%1"=="" goto START
echo Usage: " A>install c:<Enter> or A>install c: \bin<Enter> "
echo   (install from A: to C:\VZ  or C:\BIN)
goto END

:START
set VZDIR=\VZ
if not "%2"=="" set VZDIR=%2
if not exist %1%VZDIR%\*.* md %1%VZDIR%
cd %1%VZDIR%

vzsel >nul
if errorlevel 7 goto PS55
if errorlevel 6 goto DOSV
if errorlevel 5 goto AX
if errorlevel 3 goto IBM
if errorlevel 2 goto J31
if errorlevel 1 goto PC98

:ERROR
echo *** 動作機種が違います(Illegal hardware).
goto END

:PC98
echo ＰＣ-９８０１版ＶＺをドライブ %1%VZDIR% にインストールします.
pause
zcopy . %1
goto DONE

:J31
echo Ｊ-３１００版ＶＺをドライブ %1%VZDIR% にインストールします.
pause
copy j31\vzj31.* %1vz.*
copy j31\froll.com %1
zcopy @j31\instj31.lst %1
goto DONE

:DOSV
echo ＤＯＳ／Ｖ版ＶＺをドライブ %1%VZDIR% にインストールします.
pause
zcopy . %1
goto DONE

:AX
echo ＡＸ版ＶＺをドライブ %1%VZDIR% にインストールします.
pause
zcopy ax %1
copy vzibmj.def+%1ax.def %1vzax.def
zcopy . %1
goto DONE

:PS55
echo ＰＳ／５５版ＶＺをドライブ %1%VZDIR% にインストールします.
pause
zcopy ps55 %1
copy vzibmj.def+%1ps55.def %1vzax.def
zcopy . %1
goto DONE

:IBM
echo Install VZ Editor US version to Drive %1%VZDIR%
pause
copy us\vzus.* %1vz.*
copy vzibm.def %1vz.def
zcopy @us\instus.lst %1
goto END

:DONE
echo:
echo ＶＺ Editorのインストールは完了しました.
echo      vz [ﾘﾀｰﾝ] で起動します.
echo:

:END
%1
