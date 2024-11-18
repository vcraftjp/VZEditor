echo off
rem	-------	Link VZ, other tools -------
rem	usage: lk [vmap, etc.]
rem
rem	%LNK%	:linker name cap (t, ) ex. set lnk=t

if not "%1"=="" goto link_tool
set LNKFILE=@vz.lnk
set LNKCOM=vz%HARD%
goto start

:link_tool
set LNKFILE=%1
set LNKCOM=%1

:start
if "%LNK%"=="t" goto tlink

:mslink
link /m /noi %LNKFILE%,%LNKCOM%,%LNKCOM%;
exe2bin %LNKCOM%.exe %LNKCOM%.com
del %LNKCOM%.exe
goto done

:tlink
tlink /c /m /t %LNKFILE%,%LNKCOM%,%LNKCOM%;
if errorlevel 1 goto end

:done
if exist %LNKCOM%.sym del %LNKCOM%.sym
dir %LNKCOM%.com

:end
set LNKFILE=
set LNKCOM=
