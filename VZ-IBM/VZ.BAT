@echo off
echo off
vzsel >nul
if errorlevel 8 goto PS55_MONO
if errorlevel 7 goto PS55
if errorlevel 6 goto DOSV
if errorlevel 5 goto AX
if errorlevel 4 goto IBM_MONO
if errorlevel 3 goto IBM
if errorlevel 2 goto J31

:ERROR
echo *** “®ì‹@Ží‚ªˆá‚¢‚Ü‚·(Illegal hardware).
goto END

:J31
vz %1 %2 %3 %4 %5 %6 %7 %8 %9
goto END

:IBM
vzibm %1 %2 %3 %4 %5 %6 %7 %8 %9
goto END

:IBM_MONO
vzibm +bw %1 %2 %3 %4 %5 %6 %7 %8 %9
goto END

:DOSV
vzibmj %1 %2 %3 %4 %5 %6 %7 %8 %9
goto END

:AX
vzax %1 %2 %3 %4 %5 %6 %7 %8 %9
goto END

:PS55_MONO
mode 3

:PS55
if exist ps55\vz55.com goto PS55_FD
fast   >nul
cursor >nul
vz55 %1 %2 %3 %4 %5 %6 %7 %8 %9
goto END

:PS55_FD
ps55\fast   >nul
ps55\cursor >nul
ps55\vz55 %1 %2 %3 %4 %5 %6 %7 %8 %9
goto END


:END
echo:
