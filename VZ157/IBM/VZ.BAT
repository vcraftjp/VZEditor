echo off
vzsel
if errorlevel 5 goto AX
if errorlevel 3 goto IBM
if errorlevel 2 goto J31
rem if errorlevel 1 goto PC98
echo Unknown Hardware.
goto end

:PC98
set VZ=
goto run_vz

:J31
set VZ=J31
goto run_vz

:IBM
set VZ=IBM
goto run_vz

:AX
set VZ=IBM /VZAX
goto run_vz

:run_vz
vz%VZ% %1 %2 %3 %4 %5 %6 %7 %8 %9
