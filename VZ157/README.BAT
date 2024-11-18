echo off
cd \ibm
vzsel >nul
if errorlevel 5 goto AX
if errorlevel 3 goto IBM
if errorlevel 2 goto J31
if errorlevel 1 goto PC98
echo Unknown Hardware.
goto END

:PC98
cd \
vz readme.doc
goto END

:J31
cd \j31
froll
vzj31 vzj31.doc \readme.doc
goto END

:IBM
cd \ibm
vzibm vzibm.doc \readme.doc
goto END

:AX
cd \ibm
vzibm /vzax vzibm.doc \readme.doc
goto END

:END
cd \
