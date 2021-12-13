@echo off

echo generate "ZPort.def"...
impdef "ZPort.def" "..\Split\ZPort.dll"
if %ERRORLEVEL% GTR 0 goto :lError_gen_def

echo make "ZPort.lib"...
implib "ZPort.lib" "ZPort.def"
if %ERRORLEVEL% GTR 0 goto :lError_makelib

echo generate "ZGuard.def"...
impdef "ZGuard.def" "..\Split\ZGuard.dll"
if %ERRORLEVEL% GTR 0 goto :lError_gen_def

echo make "ZGuard.lib"...
implib "ZGuard.lib" "ZGuard.def"
if %ERRORLEVEL% GTR 0 goto :lError_makelib

echo Done.
goto :finish

:lError_gen_def
echo generate *.def error!
pause
goto :finish

:lError_makelib
echo make *.lib error!
pause
goto :finish

:finish
echo.
