echo generate "ZGuard.def"...
impdef "ZGuard.def" "..\ZGuard.dll"
if %ERRORLEVEL% GTR 0 goto :lError_gen_def

echo make "ZGuard.lib"...
implib "ZGuard.lib" "ZGuard.def"
if %ERRORLEVEL% GTR 0 goto :lError_makelib

echo Done.
goto :finish

:lError_gen_def
echo generate ZGuard.def error!
pause
goto :finish

:lError_makelib
echo make ZGuard.lib error!
pause
goto :finish

:finish
echo.
