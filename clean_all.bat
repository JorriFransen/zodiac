@ECHO OFF
REM Clean everything

ECHO "Cleaning everyting..."

make -f Makefile.windows.mak clean
IF %ERRORLEVEL% NEQ 0 (echo Error:%ERRORLEVEL% && exit)
