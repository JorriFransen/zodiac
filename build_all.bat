@ECHO OFF
REM Build Everything

ECHO Building everything...

make -f Makefile.windows.mak all
IF %ERRORLEVEL% NEQ 0 (echo Error:%ERRORLEVEL% && exit)

ECHO All assemblies built successfully.