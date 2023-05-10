@ECHO OFF
REM Clean everything

ECHO "Cleaning everyting..."

make -f Makefile.windows.mak clean
IF %ERRORLEVEL% NEQ 0 (echo Error:%ERRORLEVEL% && exit)


ECHO "Cleaning dyncall..."
pushd zodiac_lib\dyncall\dyncall-1.4
nmake -f Nmakefile clean
popd