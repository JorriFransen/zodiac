@ECHO OFF
REM Clean everything

ECHO "Cleaning everyting..."

make -f Makefile.zodiac_lib.windows.mak clean
IF %ERRORLEVEL% NEQ 0 (echo Error:%ERRORLEVEL% && exit)

make -f Makefile.zodiac_driver.windows.mak clean
IF %ERRORLEVEL% NEQ 0 (echo Error:%ERRORLEVEL% && exit)

make -f Makefile.zodiac_tests.windows.mak clean
IF %ERRORLEVEL% NEQ 0 (echo Error:%ERRORLEVEL% && exit)