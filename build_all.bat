@ECHO OFF
REM Build Everything

ECHO Building everything...

make -f Makefile.zodiac_lib.windows.mak all
IF %ERRORLEVEL% NEQ 0 (echo Error:%ERRORLEVEL% && exit)

make -f Makefile.zodiac_driver.windows.mak all
IF %ERRORLEVEL% NEQ 0 (echo Error:%ERRORLEVEL% && exit)

make -f Makefile.zodiac_tests.windows.mak all
IF %ERRORLEVEL% NEQ 0 (echo Error:%ERRORLEVEL% && exit)

ECHO All assemblies built successfully.