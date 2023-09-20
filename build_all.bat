@ECHO OFF
REM Build Everything

ECHO Building everything...

for /f "tokens=*" %%f in ('wmic cpu get NumberOfLogicalProcessors /value ^| find "="') do set %%f

SET VDL_FLAGS="ZODIAC_VDL=0"

SET FLAGS=%VDL_FLAGS%

make -j%NumberOfLogicalProcessors% -f Makefile.windows.mak all %FLAGS%
IF %ERRORLEVEL% NEQ 0 (echo Error:%ERRORLEVEL% && exit)

ECHO All assemblies built successfully.