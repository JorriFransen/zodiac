@ECHO OFF
REM Build Everything

ECHO Building everything...

for /f "tokens=*" %%f in ('wmic cpu get NumberOfLogicalProcessors /value ^| find "="') do set %%f

make -j%NumberOfLogicalProcessors% -f Makefile.windows.mak all
IF %ERRORLEVEL% NEQ 0 (echo Error:%ERRORLEVEL% && exit)

ECHO All assemblies built successfully.