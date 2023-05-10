@ECHO OFF
REM Build Everything

ECHO Building everything...

for /f "tokens=*" %%f in ('wmic cpu get NumberOfLogicalProcessors /value ^| find "="') do set %%f

if not exist zodiac_lib\dyncall\dyncall-1.4\dyncall\dyncall_s.lib (
    pushd zodiac_lib\dyncall\dyncall-1.4
    call configure.bat
    nmake -f Nmakefile
    popd
)

make -j%NumberOfLogicalProcessors% -f Makefile.windows.mak all
IF %ERRORLEVEL% NEQ 0 (echo Error:%ERRORLEVEL% && exit)

ECHO All assemblies built successfully.